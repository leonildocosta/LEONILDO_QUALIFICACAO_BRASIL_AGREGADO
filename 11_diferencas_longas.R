# 11_diferencas_longas.R
# Diferenças longas: a exportação está associada a ganhos de produtividade
# ao longo de vários anos? (Brasil agregado, produto x ano)
#
# Por que: o aprendizado pela exportação, se existir, é gradual (tecnologia,
# padrões de qualidade, escala). O modelo ano a ano com tendência por produto
# (scripts 09 e 10) foi desenhado para não ver esse tipo de efeito, e as
# primeiras diferenças de um ano são dominadas pela oscilação da safra (ver o
# ponto 8 da lista para o orientador). Comparar janelas de vários anos reduz o
# peso da safra de um ano só e deixa aparecer mudanças de prazo mais longo.
#
# Três abordagens:
#   A. Diferenças de k anos (k = 5 e 10), em painel anual:
#        ln_rend(t) - ln_rend(t-k)  explicado por  x(t-1) - x(t-1-k)
#      com efeito de ano. Erros de Driscoll e Kraay com defasagem k, porque as
#      diferenças de anos vizinhos se sobrepõem.
#   B. Painel de períodos (4 períodos de 7 anos): médias por produto e período;
#      a exportação média do período anterior explica o rendimento médio do
#      período seguinte, com efeitos de produto e de período. Erros agrupados por
#      produto.
#   C. Uma diferença longa só (corte transversal): variação do rendimento médio
#      entre 2001-2005 e 2020-2024 contra a variação da exportação entre as
#      mesmas janelas; e contra a exportação inicial (os produtos que já
#      exportavam em 2001-2005 ganharam mais produtividade depois?). Erros
#      robustos (HC1). Controle: rendimento inicial (convergência).
#      Começa em 2001 porque as frutas só têm quantidade em toneladas a partir de 2001.
#
# Medidas da exportação (as do script 10, sem a de denominador t-2):
#   original   = exportação / produção do mesmo ano
#   denom_fixo = exportação / produção média do produto (sem viés do denominador)
#   ln_exp     = ln(1 + toneladas exportadas)
# Saída: 07_testes/diferencas_longas.xlsx

source(here::here("scripts", "funcoes_comuns.R"))
for (p in c("plm", "lmtest", "sandwich")) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
library(dplyr)
library(tidyr)
library(writexl)
library(here)
library(plm)
library(lmtest)
library(sandwich)

arq_pan <- here("05_painel", "dados", "painel_pam_comex_final_1997_2024.rds")
exigir_arquivo(arq_pan, "06_painel_merge_pam_comex_1997_2024.R")
painel <- readRDS(arq_pan)
pasta <- here("07_testes")
dir.create(pasta, recursive = TRUE, showWarnings = FALSE)

MEDIDAS <- c(original   = "Intensidade original: exportação / produção do ano",
             denom_fixo = "Denominador fixo: exportação / produção média do produto",
             ln_exp     = "Sem denominador: ln(1 + toneladas exportadas)")

# 0. Base anual (mesma amostra dos scripts 09 e 10) ----------------------------------
base <- painel %>%
  filter(!is.na(status_ncm)) %>%
  mutate(rend = `Rendimento médio da produção`, qtd = `Quantidade produzida`,
         valido = usar_no_modelo & !is.na(rend) & rend > 0 & !is.na(qtd) & qtd > 0) %>%
  filter(valido) %>%
  mutate(exp_t = coalesce(peso_liquido_t, 0)) %>%
  group_by(produto) %>% mutate(qtd_media = mean(qtd)) %>% ungroup() %>%
  transmute(produto, ano, ln_rend = log(rend),
            original = exp_t / qtd, denom_fixo = exp_t / qtd_media, ln_exp = log1p(exp_t))
cat("Base:", nrow(base), "produto-anos,", n_distinct(base$produto), "produtos\n")

# Valor de uma coluna k anos antes, no mesmo produto (NA se o ano não existe)
valor_em <- function(df, var, defasagem) {
  chave <- df %>% transmute(produto, ano = ano + defasagem, v = .data[[var]])
  left_join(df %>% select(produto, ano), chave, by = c("produto", "ano"))$v
}

coef_tab <- function(ct, termo) tibble(beta = ct[termo, 1], erro_padrao = ct[termo, 2], p_valor = ct[termo, 4])

# A. Diferenças de k anos, painel anual ------------------------------------------------
dif_k <- function(k, medida) {
  d <- base %>%
    mutate(dy = ln_rend - valor_em(base, "ln_rend", k),
           dx = valor_em(base, medida, 1) - valor_em(base, medida, 1 + k)) %>%
    filter(!is.na(dy), !is.na(dx))
  pd <- pdata.frame(as.data.frame(d), index = c("produto", "ano"))
  m  <- plm(dy ~ dx, data = pd, model = "within", effect = "time")
  ct <- coeftest(m, vcov = vcovSCC(m, maxlag = k))
  coef_tab(ct, "dx") %>%
    mutate(abordagem = paste0("A. Diferença de ", k, " anos, efeito de ano"),
           medida = MEDIDAS[[medida]], n = nobs(m), produtos = n_distinct(d$produto),
           erro = "Driscoll e Kraay")
}
a_tab <- bind_rows(lapply(c(5, 10), function(k) bind_rows(lapply(names(MEDIDAS), dif_k, k = k))))

# B. Painel de períodos de 7 anos -------------------------------------------------------
periodos <- tibble(ano = 1997:2024, periodo = rep(1:4, each = 7))
medias <- base %>%
  inner_join(periodos, by = "ano") %>%
  summarise(across(c(ln_rend, all_of(names(MEDIDAS))), mean), anos = n(), .by = c(produto, periodo)) %>%
  filter(anos >= 3) %>%                         # pelo menos 3 anos no período
  arrange(produto, periodo) %>%
  group_by(produto) %>%
  mutate(across(all_of(names(MEDIDAS)), ~ if_else(dplyr::lag(periodo) == periodo - 1, dplyr::lag(.x), NA_real_),
                .names = "{.col}_ant")) %>%
  ungroup()

periodo_est <- function(medida) {
  d <- medias %>% filter(!is.na(.data[[paste0(medida, "_ant")]])) %>%
    mutate(x = .data[[paste0(medida, "_ant")]])
  m <- lm(ln_rend ~ x + factor(produto) + factor(periodo), data = d)
  ct <- coeftest(m, vcov = vcovCL(m, cluster = ~ produto, type = "HC1"))
  coef_tab(ct, "x") %>%
    mutate(abordagem = "B. Períodos de 7 anos: exportação do período anterior, efeitos de produto e de período",
           medida = MEDIDAS[[medida]], n = nobs(m), produtos = n_distinct(d$produto),
           erro = "agrupado por produto")
}
b_tab <- bind_rows(lapply(names(MEDIDAS), periodo_est))

# C. Uma diferença longa: 2001-2005 contra 2020-2024 --------------------------------------
JANELA_INI <- 2001:2005
JANELA_FIM <- 2020:2024
janela <- function(anos, sufixo) {
  base %>% filter(ano %in% anos) %>%
    summarise(across(c(ln_rend, all_of(names(MEDIDAS))), mean), anos = n(), .by = produto) %>%
    filter(anos >= 3) %>% select(-anos) %>%
    rename_with(~ paste0(.x, sufixo), -produto)
}
corte <- inner_join(janela(JANELA_INI, "_ini"), janela(JANELA_FIM, "_fim"), by = "produto") %>%
  mutate(dy = ln_rend_fim - ln_rend_ini)

corte_est <- function(medida) {
  d <- corte %>% mutate(dx = .data[[paste0(medida, "_fim")]] - .data[[paste0(medida, "_ini")]],
                        x_ini = .data[[paste0(medida, "_ini")]])
  linha <- function(f, termo, nome) {
    m <- lm(f, data = d)
    coef_tab(coeftest(m, vcov = vcovHC(m, type = "HC1")), termo) %>%
      mutate(abordagem = nome, medida = MEDIDAS[[medida]], n = nobs(m),
             produtos = nobs(m), erro = "robusto (HC1)")
  }
  bind_rows(
    linha(dy ~ dx, "dx", "C1. Variação 2001-05 a 2020-24: variação da exportação"),
    linha(dy ~ dx + ln_rend_ini, "dx", "C2. Idem, com controle pelo rendimento inicial"),
    linha(dy ~ x_ini + ln_rend_ini, "x_ini", "C3. Exportação inicial (2001-05) explica o ganho posterior")
  )
}
c_tab <- bind_rows(lapply(names(MEDIDAS), corte_est))

resultados <- bind_rows(a_tab, b_tab, c_tab) %>%
  select(abordagem, medida, beta, erro_padrao, p_valor, n, produtos, erro) %>%
  mutate(across(where(is.double), ~ signif(.x, 4)))

# Produtos do corte transversal, para olhar caso a caso
corte_produtos <- corte %>%
  transmute(produto,
            rendimento_ini_kg_ha = round(exp(ln_rend_ini)), rendimento_fim_kg_ha = round(exp(ln_rend_fim)),
            ganho_rendimento_pct = round(100 * (exp(dy) - 1), 1),
            intensidade_ini = round(original_ini, 4), intensidade_fim = round(original_fim, 4)) %>%
  arrange(desc(ganho_rendimento_pct))

cat("\n=== DIFERENÇAS LONGAS ===\n"); print(resultados, n = Inf, width = Inf)
cat("\nProdutos no corte 2001-05 x 2020-24:", nrow(corte), "\n")
print(corte_produtos, n = Inf, width = Inf)

leia_me <- tibble(
  aba = c("resultados", "corte_produtos"),
  o_que_mostra = c(
    "Beta da exportação em cada abordagem (A: diferenças de 5 e 10 anos; B: períodos de 7 anos; C: uma diferença longa) e cada medida",
    "Cada produto: rendimento médio em 2001-05 e 2020-24, ganho percentual e intensidade nas duas janelas"),
  leitura = c(
    "Beta positivo e significativo nas medidas sem denominador (denominador fixo e ln da exportação) indicaria associação de longo prazo que não é efeito mecânico",
    "Ajuda a ver quais produtos puxam o resultado")
)
arquivo <- file.path(pasta, "diferencas_longas.xlsx")
write_xlsx(list(leia_me = leia_me, resultados = resultados, corte_produtos = corte_produtos), arquivo)
cat("\nResultados salvos em:", arquivo, "\n")
