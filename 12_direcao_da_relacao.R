# 12_direcao_da_relacao.R
# Qual vem antes: o aumento da exportação ou o ganho de produtividade?
# (Brasil agregado, produto x ano)
#
# Por que: os scripts 10 e 11 mostraram uma associação positiva fraca no longo
# prazo, mas os produtos que já exportavam no início não ganharam mais
# produtividade depois. Isso é compatível com duas histórias opostas:
#   - aprendizado pela exportação: exportar -> ganha produtividade depois;
#   - autosseleção (Melitz, 2003): fica mais produtivo -> passa a exportar mais.
# Este script testa as duas direções com o mesmo procedimento, para comparar.
#
#   A. Causalidade de Granger em painel (Dumitrescu e Hurlin, 2012), com
#      plm::pgrangertest. Pergunta: o passado de X ajuda a prever Y, além do
#      passado do próprio Y? Feito nas duas direções, com 1 e 2 defasagens.
#      H0: X não ajuda a prever Y em NENHUM produto. Rejeitar = ajuda em pelo
#      menos parte dos produtos.
#      Exige séries sem buracos: usa os produtos com todos os anos 2001-2024.
#      Feito com o ln(rendimento) em nível e em diferença (a raiz unitária do
#      rendimento não foi descartada com 2 defasagens).
#      Limitação: o teste supõe produtos independentes; a dependência entre
#      produtos (script 09) pode inflar a rejeição. Ler junto com a parte B.
#   B. Diferenças longas defasadas, nas duas direções (painel anual, k = 5):
#        produtividade: ln_rend(t) - ln_rend(t-5) explicado por x(t-5) - x(t-10)
#        exportação:    x(t) - x(t-5)             explicado por ln_rend(t-5) - ln_rend(t-10)
#      controlando a variação anterior da própria variável. Efeito de ano;
#      erros de Driscoll e Kraay com defasagem 5.
#   C. Três janelas (2001-05, 2010-14, 2020-24), um ponto por produto:
#        a variação da exportação na 1ª década explica a variação da
#        produtividade na 2ª? E o contrário? Erros robustos (HC1).
#
# Medidas da exportação: denominador fixo (exportação / produção média do
# produto) e ln(1 + toneladas exportadas). As duas não têm o viés do denominador.
# Saída: 07_testes/direcao_da_relacao.xlsx

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

MEDIDAS <- c(denom_fixo = "Denominador fixo: exportação / produção média do produto",
             ln_exp     = "ln(1 + toneladas exportadas)")

base <- painel %>%
  filter(!is.na(status_ncm)) %>%
  mutate(rend = `Rendimento médio da produção`, qtd = `Quantidade produzida`,
         valido = usar_no_modelo & !is.na(rend) & rend > 0 & !is.na(qtd) & qtd > 0) %>%
  filter(valido) %>%
  mutate(exp_t = coalesce(peso_liquido_t, 0)) %>%
  group_by(produto) %>% mutate(qtd_media = mean(qtd)) %>% ungroup() %>%
  transmute(produto, ano, ln_rend = log(rend), denom_fixo = exp_t / qtd_media, ln_exp = log1p(exp_t))
cat("Base:", nrow(base), "produto-anos,", n_distinct(base$produto), "produtos\n")

# A. Granger em painel (Dumitrescu e Hurlin) --------------------------------------------
ANOS_GR <- 2001:2024
balanceado <- base %>% filter(ano %in% ANOS_GR) %>%
  group_by(produto) %>% filter(n() == length(ANOS_GR)) %>% ungroup() %>%
  arrange(produto, ano) %>%
  group_by(produto) %>% mutate(d_ln_rend = ln_rend - dplyr::lag(ln_rend)) %>% ungroup()
cat("Granger: ", n_distinct(balanceado$produto), " produtos com todos os anos de ",
    min(ANOS_GR), " a ", max(ANOS_GR), "\n", sep = "")

granger <- function(y, x, ordem, rotulo_y, rotulo_x, dados) {
  d <- dados %>% filter(!is.na(.data[[y]]), !is.na(.data[[x]])) %>%
    group_by(produto) %>%
    # cada produto precisa de variação nas duas séries (senão a regressão dele falha)
    filter(sd(.data[[y]]) > 0, sd(.data[[x]]) > 0, n() == max(n())) %>% ungroup()
  pd <- pdata.frame(as.data.frame(d), index = c("produto", "ano"))
  f  <- as.formula(paste(y, "~", x))
  r  <- tryCatch(pgrangertest(f, data = pd, test = "Ztilde", order = ordem), error = function(e) e)
  if (inherits(r, "error"))
    return(tibble(direcao = paste(rotulo_x, "->", rotulo_y), defasagens = ordem,
                  estatistica_Ztilde = NA_real_, p_valor = NA_real_, produtos = n_distinct(d$produto),
                  observacao = paste("Não rodou:", conditionMessage(r))))
  tibble(direcao = paste(rotulo_x, "->", rotulo_y), defasagens = ordem,
         estatistica_Ztilde = unname(r$statistic), p_valor = r$p.value,
         produtos = n_distinct(d$produto),
         observacao = "H0: X não ajuda a prever Y em nenhum produto")
}

a_tab <- bind_rows(lapply(names(MEDIDAS), function(m) bind_rows(lapply(1:2, function(k) bind_rows(
  granger("ln_rend", m, k, "ln(rendimento)", MEDIDAS[[m]], balanceado),
  granger(m, "ln_rend", k, MEDIDAS[[m]], "ln(rendimento)", balanceado),
  granger("d_ln_rend", m, k, "variação do ln(rendimento)", MEDIDAS[[m]], filter(balanceado, ano > min(ANOS_GR))),
  granger(m, "d_ln_rend", k, MEDIDAS[[m]], "variação do ln(rendimento)", filter(balanceado, ano > min(ANOS_GR)))
))))) %>% mutate(across(where(is.double), ~ signif(.x, 4)))

# B. Diferenças longas defasadas, nas duas direções -------------------------------------
K <- 5
valor_em <- function(df, var, defasagem) {
  chave <- df %>% transmute(produto, ano = ano + defasagem, v = .data[[var]])
  left_join(df %>% select(produto, ano), chave, by = c("produto", "ano"))$v
}
dif_def <- function(medida) {
  d <- base %>% mutate(
    dy_agora = ln_rend - valor_em(base, "ln_rend", K),
    dy_antes = valor_em(base, "ln_rend", K) - valor_em(base, "ln_rend", 2 * K),
    dx_agora = .data[[medida]] - valor_em(base, medida, K),
    dx_antes = valor_em(base, medida, K) - valor_em(base, medida, 2 * K)) %>%
    filter(!is.na(dy_agora), !is.na(dy_antes), !is.na(dx_agora), !is.na(dx_antes))
  pd <- pdata.frame(as.data.frame(d), index = c("produto", "ano"))
  m1 <- plm(dy_agora ~ dx_antes + dy_antes, data = pd, model = "within", effect = "time")
  m2 <- plm(dx_agora ~ dy_antes + dx_antes, data = pd, model = "within", effect = "time")
  c1 <- coeftest(m1, vcov = vcovSCC(m1, maxlag = K))
  c2 <- coeftest(m2, vcov = vcovSCC(m2, maxlag = K))
  bind_rows(
    tibble(direcao = "Exportação -> produtividade",
           pergunta = paste0("A variação da exportação em 5 anos antecede a variação do rendimento nos 5 anos seguintes?"),
           beta = c1["dx_antes", 1], erro_padrao = c1["dx_antes", 2], p_valor = c1["dx_antes", 4]),
    tibble(direcao = "Produtividade -> exportação",
           pergunta = paste0("A variação do rendimento em 5 anos antecede a variação da exportação nos 5 anos seguintes?"),
           beta = c2["dy_antes", 1], erro_padrao = c2["dy_antes", 2], p_valor = c2["dy_antes", 4])
  ) %>% mutate(medida = MEDIDAS[[medida]], n = nobs(m1), produtos = n_distinct(d$produto),
               erro = "Driscoll e Kraay, defasagem 5")
}
b_tab <- bind_rows(lapply(names(MEDIDAS), dif_def)) %>%
  select(direcao, medida, pergunta, everything()) %>%
  mutate(across(where(is.double), ~ signif(.x, 4)))

# C. Três janelas: 2001-05, 2010-14, 2020-24 ---------------------------------------------
janela <- function(anos, sufixo) {
  base %>% filter(ano %in% anos) %>%
    summarise(across(c(ln_rend, all_of(names(MEDIDAS))), mean), anos = n(), .by = produto) %>%
    filter(anos >= 3) %>% select(-anos) %>% rename_with(~ paste0(.x, sufixo), -produto)
}
tres <- janela(2001:2005, "_1") %>%
  inner_join(janela(2010:2014, "_2"), by = "produto") %>%
  inner_join(janela(2020:2024, "_3"), by = "produto")

c_est <- function(medida) {
  d <- tres %>% mutate(
    dy_1 = ln_rend_2 - ln_rend_1, dy_2 = ln_rend_3 - ln_rend_2,
    dx_1 = .data[[paste0(medida, "_2")]] - .data[[paste0(medida, "_1")]],
    dx_2 = .data[[paste0(medida, "_3")]] - .data[[paste0(medida, "_2")]])
  m1 <- lm(dy_2 ~ dx_1 + dy_1, data = d)
  m2 <- lm(dx_2 ~ dy_1 + dx_1, data = d)
  c1 <- coeftest(m1, vcov = vcovHC(m1, type = "HC1"))
  c2 <- coeftest(m2, vcov = vcovHC(m2, type = "HC1"))
  bind_rows(
    tibble(direcao = "Exportação -> produtividade",
           pergunta = "Variação da exportação (2001-05 a 2010-14) explica a variação do rendimento (2010-14 a 2020-24)?",
           beta = c1["dx_1", 1], erro_padrao = c1["dx_1", 2], p_valor = c1["dx_1", 4]),
    tibble(direcao = "Produtividade -> exportação",
           pergunta = "Variação do rendimento (2001-05 a 2010-14) explica a variação da exportação (2010-14 a 2020-24)?",
           beta = c2["dy_1", 1], erro_padrao = c2["dy_1", 2], p_valor = c2["dy_1", 4])
  ) %>% mutate(medida = MEDIDAS[[medida]], n = nobs(m1), produtos = nobs(m1), erro = "robusto (HC1)")
}
c_tab <- bind_rows(lapply(names(MEDIDAS), c_est)) %>%
  select(direcao, medida, pergunta, everything()) %>%
  mutate(across(where(is.double), ~ signif(.x, 4)))

# Saída ------------------------------------------------------------------------------
cat("\n=== A. GRANGER EM PAINEL (DUMITRESCU E HURLIN) ===\n"); print(a_tab, n = Inf, width = Inf)
cat("\n=== B. DIFERENÇAS DE 5 ANOS DEFASADAS, NAS DUAS DIREÇÕES ===\n"); print(b_tab, n = Inf, width = Inf)
cat("\n=== C. TRÊS JANELAS (2001-05, 2010-14, 2020-24) ===\n"); print(c_tab, n = Inf, width = Inf)

leia_me <- tibble(
  aba = c("a_granger", "b_diferencas_5_anos", "c_tres_janelas"),
  o_que_mostra = c(
    "Teste de Dumitrescu e Hurlin nas duas direções, 1 e 2 defasagens, rendimento em nível e em diferença",
    "A variação de uma variável em 5 anos antecede a variação da outra nos 5 anos seguintes? Nas duas direções",
    "O mesmo, com médias de 3 janelas de 5 anos e um ponto por produto"),
  leitura = c(
    "Rejeitar H0 numa direção e não na outra indica qual variável antecede. O teste supõe produtos independentes: ler com B e C",
    "Coeficiente significativo só numa direção indica qual variável antecede a outra",
    "Idem, com observações independentes (sem sobreposição)")
)
arquivo <- file.path(pasta, "direcao_da_relacao.xlsx")
write_xlsx(list(leia_me = leia_me, a_granger = a_tab, b_diferencas_5_anos = b_tab, c_tres_janelas = c_tab), arquivo)
cat("\nResultados salvos em:", arquivo, "\n")
