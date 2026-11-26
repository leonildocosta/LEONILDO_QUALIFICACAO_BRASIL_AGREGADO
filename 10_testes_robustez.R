# 10_testes_robustez.R
# Testes de robustez do coeficiente da exportação (Brasil agregado, produto x ano).
#
# Por que este script existe: no script 09, em primeiras diferenças, a
# intensidade exportadora do ano anterior aparece associada a mais rendimento
# (beta perto de 0,41, p de Driscoll e Kraay perto de 0,055). Esse resultado pode
# ser mecânico. A intensidade é exportação / produção. Num ano de safra ruim
# (t-1), a produção cai, a exportação continua (em parte com estoque) e a
# intensidade sobe só porque o denominador encolheu. No ano seguinte (t), o
# rendimento volta ao normal. Resultado: intensidade alta seguida de rendimento
# em alta, sem efeito nenhum da exportação ("viés do denominador" + volta à média).
#
# O que o script faz:
#   A. Troca o denominador da intensidade, para que a produção do próprio ano
#      t-1 não entre na variável explicativa:
#        1. intensidade original:         exportação(t-1) / produção(t-1)
#        2. denominador fixo:             exportação(t-1) / produção média do produto
#        3. denominador defasado:         exportação(t-1) / produção(t-2)
#        4. sem denominador:              ln(1 + toneladas exportadas em t-1)
#      Se o beta continuar positivo e parecido nas versões 2 a 4, o resultado
#      não é mecânico. Se sumir, era o denominador.
#   B. Controla diretamente a volta à média: acrescenta o ln(rendimento) de t-1
#      (modelo dinâmico, efeitos fixos de produto e de ano, com e sem tendência
#      por produto). Com 28 anos, o viés de Nickell é pequeno (ordem 1/T).
#   C. Retira um produto por vez (os 10 maiores exportadores, um a um, e o
#      resumo para todos os 56): mostra se o resultado depende de um produto.
#
# Todos os erros-padrão são de Driscoll e Kraay (válidos com dependência entre
# produtos e correlação ao longo do tempo), com até 2 anos de defasagem.
# Saída: 07_testes/testes_robustez.xlsx

source(here::here("scripts", "funcoes_comuns.R"))
for (p in c("plm", "lmtest", "sandwich")) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
library(dplyr)
library(tidyr)
library(writexl)
library(here)
library(plm)
library(lmtest)

arq_pan <- here("05_painel", "dados", "painel_pam_comex_final_1997_2024.rds")
exigir_arquivo(arq_pan, "06_painel_merge_pam_comex_1997_2024.R")
painel <- readRDS(arq_pan)
pasta <- here("07_testes")
dir.create(pasta, recursive = TRUE, showWarnings = FALSE)

# 0. Base -------------------------------------------------------------------------
# Mesma amostra do script 09: linhas utilizáveis com rendimento positivo.
base <- painel %>%
  filter(!is.na(status_ncm)) %>%
  arrange(produto, ano) %>%
  mutate(
    rend    = `Rendimento médio da produção`,
    qtd     = `Quantidade produzida`,
    valido  = usar_no_modelo & !is.na(rend) & rend > 0 & !is.na(qtd) & qtd > 0,
    exp_t   = if_else(valido, coalesce(peso_liquido_t, 0), NA_real_),
    intens  = if_else(valido, exp_t / qtd, NA_real_),
    ln_rend = if_else(valido, log(rend), NA_real_)
  ) %>%
  group_by(produto) %>%
  mutate(
    qtd_media = mean(qtd[valido]),
    # dplyr::lag explícito (o plm também tem lag())
    ok1 = coalesce(dplyr::lag(ano) == ano - 1 & dplyr::lag(valido), FALSE),
    ok2 = ok1 & coalesce(dplyr::lag(ano, 2) == ano - 2 & dplyr::lag(valido, 2), FALSE),
    exp_lag         = if_else(ok1, dplyr::lag(exp_t), NA_real_),
    x_original      = if_else(ok1, dplyr::lag(intens), NA_real_),
    x_denom_fixo    = exp_lag / qtd_media,
    x_denom_t2      = if_else(ok2, exp_lag / dplyr::lag(qtd, 2), NA_real_),
    x_ln_exportacao = log1p(exp_lag),
    ln_rend_lag     = if_else(ok1, dplyr::lag(ln_rend), NA_real_)
  ) %>%
  ungroup() %>%
  filter(valido) %>%
  mutate(t = ano - min(ano) + 1) %>%
  select(produto, ano, t, ln_rend, ln_rend_lag, intens, exp_t,
         x_original, x_denom_fixo, x_denom_t2, x_ln_exportacao)

cat("Base:", nrow(base), "produto-anos,", n_distinct(base$produto), "produtos\n")

VERSOES <- c(x_original      = "1. Original: exportação(t-1) / produção(t-1)",
             x_denom_fixo    = "2. Denominador fixo: exportação(t-1) / produção média do produto",
             x_denom_t2      = "3. Denominador defasado: exportação(t-1) / produção(t-2)",
             x_ln_exportacao = "4. Sem denominador: ln(1 + toneladas exportadas em t-1)")

# Coeficiente com erros convencionais e de Driscoll e Kraay
coef_dk <- function(m, var) {
  ct <- coeftest(m)
  dk <- tryCatch(coeftest(m, vcov = vcovSCC(m, maxlag = 2)), error = function(e) NULL)
  tibble(n = nobs(m), beta = ct[var, 1],
         ep_driscoll_kraay = if (is.null(dk)) NA_real_ else dk[var, 2],
         p_driscoll_kraay  = if (is.null(dk)) NA_real_ else dk[var, 4],
         ep_convencional   = ct[var, 2])
}

# Primeiras diferenças de y e x, só entre anos seguidos com x disponível
diferencas <- function(df, xvar) {
  df %>% filter(!is.na(.data[[xvar]])) %>% arrange(produto, ano) %>% group_by(produto) %>%
    mutate(consec = dplyr::lag(ano) == ano - 1,
           d_y = if_else(consec, ln_rend - dplyr::lag(ln_rend), NA_real_),
           d_x = if_else(consec, .data[[xvar]] - dplyr::lag(.data[[xvar]]), NA_real_)) %>%
    ungroup() %>% filter(!is.na(d_y), !is.na(d_x))
}
est_fd <- function(df, xvar, efeito = "twoways") {
  d  <- diferencas(df, xvar)
  pd <- pdata.frame(as.data.frame(d), index = c("produto", "ano"))
  coef_dk(plm(d_y ~ d_x, data = pd, model = "within", effect = efeito), "d_x")
}

# A. Troca do denominador ------------------------------------------------------------
# Em diferenças, com efeito de ano e constante por produto (= tendência por
# produto em nível), que é a especificação do script 09 em que o beta apareceu.
# Também em nível, com efeitos de produto e de ano e tendência por produto.
est_nivel <- function(df, xvar, tendencia = TRUE) {
  d  <- df %>% filter(!is.na(.data[[xvar]])) %>% mutate(x = .data[[xvar]])
  pd <- pdata.frame(as.data.frame(d), index = c("produto", "ano"))
  f  <- if (tendencia) ln_rend ~ x + factor(produto):t else ln_rend ~ x
  coef_dk(plm(f, data = pd, model = "within", effect = "twoways"), "x")
}

a_denominador <- bind_rows(lapply(names(VERSOES), function(v) bind_rows(
  est_fd(base, v)                       %>% mutate(modelo = "Diferenças + ano + constante por produto"),
  est_fd(base, v, efeito = "time")      %>% mutate(modelo = "Diferenças + ano"),
  est_nivel(base, v, tendencia = FALSE) %>% mutate(modelo = "Nível: efeitos de produto e de ano"),
  est_nivel(base, v, tendencia = TRUE)  %>% mutate(modelo = "Nível: produto, ano e tendência por produto")
) %>% mutate(variavel = VERSOES[[v]]))) %>%
  select(variavel, modelo, everything()) %>%
  mutate(across(where(is.double), ~ signif(.x, 4)))

# Médias e desvios das versões, para ler a escala dos betas
escalas <- base %>%
  summarise(across(all_of(names(VERSOES)),
                   list(media = ~ mean(.x, na.rm = TRUE), dp = ~ sd(.x, na.rm = TRUE),
                        n = ~ sum(!is.na(.x))))) %>%
  pivot_longer(everything(), names_to = c("versao", ".value"), names_pattern = "(x_.*)_(media|dp|n)$") %>%
  mutate(variavel = VERSOES[versao], .before = 1) %>% select(-versao) %>%
  mutate(across(where(is.double), ~ signif(.x, 4)))

# B. Controle pela volta à média (rendimento do ano anterior) -----------------------
est_dinamico <- function(df, xvar, tendencia) {
  d  <- df %>% filter(!is.na(.data[[xvar]]), !is.na(ln_rend_lag)) %>% mutate(x = .data[[xvar]])
  pd <- pdata.frame(as.data.frame(d), index = c("produto", "ano"))
  f  <- if (tendencia) ln_rend ~ x + ln_rend_lag + factor(produto):t else ln_rend ~ x + ln_rend_lag
  m  <- plm(f, data = pd, model = "within", effect = "twoways")
  bind_rows(coef_dk(m, "x") %>% mutate(termo = "intensidade (t-1)"),
            coef_dk(m, "ln_rend_lag") %>% mutate(termo = "ln(rendimento) (t-1)"))
}
b_dinamico <- bind_rows(lapply(c("x_original", "x_denom_fixo"), function(v) bind_rows(
  est_dinamico(base, v, FALSE) %>% mutate(modelo = "Dinâmico: produto e ano"),
  est_dinamico(base, v, TRUE)  %>% mutate(modelo = "Dinâmico: produto, ano e tendência por produto")
) %>% mutate(variavel = VERSOES[[v]]))) %>%
  select(variavel, modelo, termo, everything()) %>%
  mutate(across(where(is.double), ~ signif(.x, 4)))

# C. Retirar um produto por vez -------------------------------------------------------
# Especificação: diferenças + ano + constante por produto, intensidade original
# e denominador fixo.
maiores <- painel %>%
  filter(usar_no_modelo, produto %in% base$produto) %>%
  summarise(fob = sum(valor_fob_usd, na.rm = TRUE), .by = produto) %>%
  arrange(desc(fob))
top10 <- head(maiores$produto, 10)

sem_um <- bind_rows(lapply(unique(base$produto), function(p) {
  d <- filter(base, produto != p)
  bind_rows(est_fd(d, "x_original") %>% mutate(variavel = "original"),
            est_fd(d, "x_denom_fixo") %>% mutate(variavel = "denominador fixo")) %>%
    mutate(produto_retirado = p)
}))
c_top10 <- sem_um %>% filter(produto_retirado %in% top10) %>%
  mutate(produto_retirado = factor(produto_retirado, levels = top10)) %>%
  arrange(variavel, produto_retirado) %>%
  select(variavel, produto_retirado, n, beta, ep_driscoll_kraay, p_driscoll_kraay) %>%
  mutate(across(where(is.double), ~ signif(.x, 4)))
c_resumo <- sem_um %>%
  summarise(produtos_testados = n(), beta_min = min(beta), beta_mediana = median(beta),
            beta_max = max(beta),
            produto_do_menor_beta = produto_retirado[which.min(beta)],
            produto_do_maior_beta = produto_retirado[which.max(beta)],
            vezes_p_dk_menor_que_0_05 = sum(p_driscoll_kraay < 0.05, na.rm = TRUE),
            vezes_p_dk_menor_que_0_10 = sum(p_driscoll_kraay < 0.10, na.rm = TRUE),
            .by = variavel) %>%
  mutate(across(where(is.double), ~ signif(.x, 4)))

# Saída ---------------------------------------------------------------------------
cat("\n=== A. TROCA DO DENOMINADOR ===\n"); print(a_denominador, n = Inf, width = Inf)
cat("\nEscala de cada versão (para comparar os betas):\n"); print(escalas, width = Inf)
cat("\n=== B. CONTROLE PELO RENDIMENTO DO ANO ANTERIOR ===\n"); print(b_dinamico, n = Inf, width = Inf)
cat("\n=== C. RETIRANDO UM PRODUTO POR VEZ ===\n"); print(c_resumo, width = Inf)
print(c_top10, n = Inf, width = Inf)

leia_me <- tibble(
  aba = c("a_denominador", "a_escalas", "b_dinamico", "c_resumo", "c_top10", "c_todos"),
  o_que_mostra = c(
    "Beta da exportação com 4 formas de medir a exportação (só a 1 tem a produção de t-1 no denominador), em diferenças e em nível",
    "Média, desvio-padrão e número de observações de cada forma de medir (os betas não são comparáveis entre si sem a escala)",
    "Modelo com o ln(rendimento) do ano anterior como controle da volta à média",
    "Retirando um produto por vez: menor, mediano e maior beta, e quantas vezes p < 0,05 e < 0,10",
    "Retirando cada um dos 10 maiores exportadores",
    "Retirando cada um dos produtos (todos)"),
  leitura = c(
    "Se o beta continua positivo nas versões 2 a 4, o resultado não é efeito mecânico do denominador",
    "",
    "Se o beta da intensidade continua com o controle, não é só volta à média",
    "Se o sinal e a significância mudam ao retirar um produto, o resultado depende dele",
    "", "")
)
arquivo <- file.path(pasta, "testes_robustez.xlsx")
write_xlsx(list(leia_me = leia_me, a_denominador = a_denominador, a_escalas = escalas,
                b_dinamico = b_dinamico, c_resumo = c_resumo, c_top10 = c_top10,
                c_todos = mutate(sem_um, across(where(is.double), ~ signif(.x, 4)))), arquivo)
cat("\nResultados salvos em:", arquivo, "\n")
