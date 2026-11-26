# 13_bootstrap_diferencas_longas.R
# Confere a inferência das diferenças longas (script 11) com o bootstrap
# selvagem por agrupamento em produto (wild cluster bootstrap, Cameron, Gelbach
# e Miller, 2008), ao lado dos erros de Driscoll e Kraay.
#
# Por que: na diferença de 10 anos, os erros de Driscoll e Kraay contam a
# informação em anos, e sobram poucos anos efetivos (janelas sobrepostas,
# defasagem 10); o p-valor de 0,001 pode ser otimista. O bootstrap por produto
# conta a informação em produtos (cerca de 55) e preserva a sobreposição das
# janelas dentro de cada produto. Os choques comuns entre produtos ficam a cargo
# do efeito de ano (o teste de Pesaran não rejeitou nos resíduos com efeito de ano).
#
# Como funciona (versão "restrita", a recomendada):
#   1. Estima o modelo sem a exportação (hipótese nula: beta = 0) e guarda os resíduos.
#   2. Para cada repetição, sorteia um peso por PRODUTO (distribuição de Webb, 6
#      valores, boa com poucos grupos) e multiplica os resíduos de todas as
#      linhas daquele produto pelo mesmo peso: o produto entra inteiro.
#   3. Cria uma variável explicada artificial (previsto pela nula + resíduo
#      ponderado), estima o modelo completo e guarda a estatística t (erros
#      agrupados por produto).
#   4. p-valor = proporção de repetições com |t artificial| >= |t observado|.
#
# Modelos conferidos (os mesmos do script 11):
#   A. Diferença de 5 e de 10 anos, efeito de ano.
#   B. Períodos de 7 anos, efeitos de produto e de período.
# Medidas: intensidade original, denominador fixo, ln(1 + toneladas exportadas).
# Saída: 07_testes/bootstrap_diferencas_longas.xlsx

source(here::here("scripts", "funcoes_comuns.R"))
for (p in c("plm", "lmtest", "sandwich")) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
library(dplyr)
library(tidyr)
library(writexl)
library(here)
library(plm)
library(lmtest)

REPETICOES <- 9999
set.seed(20261125)   # para o resultado ser reproduzível

arq_pan <- here("05_painel", "dados", "painel_pam_comex_final_1997_2024.rds")
exigir_arquivo(arq_pan, "06_painel_merge_pam_comex_1997_2024.R")
painel <- readRDS(arq_pan)
pasta <- here("07_testes")
dir.create(pasta, recursive = TRUE, showWarnings = FALSE)

MEDIDAS <- c(original   = "Intensidade original: exportação / produção do ano",
             denom_fixo = "Denominador fixo: exportação / produção média do produto",
             ln_exp     = "Sem denominador: ln(1 + toneladas exportadas)")

# 0. Base anual (a mesma dos scripts 09 a 12) -------------------------------------------
base <- painel %>%
  filter(!is.na(status_ncm)) %>%
  mutate(rend = `Rendimento médio da produção`, qtd = `Quantidade produzida`,
         valido = usar_no_modelo & !is.na(rend) & rend > 0 & !is.na(qtd) & qtd > 0) %>%
  filter(valido) %>%
  mutate(exp_t = coalesce(peso_liquido_t, 0)) %>%
  group_by(produto) %>% mutate(qtd_media = mean(qtd)) %>% ungroup() %>%
  transmute(produto, ano, ln_rend = log(rend),
            original = exp_t / qtd, denom_fixo = exp_t / qtd_media, ln_exp = log1p(exp_t))

valor_em <- function(df, var, defasagem) {
  chave <- df %>% transmute(produto, ano = ano + defasagem, v = .data[[var]])
  left_join(df %>% select(produto, ano), chave, by = c("produto", "ano"))$v
}

# Pesos de Webb: 6 valores igualmente prováveis
pesos_webb <- c(-sqrt(1.5), -1, -sqrt(0.5), sqrt(0.5), 1, sqrt(1.5))

# Bootstrap selvagem restrito, agrupado em g. y: explicada; x: exportação;
# Z: matriz dos controles (efeitos fixos). Usa a separação de Frisch-Waugh-Lovell:
# tira de y e de x a parte explicada pelos controles e trabalha com o que sobra.
wild_cluster <- function(y, x, Z, g, B = REPETICOES) {
  qrZ <- qr(Z)
  xt  <- qr.resid(qrZ, x)                 # x sem a parte dos controles
  yt  <- qr.resid(qrZ, y)                 # y sem a parte dos controles = resíduo sob a nula
  sxx <- sum(xt^2)
  G   <- length(unique(g)); n <- length(y); k <- qrZ$rank + 1
  ajuste <- (G / (G - 1)) * ((n - 1) / (n - k))   # correção de pequenas amostras (CR1)
  t_de <- function(Yt) {                  # Yt: matriz n x R de variáveis explicadas (já sem controles)
    b  <- colSums(xt * Yt) / sxx
    E  <- Yt - outer(xt, b)
    S  <- rowsum(xt * E, g)               # soma por produto
    ep <- sqrt(ajuste * colSums(S^2) / sxx^2)
    list(b = b, ep = ep, t = b / ep)
  }
  obs <- t_de(matrix(yt, ncol = 1))
  grupos <- unique(g); idx <- match(g, grupos)
  t_boot <- numeric(0)
  for (lote in split(seq_len(B), ceiling(seq_len(B) / 1000))) {   # em lotes, para poupar memória
    W  <- matrix(sample(pesos_webb, G * length(lote), replace = TRUE), nrow = G)
    Ys <- qr.resid(qrZ, yt * W[idx, , drop = FALSE])                # resíduo da nula x peso do produto
    t_boot <- c(t_boot, t_de(Ys)$t)
  }
  tibble(beta = obs$b, erro_padrao_agrupado = obs$ep, t = obs$t,
         p_agrupado = 2 * pt(-abs(obs$t), df = G - 1),
         p_bootstrap = mean(abs(t_boot) >= abs(obs$t)),
         produtos = G, n = n)
}

# A. Diferenças de 5 e 10 anos, efeito de ano -----------------------------------------
dif_k <- function(k, medida) {
  d <- base %>%
    mutate(dy = ln_rend - valor_em(base, "ln_rend", k),
           dx = valor_em(base, medida, 1) - valor_em(base, medida, 1 + k)) %>%
    filter(!is.na(dy), !is.na(dx))
  # Driscoll e Kraay, como no script 11
  pd <- pdata.frame(as.data.frame(d), index = c("produto", "ano"))
  m  <- plm(dy ~ dx, data = pd, model = "within", effect = "time")
  dk <- coeftest(m, vcov = vcovSCC(m, maxlag = k))
  Z  <- model.matrix(~ factor(ano), d)
  wild_cluster(d$dy, d$dx, Z, d$produto) %>%
    mutate(p_driscoll_kraay = dk["dx", 4],
           abordagem = paste0("A. Diferença de ", k, " anos, efeito de ano"),
           medida = MEDIDAS[[medida]])
}

# B. Períodos de 7 anos, efeitos de produto e de período --------------------------------
periodos <- tibble(ano = 1997:2024, periodo = rep(1:4, each = 7))
medias <- base %>%
  inner_join(periodos, by = "ano") %>%
  summarise(across(c(ln_rend, all_of(names(MEDIDAS))), mean), anos = n(), .by = c(produto, periodo)) %>%
  filter(anos >= 3) %>% arrange(produto, periodo) %>% group_by(produto) %>%
  mutate(across(all_of(names(MEDIDAS)),
                ~ if_else(dplyr::lag(periodo) == periodo - 1, dplyr::lag(.x), NA_real_), .names = "{.col}_ant")) %>%
  ungroup()
periodo_est <- function(medida) {
  d <- medias %>% filter(!is.na(.data[[paste0(medida, "_ant")]])) %>% mutate(x = .data[[paste0(medida, "_ant")]])
  Z <- model.matrix(~ factor(produto) + factor(periodo), d)
  wild_cluster(d$ln_rend, d$x, Z, d$produto) %>%
    mutate(p_driscoll_kraay = NA_real_,
           abordagem = "B. Períodos de 7 anos, efeitos de produto e de período",
           medida = MEDIDAS[[medida]])
}

cat("Rodando", REPETICOES, "repetições por modelo (cerca de 1 a 3 minutos no total)...\n")
resultados <- bind_rows(
  bind_rows(lapply(names(MEDIDAS), dif_k, k = 5)),
  bind_rows(lapply(names(MEDIDAS), dif_k, k = 10)),
  bind_rows(lapply(names(MEDIDAS), periodo_est))
) %>%
  select(abordagem, medida, beta, erro_padrao_agrupado, p_agrupado, p_driscoll_kraay, p_bootstrap, n, produtos) %>%
  mutate(across(where(is.double), ~ signif(.x, 4)))

cat("\n=== BOOTSTRAP SELVAGEM POR PRODUTO x DRISCOLL E KRAAY ===\n")
print(resultados, n = Inf, width = Inf)

leia_me <- tibble(
  coluna = c("beta", "erro_padrao_agrupado", "p_agrupado", "p_driscoll_kraay", "p_bootstrap"),
  significado = c(
    "Coeficiente da exportação (o mesmo do script 11)",
    "Erro-padrão agrupado por produto (fórmula com correção para poucos grupos)",
    "p-valor pela fórmula agrupada por produto (distribuição t com G - 1 graus de liberdade)",
    "p-valor de Driscoll e Kraay (script 11); não se aplica aos períodos de 7 anos",
    paste0("p-valor do bootstrap selvagem restrito por produto, pesos de Webb, ", REPETICOES, " repetições")),
  leitura = c("", "", "",
              "Conta a informação em anos: fraco com poucos anos efetivos",
              "Conta a informação em produtos e preserva a sobreposição das janelas. Se a diferença de 10 anos continuar abaixo de 0,05 aqui e em Driscoll e Kraay, o resultado é robusto à forma de inferência")
)
arquivo <- file.path(pasta, "bootstrap_diferencas_longas.xlsx")
write_xlsx(list(leia_me = leia_me, resultados = resultados), arquivo)
cat("\nResultados salvos em:", arquivo, "\n")
