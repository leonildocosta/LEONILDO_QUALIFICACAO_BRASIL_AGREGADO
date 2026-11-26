# 09_testes_blocos_1a3.R
# Testes preliminares, blocos 1 a 3, sobre o painel produto x ano (script 06).
#
#   Bloco 1 - a amostra: quantos produtos, anos e zeros; variação "entre"
#             produtos (between) e "dentro" de cada produto (within).
#   Bloco 2 - as séries: dependência entre produtos (Pesaran CD), com a
#             média e com a tendência de cada produto retiradas; raiz
#             unitária (Maddala-Wu e IPS; CIPS, robusto à dependência, para
#             rendimento com 1 e 2 defasagens; intensidade, se houver N >= 10).
#   Bloco 3 - a especificação: efeitos fixos de produto e de ano (F),
#             Hausman e Mundlak (Hausman robusto), tendências por produto,
#             coeficientes em nível e em primeiras diferenças (com erros
#             convencionais e de Driscoll-Kraay).
#
# Variáveis:
#   ln_rend      = ln(rendimento médio, kg/ha)
#   intens       = toneladas exportadas / toneladas produzidas no ano
#   intens_lag   = intensidade do ano anterior (só quando o ano anterior
#                  também é utilizável e é exatamente t-1)
# Amostra: linhas com usar_no_modelo = TRUE e rendimento > 0.
#
# Saída: 07_testes/testes_blocos_1a3.xlsx (uma aba por teste + leia_me)
# (Este script incorpora as pendências que estavam no antigo 10_testes_pendencias.R.)

source(here::here("scripts", "funcoes_comuns.R"))
for (p in c("plm", "lmtest", "sandwich")) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
library(dplyr)
library(tidyr)
library(writexl)
library(here)
library(plm)
library(lmtest)

MIN_ANOS_SERIE   <- 10  # mínimo de anos seguidos para testar raiz unitária
MIN_ANOS_EXPORTA <- 5   # mínimo de anos com exportação para testar a intensidade
MIN_ANOS_COMUNS  <- 8   # mínimo de anos em comum para a correlação do teste CD

arq_pan <- here("05_painel", "dados", "painel_pam_comex_final_1997_2024.rds")
exigir_arquivo(arq_pan, "06_painel_merge_pam_comex_1997_2024.R")
painel <- readRDS(arq_pan)

pasta <- here("07_testes")
dir.create(pasta, recursive = TRUE, showWarnings = FALSE)

# Executa um teste e devolve uma linha de resultado; se falhar, registra o erro
# em vez de parar o script.
rodar <- function(nome, expr) {
  tryCatch(expr, error = function(e) {
    message("  [", nome, "] não rodou: ", conditionMessage(e))
    tibble(teste = nome, estatistica = NA_real_, p_valor = NA_real_,
           observacao = paste("Não rodou:", conditionMessage(e)))
  })
}
linha_teste <- function(nome, est, p, obs = "") {
  tibble(teste = nome, estatistica = unname(est), p_valor = unname(p), observacao = obs)
}

# 0. Base de análise -----------------------------------------------------------
base <- painel %>%
  filter(!is.na(status_ncm)) %>%
  arrange(produto, ano) %>%
  mutate(
    rend   = `Rendimento médio da produção`,
    qtd    = `Quantidade produzida`,
    valido = usar_no_modelo & !is.na(rend) & rend > 0,
    intens = if_else(valido & qtd > 0, coalesce(peso_liquido_t, 0) / qtd, NA_real_)
  ) %>%
  group_by(produto) %>%
  # dplyr::lag explícito: o pacote plm também tem uma função lag() e, carregado
  # depois do dplyr, a substitui (com ela a defasagem saía toda vazia)
  mutate(intens_lag = if_else(dplyr::lag(ano) == ano - 1 & dplyr::lag(valido),
                              dplyr::lag(intens), NA_real_)) %>%
  ungroup() %>%
  filter(valido) %>%
  mutate(ln_rend = log(rend), exportou = intens > 0, t = ano - min(ano) + 1) %>%
  select(produto, ano, t, rend, ln_rend, qtd, intens, intens_lag, exportou)

reg <- base %>% filter(!is.na(intens_lag))  # amostra da regressão
if (nrow(reg) == 0) stop("A amostra da regressão ficou vazia: a intensidade defasada não foi calculada.")

cat("\nBase de análise:", nrow(base), "linhas,", n_distinct(base$produto), "produtos.",
    "Amostra da regressão (com intensidade defasada):", nrow(reg), "linhas.\n")

# =============================================================================
# BLOCO 1 - a amostra
# =============================================================================
b1_resumo <- tibble(
  item = c("Produtos com rendimento utilizável",
           "Produto-anos com rendimento utilizável",
           "Produtos que exportaram em algum ano",
           "Produto-anos com exportação positiva",
           "Proporção de zeros na intensidade (%)",
           "Produto-anos na amostra da regressão (com intensidade defasada)",
           "Produtos na amostra da regressão",
           "Participação da soja na exportação dos produtos-anos utilizáveis (%)"),
  valor = c(n_distinct(base$produto),
            nrow(base),
            n_distinct(base$produto[base$exportou]),
            sum(base$exportou),
            round(100 * mean(!base$exportou), 1),
            nrow(reg),
            n_distinct(reg$produto),
            round(100 * sum(painel$valor_fob_usd[painel$produto == "Soja (em grão)" & painel$usar_no_modelo], na.rm = TRUE) /
                    sum(painel$valor_fob_usd[painel$usar_no_modelo], na.rm = TRUE), 1))
)

b1_por_produto <- base %>%
  group_by(produto) %>%
  summarise(anos_com_rendimento = n(),
            primeiro_ano = min(ano), ultimo_ano = max(ano),
            anos_com_exportacao = sum(exportou),
            rendimento_medio_kg_ha = round(mean(rend)),
            intensidade_media = round(mean(intens), 4),
            intensidade_maxima = round(max(intens), 4),
            .groups = "drop") %>%
  arrange(desc(anos_com_exportacao), produto)

# Variação total, entre produtos e dentro de cada produto (como o xtsum do Stata).
# Com efeito fixo de produto, só a variação "within" identifica o coeficiente.
decompor <- function(df, var, rotulo) {
  x <- df[[var]]; g <- df$produto
  media_g <- ave(x, g)
  tibble(variavel = rotulo,
         media = mean(x),
         dp_total = sd(x),
         dp_between = sd(tapply(x, g, mean)),
         dp_within = sd(x - media_g + mean(x)),
         n_obs = length(x), n_produtos = n_distinct(g)) %>%
    mutate(within_sobre_total_pct = round(100 * dp_within^2 / dp_total^2, 1))
}
b1_variacao <- bind_rows(
  decompor(reg, "ln_rend", "ln(rendimento) - todos os produtos"),
  decompor(reg, "intens_lag", "intensidade defasada - todos os produtos"),
  decompor(filter(reg, produto %in% base$produto[base$exportou]), "intens_lag",
           "intensidade defasada - só produtos que exportaram"),
  decompor(filter(reg, produto %in% base$produto[base$exportou], produto != "Soja (em grão)"),
           "intens_lag", "intensidade defasada - exportadores sem a soja")
) %>% mutate(across(where(is.double), ~ round(.x, 4)))

cat("\n=== BLOCO 1 ===\n"); print(b1_resumo, n = Inf); print(b1_variacao, width = Inf)

# =============================================================================
# BLOCO 2 - as séries
# =============================================================================
exportadores <- unique(base$produto[base$exportou])

cd_pesaran <- function(df, var, nome, nota) {
  rodar(nome, {
    largo <- df %>% select(produto, ano, v = all_of(var)) %>%
      pivot_wider(names_from = produto, values_from = v) %>% arrange(ano)
    m <- as.matrix(largo[, -1])
    m <- m[, apply(m, 2, function(z) sum(!is.na(z)) >= MIN_ANOS_COMUNS && sd(z, na.rm = TRUE) > 0), drop = FALSE]
    n <- ncol(m); soma <- 0; rhos <- c()
    for (i in 1:(n - 1)) for (j in (i + 1):n) {
      ok <- !is.na(m[, i]) & !is.na(m[, j])
      if (sum(ok) >= MIN_ANOS_COMUNS && sd(m[ok, i]) > 0 && sd(m[ok, j]) > 0) {
        r <- cor(m[ok, i], m[ok, j]); soma <- soma + sqrt(sum(ok)) * r; rhos <- c(rhos, r)
      }
    }
    cd <- sqrt(2 / (n * (n - 1))) * soma
    linha_teste(nome, cd, 2 * pnorm(-abs(cd)),
                sprintf("%s | %d produtos, %d pares; correlação média = %.2f; média absoluta = %.2f",
                        nota, n, length(rhos), mean(rhos), mean(abs(rhos))))
  })
}

# Retira de cada produto a sua média (demean) ou a sua reta no tempo (detrend)
sem_tendencia <- function(y, x) if (length(y) >= 3) unname(residuals(lm(y ~ x))) else rep(NA_real_, length(y))
ajustar <- function(df, var, modo) {
  df %>% mutate(v = .data[[var]]) %>% group_by(produto) %>%
    mutate(aj = if (modo == "media") v - mean(v) else sem_tendencia(v, ano)) %>%
    ungroup()
}

fe_resid  <- lm(ln_rend ~ intens_lag + factor(produto) + factor(ano), data = reg)
fet_resid <- lm(ln_rend ~ intens_lag + factor(produto) + factor(ano) + factor(produto):t, data = reg)
reg$res_fe  <- residuals(fe_resid)
reg$res_fet <- residuals(fet_resid)

b2_cd <- bind_rows(
  cd_pesaran(ajustar(base, "ln_rend", "media"), "aj", "CD - ln(rendimento), só média retirada", "mede também o crescimento comum"),
  cd_pesaran(ajustar(base, "ln_rend", "tendencia"), "aj", "CD - ln(rendimento), tendência retirada", "reta própria de cada produto retirada"),
  cd_pesaran(ajustar(filter(base, produto %in% exportadores), "intens", "media"), "aj",
             "CD - intensidade, só média retirada", "exportadores"),
  cd_pesaran(ajustar(filter(base, produto %in% exportadores), "intens", "tendencia"), "aj",
             "CD - intensidade, tendência retirada", "exportadores"),
  cd_pesaran(reg, "res_fe",  "CD - resíduos: efeitos de produto e ano", ""),
  cd_pesaran(reg, "res_fet", "CD - resíduos: produto, ano e tendência por produto", "modelo indicado pelo bloco 3")
)

# Raiz unitária: só trechos de anos seguidos (sem buracos) com >= MIN_ANOS_SERIE.
maior_trecho <- function(df) {
  df %>% arrange(produto, ano) %>% group_by(produto) %>%
    mutate(bloco = cumsum(c(1, diff(ano) != 1))) %>%
    group_by(produto, bloco) %>% mutate(n = n()) %>%
    group_by(produto) %>% filter(n == max(n)) %>% filter(bloco == min(bloco)) %>%
    ungroup() %>% filter(n >= MIN_ANOS_SERIE) %>% select(-bloco, -n)
}
raiz <- function(df, var, teste, nome, exo = "trend") {
  rodar(nome, {
    pd <- pdata.frame(as.data.frame(df), index = c("produto", "ano"))
    r <- purtest(pd[[var]], test = teste, exo = exo, lags = "AIC", pmax = 2)
    linha_teste(nome, r$statistic$statistic, r$statistic$p.value,
                sprintf("%d produtos; com %s; H0: raiz unitária",
                        n_distinct(df$produto), ifelse(exo == "trend", "tendência", "intercepto")))
  })
}
serie_rend <- maior_trecho(base)
exportadores_freq <- b1_por_produto$produto[b1_por_produto$anos_com_exportacao >= MIN_ANOS_EXPORTA]
serie_int <- maior_trecho(filter(base, produto %in% exportadores_freq))

# CIPS (Pesaran, 2007), robusto à dependência entre produtos: exige painel
# balanceado, então usa os produtos com rendimento em todos os anos 2001-2024.
balanceado <- base %>% filter(ano >= 2001) %>% group_by(produto) %>%
  filter(n() == length(2001:2024)) %>% ungroup()
bal_int <- balanceado %>% filter(produto %in% exportadores_freq) %>%
  group_by(produto) %>% filter(sd(intens) > 0) %>% ungroup()
cips <- function(df, var, nome, tipo, defas) {
  nprod <- n_distinct(df$produto)
  # a tabela de valores críticos de Pesaran (2007), usada pelo plm para o
  # p-valor, começa em N = 10: com menos produtos não há como decidir
  if (nprod < 10) return(linha_teste(nome, NA, NA,
    sprintf("Não aplicável: só %d produtos com série completa 2001-2024; a tabela do CIPS exige N >= 10", nprod)))
  rodar(nome, {
    pd <- pdata.frame(as.data.frame(df), index = c("produto", "ano"))
    r <- suppressWarnings(cipstest(pd[[var]], type = tipo, lags = defas))
    linha_teste(nome, r$statistic, r$p.value,
                sprintf("%d produtos, 2001-2024; %s; %d defasagem(ns); H0: raiz unitária; p-valor = 0,1 significa \"0,10 ou mais\"",
                        n_distinct(df$produto), ifelse(tipo == "trend", "com tendência", "com intercepto"), defas))
  })
}

b2_raiz <- bind_rows(
  raiz(serie_rend, "ln_rend", "madwu", "Maddala-Wu - ln(rendimento)"),
  raiz(serie_rend, "ln_rend", "ips",   "IPS - ln(rendimento)"),
  raiz(serie_int,  "intens",  "madwu", "Maddala-Wu - intensidade (exportadores frequentes)"),
  raiz(serie_int,  "intens",  "ips",   "IPS - intensidade (exportadores frequentes)"),
  cips(balanceado, "ln_rend", "CIPS - ln(rendimento)", "trend", 1),
  cips(balanceado, "ln_rend", "CIPS - ln(rendimento)", "trend", 2),
  cips(bal_int, "intens", "CIPS - intensidade", "trend", 1),
  cips(bal_int, "intens", "CIPS - intensidade", "drift", 1)
)

cat("\n=== BLOCO 2 ===\n"); print(b2_cd, width = Inf); print(b2_raiz, width = Inf)

# =============================================================================
# BLOCO 3 - especificação (modelo estático: ln_rend ~ intens_lag)
# =============================================================================
pd_reg <- pdata.frame(as.data.frame(reg), index = c("produto", "ano"))
f <- ln_rend ~ intens_lag

m_pool <- plm(f, data = pd_reg, model = "pooling")
m_fe   <- plm(f, data = pd_reg, model = "within", effect = "individual")
m_fe2  <- plm(f, data = pd_reg, model = "within", effect = "twoways")

b3 <- bind_rows(
  rodar("F - efeitos fixos de produto (vs. MQO empilhado)", {
    r <- pFtest(m_fe, m_pool)
    linha_teste("F - efeitos fixos de produto (vs. MQO empilhado)", r$statistic, r$p.value,
                "H0: sem efeito fixo de produto")
  }),
  rodar("F - efeitos de ano (dado o efeito de produto)", {
    r <- pFtest(m_fe2, m_fe)
    linha_teste("F - efeitos de ano (dado o efeito de produto)", r$statistic, r$p.value,
                "H0: sem efeito de ano")
  }),
  rodar("Hausman - efeitos fixos x aleatórios", {
    m_re <- plm(f, data = pd_reg, model = "random", effect = "individual")
    r <- phtest(m_fe, m_re)
    linha_teste("Hausman - efeitos fixos x aleatórios", r$statistic, r$p.value,
                "H0: efeitos aleatórios consistentes; rejeitar = usar efeitos fixos")
  }),
  rodar("Mundlak (Hausman robusto)", {
    reg_m <- reg %>% group_by(produto) %>% mutate(media_intens = mean(intens_lag)) %>% ungroup()
    pd_m <- pdata.frame(as.data.frame(reg_m), index = c("produto", "ano"))
    m <- plm(ln_rend ~ intens_lag + media_intens + factor(ano), data = pd_m, model = "pooling")
    ct <- coeftest(m, vcov = vcovHC(m, method = "arellano", cluster = "group"))
    linha_teste("Mundlak (Hausman robusto)", ct["media_intens", "t value"], ct["media_intens", "Pr(>|t|)"],
                sprintf("coeficiente da média do produto = %.3f; erros robustos por produto; H0: efeitos aleatórios adequados",
                        ct["media_intens", "Estimate"]))
  }),
  rodar("F - tendências específicas por produto", {
    sem <- lm(ln_rend ~ intens_lag + factor(produto) + factor(ano), data = reg)
    com <- lm(ln_rend ~ intens_lag + factor(produto) + factor(ano) + factor(produto):t, data = reg)
    a <- anova(sem, com)
    linha_teste("F - tendências específicas por produto", a$F[2], a$`Pr(>F)`[2],
                "H0: sem tendência própria por produto; rejeitar = incluir tendência por produto")
  })
)

# Coeficiente da intensidade nas especificações estáticas (só descritivo:
# erros-padrão corretos e modelo dinâmico ficam para os blocos 4 e 5)
coef_int <- function(m, nome) {
  ct <- coeftest(m)
  tibble(especificacao = nome, coef_intens_lag = ct["intens_lag", 1],
         erro_padrao_convencional = ct["intens_lag", 2], n = nobs(m))
}
b3_coef <- bind_rows(
  coef_int(m_pool, "MQO empilhado"),
  coef_int(m_fe,   "Efeito fixo de produto"),
  coef_int(m_fe2,  "Efeitos fixos de produto e de ano"),
  coef_int(lm(ln_rend ~ intens_lag + factor(produto) + factor(ano) + factor(produto):t, data = reg),
           "Efeitos fixos + tendência por produto")
) %>% mutate(across(where(is.double), ~ round(.x, 4)))


# Primeiras diferenças: variação de um ano para o outro. Não sofre de
# regressão espúria mesmo se houver raiz unitária. Uma constante por produto
# em diferenças equivale à tendência por produto em nível.
dif <- reg %>% arrange(produto, ano) %>% group_by(produto) %>%
  mutate(consec = dplyr::lag(ano) == ano - 1,
         d_ln_rend    = if_else(consec, ln_rend - dplyr::lag(ln_rend), NA_real_),
         d_intens_lag = if_else(consec, intens_lag - dplyr::lag(intens_lag), NA_real_)) %>%
  ungroup() %>% filter(!is.na(d_ln_rend), !is.na(d_intens_lag))

coef_linha <- function(m, nome, pd = NULL) {
  ct  <- coeftest(m)
  dk  <- tryCatch(coeftest(m, vcov = vcovSCC(m, maxlag = 2)), error = function(e) NULL)
  tibble(especificacao = nome, n = nobs(m),
         beta = ct["d_intens_lag", 1],
         ep_convencional = ct["d_intens_lag", 2],
         p_convencional  = ct["d_intens_lag", 4],
         ep_driscoll_kraay = if (is.null(dk)) NA_real_ else dk["d_intens_lag", 2],
         p_driscoll_kraay  = if (is.null(dk)) NA_real_ else dk["d_intens_lag", 4])
}
pd_dif <- pdata.frame(as.data.frame(dif), index = c("produto", "ano"))
b3_dif <- bind_rows(
  coef_linha(plm(d_ln_rend ~ d_intens_lag, data = pd_dif, model = "pooling"),
             "Diferenças, sem efeitos"),
  coef_linha(plm(d_ln_rend ~ d_intens_lag, data = pd_dif, model = "within", effect = "time"),
             "Diferenças + efeito de ano"),
  coef_linha(plm(d_ln_rend ~ d_intens_lag, data = pd_dif, model = "within", effect = "twoways"),
             "Diferenças + efeito de ano + constante por produto (= tendência por produto)")
) %>% mutate(across(where(is.double), ~ signif(.x, 4)))


cat("\n=== BLOCO 3 ===\n"); print(b3, width = Inf); print(b3_coef, width = Inf)
cat("\nPrimeiras diferenças (", nrow(dif), "observações,", n_distinct(dif$produto), "produtos):\n")
print(b3_dif, width = Inf)

# Saída ------------------------------------------------------------------------
leia_me <- tibble(
  aba = c("b1_resumo", "b1_por_produto", "b1_variacao", "b2_dependencia_cd",
          "b2_raiz_unitaria", "b3_especificacao", "b3_coeficientes", "b3_primeiras_diferencas"),
  o_que_mostra = c(
    "Tamanho real da amostra: produtos, anos, exportações, zeros",
    "Cada produto: anos com dado, anos com exportação, rendimento e intensidade médios",
    "Variação total, entre produtos (between) e dentro de cada produto (within)",
    "Pesaran CD com média e com tendência retiradas. H0: sem dependência entre produtos",
    "Raiz unitária (Maddala-Wu, IPS, CIPS). H0: raiz unitária. No CIPS, p = 0,1 significa 0,10 ou mais",
    "Testes F, Hausman, Mundlak e tendências por produto",
    "Coeficiente da intensidade em nível, em cada especificação (erros convencionais)",
    "Coeficiente em primeiras diferenças, com erros convencionais e de Driscoll-Kraay")
)
arquivo <- file.path(pasta, "testes_blocos_1a3.xlsx")
write_xlsx(list(leia_me = leia_me, b1_resumo = b1_resumo, b1_por_produto = b1_por_produto,
                b1_variacao = b1_variacao, b2_dependencia_cd = b2_cd, b2_raiz_unitaria = b2_raiz,
                b3_especificacao = b3, b3_coeficientes = b3_coef, b3_primeiras_diferencas = b3_dif), arquivo)
cat("\nResultados salvos em:", arquivo, "\n")
