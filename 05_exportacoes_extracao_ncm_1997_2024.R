# 05_exportacoes_extracao_ncm_1997_2024.R
# Extrai as exportações dos estados de UFS (Comex Stat, critério UF do Produto) por
# ano x estado x NCM x país e monta a tabela do orientador: ano | sigla_uf | codigo_ncm | produto_ncm_desc | <País>_fob | <País>_kg ...
# Saídas: 02_exportacoes/exportacoes_ncm_<regiao>_estado_1997_2024 (.rds long, _wide.rds, .xlsx
#         com as abas long, por_pais e por_pais_35_produtos)

REEXTRAIR <- FALSE  # TRUE baixa tudo de novo da API; FALSE reaproveita o .rds salvo

source(here::here("scripts", "funcoes_comuns.R"))
library(dplyr)
library(tidyr)
library(here)
library(writexl)
library(stringr)

pasta    <- here("02_exportacoes")
arq_long <- file.path(pasta, paste0("exportacoes_ncm_", REGIAO, "_estado_1997_2024.rds"))
arq_wide <- file.path(pasta, paste0("exportacoes_ncm_", REGIAO, "_estado_1997_2024_wide.rds"))
arq_xlsx <- file.path(pasta, paste0("exportacoes_ncm_", REGIAO, "_estado_1997_2024.xlsx"))
dir.create(pasta, showWarnings = FALSE, recursive = TRUE)

# 1. Extração ---------------------------------------------------------------------
# O Brasil inteiro, com detalhe NCM x país, passa de 130 mil registros por ano a
# partir de 2018, e a API do Comex Stat falha com "HTTP 500 timeout: fullResults"
# (aconteceu em 24/11/2026 para 2019-2024). Por isso cada ano é consultado em 4
# trimestres, somados depois. No erro 429 (limite) ou 500 (timeout), espera e
# tenta de novo, até 5 vezes. Só os anos que faltam na extração salva são
# baixados de novo (REEXTRAIR = TRUE baixa tudo).
TRIMESTRES <- list(c("01", "03"), c("04", "06"), c("07", "09"), c("10", "12"))

extrair_trimestre <- function(ano, tri, tentativa = 1) {
  ini <- TRIMESTRES[[tri]][1]; fim <- TRIMESTRES[[tri]][2]
  cat("Extraindo Brasil", ano, "trimestre", tri, "\n")
  resultado <- tryCatch(
    comexr::comex_query(
      flow         = "export",
      start_period = sprintf("%d-%s", ano, ini),
      end_period   = sprintf("%d-%s", ano, fim),
      details      = c("country", "ncm"),
      # SEM filtro de estado: total do Brasil, inclusive o que o Comex Stat não
      # atribui a nenhum estado ("Estados Diversos - Café", "Não Declarada" etc.)
      month_detail = FALSE,
      metric_fob   = TRUE,
      metric_kg    = TRUE,
      language     = "pt"
    ),
    error = function(e) {
      if (grepl("429|500|timeout", conditionMessage(e)) && tentativa < 5) {
        cat("  API ocupada ou lenta, nova tentativa em 30 s\n")
        Sys.sleep(30)
        return(extrair_trimestre(ano, tri, tentativa + 1))
      }
      message("ERRO em ", ano, " trimestre ", tri, ": ", conditionMessage(e))
      NULL
    }
  )
  Sys.sleep(2)
  if (is.null(resultado) || nrow(resultado) == 0) return(NULL)
  mutate(resultado, trimestre = tri)
}

extrair_anos <- function(anos) {
  combinacoes <- expand.grid(ano = anos, tri = seq_along(TRIMESTRES))
  brutos <- Map(extrair_trimestre, combinacoes$ano, combinacoes$tri)
  # Um ano só entra se os 4 trimestres vieram; senão fica para a próxima rodada
  ok <- vapply(brutos, Negate(is.null), logical(1))
  anos_completos <- anos[vapply(anos, function(a) all(ok[combinacoes$ano == a]), logical(1))]
  if (length(anos_completos) == 0) return(NULL)
  bind_rows(brutos[ok]) %>%
    filter(as.integer(year) %in% anos_completos) %>%
    transmute(
      ano              = as.integer(year),
      codigo_uf        = as.integer(UFS[[1]]),
      sigla_uf         = names(UFS)[1],
      codigo_ncm       = str_pad(as.character(coNcm), 8, pad = "0"),  # preserva o zero à esquerda
      produto_ncm_desc = ncm,
      pais             = country,
      valor_fob_usd    = as.numeric(metricFOB),
      peso_liquido_kg  = as.numeric(metricKG)
    ) %>%
    # soma os 4 trimestres
    summarise(produto_ncm_desc = first(produto_ncm_desc),
              valor_fob_usd = sum(valor_fob_usd), peso_liquido_kg = sum(peso_liquido_kg),
              .by = c(ano, codigo_uf, sigla_uf, codigo_ncm, pais)) %>%
    mutate(codigo_sh4 = as.integer(substr(codigo_ncm, 1, 4)), .after = codigo_ncm)
}

salvo <- if (file.exists(arq_long) && !REEXTRAIR) readRDS(arq_long) else NULL
anos_faltando <- setdiff(ANOS, salvo$ano)
if (length(anos_faltando) > 0) {
  cat("Anos a extrair:", paste(anos_faltando, collapse = ", "), "\n")
  novos <- extrair_anos(anos_faltando)
  exportacoes_ncm <- bind_rows(salvo, novos) %>%
    arrange(codigo_uf, ano, codigo_ncm, desc(valor_fob_usd))
  saveRDS(exportacoes_ncm, arq_long)
} else {
  cat("Usando a extração já salva:", arq_long, "\n")
  exportacoes_ncm <- salvo
}

# 2. Conferências --------------------------------------------------------------
faltando <- anti_join(expand.grid(ano = ANOS, codigo_uf = as.integer(UFS)),
                      distinct(exportacoes_ncm, ano, codigo_uf), by = c("ano", "codigo_uf"))
if (nrow(faltando) > 0) {
  warning("Estado-anos sem dados (rode de novo com REEXTRAIR = TRUE): ",
          paste(sigla_uf(faltando$codigo_uf), faltando$ano, collapse = ", "))
}

stopifnot(
  !anyNA(exportacoes_ncm$ano), !anyNA(exportacoes_ncm$codigo_uf),
  !anyNA(exportacoes_ncm$valor_fob_usd),
  all(nchar(exportacoes_ncm$codigo_ncm) == 8),
  # cada NCM tem uma única descrição em cada ano
  !anyDuplicated(distinct(exportacoes_ncm, ano, codigo_ncm, produto_ncm_desc)[c("ano", "codigo_ncm")])
)

# 3. Tabela no formato do orientador -------------------------------------------
# Países em ordem alfabética do português ("África do Sul" antes de "Alemanha")
# No Brasil inteiro, a tabela larga com todos os NCMs teria centenas de milhares
# de linhas x ~470 colunas (vários GB de memória). Por isso a aba por_pais fica
# restrita aos capítulos da NCM de origem vegetal e seus primeiros
# processamentos (os mesmos do script 05b). O arquivo long (.rds) mantém tudo.
CAPITULOS_AGRO <- c(sprintf("%02d", 6:15), "17", "18", "20", "24", "52")
base_wide <- exportacoes_ncm %>% filter(substr(codigo_ncm, 1, 2) %in% CAPITULOS_AGRO)

paises       <- stringi::stri_sort(unique(base_wide$pais), locale = "pt_BR")
colunas_pais <- as.vector(rbind(paste0(paises, "_fob"), paste0(paises, "_kg")))

exportacoes_ncm_wide <- base_wide %>%
  summarise(fob = sum(valor_fob_usd), kg = sum(peso_liquido_kg),
            .by = c(ano, codigo_uf, sigla_uf, codigo_ncm, produto_ncm_desc, pais)) %>%
  pivot_wider(names_from = pais, values_from = c(fob, kg),
              names_glue = "{pais}_{.value}", values_fill = 0) %>%
  select(ano, codigo_uf, sigla_uf, codigo_ncm, produto_ncm_desc, all_of(colunas_pais)) %>%
  arrange(codigo_uf, ano, codigo_ncm)

# O formato largo tem os mesmos totais do longo
stopifnot(
  ncol(exportacoes_ncm_wide) == 5 + 2 * length(paises),
  abs(sum(select(exportacoes_ncm_wide, ends_with("_fob"))) - sum(base_wide$valor_fob_usd)) < 1,
  abs(sum(select(exportacoes_ncm_wide, ends_with("_kg")))  - sum(base_wide$peso_liquido_kg)) < 1
)

cat(sprintf(
  "\n%d linhas (long) | %d linhas x %d colunas (por_pais) | %d NCMs | %d países | %d anos | FOB total US$ %s\n",
  nrow(exportacoes_ncm), nrow(exportacoes_ncm_wide), ncol(exportacoes_ncm_wide),
  n_distinct(exportacoes_ncm$codigo_ncm), length(paises), n_distinct(exportacoes_ncm$ano),
  format(sum(exportacoes_ncm$valor_fob_usd), big.mark = ".", decimal.mark = ",")
))

# 4. Aba só com os 35 produtos agrícolas da pesquisa ---------------------------
# Usa o crosswalk do script 04. Mesma estrutura da aba por_pais, com a coluna
# produto_pam após a descrição do NCM; ficam só os países que compraram algum
# desses produtos. Manga e Goiaba dividem o NCM 08045000 e aparecem juntas.
arq_cw <- here("05_painel", "dados", "crosswalk_produto_pam_ncm_1997_2024.xlsx")
exigir_arquivo(arq_cw, "04_crosswalk_produto_pam_ncm.R")

mapa_ncm <- readxl::read_excel(arq_cw, col_types = "text") %>%
  filter(!is.na(codigos_ncm)) %>%
  separate_rows(codigos_ncm, sep = ",") %>%
  transmute(codigo_ncm  = str_trim(codigos_ncm),
            produto_pam = if_else(produto_pam %in% c("Manga", "Goiaba"), "Manga e Goiaba", produto_pam)) %>%
  distinct()
stopifnot(!anyDuplicated(mapa_ncm$codigo_ncm))

por_pais_35 <- exportacoes_ncm_wide %>%
  inner_join(mapa_ncm, by = "codigo_ncm") %>%
  relocate(produto_pam, .after = produto_ncm_desc)

paises_35 <- paises[vapply(paises, function(p)
  sum(por_pais_35[[paste0(p, "_fob")]]) > 0 || sum(por_pais_35[[paste0(p, "_kg")]]) > 0, logical(1))]
por_pais_35 <- por_pais_35 %>%
  select(ano, codigo_uf, sigla_uf, codigo_ncm, produto_ncm_desc, produto_pam,
         all_of(as.vector(rbind(paste0(paises_35, "_fob"), paste0(paises_35, "_kg"))))) %>%
  arrange(codigo_uf, ano, codigo_ncm)

fob_35 <- sum(exportacoes_ncm$valor_fob_usd[exportacoes_ncm$codigo_ncm %in% mapa_ncm$codigo_ncm])
stopifnot(abs(sum(select(por_pais_35, ends_with("_fob"))) - fob_35) < 1)
cat(sprintf("Aba por_pais_35_produtos: %d linhas x %d colunas | %d NCMs | %d países | FOB US$ %s\n",
            nrow(por_pais_35), ncol(por_pais_35), n_distinct(por_pais_35$codigo_ncm),
            length(paises_35), format(round(fob_35), big.mark = ".", decimal.mark = ",")))

# 5. Saídas --------------------------------------------------------------------
saveRDS(exportacoes_ncm_wide, arq_wide)
# O Excel aceita até 1.048.576 linhas e 16.384 colunas por aba; aba maior fica só no .rds
cabe_no_excel <- function(df) nrow(df) < 1048576 && ncol(df) <= 16384
abas <- list(long = exportacoes_ncm, por_pais = exportacoes_ncm_wide,
             por_pais_35_produtos = por_pais_35)
grandes <- names(abas)[!vapply(abas, cabe_no_excel, logical(1))]
if (length(grandes) > 0) {
  message("Abas grandes demais para o Excel, salvas só em .rds: ", paste(grandes, collapse = ", "))
  for (a in grandes) saveRDS(abas[[a]], sub("\\.xlsx$", paste0("_", a, ".rds"), arq_xlsx))
}
write_xlsx(abas[setdiff(names(abas), grandes)], arq_xlsx)
cat("Arquivos salvos em", pasta, "\n")
