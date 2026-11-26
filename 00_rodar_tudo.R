# 00_rodar_tudo.R
# Roda o pipeline inteiro, em ordem, e grava registro_execucao.txt (tempos e versões).
# Abra antes o projeto pelo .Rproj. A etapa mais longa é a 05 (API do Comex Stat).

if (!requireNamespace("here", quietly = TRUE)) install.packages("here")
source(here::here("scripts", "funcoes_comuns.R"))

# Brasil agregado, produto x ano.
etapas <- c(
  "01_pam_extracao_1997_2024.R"                   = "Extrai a PAM/IBGE (tabela 5457), total do Brasil",
  "02_pam_validacao_cruzada_sidra_1997_2024.R"    = "Revalida a PAM célula a célula contra a API do SIDRA",
  "03_pam_analise_descritiva_1997_2024.R"         = "Estatísticas e gráficos da PAM (Brasil)",
  "04_crosswalk_produto_pam_ncm.R"                = "Tabela de correspondência produto PAM -> NCM (versão Brasil)",
  "05_exportacoes_extracao_ncm_1997_2024.R"       = "Extrai as exportações do Brasil (Comex Stat, NCM x país, sem filtro de estado)",
  "05b_diagnostico_ncm_fora_do_crosswalk.R"       = "Lista os NCMs agrícolas da região fora do crosswalk",
  "06_painel_merge_pam_comex_1997_2024.R"         = "Junta PAM e exportações no painel produto x ano (Brasil)",
  "07_exportacoes_analise_descritiva_1997_2024.R" = "Estatísticas e gráficos das exportações (Brasil)",
  "08_pam_verificacao_lista_produtos_1997_2024.R" = "Verifica entrada e saída de produtos na PAM",
  "09_testes_blocos_1a3.R"                        = "Testes: amostra, séries (dependência e raiz unitária) e especificação",
  "10_testes_robustez.R"                          = "Robustez: denominador da intensidade, rendimento defasado, um produto por vez",
  "11_diferencas_longas.R"                        = "Diferenças longas: 5 e 10 anos, períodos de 7 anos, 2001-05 x 2020-24",
  "12_direcao_da_relacao.R"                       = "Direção: Granger em painel e diferenças longas defasadas nos dois sentidos",
  "13_bootstrap_diferencas_longas.R"              = "Bootstrap selvagem por produto nas diferenças longas"
)

minutos_desde <- function(t0) round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1)
linha <- strrep("=", 62)

registro     <- character()
inicio_total <- Sys.time()

for (script in names(etapas)) {
  cat("\n", linha, "\n", script, " - ", etapas[[script]], "\n", linha, "\n", sep = "")

  t0 <- Sys.time()
  tryCatch(
    source(here::here("scripts", script), local = new.env(), encoding = "UTF-8"),
    error = function(e) {
      stop("A etapa ", script, " falhou: ", conditionMessage(e),
           "\nCorrija e rode de novo (as etapas anteriores já salvaram seus arquivos).",
           call. = FALSE)
    }
  )
  registro <- c(registro, sprintf("%-48s %6.1f min", script, minutos_desde(t0)))
  cat("\nOK:", script, "em", minutos_desde(t0), "min\n")
}

dt_total <- minutos_desde(inicio_total)

writeLines(c(
  paste("Execução completa do pipeline em", format(Sys.time(), "%d/%m/%Y %H:%M")),
  paste("Tempo total:", dt_total, "min"),
  "",
  registro,
  "",
  capture.output(sessionInfo())
), here::here("registro_execucao.txt"))

cat("\nPipeline concluído em", dt_total, "min. Registro salvo em registro_execucao.txt\n")
