# 05b_diagnostico_ncm_fora_do_crosswalk.R
# Serve para revisar o crosswalk (script 04) para a região. O crosswalk foi
# montado com as exportações do Piauí; outros estados exportam produtos que o
# Piauí não exporta (café, suco de laranja, fumo, cacau, uva, manga...).
# Lista todos os NCMs de origem vegetal exportados pelos estados de UFS,
# marcando os que já estão no crosswalk e os que não estão.
# Saída: 05_painel/dados/diagnostico_ncm_<regiao>.xlsx

source(here::here("scripts", "funcoes_comuns.R"))
library(dplyr)
library(tidyr)
library(stringr)
library(readxl)
library(writexl)
library(here)

arq_exp <- here("02_exportacoes", paste0("exportacoes_ncm_", REGIAO, "_estado_1997_2024.rds"))
arq_cw  <- here("05_painel", "dados", "crosswalk_produto_pam_ncm_1997_2024.xlsx")
exigir_arquivo(arq_exp, "05_exportacoes_extracao_ncm_1997_2024.R")
exigir_arquivo(arq_cw,  "04_crosswalk_produto_pam_ncm.R")

# Capítulos da NCM com produtos de lavoura e seus primeiros processamentos:
# 06-14 produtos vegetais; 15 óleos; 17 açúcar; 18 cacau; 20 sucos e
# preparações de frutas e hortaliças; 24 fumo; 52 algodão
CAPITULOS <- c(sprintf("%02d", 6:15), "17", "18", "20", "24", "52")

mapa <- read_excel(arq_cw, col_types = "text") %>%
  filter(!is.na(codigos_ncm)) %>%
  separate_rows(codigos_ncm, sep = ",") %>%
  transmute(codigo_ncm = str_trim(codigos_ncm), produto_pam) %>%
  summarise(produto_pam = paste(produto_pam, collapse = " / "), .by = codigo_ncm)

exp <- readRDS(arq_exp) %>%
  filter(substr(codigo_ncm, 1, 2) %in% CAPITULOS) %>%
  left_join(mapa, by = "codigo_ncm")

por_ncm <- exp %>%
  summarise(fob_total_usd = sum(valor_fob_usd), peso_total_t = sum(peso_liquido_kg) / 1000,
            anos_com_exportacao = n_distinct(ano),
            estados = paste(sort(unique(sigla_uf)), collapse = ","),
            .by = c(codigo_ncm, produto_ncm_desc, produto_pam)) %>%
  # a descrição muda entre anos; fica a mais recente por NCM
  slice_max(fob_total_usd, n = 1, with_ties = FALSE, by = codigo_ncm) %>%
  mutate(capitulo = substr(codigo_ncm, 1, 2), sh4 = substr(codigo_ncm, 1, 4),
         no_crosswalk = !is.na(produto_pam),
         participacao_pct = round(100 * fob_total_usd / sum(fob_total_usd), 3)) %>%
  arrange(no_crosswalk, desc(fob_total_usd)) %>%
  relocate(capitulo, sh4, .before = codigo_ncm)

por_ncm_estado <- exp %>%
  filter(is.na(produto_pam)) %>%
  summarise(fob_total_usd = sum(valor_fob_usd), .by = c(codigo_ncm, sigla_uf)) %>%
  pivot_wider(names_from = sigla_uf, values_from = fob_total_usd, values_fill = 0)

resumo <- por_ncm %>%
  summarise(ncms = n(), fob_total_usd = sum(fob_total_usd), .by = no_crosswalk) %>%
  mutate(participacao_pct = round(100 * fob_total_usd / sum(fob_total_usd), 1))
print(resumo)
cat("\n30 maiores NCMs fora do crosswalk:\n")
print(por_ncm %>% filter(!no_crosswalk) %>% select(codigo_ncm, produto_ncm_desc, fob_total_usd, estados) %>%
        head(30), n = 30, width = 200)

arq <- here("05_painel", "dados", paste0("diagnostico_ncm_", REGIAO, ".xlsx"))
write_xlsx(list(resumo = resumo, por_ncm = por_ncm,
                fora_do_crosswalk_por_estado = left_join(
                  filter(por_ncm, !no_crosswalk) %>% select(codigo_ncm, produto_ncm_desc, fob_total_usd),
                  por_ncm_estado, by = "codigo_ncm")), arq)
cat("\nDiagnóstico salvo em", arq, "\n")
