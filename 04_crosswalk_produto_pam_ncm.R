# 04_crosswalk_produto_pam_ncm.R  (versão Brasil, 27 estados)
# Correspondência entre os produtos da PAM e os NCMs do Comex Stat. Não existe
# tabela oficial: cada ligação segue a descrição do NCM, conferida contra as
# exportações reais do Brasil (diagnóstico do script 05b, 24/11/2026). O motivo
# de cada decisão está na coluna observacao.
# Saída: 05_painel/dados/crosswalk_produto_pam_ncm_1997_2024.xlsx
#
# Regras
# 1. Entra o NCM do produto na forma em que a PAM o mede, ou na forma mínima de
#    comercialização (seco, descascado, debulhado, destalado). Sementes para
#    semeadura do mesmo produto também entram (são o mesmo grão).
# 2. Formas processadas (óleo, suco, farinha, açúcar, torrado, chocolate) NÃO
#    entram: a tonelada exportada não é a tonelada colhida.
# 3. Produto exportado principalmente processado fica com status
#    "confirmado_real_produto_processado" e sai do modelo (como a cana no Piauí).
#    O NCM in natura, quando existe, fica listado só para registro.
# 4. Produtos da PAM sem NCM exportado: status "sem_ncm_exportado" (exportação 0).
# 5. Café: entra só o "Café (em grão) Total"; Arábica e Canephora são parcelas
#    dele e ficariam em dobro.
# Fora do escopo (não são lavouras da PAM): castanha-do-pará (extrativismo,
# PEVS), ceras vegetais (carnaúba, PEVS), gengibre, cravo, especiarias, frutas
# sem produto na PAM, sementes forrageiras.

source(here::here("scripts", "funcoes_comuns.R"))
library(dplyr)
library(writexl)
library(here)

P  <- "confirmado_real"
PF <- "confirmado_real_forma_minima"            # descascado, destalado, debulhado, seco
PO <- "confirmado_real_ncm_inclui_outros"       # o NCM inclui produtos parecidos
PR <- "confirmado_real_produto_processado"      # sai do modelo
RT <- "confirmado_real_compartilhado_rateio"    # 08045000, manga e goiaba
Z  <- "sem_ncm_exportado"
DOM <- "zero_por_regra_produto_dominante"

crosswalk_ncm <- tribble(
  ~produto_pam, ~codigos_ncm, ~status_ncm, ~observacao,
  # Grãos e oleaginosas ---------------------------------------------------------
  "Soja (em grão)", "12019000,12010090,12010010,12011000", P, "12010090/12010010 são os códigos anteriores à revisão da NCM; inclui soja para semeadura. Óleo e farelo (cap. 15 e 23) não entram",
  "Milho (em grão)", "10059010,10059090,10051000", P, "Inclui milho para semeadura. Farinha, amido e óleo não entram",
  "Sorgo (em grão)", "10079000,10070090,10071000,10070010", P, "Inclui sorgo para semeadura",
  "Trigo (em grão)", "10019900,10019090,10011900,10011090,10019010,10011100,10011010,10019100", P, "Trigo duro e comum, inclusive para semeadura",
  "Centeio (em grão)", "10029000,10020090,10020010,10021000", P, "",
  "Cevada (em grão)", "10030098,10039080,10039010,10030091,10031000", P, "Só cevada em grão; 10039090 e 10030099 (exceto em grão) não entram",
  "Aveia (em grão)", "10049000,10040090,10040010,10041000", P, "",
  "Triticale (em grão)", "10086090", P, "",
  "Arroz (em casca)", "10061092,10061010,10061091", PR, "Exportado sobretudo beneficiado (10062x a 10064x, cerca de 3 vezes o valor do arroz em casca). Sai do modelo; listado só o arroz em casca. Alternativa: converter o beneficiado em casca por coeficiente técnico (falar com o orientador)",
  "Feijão (em grão)", "07133399,07133190,07133590,07133319,07133990,07133290,07133329,07133311,07133391,07133110,07133321,07133910,07133210,07133510", P, "Todas as variedades de Phaseolus e Vigna, inclusive para semeadura; feijão-guando e bambara não entram",
  "Fava (em grão)", "07135010,07135090", P, "",
  "Ervilha (em grão)", "07131090,07131010", P, "",
  "Amendoim (em casca)", "12024200,12022090,12021000,12024100,12023000,12022010", PF, "Exportado quase todo descascado (o peso cai cerca de 30%); a PAM mede em casca. Amendoim preparado e óleo não entram",
  "Girassol (em grão)", "12060010,12060090", P, "",
  "Linho (semente)", "12040090,12040010", P, "",
  "Canola", "12051090,12051010,12059090,12059010", P, "",
  "Mamona (baga)", "12073010,12073090,12079911,12079992", P, "Sementes de rícino",
  "Gergelim", "12074090,12074010", P, "A PAM só pesquisa gergelim em parte do período; anos sem produção saem pela regra de exportação sem produção",
  "Urucum (semente)", "12079990", PO, "NCM genérico de outras sementes oleaginosas",
  "Algodão herbáceo (em caroço)", "52010020,52010010,52010090", PF, "NCM é de algodão em pluma (debulhado); a PAM mede em caroço (a pluma é cerca de 40% do peso)",
  "Algodão arbóreo (em caroço)", NA, DOM, "NCM 5201 não separa herbáceo e arbóreo; tudo atribuído ao herbáceo",
  "Café (em grão) Total", "09011110,09011190,09011200", P, "Café verde (não torrado). Parte da exportação de café o Comex Stat registra como 'Estados Diversos - Café', sem estado: subestima os estados, mas entra no painel Brasil somado (06b)",
  "Cacau (em amêndoa)", "18010000", PR, "Exportado sobretudo processado (manteiga, pasta, pó, chocolate: cerca de 80 vezes o valor da amêndoa). Sai do modelo",
  "Fumo (em folha)", "24012030,24012040,24011030,24012090,24011090,24011020,24011040,24012020,24011010,24012010", PF, "Fumo em folha, destalado ou não. Desperdícios (24013000) e fumo manufaturado não entram",
  "Pimenta-do-reino", "09041100", P, "Pimenta em grão; triturada ou em pó (09041200) não entra",
  "Erva-mate (folha verde)", "09030010,09030090", PR, "O mate exportado é cancheado ou beneficiado; a PAM mede a folha verde. Sai do modelo",
  "Chá-da-índia (folha verde)", "09021000,09022000,09023000,09024000", PR, "O chá exportado é seco/fermentado; a PAM mede a folha verde. Sai do modelo",
  "Cana-de-açúcar", "17011400,17011100,17019900", PR, "Exportação é de açúcar, não de cana. Sai do modelo",
  "Dendê (cacho de coco)", NA, PR, "Exportado como óleo de dendê (15111000). Sai do modelo",
  # Hortaliças e tubérculos --------------------------------------------------------
  "Batata-inglesa", "07019000,07011000", P, "",
  "Batata-doce", "07142000", P, "",
  "Mandioca", "07141000", P, "Fécula e farinha não entram",
  "Inhame", "07143000", P, "",
  "Tomate", "07020000", P, "",
  "Cebola", "07031019,07031011", P, "",
  "Alho", "07032090,07032010", P, "",
  "Cenoura", "07061000", PO, "NCM inclui nabos",
  "Abóbora", "07099300", PO, "NCM inclui abobrinhas e cabaças",
  "Pimentão", "07096000", PO, "NCM inclui pimentas Capsicum frescas",
  "Repolho", "07049000", PO, "NCM inclui couves do gênero Brassica",
  "Azeitona", "07099200", P, "",
  # Frutas -------------------------------------------------------------------------
  "Laranja", "08051000", PR, "Exportada sobretudo como suco (2009.1x, cerca de 140 vezes o valor da fruta fresca). Sai do modelo",
  "Tangerina", "08052000,08052100,08052200,08052900", P, "",
  "Limão", "08055000,08053000", P, "08053000 é o código anterior à revisão da NCM",
  "Uva", "08061000,08062000", P, "Inclui passas; suco e vinho não entram",
  "Maçã", "08081000", P, "Suco de maçã não entra",
  "Pera", "08083000,08082010", P, "",
  "Marmelo", "08084000,08082020", P, "",
  "Pêssego", "08093010", P, "",
  "Caqui", "08107000", P, "",
  "Morango", "08101000", P, "",
  "Maracujá", "08109015", P, "NCM específico só existe nos anos mais recentes",
  "Figo", "08042010,08042020", P, "",
  "Abacate", "08044000", P, "",
  "Abacaxi*", "08043000", P, "PAM em mil frutos: fora do modelo pela regra de unidade",
  "Banana (cacho)", "08030000,08039000,08031000", P, "08030000 é o código anterior à revisão da NCM",
  "Mamão", "08072000", P, "",
  "Melão", "08071900", P, "",
  "Melancia", "08071100", P, "",
  "Manga", "08045020,08045000", RT, "08045020 é só manga; 08045000 (goiabas e mangas) é rateado com a goiaba",
  "Goiaba", "08045010,08045000", RT, "08045010 é só goiaba; 08045000 é rateado com a manga",
  "Coco-da-baía*", "08011900,08011100,08011190,08011110,08011200", P, "PAM em mil frutos: fora do modelo pela regra de unidade. Água de coco não entra",
  "Castanha de caju", "08013200,08013100", PF, "Exportada sem casca; a PAM mede com casca. Parte da castanha exportada é importada da África e beneficiada no Brasil (exportação maior que a produção em vários estados)",
  "Noz (fruto seco)", "08023100,08023200", P, ""
)

# Produtos da PAM sem exportação ou fora do comércio exterior em grão
sem_ncm <- c("Alface", "Alfafa fenada", "Açaí", "Borracha (látex coagulado)", "Borracha (látex líquido)",
             "Caju", "Cana para forragem", "Chuchu", "Cupuaçu", "Graviola", "Guaraná (semente)",
             "Juta (fibra)", "Malva (fibra)", "Milho verde", "Palmito", "Rami (fibra)",
             "Sisal ou agave (fibra)", "Tungue (fruto seco)", "Acerola")
crosswalk_ncm <- bind_rows(
  crosswalk_ncm,
  tibble(produto_pam = sem_ncm, codigos_ncm = NA, status_ncm = Z,
         observacao = "Sem NCM do produto in natura exportado, ou fibra/látex fora dos capítulos extraídos")
)

# Conferências
codigos <- unlist(strsplit(na.omit(crosswalk_ncm$codigos_ncm), ","))
stopifnot(
  !anyDuplicated(crosswalk_ncm$produto_pam),
  all(nchar(codigos) == 8),
  sum(codigos == "08045000") == 2,          # único NCM compartilhado (manga e goiaba)
  !anyDuplicated(codigos[codigos != "08045000"])
)

pasta <- here("05_painel", "dados")
dir.create(pasta, recursive = TRUE, showWarnings = FALSE)
write_xlsx(crosswalk_ncm, file.path(pasta, "crosswalk_produto_pam_ncm_1997_2024.xlsx"))

cat("Crosswalk salvo:", nrow(crosswalk_ncm), "produtos,",
    sum(!is.na(crosswalk_ncm$codigos_ncm)), "com NCM,",
    length(unique(codigos)), "NCMs distintos,",
    sum(crosswalk_ncm$status_ncm == PR), "processados (fora do modelo).\n")
