##### packages
library(tidyverse)
library(gtsummary)
library(fixest)
library(broom)


##### load data deforestation-price data
load("../Data/PriceData_Clean_v7_FullSample.R")


#### table with reasons for exclusion
n_total <- nrow(df)

df %>%
  mutate(
    DUPLICATE_EXCL = duplicated(.),
    OUTLIER_EXCL = OUTLIER %in% TRUE,
    INSPECTION_EXCL = !(NIVEL_DE_INSPECAO %in% c("SIF", "SIE")),
    MISSING_DATA_EXCL = is.na(COMPROMISSOS) |
      is.na(DISTANCIA_KM) |
      is.na(PRECO_POR_KG) |
      is.na(DESMATAMENTO)
  ) %>%
  summarise(
    Duplicates = sum(DUPLICATE_EXCL, na.rm = TRUE),
    Outliers = sum(OUTLIER_EXCL, na.rm = TRUE),
    Invalid_Inspection = sum(INSPECTION_EXCL, na.rm = TRUE),
    Missing = sum(MISSING_DATA_EXCL, na.rm = TRUE)
  ) %>%
  tidyr::pivot_longer(
    cols = everything(),
    names_to = "Reason",
    values_to = "Count"
  ) %>%
  mutate(
    Percent = round(100 * Count / n_total, 2)
  ) %>% 
  writexl::write_xlsx(., "../Outputs/ReasonsForExclusion.xlsx")


### adjust factors to be displayed in results
df <- df %>%
  mutate(ESTADO_LOJA = factor(ESTADO_LOJA,
                              levels = c("AC", "MS", "PE", "SC", "SP"),
                              labels = c("Acre", "Mato Grosso do Sul", "Pernambuco", "Santa Catarina", "São Paulo"),
                              ordered = T),
         ABRAS_CLASBR_CAT = factor(ABRAS_CLASBR_CAT,
                                   levels = c("Top 10 ABRAS", "Top 900 ABRAS", "Other stores"),
                                   labels = c("Top 10", "Top 900", "Smaller stores"),
                                   ordered = T),
         TIPO_DE_PRODUTO_CLASS = factor(TIPO_DE_PRODUTO_CLASS,
                                        levels = c("premium", "non-premium", "offal", "processed"),
                                        labels = c("Premium", "Non-premium", "Offal", "Processed"),
                                        ordered = T),
         NIVEL_DE_INSPECAO = factor(NIVEL_DE_INSPECAO,
                                    levels = c("SIF", "SIE"),
                                    labels = c("Federal", "State"),
                                    ordered = T),
         COMPROMISSOS = factor(COMPROMISSOS,
                               levels = c("COMPROMISSO", "NAO POSSUI ACORDOS"),
                               labels = c("Yes", "No"),
                               ordered = T),
         AMAZONIALEGAL = factor(AMAZONIALEGAL,
                                levels = c("SIM", "NAO"),
                                labels = c("Yes", "No"),
                                ordered = T),
         JBS = factor(JBS,
                      levels = c("SIM", "NAO"),
                      labels = c("Yes", "No"),
                      ordered = T),
         PRECO_POR_KG = round(PRECO_POR_KG, 0),
         # distance as 100 km
         DISTANCIA_100KM = DISTANCIA_KM/100)



#######################################################################################################################
### Sample description
tab_all <- df %>%
  select(ESTADO_LOJA, ABRAS_CLASBR_CAT, TIPO_DE_PRODUTO_CLASS, 
         NIVEL_DE_INSPECAO, COMPROMISSOS, DISTANCIA_KM,
         DESMATAMENTO, PRECO_POR_KG) %>%
  tbl_summary(
    statistic = list(all_continuous()  ~ "{median} ({p25}, {p75})",
                     all_categorical() ~ "{n}    ({p}%)"),
    digits = list(all_continuous()  ~ c(0, 0),
                  all_categorical() ~ c(0, 0)),
    type = list(ESTADO_LOJA ~ "categorical",
                ABRAS_CLASBR_CAT ~ "categorical",
                TIPO_DE_PRODUTO_CLASS ~ "categorical",
                NIVEL_DE_INSPECAO ~ "categorical",
                COMPROMISSOS ~ "categorical",
                DISTANCIA_KM ~ "continuous",
                DESMATAMENTO ~ "continuous",
                PRECO_POR_KG ~ "continuous"
    ),
    label  = list(ESTADO_LOJA ~ "Retail store state",
                  ABRAS_CLASBR_CAT ~ "Retail store size",
                  TIPO_DE_PRODUTO_CLASS ~ "Product type",
                  NIVEL_DE_INSPECAO ~ "Sanitary inspection level",
                  COMPROMISSOS ~ "Supply chain monitoring",
                  DISTANCIA_KM ~ "Meatpacker - retail store distance (km)",
                  DESMATAMENTO ~ "Deforestation",
                  PRECO_POR_KG ~ "Price per kg (Brazilian Reais)"
    )
  ) %>%
  modify_header(label = "**Characteristic**") %>%
  modify_caption("Description of beef products sampled") %>%
  bold_labels()


tab_excl <- df %>%
  select(ESTADO_LOJA, ABRAS_CLASBR_CAT, TIPO_DE_PRODUTO_CLASS, 
         NIVEL_DE_INSPECAO, COMPROMISSOS, DISTANCIA_KM, 
         DESMATAMENTO, PRECO_POR_KG, EXCLUDE) %>%
  tbl_summary(
    by = EXCLUDE, 
    statistic = list(all_continuous()  ~ "{median} ({p25}, {p75})",
                     all_categorical() ~ "{n}    ({p}%)"),
    digits = list(all_continuous()  ~ c(0, 0),
                  all_categorical() ~ c(0, 0)),
    type = list(ESTADO_LOJA ~ "categorical",
                ABRAS_CLASBR_CAT ~ "categorical",
                TIPO_DE_PRODUTO_CLASS ~ "categorical",
                NIVEL_DE_INSPECAO ~ "categorical",
                COMPROMISSOS ~ "categorical",
                DISTANCIA_KM ~ "continuous",
                DESMATAMENTO ~ "continuous",
                PRECO_POR_KG ~ "continuous"
    ),
    label  = list(ESTADO_LOJA ~ "Retail store state",
                  ABRAS_CLASBR_CAT ~ "Retail store size",
                  TIPO_DE_PRODUTO_CLASS ~ "Product type",
                  NIVEL_DE_INSPECAO ~ "Sanitary inspection level",
                  COMPROMISSOS ~ "Supply chain monitoring",
                  DISTANCIA_KM ~ "Meatpacker - retail store distance (km)",
                  DESMATAMENTO ~ "Deforestation",
                  PRECO_POR_KG ~ "Price per kg (Brazilian Reais)"
    )
  ) %>%
  modify_header(label = "**Characteristic**") %>%
  modify_caption("Description of beef products sampled") %>%
  bold_labels()


# merge and export tables
tbl_merge(
  tbls = list(tab_all, tab_excl),
  tab_spanner = c("**Total**", "**Exclusion**")
  ) %>%
  gtsummary::as_tibble() %>%
  writexl::write_xlsx(., "../Outputs/SampleDescription_Exclusion.xlsx")
