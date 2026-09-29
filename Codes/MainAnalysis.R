##### packages
library(tidyverse)
library(gtsummary)
library(fixest)
library(broom)


##### load data deforestation-price data
load("../Data/PriceData_Clean_v7_FinalSample.R")


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
### Table 1 - sample description
tab1_all <- df %>%
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


tab1_lam <- df %>%
  select(ESTADO_LOJA, ABRAS_CLASBR_CAT, TIPO_DE_PRODUTO_CLASS, 
         NIVEL_DE_INSPECAO, COMPROMISSOS, DISTANCIA_KM, 
         DESMATAMENTO, PRECO_POR_KG, AMAZONIALEGAL) %>%
  tbl_summary(
    by = AMAZONIALEGAL, 
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


tab1_jbs <- df %>%
  select(ESTADO_LOJA, ABRAS_CLASBR_CAT, TIPO_DE_PRODUTO_CLASS, 
         NIVEL_DE_INSPECAO, COMPROMISSOS, DISTANCIA_KM, 
         DESMATAMENTO, PRECO_POR_KG, JBS) %>%
  tbl_summary(
    by = JBS, 
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
  tbls = list(tab1_all, tab1_lam, tab1_jbs),
  tab_spanner = c("**Total**", "**Legal Amazon**", "**JBS**")
  ) %>%
  gtsummary::as_tibble() %>%
  writexl::write_xlsx(., "../Outputs/Results_01_SampleDescription.xlsx")



#######################################################################################################################
### Table 2 - desmatamento
tab_vars <- c("TIPO_DE_PRODUTO_CLASS", "ESTADO_LOJA", "ABRAS_CLASBR_CAT", "NIVEL_DE_INSPECAO", "COMPROMISSOS")

tab2_all <- df %>%
  mutate_at(.vars=tab_vars,.funs=funs(as.character(.))) %>%
  select(all_of(tab_vars), DESMATAMENTO) %>%
  pivot_longer(-DESMATAMENTO) %>%
  group_by(name, value) %>%
  summarise(ALL = paste0(median(DESMATAMENTO),
                        " (",
                        quantile(DESMATAMENTO, 0.25),
                        "; ",
                        quantile(DESMATAMENTO, 0.75),
                        ")"
                        )
            )

tab2_lam_yes <- df %>%
  filter(AMAZONIALEGAL %in% "Yes") %>%
  mutate_at(.vars=tab_vars,.funs=funs(as.character(.))) %>%
  select(all_of(tab_vars), DESMATAMENTO) %>%
  pivot_longer(-DESMATAMENTO) %>%
  group_by(name, value) %>%
  summarise(LAM_yes = paste0(median(DESMATAMENTO),
                         " (",
                         quantile(DESMATAMENTO, 0.25),
                         "; ",
                         quantile(DESMATAMENTO, 0.75),
                         ")"
                         )
            )


tab2_lam_no <- df %>%
  filter(AMAZONIALEGAL %in% "No") %>%
  mutate_at(.vars=tab_vars,.funs=funs(as.character(.))) %>%
  select(all_of(tab_vars), DESMATAMENTO) %>%
  pivot_longer(-DESMATAMENTO) %>%
  group_by(name, value) %>%
  summarise(LAM_no = paste0(median(DESMATAMENTO),
                             " (",
                             quantile(DESMATAMENTO, 0.25),
                             "; ",
                             quantile(DESMATAMENTO, 0.75),
                             ")"
  )
  )


tab2_jbs_yes <- df %>%
  filter(JBS %in% "Yes") %>%
  mutate_at(.vars=tab_vars,.funs=funs(as.character(.))) %>%
  select(all_of(tab_vars), DESMATAMENTO) %>%
  pivot_longer(-DESMATAMENTO) %>%
  group_by(name, value) %>%
  summarise(JBS_yes = paste0(median(DESMATAMENTO),
                             " (",
                             quantile(DESMATAMENTO, 0.25),
                             "; ",
                             quantile(DESMATAMENTO, 0.75),
                             ")"
                             )
            )

tab2_jbs_no <- df %>%
  filter(JBS %in% "No") %>%
  mutate_at(.vars=tab_vars,.funs=funs(as.character(.))) %>%
  select(all_of(tab_vars), DESMATAMENTO) %>%
  pivot_longer(-DESMATAMENTO) %>%
  group_by(name, value) %>%
  summarise(JBS_no = paste0(median(DESMATAMENTO),
                             " (",
                             quantile(DESMATAMENTO, 0.25),
                             "; ",
                             quantile(DESMATAMENTO, 0.75),
                             ")"
                             )
            )

tab2_all %>%
  left_join(tab2_lam_yes) %>%
  left_join(tab2_lam_no) %>%
  left_join(tab2_jbs_yes) %>%
  left_join(tab2_jbs_no) %>%
  writexl::write_xlsx("../Outputs/Results_02_DeforestationMedian.xlsx")



#######################################################################################################################
### Table 3 - prices

tab3_all <- df %>%
  mutate_at(.vars=tab_vars,.funs=funs(as.character(.))) %>%
  select(all_of(tab_vars), PRECO_POR_KG) %>%
  pivot_longer(-PRECO_POR_KG) %>%
  group_by(name, value) %>%
  summarise(ALL = paste0(median(PRECO_POR_KG),
                         " (",
                         quantile(PRECO_POR_KG, 0.25),
                         "; ",
                         quantile(PRECO_POR_KG, 0.75),
                         ")"
  )
  )

tab3_lam_yes <- df %>%
  filter(AMAZONIALEGAL %in% "Yes") %>%
  mutate_at(.vars=tab_vars,.funs=funs(as.character(.))) %>%
  select(all_of(tab_vars), PRECO_POR_KG) %>%
  pivot_longer(-PRECO_POR_KG) %>%
  group_by(name, value) %>%
  summarise(LAM_yes = paste0(median(PRECO_POR_KG),
                             " (",
                             quantile(PRECO_POR_KG, 0.25),
                             "; ",
                             quantile(PRECO_POR_KG, 0.75),
                             ")"
  )
  )


tab3_lam_no <- df %>%
  filter(AMAZONIALEGAL %in% "No") %>%
  mutate_at(.vars=tab_vars,.funs=funs(as.character(.))) %>%
  select(all_of(tab_vars), PRECO_POR_KG) %>%
  pivot_longer(-PRECO_POR_KG) %>%
  group_by(name, value) %>%
  summarise(LAM_no = paste0(median(PRECO_POR_KG),
                            " (",
                            quantile(PRECO_POR_KG, 0.25),
                            "; ",
                            quantile(PRECO_POR_KG, 0.75),
                            ")"
  )
  )


tab3_jbs_yes <- df %>%
  filter(JBS %in% "Yes") %>%
  mutate_at(.vars=tab_vars,.funs=funs(as.character(.))) %>%
  select(all_of(tab_vars), PRECO_POR_KG) %>%
  pivot_longer(-PRECO_POR_KG) %>%
  group_by(name, value) %>%
  summarise(JBS_yes = paste0(median(PRECO_POR_KG),
                             " (",
                             quantile(PRECO_POR_KG, 0.25),
                             "; ",
                             quantile(PRECO_POR_KG, 0.75),
                             ")"
  )
  )

tab3_jbs_no <- df %>%
  filter(JBS %in% "No") %>%
  mutate_at(.vars=tab_vars,.funs=funs(as.character(.))) %>%
  select(all_of(tab_vars), PRECO_POR_KG) %>%
  pivot_longer(-PRECO_POR_KG) %>%
  group_by(name, value) %>%
  summarise(JBS_no = paste0(median(PRECO_POR_KG),
                            " (",
                            quantile(PRECO_POR_KG, 0.25),
                            "; ",
                            quantile(PRECO_POR_KG, 0.75),
                            ")"
  )
  )

tab3_all %>%
  left_join(tab3_lam_yes) %>%
  left_join(tab3_lam_no) %>%
  left_join(tab3_jbs_yes) %>%
  left_join(tab3_jbs_no) %>%
  writexl::write_xlsx("Outputs/Results_03_PricesMedian.xlsx")



#######################################################################################################################
##### models

df <- df %>%
  mutate_at(.vars=tab_vars,.funs=funs(as.character(.)))


## test interaction term requested by reviwer #2
summary(feols(PRECO_POR_KG ~ DESMATAMENTO + DESMATAMENTO*COMPROMISSOS | BAIRRO_MUNI_LOJA,
              data = df,
              cluster = ~BAIRRO_MUNI_LOJA))

summary(feols(PRECO_POR_KG ~ DESMATAMENTO + NIVEL_DE_INSPECAO + TIPO_DE_PRODUTO_CLASS +
                DISTANCIA_100KM + COMPROMISSOS + ABRAS_CLASBR_CAT + DESMATAMENTO*COMPROMISSOS
              | BAIRRO_MUNI_LOJA,
              data = df,
              cluster = ~BAIRRO_MUNI_LOJA))


## standardize desmatamento and distancia, following peer review
df$DESMATAMENTO_std <- as.numeric(scale(df$DESMATAMENTO))
df$DISTANCIA_std    <- as.numeric(scale(df$DISTANCIA_KM))


## main model
mod_main <- feols(PRECO_POR_KG ~ DESMATAMENTO_std + NIVEL_DE_INSPECAO + TIPO_DE_PRODUTO_CLASS +
                    DISTANCIA_std + COMPROMISSOS + ABRAS_CLASBR_CAT | BAIRRO_MUNI_LOJA, 
                  data = df, 
                  cluster = ~BAIRRO_MUNI_LOJA)


## sensitivity analysis 1 - not controlling for supply chain monitoring
# all data
mod_sens1_all <- feols(PRECO_POR_KG ~ DESMATAMENTO_std + NIVEL_DE_INSPECAO + TIPO_DE_PRODUTO_CLASS +
                         DISTANCIA_std + ABRAS_CLASBR_CAT | BAIRRO_MUNI_LOJA, 
                       data = df, 
                       cluster = ~BAIRRO_MUNI_LOJA)

# amazon only, no supply chain
mod_sens1_amazon1 <- feols(PRECO_POR_KG ~ DESMATAMENTO_std + NIVEL_DE_INSPECAO + TIPO_DE_PRODUTO_CLASS +
                             DISTANCIA_std + ABRAS_CLASBR_CAT | BAIRRO_MUNI_LOJA,
                          data = df %>% filter(AMAZONIALEGAL %in% "Yes"), 
                          cluster = ~BAIRRO_MUNI_LOJA)

# amazon only, with supply chain
mod_sens1_amazon2 <- feols(PRECO_POR_KG ~ DESMATAMENTO_std + NIVEL_DE_INSPECAO + TIPO_DE_PRODUTO_CLASS +
                             DISTANCIA_std + COMPROMISSOS +ABRAS_CLASBR_CAT | BAIRRO_MUNI_LOJA,
                          data = df %>% filter(AMAZONIALEGAL %in% "Yes"), 
                          cluster = ~BAIRRO_MUNI_LOJA)

## sensitivity analysis 2 - jbs only
mod_sens2 <- feols(PRECO_POR_KG ~ DESMATAMENTO_std + TIPO_DE_PRODUTO_CLASS + 
                     DISTANCIA_std + ABRAS_CLASBR_CAT | BAIRRO_MUNI_LOJA, 
                   data = df %>% filter(JBS == "Yes"), 
                   cluster = ~BAIRRO_MUNI_LOJA)


tab_main <- tidy(mod_main, conf.int = TRUE)
tab_sens1_all <- tidy(mod_sens1_all, conf.int = TRUE)
tab_sens1_amazon1 <- tidy(mod_sens1_amazon1, conf.int = TRUE)
tab_sens1_amazon2 <- tidy(mod_sens1_amazon2, conf.int = TRUE)
tab_sens2 <- tidy(mod_sens2, conf.int = TRUE)

sheets <- list("Main" = tab_main, 
               "Sensitivity1_ALL" = tab_sens1_all,
               "Sensitivity1_LAM1" = tab_sens1_amazon1,
               "Sensitivity1_LAM2" = tab_sens1_amazon2,
               "Sensitivity2_JBS" = tab_sens2
               ) 

writexl::write_xlsx(sheets, "../Outputs/Results_04_Models_v2.xlsx")



#######################################################################################################################
###### plots
library(forestplot)

tab_names <- c("Deforestation (standard deviation)",
               "Inspection level: State",
               "Product type: Offal",
               "Product type: Premium",
               "Product type: Processed",
               "Distance meatpacker/retail (standard deviation)",
               "Supply chain monitoring: Yes",
               "Store size: Top 10",
               "Store size: Top 900")



### main analysis
df_plot_main <- tab_main %>%
    mutate(mean = estimate,
           lower = conf.low,
           upper = conf.high,
           sig = case_when(p.value < 0.001 ~ "***",
                           p.value < 0.01  ~ "**",
                           p.value < 0.05  ~ "*",
                           TRUE ~ ""),
           term = tab_names,
           estimate = format(round(estimate, 2), nsmall = 2)) 

# forest plot
svg("../Outputs/Figura_Main_v2.svg",
    width = 16,
    height = 12)
df_plot_main %>%
  forestplot(labeltext = c(term, estimate), 
             txt_gp = fpTxtGp(ticks = gpar(fontfamily = "", cex=1.8), # x axis label size
                              xlab  = gpar(fontfamily = "", cex = 2), # x legend size
                              label = gpar(fontfamily = "", cex = 1.8)
             ),
             xticks = seq(-20, 20, by = 5),# add this line to set x-axis ticks
             col=fpColors(box="darkblue",line="darkblue", summary="darkblue"),
             fn.ci_norm =fpDrawCircleCI, # circles
             lty.ci = c(7), # lines
             lwd.ci = 1.8, # line width
             boxsize = .125, # circle size
             xlab="Beef retail price (R$/kg)",
             lineheight = unit(20, "mm"),  # Adjusts the space between lines, or set a numeric value
             colgap = unit(6, "mm")  # Adjusts the gap between columns
  ) %>%
  # column titles
  fp_add_header(term = c("Characteristic"),
                estimate = c("Estimate (R$/kg)")) %>%
  # plot style
  fp_set_zebra_style("#EFEFEF") %>%
  fp_decorate_graph(box=T) 

# title 
grid.text("95% Confidence Interval",
          0.80, # horizontal text position
          0.845, # vertical text position
          gp = gpar(col = "black",
                    font = 2,
                    fontsize = 24))


### manually add stars for p-values
# inspection level (row 2)
grid.text(
  df_plot_main$sig[2],  # Asterisk label
  x = 0.81,  # x position
  y = 0.775,  # y position
  gp = gpar(col = "darkred", cex = 1.5)  # Style: Red, large
)

# offal (row 3)
grid.text(
  df_plot_main$sig[3],  # Asterisk label
  x = 0.73,  # x position
  y = 0.645,  # y position
  gp = gpar(col = "darkred", cex = 1.5)  # Style: Red, large
)

# premium (row 4)
grid.text(
  df_plot_main$sig[4],  # Asterisk label
  x = 0.976,
  y = 0.58,  # Adjust y position
  gp = gpar(col = "darkred", cex = 1.5)  # Style: Red, large
)

# distance (row 6)
grid.text(
  df_plot_main$sig[6],  # Asterisk label
  x = 0.80,  # Position right of CI
  y = 0.45,  # Adjust y position
  gp = gpar(col = "darkred", cex = 1.5)  # Style: Red, large
)

dev.off()




######################### sensitivity analysis

library(sjPlot)
library(viridisLite)
library(unikn)

# define colours
vir_5 <- viridis(n = 5)
seecol(vir_5, col_brd = "white", lwd_brd = 4, 
       main = "Example of a viridis color palette (n = 10)",
       pal_names = paste0("viridis_", 1:10))

### vector to label predictors - I have no idea how plot_models sort them, but I figured this is the correct order
tab_names <- c("Supply chain monitoring: Yes",
               "Store size: Top 900",
               "Store size: Top 10",
               "Distance meatpacker/retail (standard deviation)",
               "Product type: Processed",
               "Product type: Premium",
               "Product type: Offal",
               "Inspection level: State",
               "Deforestation (standard deviation)")


svg("../Outputs/Figura_SensitivityAnalysis_v2.svg",
    width = 12,
    height = 8)

plot_models(mod_sens1_all, 
            mod_sens1_amazon1, 
            mod_sens1_amazon2, 
            mod_sens2,
            axis.labels = tab_names,
            m.labels = c("All sample, not controlling for supply chain monitoring", 
                         "Legal Amazon, not controlling for supply chain monitoring", 
                         "Legal Amazon, controlling for supply chain monitoring",
                         "Largest meatpacker"),
            #show.values = T, 
            #show.p = T,
            legend.title = c(""),
            wrap.labels = 200,
            dot.size = 3,
            spacing = 0.7,
            colors = vir_5,
            vline.color = "grey"
) +
  theme_classic(base_size = 22) +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(ncol = 1)) +
  ylab("Estimate and 95% confidence interval")  

dev.off()

