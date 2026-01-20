library(tidyverse)





#library(RedGCSIM)
#remove.packages("RedGCSIM")
library(RedGCSIM)

#Tratamento da Base -----
#path="/Users/renatoteixeira/Downloads/SIM_DOBR2019"
#list.files(path)

#out.file <- RedGCSIM::Coletor_dados(caminho = path, padrao = "SIM 2019.dbf") 


load('/Users/renatoteixeira/Library/CloudStorage/OneDrive-Personal/SIM/1996 a 2021/dados_sim_2019.Rdata')

dados_sim_2019 <- dados_sim_2019 %>% 
  mutate(CODMUNRES=310620)


out.file <- dados_sim_2019

rm(dados_sim_2019)

out.file2 <- RedGCSIM::padroniza_idade(x = out.file)

out.file3 <- RedGCSIM::padroniza_local(out.file2)

out.file4 <- RedGCSIM::tabela_final_1(out.file3)

out.file5 <- RedGCSIM::separa_reg_ing(out.file4)

out.file5[["completos"]]

out.file5[["ignorados"]]


# identificar as colunas de faixas etárias
faixas <- c(
  "0 a 5", "5 a 10", "10 a 15", "15 a 20", "20 a 25",
  "25 a 30", "30 a 35", "35 a 40", "40 a 45",
  "45 a 50", "50 a 55", "55 a 60", "60 a 65",
  "65 a 70","70 a 75","75 a 80","80 a 85","85 a 90","90+",
  "Total","Early Neonatal","Late Neonatal","Post Neonatal"
)

# agregação para Brasil
df_brasil <- pop %>%
  group_by(Ano, Sexo) %>%
  summarise(
    across(all_of(faixas), sum, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    ARmaior   = "MINAS GERAIS",
    Armenor  = 3106200,
    NomeMunic = "Belo Horizonte - MG"
  )

# combinar com a base original
#pop_br <- rbind(pop, df_brasil)
pop_br <- df_brasil


#####
base_dados_completos <- out.file5[["completos"]]

causas = unique(ICD$CLASS_GPEAS_PRODUCAO)[!grepl(pattern = "^_", 
                                                 x = unique(ICD$CLASS_GPEAS_PRODUCAO))]
causas = c(causas, "_pneumo")
mumime <- RedGCSIM::mumime
mumime <- mumime %>% select(municode.6, microcode, mesocode)
colnames(mumime) <- c("cdmun", "micro", "meso")
mumime$cdmun <- as.character(mumime$cdmun)
cdmun <- unique(as.character(out.file5[["completos"]]$cdmun))
notwantedlevels <- c(110000, 120000, 130000, 140000, 150000, 
                     160000, 170000, 210000, 220000, 230000, 240000, 250000, 
                     260000, 270000, 280000, 290000, 310000, 320000, 330000, 
                     350000, 410000, 420000, 430000, 5e+05, 510000, 520000, 
                     0)

`%notin%` <- Negate(`%in%`)
cdmun <- cdmun[cdmun %notin% notwantedlevels]
pop <- RedGCSIM::pop
names(pop)
df_brasil <- df_brasil %>% 
  select(names(pop))
pop <- df_brasil
pop <- pop %>% filter(Ano %in% base_dados_completos$ano)
colnames(pop) <- c("ano", "uf", "sexo", "cdmun.7", "nome", 
                   as.character(seq(0, 90, 5)), "total", "Early Neonatal", 
                   "Late Neonatal", "Post Neonatal")
cols <- variable.names(pop[, 6:25])
pop <- pop %>% mutate_at(all_of(cols), ~str_replace(., ",", 
                                                    "."))
pop[cols] <- as.data.frame(lapply(pop[cols], function(y) as.numeric(y)))
pop.2 <- pop %>% select(-total, -nome, -uf) %>% pivot_longer(!(ano:cdmun.7), 
                                                             names_to = "idade", values_to = "pop") %>% mutate(cdmun = str_sub(cdmun.7, 
                                                                                                                               1, 6), uf = str_sub(cdmun.7, 1, 2)) %>% select(-cdmun.7)
rm(pop)
pop.2$sexo <- recode(pop.2$sexo, f = "Feminino", m = "Masculino")
pop.2$idade <- as.character(pop.2$idade)
pop.2$ano <- as.character(pop.2$ano)
pop.2$idade <- as.character(pop.2$idade)
causa <- unique(base_dados_completos$GBD)
sexo <- unique(base_dados_completos$sexo)
anos <- unique(base_dados_completos$ano)
age <- c(seq(0, 90, 5), "Early Neonatal", "Post Neonatal", 
         "Late Neonatal")
mat <- c(causas[grepl("^materna", causas)])
infant <- c(causas[grepl("^infant", causas)])
base.1 <- expand.grid(cdmun, anos, age, sexo, causa)
colnames(base.1) <- c("cdmun", "ano", "idade", "sexo", "GBD")
base.1 <- base.1 %>% mutate(to_exclude = case_when(GBD %in% 
                                                     mat & idade %in% c("Early Neonatal", "Post Neonatal", 
                                                                        "Late Neonatal", "<1 year", "0", "5", "60", "65", "70", 
                                                                        "75", "80", "85", "90") ~ 1, GBD %in% mat & sexo == 
                                                     "Masculino" ~ 1, TRUE ~ 0)) %>% filter(to_exclude == 
                                                                                              0) %>% select(-to_exclude)
base.1 <- left_join(base.1, mumime, by = "cdmun")
base.1$idade <- as.character(base.1$idade)
base_dados_completos$idade <- as.character(base_dados_completos$idade)
base.2 <- left_join(base.1, base_dados_completos[1:8], by = c(colnames(base_dados_completos)[1:7]))
print(c(colnames(base_dados_completos)[1:7]))
base.2 <- base.2 %>% mutate(uf = str_sub(cdmun, 1, 2))
base.2 <- left_join(base.2, pop.2, by = c("cdmun", "uf", 
                                          "ano", "sexo", "idade"))
base.2$obitos[is.na(base.2$obitos)] <- 0


out.file5[["completos"]] <- base.2
  

out.file6 <- RedGCSIM::prepara_base_generalizada(out.file5[["completos"]])

#Aqui rodo a funçao fora do pacote e ela funciona.
out.file7 <- RedGCSIM::prop_causas(base.2)


df_weigth_missing_out7 <- out.file7

save(df_weigth_missing_out7,
     file="/Users/renatoteixeira/Library/CloudStorage/OneDrive-Personal/Small Areas Estimation/Redistribuição GC/SAS to R/git_pacote/RedGCSIM_dev/df_peso/df_weigth_missing_out7.Rdata")


out.file8 <-RedGCSIM::redistribuicao_dados_faltantes(base_prop = out.file7, dados_ign = out.file5[["ignorados"]])



##### Redistribuição dos GC ----

out.file9 <- RedGCSIM::separa_reg_GC(out.file8)



########## redistribuicao_causas_externas 10 ----

causas = unique(ICD$CLASS_GPEAS_PRODUCAO)[!grepl(pattern = "^_", 
                                                 x = unique(ICD$CLASS_GPEAS_PRODUCAO))]
causas = c(causas, "_pneumo")
causas_inj <- c(causas[grepl("^Injuries", causas)])
causas_inj.hom.sui <- c("Injuries - Homicide", "Injuries - Suicide")
causas_inj.hsf <- c("Injuries - Falls", "Injuries - Homicide", 
                    "Injuries - Suicide", "Injuries - Road - Buses and Heavy Vehicles", 
                    "Injuries - Road - Cyclist", "Injuries - Road - Four-Wheel Cars and Light Vechicles", 
                    "Injuries - Road - Motocyclist", "Injuries - Road - Other", 
                    "Injuries - Road - Pedestrian")
causas_inj.hst <- c("Injuries - Suicide", "Injuries - Homicide", 
                    "Injuries - Other transport injuries", "Injuries - Road - Buses and Heavy Vehicles", 
                    "Injuries - Road - Cyclist", "Injuries - Road - Four-Wheel Cars and Light Vechicles", 
                    "Injuries - Road - Motocyclist", "Injuries - Road - Other", 
                    "Injuries - Road - Pedestrian")
causas_inj.road <- c("Injuries - Road - Buses and Heavy Vehicles", 
                     "Injuries - Road - Cyclist", "Injuries - Road - Four-Wheel Cars and Light Vechicles", 
                     "Injuries - Road - Motocyclist", "Injuries - Road - Other", 
                     "Injuries - Road - Pedestrian")
causas_inj.transport <- c("Injuries - Other transport injuries", 
                          "Injuries - Road - Buses and Heavy Vehicles", "Injuries - Road - Cyclist", 
                          "Injuries - Road - Four-Wheel Cars and Light Vechicles", 
                          "Injuries - Road - Motocyclist", "Injuries - Road - Other", 
                          "Injuries - Road - Pedestrian")
causas_inj.hso <- c("Injuries - Others", "Injuries - Suicide", 
                    "Injuries - Homicide")

dados_completos <- out.file9[["completos"]]

dados_redis <- out.file9[["redistribuir"]]

base_final <- dados_completos %>% select(-any_of(grep("^pr\\.", 
                                                      names(.), value = TRUE)), -any_of(grep("^ob\\.", names(.), 
                                                                                             value = TRUE)), -any_of(c("redis", "redis.2", "redis.3", 
                                                                                                                       "redis.4", "c.red"))) %>% mutate(c.red = ifelse(GBD %in% 
                                                                                                                                                                         causas_inj, "_injuries", NA)) %>% left_join(dados_redis, 
                                                                                                                                                                                                                     by = c("cdmun", "micro", "meso", "ano", "sexo", "idade", 
                                                                                                                                                                                                                        "uf", "c.red"))
source("/Users/renatoteixeira/Library/CloudStorage/OneDrive-Personal/Small Areas Estimation/Redistribuição GC/SAS to R/git_pacote/RedGCSIM_dev/R/calc_props.R")
base_final <- calc_props(base = base_final, causa = causas_inj, 
                         prefix = "inj", obito_in = "obitos.2", obito_out = "obitos.3", 
                         sexo_filtro = NULL, idades_filtro = NULL)


df_weigth_1_causas_inj <- base_final %>% 
  select(ano, sexo, idade, GBD, obitos.2, obitos.3,pr.mu) %>% 
  rename(pr.mu.ob3=pr.mu)

save(df_weigth_1_causas_inj,
     file="/Users/renatoteixeira/Library/CloudStorage/OneDrive-Personal/Small Areas Estimation/Redistribuição GC/SAS to R/git_pacote/RedGCSIM_dev/df_peso/df_weigth_1_causas_inj.Rdata")


base_final <- base_final %>% select(-any_of(grep("^pr\\.", 
                                                 names(.), value = TRUE)), -any_of(grep("^ob\\.", names(.), 
                                                                                        value = TRUE)), -any_of(c("redis", "redis.2", "redis.3", 
                                                                                                                  "redis.4", "c.red"))) %>% mutate(c.red = ifelse(GBD %in% 
                                                                                                                                                                    causas_inj.hom.sui, "_inj (hom,sui)", NA)) %>% mutate(c.red = ifelse(GBD %in% 
                                                                                                                                                                                                                                           "Injuries - Suicide" & idade %in% c("Early Neonatal", 
                                                                                                                                                                                                                                                                               "Post Neonatal", "Late Neonatal", "<1 year", "0"), NA, 
                                                                                                                                                                                                                                         c.red)) %>% left_join(dados_redis, by = c("cdmun", "micro", 
                                                                                                                                                                                                                                                                                   "meso", "ano", "sexo", "idade", "uf", "c.red"))
base_final <- calc_props(base = base_final, causa = causas_inj.hom.sui, 
                         prefix = "inj.h.s", obito_in = "obitos.3", obito_out = "obitos.4", 
                         sexo_filtro = NULL, idades_filtro = NULL)


df_weigth_2_causas_inj.hom.sui <- base_final %>% 
  select( ano, sexo, idade,  GBD, obitos.2, obitos.3,obitos.4,pr.mu) %>% 
  rename(pr.mu.ob4=pr.mu)

save(df_weigth_2_causas_inj.hom.sui,
     file="/Users/renatoteixeira/Library/CloudStorage/OneDrive-Personal/Small Areas Estimation/Redistribuição GC/SAS to R/git_pacote/RedGCSIM_dev/df_peso/df_weigth_2_causas_inj.hom.sui.Rdata")


base_final <- base_final %>% select(-any_of(grep("^pr\\.", 
                                                 names(.), value = TRUE)), -any_of(grep("^ob\\.", names(.), 
                                                                                        value = TRUE)), -any_of(c("redis", "redis.2", "redis.3", 
                                                                                                                  "redis.4", "c.red"))) %>% mutate(c.red = ifelse(GBD %in% 
                                                                                                                                                                    causas_inj.hsf, "_inj (hom,suic, fall,road)", NA)) %>% 
  mutate(c.red = ifelse(GBD %in% "Injuries - Suicide" & 
                          idade %in% c("Early Neonatal", "Post Neonatal", 
                                       "Late Neonatal", "<1 year", "0"), NA, c.red)) %>% 
  left_join(dados_redis, by = c("cdmun", "micro", "meso", 
                                "ano", "sexo", "idade", "uf", "c.red"))

base_final <- calc_props(base = base_final, causa = causas_inj.hsf, 
                         prefix = "inj.hsf", obito_in = "obitos.4", obito_out = "obitos.5", 
                         sexo_filtro = NULL, idades_filtro = NULL)


df_weigth_3_causas_inj.hsf <- base_final %>% 
  select(ano, sexo, idade,GBD, obitos.2, obitos.3,obitos.4,obitos.5,pr.mu) %>% 
  rename(pr.mu.ob5=pr.mu)

save(df_weigth_3_causas_inj.hsf,
     file="/Users/renatoteixeira/Library/CloudStorage/OneDrive-Personal/Small Areas Estimation/Redistribuição GC/SAS to R/git_pacote/RedGCSIM_dev/df_peso/df_weigth_3_causas_inj.hsf.Rdata")


base_final <- base_final %>% select(-any_of(grep("^pr\\.", 
                                                 names(.), value = TRUE)), -any_of(grep("^ob\\.", names(.), 
                                                                                        value = TRUE)), -any_of(c("redis", "redis.2", "redis.3", 
                                                                                                                  "redis.4", "c.red"))) %>% mutate(c.red = ifelse(GBD %in% 
                                                                                                                                                                    causas_inj.hst, "_inj (hom,sui,transp)", NA)) %>% mutate(c.red = ifelse(GBD %in% 
                                                                                                                                                                                                                                              "Injuries - Suicide" & idade %in% c("Early Neonatal", 
                                                                                                                                                                                                                                                                                  "Post Neonatal", "Late Neonatal", "<1 year", "0"), NA, 
                                                                                                                                                                                                                                            c.red)) %>% left_join(dados_redis, by = c("cdmun", "micro", 
                                                                                                                                                                                                                                                                                      "meso", "ano", "sexo", "idade", "uf", "c.red"))
base_final <- calc_props(base = base_final, causa = causas_inj.hst, 
                         prefix = "inj.hst", obito_in = "obitos.5", obito_out = "obitos.6.0", 
                         sexo_filtro = NULL, idades_filtro = NULL)

df_weigth_4_causas_inj.hst <- base_final %>% 
  select(ano, sexo, idade,  GBD, obitos.2, obitos.3,obitos.4,obitos.5,obitos.6.0,pr.mu) %>% 
  rename(pr.mu.ob6.0=pr.mu)

save(df_weigth_4_causas_inj.hst,
     file="/Users/renatoteixeira/Library/CloudStorage/OneDrive-Personal/Small Areas Estimation/Redistribuição GC/SAS to R/git_pacote/RedGCSIM_dev/df_peso/df_weigth_4_causas_inj.hst.Rdata")


base_final <- base_final %>% select(-any_of(grep("^pr\\.", 
                                                 names(.), value = TRUE)), -any_of(grep("^ob\\.", names(.), 
                                                                                        value = TRUE)), -any_of(c("redis", "redis.2", "redis.3", 
                                                                                                                  "redis.4", "c.red"))) %>% mutate(c.red = ifelse(GBD %in% 
                                                                                                                                                                    causas_inj.road, "_gc_inj_road", NA)) %>% left_join(dados_redis, 
                                                                                                                                                                                                                        by = c("cdmun", "micro", "meso", "ano", "sexo", "idade", 
                                                                                                                                                                                                                               "uf", "c.red"))
base_final <- calc_props(base = base_final, causa = causas_inj.road, 
                         prefix = "inj.road", obito_in = "obitos.6.0", obito_out = "obitos.6.1", 
                         sexo_filtro = NULL, idades_filtro = NULL)


df_weigth_5_causas_inj.road <- base_final %>% 
  select(ano, sexo, idade, GBD, obitos.2, obitos.3,obitos.4,obitos.5,obitos.6.0,obitos.6.1,pr.mu) %>% 
  rename(pr.mu.ob6.1=pr.mu)

save(df_weigth_5_causas_inj.road,
     file="/Users/renatoteixeira/Library/CloudStorage/OneDrive-Personal/Small Areas Estimation/Redistribuição GC/SAS to R/git_pacote/RedGCSIM_dev/df_peso/df_weigth_5_causas_inj.road.Rdata")

base_final <- base_final %>% select(-any_of(grep("^pr\\.", 
                                                 names(.), value = TRUE)), -any_of(grep("^ob\\.", names(.), 
                                                                                        value = TRUE)), -any_of(c("redis", "redis.2", "redis.3", 
                                                                                                                  "redis.4", "c.red"))) %>% mutate(c.red = ifelse(GBD %in% 
                                                                                                                                                                    causas_inj.transport, "_gc_inj_transport", NA)) %>% 
  left_join(dados_redis, by = c("cdmun", "micro", "meso", 
                                "ano", "sexo", "idade", "uf", "c.red"))
base_final <- calc_props(base = base_final, causa = causas_inj.transport, 
                         prefix = "inj.transport", obito_in = "obitos.6.1", obito_out = "obitos.6", 
                         sexo_filtro = NULL, idades_filtro = NULL)


df_weigth_6_causas_inj.transport <- base_final %>% 
  select(ano, sexo, idade, GBD, obitos.2, obitos.3,obitos.4,obitos.5,obitos.6.0,obitos.6.1,obitos.6,pr.mu) %>% 
  rename(pr.mu.ob6=pr.mu)

save(df_weigth_6_causas_inj.transport,
     file="/Users/renatoteixeira/Library/CloudStorage/OneDrive-Personal/Small Areas Estimation/Redistribuição GC/SAS to R/git_pacote/RedGCSIM_dev/df_peso/df_weigth_6_causas_inj.transport.Rdata")

base_final <- base_final %>% select(-any_of(grep("^pr\\.", 
                                                 names(.), value = TRUE)), -any_of(grep("^ob\\.", names(.), 
                                                                                        value = TRUE)), -any_of(c("redis", "redis.2", "redis.3", 
                                                                                                                  "redis.4", "c.red"))) %>% mutate(c.red = ifelse(GBD %in% 
                                                                                                                                                                    causas_inj.hso, "_inj (hom,suic,other)", NA)) %>% mutate(c.red = ifelse(GBD %in% 
                                                                                                                                                                                                                                              "Injuries - Suicide" & idade %in% c("Early Neonatal", 
                                                                                                                                                                                                                                                                                  "Post Neonatal", "Late Neonatal", "<1 year", "0"), NA, 
                                                                                                                                                                                                                                            c.red)) %>% left_join(dados_redis, by = c("cdmun", "micro", 
                                                                                                                                                                                                                                                                                      "meso", "ano", "sexo", "idade", "uf", "c.red"))
base_final <- calc_props(base = base_final, causa = causas_inj.hso, 
                         prefix = "inj.hso", obito_in = "obitos.6", obito_out = "obitos.7", 
                         sexo_filtro = NULL, idades_filtro = NULL)

df_weigth_7_causas_inj.hso <- base_final %>% 
  select(ano, sexo, idade, GBD, obitos.2, obitos.3,obitos.4,obitos.5,obitos.6.0,obitos.6.1,obitos.6,obitos.7,pr.mu) %>% 
  rename(pr.mu.ob7=pr.mu)

save(df_weigth_7_causas_inj.hso,
     file="/Users/renatoteixeira/Library/CloudStorage/OneDrive-Personal/Small Areas Estimation/Redistribuição GC/SAS to R/git_pacote/RedGCSIM_dev/df_peso/df_weigth_7_causas_inj.hso.Rdata")









out.file10 <- RedGCSIM::redistribuicao_causas_externas(dados_completos = out.file9[["completos"]],dados_redis = out.file9[["redistribuir"]])

########## redistribuicao_causas_mat_inf 11 ----

causas = unique(ICD$CLASS_GPEAS_PRODUCAO)[!grepl(pattern = "^_", 
                                                 x = unique(ICD$CLASS_GPEAS_PRODUCAO))]
causas = c(causas, "_pneumo")
causas_mat <- causas[grepl("^materna", causas)]
causas_inf <- causas[grepl("^infant|anom_congenitas|aspiracao_pulmunar|obst_intestinal|lri_post_neo", 
                           causas)]
idades_mat <- c("10", "15", "20", "25", "30", "35", "40", 
                "45", "50")
idades_inf <- c("Early Neonatal", "Post Neonatal", "Late Neonatal", 
                "<1 year")


dados_completos <- out.file10

base_final <- dados_completos %>% select(-starts_with("pr."), 
                                         -starts_with("ob."), -redis, -redis.2, -redis.3, -redis.4, 
                                         -c.red) %>% mutate(c.red = ifelse(GBD %in% causas_mat & 
                                                                             sexo == "Feminino" & idade %in% c("10", "15", "20", 
                                                                                                               "25", "30", "35", "40", "45", "50"), "_maternas", NA)) %>% 
  left_join(dados_redis, by = c("cdmun", "micro", "meso", 
                                "ano", "sexo", "idade", "uf", "c.red"))


base_final <- calc_props(base = base_final, causa = causas_mat, 
                         prefix = "mat", obito_in = "obitos.7", obito_out = "obitos.8", 
                         sexo_filtro = "Feminino", idades_filtro = idades_mat)


df_weigth_8_causas_mat <- base_final %>% 
  select(ano, sexo, idade, GBD, obitos.2, obitos.3,obitos.4,obitos.5,obitos.6.0,obitos.6.1,obitos.6,obitos.7,obitos.8,pr.mu) %>% 
  rename(pr.mu.ob8=pr.mu)

save(df_weigth_8_causas_mat,
     file="/Users/renatoteixeira/Library/CloudStorage/OneDrive-Personal/Small Areas Estimation/Redistribuição GC/SAS to R/git_pacote/RedGCSIM_dev/df_peso/df_weigth_8_causas_mat.Rdata")




base_final <- base_final %>% select(-starts_with("pr."), 
                                    -starts_with("ob."), -redis, -redis.2, -redis.3, -redis.4, 
                                    -c.red) %>% mutate(c.red = ifelse(GBD %in% causas_inf & 
                                                                        idade %in% c("Early Neonatal", "Post Neonatal", "Late Neonatal", 
                                                                                     "<1 year"), "_infant_neonat", NA)) %>% left_join(dados_redis, 
                                                                                                                                      by = c("cdmun", "micro", "meso", "ano", "sexo", "idade", 
                                                                                                                                             "uf", "c.red"))
base_final <- calc_props(base = base_final, causa = causas_inf, 
                         prefix = "infant", obito_in = "obitos.8", obito_out = "obitos.9", 
                         sexo_filtro = NULL, idades_filtro = idades_inf)




df_weigth_9_causas_inf <- base_final %>% 
  select(ano, sexo, idade, GBD, obitos.2, obitos.3,obitos.4,obitos.5,obitos.6.0,obitos.6.1,obitos.6,obitos.7,obitos.8,obitos.9,pr.mu) %>% 
  rename(pr.mu.ob9=pr.mu)

save(df_weigth_9_causas_inf,
     file="/Users/renatoteixeira/Library/CloudStorage/OneDrive-Personal/Small Areas Estimation/Redistribuição GC/SAS to R/git_pacote/RedGCSIM_dev/df_peso/df_weigth_9_causas_inf.Rdata")



out.file11 <- RedGCSIM::redistribuicao_causas_mat_inf(dados_completos = out.file10,dados_redis = out.file9[["redistribuir"]])




########## redistribuicao_causas_ivestigacao 12 ----


######### X59----

ICD_pesos <- RedGCSIM::ICD_pesos
causas = unique(ICD$CLASS_GPEAS_PRODUCAO)[!grepl(pattern = "^_", 
                                                 x = unique(ICD$CLASS_GPEAS_PRODUCAO))]
causas = c(causas, "_pneumo")
trans <- c(causas[grepl("^trans", causas)])
inj <- c("Injuries - Falls", "Injuries - Homicide", "Injuries - Other transport injuries", 
         "Injuries - Others", "Injuries - Suicide")
road <- c(causas[grepl("^Injuries - Road", causas)])
mat <- c(causas[grepl("^materna", causas)])
cod_others <- c(causas[grepl("^other", causas)])
ICD_x59 <- ICD_pesos %>% filter(CG == "_x59") %>% select(target, 
                                                         weight)


dados_completos <- out.file11

base.5 <- dados_completos %>% select(!(pr.mu:ob.rg), -redis, 
                                     -redis.2, -redis.3, -redis.4, -c.red) %>% mutate(c.red = ifelse(GBD %in% 
                                                                                                       ICD_x59$target, "_x59", NA)) %>% left_join(dados_redis, 
                                                                                                                                                  by = c("cdmun", "micro", "meso", "ano", "sexo", "idade", 
                                                                                                                                                         "uf", "c.red"))
muni.x59 <- base.5 %>% filter(GBD %in% c(trans, mat, "other_causes_all", 
                                         "other_causes-lri", "other_desnutricao_all_ages")) %>% 
  group_by(cdmun, micro, meso, GBD, idade, ano, sexo, 
           uf) %>% summarise(ob = sum(obitos.9, na.rm = T)) %>% 
  ungroup() %>% group_by(cdmun, micro, meso, idade, ano, 
                         sexo, uf) %>% mutate(pr.mu = ob/sum(ob, na.rm = T), 
                                              ob.mu = sum(ob)) %>% select(-ob)




muni.x59.2 <- base.5 %>% filter(GBD %in% road) %>% group_by(cdmun, 
                                                            micro, meso, GBD, idade, ano, sexo, uf) %>% summarise(ob = sum(obitos.9, 
                                                                                                                           na.rm = T)) %>% ungroup() %>% group_by(cdmun, micro, 
                                                                                                                                                                  meso, idade, ano, sexo, uf) %>% mutate(pr.mu = ob/sum(ob, 
                                                                                                                                                                                                                        na.rm = T), ob.mu = sum(ob)) %>% select(-ob)
muni.x59 <- rbind(muni.x59, muni.x59.2)

micro.x59 <- base.5 %>% filter(GBD %in% c(trans, mat, "other_causes_all", 
                                          "other_causes-lri", "other_desnutricao_all_ages")) %>% 
  group_by(micro, meso, GBD, idade, ano, sexo, uf) %>% 
  summarise(ob = sum(obitos.9, na.rm = T)) %>% ungroup() %>% 
  group_by(micro, meso, idade, ano, sexo, uf) %>% mutate(pr.mi = ob/sum(ob, 
                                                                        na.rm = T), ob.mi = sum(ob)) %>% select(-ob)
micro.x59.2 <- base.5 %>% filter(GBD %in% road) %>% group_by(micro, 
                                                             meso, GBD, idade, ano, sexo, uf) %>% summarise(ob = sum(obitos.9, 
                                                                                                                     na.rm = T)) %>% ungroup() %>% group_by(micro, meso, 
                                                                                                                                                            idade, ano, sexo, uf) %>% mutate(pr.mi = ob/sum(ob, 
                                                                                                                                                                                                            na.rm = T), ob.mi = sum(ob)) %>% select(-ob)
micro.x59 <- rbind(micro.x59, micro.x59.2)
rm(micro.x59.2)
meso.x59 <- base.5 %>% filter(GBD %in% c(trans, mat, "other_causes_all", 
                                         "other_causes-lri", "other_desnutricao_all_ages")) %>% 
  group_by(meso, GBD, idade, ano, sexo, uf) %>% summarise(ob = sum(obitos.9, 
                                                                   na.rm = T)) %>% ungroup() %>% group_by(meso, idade, 
                                                                                                          ano, sexo, uf) %>% mutate(pr.me = ob/sum(ob), ob.me = sum(ob)) %>% 
  select(-ob)
meso.x59.2 <- base.5 %>% filter(GBD %in% road) %>% group_by(meso, 
                                                            GBD, idade, ano, sexo, uf) %>% summarise(ob = sum(obitos.9, 
                                                                                                              na.rm = T)) %>% ungroup() %>% group_by(meso, idade, 
                                                                                                                                                     ano, sexo, uf) %>% mutate(pr.me = ob/sum(ob), ob.me = sum(ob)) %>% 
  select(-ob)
meso.x59 <- rbind(meso.x59, meso.x59.2)
rm(meso.x59.2)
uf.x59 <- base.5 %>% filter(GBD %in% c(trans, mat, "other_causes_all", 
                                       "other_causes-lri", "other_desnutricao_all_ages")) %>% 
  group_by(GBD, idade, ano, sexo, uf) %>% summarise(ob = sum(obitos.9, 
                                                             na.rm = T)) %>% ungroup() %>% group_by(idade, ano, sexo, 
                                                                                                    uf) %>% mutate(pr.uf = ob/sum(ob), ob.uf = sum(ob)) %>% 
  select(-ob)
uf.x59.2 <- base.5 %>% filter(GBD %in% road) %>% group_by(GBD, 
                                                          idade, ano, sexo, uf) %>% summarise(ob = sum(obitos.9, 
                                                                                                       na.rm = T)) %>% ungroup() %>% group_by(idade, ano, sexo, 
                                                                                                                                              uf) %>% mutate(pr.uf = ob/sum(ob), ob.uf = sum(ob)) %>% 
  select(-ob)
uf.x59 <- rbind(uf.x59, uf.x59.2)
rm(uf.x59.2)
rg.x59 <- base.5 %>% filter(GBD %in% c(trans, mat, "other_causes_all", 
                                       "other_causes-lri", "other_desnutricao_all_ages")) %>% 
  group_by(GBD, idade, ano, sexo, reg) %>% summarise(ob = sum(obitos.9, 
                                                              na.rm = T)) %>% ungroup() %>% group_by(idade, ano, sexo, 
                                                                                                     reg) %>% mutate(pr.rg = ob/sum(ob, na.rm = T), ob.rg = sum(ob)) %>% 
  select(-ob)
rg.x59.2 <- base.5 %>% filter(GBD %in% road) %>% group_by(GBD, 
                                                          idade, ano, sexo, reg) %>% summarise(ob = sum(obitos.9, 
                                                                                                        na.rm = T)) %>% ungroup() %>% group_by(idade, ano, sexo, 
                                                                                                                                               reg) %>% mutate(pr.rg = ob/sum(ob, na.rm = T), ob.rg = sum(ob)) %>% 
  select(-ob)
rg.x59 <- rbind(rg.x59, rg.x59.2)
rm(rg.x59.2)

base.5 <- base.5 %>% left_join(muni.x59, by = c("cdmun", 
                                                "micro", "meso", "GBD", "idade", "ano", "sexo", "uf")) %>% 
  left_join(micro.x59, by = c("micro", "meso", "GBD", 
                              "idade", "ano", "sexo", "uf")) %>% left_join(meso.x59, 
                                                                           by = c("meso", "GBD", "idade", "ano", "sexo", "uf")) %>% 
  left_join(uf.x59, by = c("GBD", "idade", "ano", "sexo", 
                           "uf")) %>% left_join(rg.x59, by = c("GBD", "idade", 
                                                               "ano", "sexo", "reg"))
base.5 <- base.5 %>% left_join(ICD_x59, by = c(GBD = "target")) %>% 
  mutate(redis = ifelse(!is.na(weight), redis * weight, 
                        redis)) %>% mutate(x59.1 = ifelse(GBD %in% inj, 
                                                          redis, redis * pr.mu), redis.2 = ifelse(is.na(x59.1) | 
                                                                                                    x59.1 == 0 & ob.mu == 0, redis, NA), x59.2 = ifelse(GBD %in% 
                                                                                                                                                          inj, redis.2, redis.2 * pr.mi), redis.3 = ifelse(is.na(x59.2) & 
                                                                                                                                                                                                             ob.mi == 0, redis.2, NA), x59.3 = ifelse(GBD %in% inj, 
                                                                                                                                                                                                                                                      redis.3, redis.3 * pr.me), redis.4 = ifelse(is.na(x59.3) & 
                                                                                                                                                                                                                                                                                                    ob.me == 0, redis.3, NA), x59.4 = ifelse(GBD %in% inj, 
                                                                                                                                                                                                                                                                                                                                             redis.4, redis.4 * pr.uf), redis.5 = ifelse(is.na(x59.4) & 
                                                                                                                                                                                                                                                                                                                                                                                           ob.uf == 0, redis.4, NA), x59.5 = ifelse(GBD %in% inj, 
                                                                                                                                                                                                                                                                                                                                                                                                                                    redis.5, redis.5 * pr.rg), obitos.10 = ifelse(!is.na(x59.1), 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  obitos.9 + x59.1, ifelse(!is.na(x59.2), obitos.9 + x59.2, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           ifelse(!is.na(x59.3), obitos.9 + x59.3, ifelse(!is.na(x59.4), 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          obitos.9 + x59.4, ifelse(!is.na(x59.5), obitos.9 + 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     x59.5, obitos.9))))))



df_weigth_10_1_x59 <- base.5 %>% 
  select(ano, sexo, idade, GBD, obitos.2, obitos.3,obitos.4,obitos.5,obitos.6.0,obitos.6.1,obitos.6,obitos.7,obitos.8,obitos.9,obitos.10,pr.mu) %>% 
  rename(pr.mu.ob10=pr.mu)

save(df_weigth_10_1_x59,
     file="/Users/renatoteixeira/Library/CloudStorage/OneDrive-Personal/Small Areas Estimation/Redistribuição GC/SAS to R/git_pacote/RedGCSIM_dev/df_peso/df_weigth_10_1_x59.Rdata")


######### Y34 ----
rm(trans, inj, mat, ICD_x59)
gc()
trans <- c(causas[grepl("^trans", causas)])
dcnt <- c(causas[grepl("^dcnt", causas)])
inj <- c("Injuries - Falls", "Injuries - Homicide", "Injuries - Other transport injuries", 
         "Injuries - Others", "Injuries - Suicide")
mat <- c(causas[grepl("^materna", causas)])
road <- c(causas[grepl("^Injuries - Road", causas)])
ICD_y34 <- ICD_pesos %>% filter(CG == "_y34") %>% select(target, 
                                                         weight)
y34 <- data.frame(target = c(dcnt, inj, trans, mat, road, 
                             "other_causes_all", "other_causes-lri", "other_desnutricao_all_ages"))
y34 <- y34 %>% left_join(ICD_y34, by = "target")
base.5 <- base.5 %>% select(!(pr.mu:ob.rg), -redis, -redis.2, 
                            -redis.3, -redis.4, -c.red, -weight) %>% mutate(c.red = ifelse(GBD %in% 
                                                                                             y34$target, "_y34", NA)) %>% left_join(dados_redis, 
                                                                                                                                    by = c("cdmun", "micro", "meso", "ano", "sexo", "idade", 
                                                                                                                                           "uf", "c.red"))
muni.y34 <- base.5 %>% filter(GBD %in% c(trans, mat, "other_causes_all", 
                                         "other_causes-lri", "other_desnutricao_all_ages")) %>% 
  group_by(cdmun, micro, meso, GBD, idade, ano, sexo, 
           uf) %>% summarise(ob = sum(obitos.10, na.rm = T)) %>% 
  ungroup() %>% group_by(cdmun, micro, meso, idade, ano, 
                         sexo, uf) %>% mutate(pr.mu = ob/sum(ob, na.rm = T), 
                                              ob.mu = sum(ob, na.rm = T)) %>% select(-ob)
muni.y34.2 <- base.5 %>% filter(GBD %in% road) %>% group_by(cdmun, 
                                                            micro, meso, GBD, idade, ano, sexo, uf) %>% summarise(ob = sum(obitos.10, 
                                                                                                                           na.rm = T)) %>% ungroup() %>% group_by(cdmun, micro, 
                                                                                                                                                                  meso, idade, ano, sexo, uf) %>% mutate(pr.mu = ob/sum(ob, 
                                                                                                                                                                                                                        na.rm = T), ob.mu = sum(ob)) %>% select(-ob)
muni.y34 <- rbind(muni.y34, muni.y34.2)
rm(muni.y34.2)
micro.y34 <- base.5 %>% filter(GBD %in% c(trans, mat, "other_causes_all", 
                                          "other_causes-lri", "other_desnutricao_all_ages")) %>% 
  group_by(micro, meso, GBD, idade, ano, sexo, uf) %>% 
  summarise(ob = sum(obitos.10, na.rm = T)) %>% ungroup() %>% 
  group_by(micro, meso, idade, ano, sexo, uf) %>% mutate(pr.mi = ob/sum(ob, 
                                                                        na.rm = T), ob.mi = sum(ob, na.rm = T)) %>% select(-ob)
micro.y34.2 <- base.5 %>% filter(GBD %in% road) %>% group_by(micro, 
                                                             meso, GBD, idade, ano, sexo, uf) %>% summarise(ob = sum(obitos.10, 
                                                                                                                     na.rm = T)) %>% ungroup() %>% group_by(micro, meso, 
                                                                                                                                                            idade, ano, sexo, uf) %>% mutate(pr.mi = ob/sum(ob, 
                                                                                                                                                                                                            na.rm = T), ob.mi = sum(ob, na.rm = T)) %>% select(-ob)
micro.y34 <- rbind(micro.y34, micro.y34.2)
rm(micro.y34.2)
meso.y34 <- base.5 %>% filter(GBD %in% c(trans, mat, "other_causes_all", 
                                         "other_causes-lri", "other_desnutricao_all_ages")) %>% 
  group_by(meso, GBD, idade, ano, sexo, uf) %>% summarise(ob = sum(obitos.10, 
                                                                   na.rm = T)) %>% ungroup() %>% group_by(meso, idade, 
                                                                                                          ano, sexo, uf) %>% mutate(pr.me = ob/sum(ob), ob.me = sum(ob)) %>% 
  select(-ob)
meso.y34.2 <- base.5 %>% filter(GBD %in% road) %>% group_by(meso, 
                                                            GBD, idade, ano, sexo, uf) %>% summarise(ob = sum(obitos.10, 
                                                                                                              na.rm = T)) %>% ungroup() %>% group_by(meso, idade, 
                                                                                                                                                     ano, sexo, uf) %>% mutate(pr.me = ob/sum(ob), ob.me = sum(ob)) %>% 
  select(-ob)
meso.y34 <- rbind(meso.y34, meso.y34.2)
rm(meso.y34.2)
uf.y34 <- base.5 %>% filter(GBD %in% c(trans, mat, "other_causes_all", 
                                       "other_causes-lri", "other_desnutricao_all_ages")) %>% 
  group_by(GBD, idade, ano, sexo, uf) %>% summarise(ob = sum(obitos.10, 
                                                             na.rm = T)) %>% ungroup() %>% group_by(idade, ano, sexo, 
                                                                                                    uf) %>% mutate(pr.uf = ob/sum(ob), ob.uf = sum(ob)) %>% 
  select(-ob)
uf.y34.2 <- base.5 %>% filter(GBD %in% road) %>% group_by(GBD, 
                                                          idade, ano, sexo, uf) %>% summarise(ob = sum(obitos.10, 
                                                                                                       na.rm = T)) %>% ungroup() %>% group_by(idade, ano, sexo, 
                                                                                                                                              uf) %>% mutate(pr.uf = ob/sum(ob), ob.uf = sum(ob)) %>% 
  select(-ob)
uf.y34 <- rbind(uf.y34, uf.y34.2)
rm(uf.y34.2)
rg.y34 <- base.5 %>% filter(GBD %in% c(trans, mat, "other_causes_all", 
                                       "other_causes-lri", "other_desnutricao_all_ages")) %>% 
  group_by(GBD, idade, ano, sexo, reg) %>% summarise(ob = sum(obitos.10, 
                                                              na.rm = T)) %>% ungroup() %>% group_by(idade, ano, sexo, 
                                                                                                     reg) %>% mutate(pr.rg = ob/sum(ob, na.rm = T), ob.rg = sum(ob, 
                                                                                                                                                                na.rm = T)) %>% select(-ob)
rg.y34.2 <- base.5 %>% filter(GBD %in% road) %>% group_by(GBD, 
                                                          idade, ano, sexo, reg) %>% summarise(ob = sum(obitos.10, 
                                                                                                        na.rm = T)) %>% ungroup() %>% group_by(idade, ano, sexo, 
                                                                                                                                               reg) %>% mutate(pr.rg = ob/sum(ob, na.rm = T), ob.rg = sum(ob, 
                                                                                                                                                                                                          na.rm = T)) %>% select(-ob)
rg.y34 <- rbind(rg.y34, rg.y34.2)
rm(rg.y34.2)
base.5 <- base.5 %>% left_join(muni.y34, by = c("cdmun", 
                                                "micro", "meso", "GBD", "idade", "ano", "sexo", "uf")) %>% 
  left_join(micro.y34, by = c("micro", "meso", "GBD", 
                              "idade", "ano", "sexo", "uf")) %>% left_join(meso.y34, 
                                                                           by = c("meso", "GBD", "idade", "ano", "sexo", "uf")) %>% 
  left_join(uf.y34, by = c("GBD", "idade", "ano", "sexo", 
                           "uf")) %>% left_join(rg.y34, by = c("GBD", "idade", 
                                                               "ano", "sexo", "reg"))
base.5 <- base.5 %>% left_join(ICD_y34, by = c(GBD = "target")) %>% 
  mutate(redis = ifelse(!is.na(weight), redis * weight, 
                        redis)) %>% mutate(y34.1 = ifelse(GBD %in% c(inj, 
                                                                     dcnt), redis, redis * pr.mu), redis.2 = ifelse(is.na(y34.1) | 
                                                                                                                      y34.1 == 0 & ob.mu == 0, redis, NA), y34.2 = ifelse(GBD %in% 
                                                                                                                                                                            c(inj, dcnt), redis.2, redis.2 * pr.mi), redis.3 = ifelse(is.na(y34.2) & 
                                                                                                                                                                                                                                        ob.mi == 0, redis.2, NA), y34.3 = ifelse(GBD %in% c(inj, 
                                                                                                                                                                                                                                                                                            dcnt), redis.3, redis.3 * pr.me), redis.4 = ifelse(is.na(y34.3) & 
                                                                                                                                                                                                                                                                                                                                                 ob.me == 0, redis.3, NA), y34.4 = ifelse(GBD %in% c(inj, 
                                                                                                                                                                                                                                                                                                                                                                                                     dcnt), redis.4, redis.4 * pr.uf), redis.5 = ifelse(is.na(y34.4) & 
                                                                                                                                                                                                                                                                                                                                                                                                                                                          ob.uf == 0, redis.4, NA), y34.5 = ifelse(GBD %in% c(inj, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              dcnt), redis.5, redis.5 * pr.rg), obitos.11 = ifelse(!is.na(y34.1), 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   obitos.10 + y34.1, ifelse(!is.na(y34.2), obitos.10 + 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               y34.2, ifelse(!is.na(y34.3), obitos.10 + y34.3, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             ifelse(!is.na(y34.4), obitos.10 + y34.4, ifelse(!is.na(y34.5), 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             obitos.10 + y34.5, obitos.10))))))


df_weigth_10_2_y34 <- base.5 %>% 
  select(ano, sexo, idade, GBD, obitos.2, obitos.3,obitos.4,obitos.5,obitos.6.0,obitos.6.1,obitos.6,obitos.7,obitos.8,obitos.9,obitos.10,obitos.11,pr.mu) %>% 
  rename(pr.mu.ob11=pr.mu)

save(df_weigth_10_2_y34,
     file="/Users/renatoteixeira/Library/CloudStorage/OneDrive-Personal/Small Areas Estimation/Redistribuição GC/SAS to R/git_pacote/RedGCSIM_dev/df_peso/df_weigth_10_2_y34.Rdata")





######### Pneumonia ----
rm(trans, inj, mat, dcnt, ICD_y34)
rm(list = ls()[grepl("^meso|^micro|^muni|^rg|^uf", ls())])
gc()
trans <- c(causas[grepl("^trans", causas)])
dcnt <- c(causas[grepl("^dcnt", causas)])
inj <- c("Injuries - Falls", "Injuries - Homicide", "Injuries - Other transport injuries", 
         "Injuries - Others", "Injuries - Suicide")
mat <- c(causas[grepl("^materna", causas)])
road <- c(causas[grepl("^Injuries - Road", causas)])
ICD_pneumo <- ICD_pesos %>% filter(CG == "_pneumo") %>% 
  select(target, age, weight)
pneumo <- data.frame(target = c("_pneumo", dcnt, inj, trans, 
                                mat, road, "other_causes_all", "other_causes-lri", "other_desnutricao_all_ages"))
pneumo <- pneumo %>% left_join(ICD_pneumo, by = "target") %>% 
  mutate(c.red = case_when(age == "<10" ~ "_pneumo_inf", 
                           age == "10 a 59" ~ "_pneumo_adult", age == "60 emais" ~ 
                             "_pneumo_idoso", )) %>% select(-age)
base.r.pneumo <- dados_redis %>% filter(c.red == "_pneumo") %>% 
  mutate(c.red = case_when(c.red == "_pneumo" & idade %in% 
                             c("Early Neonatal", "Post Neonatal", "Late Neonatal", 
                               "<1 year", "0", "5") ~ "_pneumo_inf", c.red == 
                             "_pneumo" & idade %in% c("10", "15", "20", "25", 
                                                      "30", "35", "40", "45", "50", "55") ~ "_pneumo_adult", 
                           c.red == "_pneumo" & idade %in% c("60", "65", "70", 
                                                             "75", "80", "85", "90") ~ "_pneumo_idoso"))
base.5 <- base.5 %>% select(!(pr.mu:ob.rg), -redis, -redis.2, 
                            -redis.3, -redis.4, -c.red, -weight) %>% mutate(c.red = case_when(GBD %in% 
                                                                                                pneumo$target & idade %in% c("Early Neonatal", "Post Neonatal", 
                                                                                                                             "Late Neonatal", "<1 year", "0", "5") ~ "_pneumo_inf", 
                                                                                              GBD %in% pneumo$target & idade %in% c("10", "15", "20", 
                                                                                                                                    "25", "30", "35", "40", "45", "50", "55") ~ "_pneumo_adult", 
                                                                                              GBD %in% pneumo$target & idade %in% c("60", "65", "70", 
                                                                                                                                    "75", "80", "85", "90") ~ "_pneumo_idoso")) %>% 
  left_join(base.r.pneumo, by = c("cdmun", "micro", "meso", 
                                  "ano", "sexo", "idade", "uf", "c.red"))
muni.pne <- base.5 %>% filter(GBD %in% c(trans, mat, "other_causes_all", 
                                         "other_causes-lri", "other_desnutricao_all_ages")) %>% 
  group_by(cdmun, micro, meso, c.red, idade, GBD, ano, 
           sexo, uf) %>% summarise(ob = sum(obitos.11, na.rm = T)) %>% 
  ungroup() %>% group_by(cdmun, micro, meso, c.red, idade, 
                         ano, sexo, uf) %>% mutate(pr.mu = ob/sum(ob, na.rm = T), 
                                                   ob.mu = sum(ob, na.rm = T)) %>% select(-ob)
muni.pne.2 <- base.5 %>% filter(GBD %in% road) %>% group_by(cdmun, 
                                                            micro, meso, c.red, idade, GBD, ano, sexo, uf) %>% summarise(ob = sum(obitos.11, 
                                                                                                                                  na.rm = T)) %>% ungroup() %>% group_by(cdmun, micro, 
                                                                                                                                                                         meso, c.red, idade, ano, sexo, uf) %>% mutate(pr.mu = ob/sum(ob, 
                                                                                                                                                                                                                                      na.rm = T), ob.mu = sum(ob, na.rm = T)) %>% select(-ob)
muni.pne <- rbind(muni.pne, muni.pne.2)
rm(muni.pne.2)
micro.pne <- base.5 %>% filter(GBD %in% c(trans, mat, "other_causes_all", 
                                          "other_causes-lri", "other_desnutricao_all_ages")) %>% 
  group_by(micro, meso, c.red, idade, GBD, ano, sexo, 
           uf) %>% summarise(ob = sum(obitos.11, na.rm = T)) %>% 
  ungroup() %>% group_by(micro, meso, c.red, idade, ano, 
                         sexo, uf) %>% mutate(pr.mi = ob/sum(ob, na.rm = T), 
                                              ob.mi = sum(ob, na.rm = T)) %>% select(-ob)
micro.pne.2 <- base.5 %>% filter(GBD %in% road) %>% group_by(micro, 
                                                             meso, c.red, idade, GBD, ano, sexo, uf) %>% summarise(ob = sum(obitos.11, 
                                                                                                                            na.rm = T)) %>% ungroup() %>% group_by(micro, meso, 
                                                                                                                                                                   c.red, idade, ano, sexo, uf) %>% mutate(pr.mi = ob/sum(ob, 
                                                                                                                                                                                                                          na.rm = T), ob.mi = sum(ob, na.rm = T)) %>% select(-ob)
micro.pne <- rbind(micro.pne, micro.pne.2)
rm(micro.pne.2)
meso.pne <- base.5 %>% filter(GBD %in% c(trans, mat, "other_causes_all", 
                                         "other_causes-lri", "other_desnutricao_all_ages")) %>% 
  group_by(meso, c.red, idade, GBD, ano, sexo, uf) %>% 
  summarise(ob = sum(obitos.11, na.rm = T)) %>% ungroup() %>% 
  group_by(meso, c.red, idade, ano, sexo, uf) %>% mutate(pr.me = ob/sum(ob, 
                                                                        na.rm = T), ob.me = sum(ob, na.rm = T)) %>% select(-ob)
meso.pne.2 <- base.5 %>% filter(GBD %in% road) %>% group_by(meso, 
                                                            c.red, idade, GBD, ano, sexo, uf) %>% summarise(ob = sum(obitos.11, 
                                                                                                                     na.rm = T)) %>% ungroup() %>% group_by(meso, c.red, 
                                                                                                                                                            idade, ano, sexo, uf) %>% mutate(pr.me = ob/sum(ob, 
                                                                                                                                                                                                            na.rm = T), ob.me = sum(ob, na.rm = T)) %>% select(-ob)
meso.pne <- rbind(meso.pne, meso.pne.2)
rm(meso.pne.2)
uf.pne <- base.5 %>% filter(GBD %in% c(trans, mat, "other_causes_all", 
                                       "other_causes-lri", "other_desnutricao_all_ages")) %>% 
  group_by(c.red, idade, GBD, ano, sexo, uf) %>% summarise(ob = sum(obitos.11, 
                                                                    na.rm = T)) %>% ungroup() %>% group_by(c.red, idade, 
                                                                                                           ano, sexo, uf) %>% mutate(pr.uf = ob/sum(ob, na.rm = T), 
                                                                                                                                     ob.uf = sum(ob, na.rm = T)) %>% select(-ob)
uf.pne.2 <- base.5 %>% filter(GBD %in% road) %>% group_by(c.red, 
                                                          idade, GBD, ano, sexo, uf) %>% summarise(ob = sum(obitos.11, 
                                                                                                            na.rm = T)) %>% ungroup() %>% group_by(c.red, idade, 
                                                                                                                                                   ano, sexo, uf) %>% mutate(pr.uf = ob/sum(ob, na.rm = T), 
                                                                                                                                                                             ob.uf = sum(ob, na.rm = T)) %>% select(-ob)
uf.pne <- rbind(uf.pne, uf.pne.2)
rm(uf.pne.2)
rg.pne <- base.5 %>% filter(GBD %in% c(trans, mat, "other_causes_all", 
                                       "other_causes-lri", "other_desnutricao_all_ages")) %>% 
  group_by(c.red, idade, GBD, ano, sexo, reg) %>% summarise(ob = sum(obitos.11, 
                                                                     na.rm = T)) %>% ungroup() %>% group_by(c.red, idade, 
                                                                                                            ano, sexo, reg) %>% mutate(pr.rg = ob/sum(ob, na.rm = T), 
                                                                                                                                       ob.rg = sum(ob, na.rm = T)) %>% select(-ob)
rg.pne.2 <- base.5 %>% filter(GBD %in% road) %>% group_by(c.red, 
                                                          idade, GBD, ano, sexo, reg) %>% summarise(ob = sum(obitos.11, 
                                                                                                             na.rm = T)) %>% ungroup() %>% group_by(c.red, idade, 
                                                                                                                                                    ano, sexo, reg) %>% mutate(pr.rg = ob/sum(ob, na.rm = T), 
                                                                                                                                                                               ob.rg = sum(ob, na.rm = T)) %>% select(-ob)
rg.pne <- rbind(rg.pne, rg.pne.2)
rm(rg.pne.2)
head(base.5)
base.5 <- base.5 %>% left_join(muni.pne, by = c("cdmun", 
                                                "micro", "meso", "c.red", "idade", "GBD", "ano", "sexo", 
                                                "uf")) %>% left_join(micro.pne, by = c("micro", "meso", 
                                                                                       "c.red", "idade", "GBD", "ano", "sexo", "uf")) %>% left_join(meso.pne, 
                                                                                                                                                    by = c("meso", "c.red", "idade", "GBD", "ano", "sexo", 
                                                                                                                                                           "uf")) %>% left_join(uf.pne, by = c("c.red", "idade", 
                                                                                                                                                                                               "GBD", "ano", "sexo", "uf")) %>% left_join(rg.pne, by = c("c.red", 
                                                                                                                                                                                                                                                         "idade", "GBD", "ano", "sexo", "reg"))
base.5 <- base.5 %>% left_join(pneumo, by = c(GBD = "target", 
                                              c.red = "c.red")) %>% mutate(redis = ifelse(!is.na(weight), 
                                                                                          redis * weight, redis)) %>% mutate(pne.1 = ifelse(GBD %in% 
                                                                                                                                              c("_pneumo", inj, dcnt), redis, redis * pr.mu), redis.2 = ifelse(is.na(pne.1) & 
                                                                                                                                                                                                                 ob.mu == 0, redis, NA), pne.2 = ifelse(GBD %in% c("_pneumo", 
                                                                                                                                                                                                                                                                   inj, dcnt), redis.2, redis.2 * pr.mi), redis.3 = ifelse(is.na(pne.2) & 
                                                                                                                                                                                                                                                                                                                             ob.mi == 0, redis.2, NA), pne.3 = ifelse(GBD %in% c("_pneumo", 
                                                                                                                                                                                                                                                                                                                                                                                 inj, dcnt), redis.3, redis.3 * pr.me), redis.4 = ifelse(is.na(pne.3) & 
                                                                                                                                                                                                                                                                                                                                                                                                                                           ob.me == 0, redis.3, NA), pne.4 = ifelse(GBD %in% c("_pneumo", 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               inj, dcnt), redis.4, redis.4 * pr.uf), redis.5 = ifelse(is.na(pne.4) & 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         ob.uf == 0, redis.4, NA), pne.5 = ifelse(GBD %in% c("_pneumo", 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             inj, dcnt), redis.5, redis.5 * pr.rg), obitos.12 = ifelse(!is.na(pne.1), 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       obitos.11 + pne.1, ifelse(!is.na(pne.2), obitos.11 + 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   pne.2, ifelse(!is.na(pne.3), obitos.11 + pne.3, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 ifelse(!is.na(pne.4), obitos.11 + pne.4, ifelse(!is.na(pne.5), 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 obitos.11 + pne.5, obitos.11)))))) %>% mutate(obitos.12 = ifelse(GBD %in% 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    "_pneumo", obitos.12 - obitos.11, obitos.12))



df_weigth_10_3_pneumo1 <- base.5 %>% 
  select(ano, sexo, idade, GBD, obitos.2, obitos.3,obitos.4,obitos.5,obitos.6.0,obitos.6.1,obitos.6,obitos.7,obitos.8,obitos.9,obitos.10,obitos.12,pr.mu) %>% 
  rename(pr.mu.ob12=pr.mu)

save(df_weigth_10_3_pneumo1,
     file="/Users/renatoteixeira/Library/CloudStorage/OneDrive-Personal/Small Areas Estimation/Redistribuição GC/SAS to R/git_pacote/RedGCSIM_dev/df_peso/df_weigth_10_3_pneumo1.Rdata")



######### _all  ----
rm(trans, inj, mat, dcnt, road, ICD_pneumo)
gc()
base.5 <- base.5 %>% select(-redis, -redis.2, -redis.3, 
                            -redis.4, -redis.5, -pr.mu, -pr.mi, -pr.me, -pr.uf, 
                            -pr.rg, -ob.mu, -ob.mi, -ob.me, -ob.uf, -ob.rg) %>% 
  mutate(c.red = ifelse(GBD %in% "_pneumo", NA, "_all")) %>% 
  left_join(dados_redis, by = c("cdmun", "micro", "meso", 
                                "ano", "sexo", "idade", "uf", "c.red"))
muni.all <- base.5 %>% filter(GBD != "_pneumo") %>% group_by(cdmun, 
                                                             micro, meso, GBD, idade, ano, sexo, uf) %>% summarise(ob = sum(obitos.2, 
                                                                                                                            na.rm = T)) %>% ungroup() %>% group_by(cdmun, micro, 
                                                                                                                                                                   meso, idade, ano, sexo, uf) %>% mutate(pr.mu = ob/sum(ob, 
                                                                                                                                                                                                                         na.rm = T), ob.mu = sum(ob, na.rm = T)) %>% select(-ob)
micro.all <- base.5 %>% filter(GBD != "_pneumo") %>% group_by(micro, 
                                                              meso, GBD, idade, ano, sexo, uf) %>% summarise(ob = sum(obitos.2, 
                                                                                                                      na.rm = T)) %>% ungroup() %>% group_by(micro, meso, 
                                                                                                                                                             idade, ano, sexo, uf) %>% mutate(pr.mi = ob/sum(ob, 
                                                                                                                                                                                                             na.rm = T), ob.mi = sum(ob, na.rm = T)) %>% select(-ob)
meso.all <- base.5 %>% filter(GBD != "_pneumo") %>% group_by(meso, 
                                                             GBD, idade, ano, sexo, uf) %>% summarise(ob = sum(obitos.2, 
                                                                                                               na.rm = T)) %>% ungroup() %>% group_by(meso, idade, 
                                                                                                                                                      ano, sexo, uf) %>% mutate(pr.me = ob/sum(ob), ob.me = sum(ob)) %>% 
  select(-ob)
uf.all <- base.5 %>% filter(GBD != "_pneumo") %>% group_by(GBD, 
                                                           idade, ano, sexo, uf) %>% summarise(ob = sum(obitos.2, 
                                                                                                        na.rm = T)) %>% ungroup() %>% group_by(idade, ano, sexo, 
                                                                                                                                               uf) %>% mutate(pr.uf = ob/sum(ob), ob.uf = sum(ob)) %>% 
  select(-ob)
reg.all <- base.5 %>% filter(GBD != "_pneumo") %>% mutate(reg = str_sub(cdmun, 
                                                                        1, 1)) %>% group_by(GBD, idade, ano, sexo, reg) %>% 
  summarise(ob = sum(obitos.2, na.rm = T)) %>% ungroup() %>% 
  group_by(idade, ano, sexo, reg) %>% mutate(pr.rg = ob/sum(ob, 
                                                            na.rm = T), ob.rg = sum(ob, na.rm = T)) %>% select(-ob)
base.5 <- base.5 %>% left_join(muni.all, by = c("cdmun", 
                                                "micro", "meso", "GBD", "idade", "ano", "sexo", "uf")) %>% 
  left_join(micro.all, by = c("micro", "meso", "GBD", 
                              "idade", "ano", "sexo", "uf")) %>% left_join(meso.all, 
                                                                           by = c("meso", "GBD", "idade", "ano", "sexo", "uf")) %>% 
  left_join(uf.all, by = c("GBD", "idade", "ano", "sexo", 
                           "uf")) %>% left_join(reg.all, by = c("GBD", "idade", 
                                                                "ano", "sexo", "reg"))
base.5 <- base.5 %>% mutate(all.1 = redis * pr.mu, redis.2 = ifelse(is.na(all.1) & 
                                                                      ob.mu == 0, redis, NA), all.2 = redis.2 * pr.mi, redis.3 = ifelse(is.na(all.2) & 
                                                                                                                                          ob.mi == 0, redis.2, NA), all.3 = redis.3 * pr.me, redis.4 = ifelse(is.na(all.3) & 
                                                                                                                                                                                                                ob.me == 0, redis.3, NA), all.4 = redis.4 * pr.uf, redis.5 = ifelse(is.na(all.4) & 
                                                                                                                                                                                                                                                                                      ob.uf == 0, redis.4, NA), all.5 = redis.5 * pr.rg, obitos.13 = ifelse(!is.na(all.1), 
                                                                                                                                                                                                                                                                                                                                                            obitos.12 + all.1, ifelse(!is.na(all.2), obitos.12 + 
                                                                                                                                                                                                                                                                                                                                                                                        all.2, ifelse(!is.na(all.3), obitos.12 + all.3, 
                                                                                                                                                                                                                                                                                                                                                                                                      ifelse(!is.na(all.4), obitos.12 + all.4, ifelse(!is.na(all.5), 
                                                                                                                                                                                                                                                                                                                                                                                                                                                      obitos.12 + all.5, obitos.12))))))











df_weigth_11_all <- base.5 %>% 
  select(ano, sexo, idade, GBD, obitos.2, obitos.3,obitos.4,obitos.5,obitos.6.0,obitos.6.1,obitos.6,obitos.7,obitos.8,obitos.9,obitos.10,obitos.12,obitos.13,pr.mu) %>% 
  rename(pr.mu.ob13=pr.mu)

save(df_weigth_11_all,
     file="/Users/renatoteixeira/Library/CloudStorage/OneDrive-Personal/Small Areas Estimation/Redistribuição GC/SAS to R/git_pacote/RedGCSIM_dev/df_peso/df_weigth_11_all.Rdata")


base_covid <- base_covid %>% mutate(obitos.3 = obitos.2, 
                                    obitos.4 = obitos.2, obitos.5 = obitos.2, obitos.6 = obitos.2, 
                                    obitos.7 = obitos.2, obitos.8 = obitos.2, obitos.9 = obitos.2, 
                                    obitos.10 = obitos.2, obitos.11 = obitos.2, obitos.12 = obitos.2, 
                                    obitos.13 = obitos.2)
base.5 <- bind_rows(base.5, base_covid)




out.file12 <- RedGCSIM::redistribuicao_causas_ivestigacao(dados_completos = out.file11,dados_redis = out.file9[["redistribuir"]], pesos =paste0(path,"/ICD_MAPPING_V6_2023 pos OPAS_nov2023.xlsx"))




########### juntando todas as bases de pesos
rm(list = ls())

files_pesos <- list.files(path = "/Users/renatoteixeira/Library/CloudStorage/OneDrive-Personal/Small Areas Estimation/Redistribuição GC/SAS to R/git_pacote/RedGCSIM_dev/df_peso",
           pattern = ".Rdata")


for (i in seq_along(files_pesos)) {
  load(file = paste0("/Users/renatoteixeira/Library/CloudStorage/OneDrive-Personal/Small Areas Estimation/Redistribuição GC/SAS to R/git_pacote/RedGCSIM_dev/df_peso/",files_pesos[i]))
}

df_weigth_all <- df_weigth_1_causas_inj %>% select(-obitos.3) %>%
  left_join(df_weigth_2_causas_inj.hom.sui %>% 
              select(ano,sexo,idade,GBD,obitos.2,last_col()), by = c("ano", "sexo", "idade", "GBD","obitos.2")) %>%
  left_join(df_weigth_3_causas_inj.hsf%>% 
              select(ano,sexo,idade,GBD,obitos.2,last_col()), by = c("ano", "sexo", "idade", "GBD","obitos.2")) %>%
  left_join(df_weigth_4_causas_inj.hst%>% 
              select(ano,sexo,idade,GBD,obitos.2,last_col()), by = c("ano", "sexo", "idade", "GBD","obitos.2")) %>%
  left_join(df_weigth_5_causas_inj.road%>% 
              select(ano,sexo,idade,GBD,obitos.2,last_col()), by = c("ano", "sexo", "idade", "GBD","obitos.2")) %>%
  left_join(df_weigth_6_causas_inj.transport%>% 
              select(ano,sexo,idade,GBD,obitos.2,last_col()), by = c("ano", "sexo", "idade", "GBD","obitos.2")) %>%
  left_join(df_weigth_7_causas_inj.hso%>% 
              select(ano,sexo,idade,GBD,obitos.2,last_col()), by = c("ano", "sexo", "idade", "GBD","obitos.2")) %>%
  left_join(df_weigth_8_causas_mat%>% 
              select(ano,sexo,idade,GBD,obitos.2,last_col()), by = c("ano", "sexo", "idade", "GBD","obitos.2")) %>%
  left_join(df_weigth_9_causas_inf%>% 
              select(ano,sexo,idade,GBD,obitos.2,last_col()), by = c("ano", "sexo", "idade", "GBD","obitos.2")) %>%
  left_join(df_weigth_10_1_x59%>% 
              select(ano,sexo,idade,GBD,obitos.2,last_col()), by = c("ano", "sexo", "idade", "GBD","obitos.2")) %>%
  left_join(df_weigth_10_2_y34%>% 
              select(ano,sexo,idade,GBD,obitos.2,last_col()), by = c("ano", "sexo", "idade", "GBD","obitos.2")) %>% 
  left_join(df_weigth_10_3_pneumo1%>% 
              select(ano,sexo,idade,GBD,obitos.2,last_col()), by = c("ano", "sexo", "idade", "GBD","obitos.2")) %>% 
  left_join(df_weigth_11_all%>% 
              select(ano,sexo,idade,GBD,obitos.2,last_col()), by = c("ano", "sexo", "idade", "GBD","obitos.2"))




save(df_weigth_all,
     file="/Users/renatoteixeira/Library/CloudStorage/OneDrive-Personal/Small Areas Estimation/Redistribuição GC/SAS to R/git_pacote/RedGCSIM_dev/df_peso/df_weigth_all.Rdata")
