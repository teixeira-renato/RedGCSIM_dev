load("C:/Users/ademar.junior/Downloads/df_weigth_all.Rdata")

load("C:/Users/ademar.junior/Downloads/df_weigth_missing_out7.Rdata")
#Trecho das funções calc_props e calc_investig
base_alt <- base_filt %>%
  left_join(muni, by=c("cdmun","micro","meso","GBD","idade","ano","sexo","uf")) %>%
  left_join(micro_df, by=c("micro","meso","GBD","idade","ano","sexo","uf")) %>%
  left_join(meso_df, by=c("meso","GBD","idade","ano","sexo","uf")) %>%
  left_join(uf_df, by=c("GBD","idade","ano","sexo","uf")) %>%
  left_join(reg_df, by=c("GBD","idade","ano","sexo","reg")) %>%
  left_join(df_weigth_all, by=c("GBD","idade","ano","sexo")) %>% #pesos gerais
  left_join(data_weight, by=c("GBD"="target")) %>% #relationship = "many-to-many"
  mutate(redis = if_else(!is.na(weight), redis*weight, redis))
