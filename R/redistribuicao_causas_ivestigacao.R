#' Redistribuição de Causas por Investigação
#'
#' Esta função redistribui causas de óbito classificadas como garbage (CG) nos dados do SIM,
#' utilizando critérios específicos baseados em pesos e categorias predefinidas.
#'
#' @param dados_completos Data frame contendo os dados completos, incluindo colunas como `cdmun`, `idade`, `sexo`, `ano`, e `GBD`.
#' @param dados_redis Data frame contendo as causas garbage a serem redistribuídas, com as colunas necessárias para a redistribuição.
#' @param pesos Data frame com os pesos de redistribuição associados às causas garbage e suas respectivas categorias.
#' @return Um data frame com os dados redistribuídos, incluindo as novas colunas de redistribuição (`obitos.10`, `obitos.11`, `obitos.12`, etc.) e validações.
#' @examples
#' \dontrun{
#' # Dados fictícios
#' dados_completos <- data.frame(
#'   cdmun = c("110001", "120001"),
#'   idade = c("30", "25"),
#'   sexo = c("Masculino", "Feminino"),
#'   ano = c(2020, 2021),
#'   GBD = c("Injuries - Falls", "materna_hemorragia"),
#'   obitos = c(5, 10)
#' )
#'
#' dados_redis <- data.frame(
#'   cdmun = c("110001", "120001"),
#'   idade = c("30", "25"),
#'   sexo = c("Masculino", "Feminino"),
#'   ano = c(2020, 2021),
#'   c.red = c("_x59", "_pneumo"),
#'   redis = c(1.2, 2.5)
#' )
#'
#' pesos <- data.frame(
#'   CG = c("_x59", "_pneumo"),
#'   target = c("Injuries - Falls", "materna_hemorragia"),
#'   weight = c(0.8, 1.5)
#' )
#'
#' resultado <- redistribuicao_causas_ivestigacao(dados_completos, dados_redis, pesos)
#' head(resultado)
#' }
#' @export

redistribuicao_causas_ivestigacao = function (dados_completos,dados_redis){
  if (!require("pacman")) install.packages("pacman") #garantir que o pacman está instalado
  pacman::p_load(tidyverse,rio) # pacotes necessários
  
  # ============================================================
  # 1. Targets
  # ============================================================
  causas=unique(ICD$CLASS_GPEAS_PRODUCAO)[!grepl(pattern = "^_",x = unique(ICD$CLASS_GPEAS_PRODUCAO))]
  causas=c(causas,"_pneumo")
  
  trans <-c(causas[grepl("^trans",causas)])
  inj <- c("Injuries - Falls", "Injuries - Homicide", "Injuries - Other transport injuries",
           "Injuries - Others","Injuries - Suicide")
  dcnt<- c(causas[grepl("^dcnt",causas)])
  road <- c(causas[grepl("^Injuries - Road",causas)])
  mat <-c(causas[grepl("^materna",causas)])
  cod_others <- c(causas[grepl("^other",causas)])
  
  ICD_pesos <- RedGCSIM::ICD_pesos
  # POR RedGCSIM_dev
  # ============================================================
  # 2. CAUSA X59
  # ============================================================
  
  ICD_x59 <- ICD_pesos%>%
    filter(CG=="_x59")%>%
    select(target,weight)
  
  base_final <- dados_completos %>%
    select(
      -any_of(grep("^pr\\.", names(.), value = TRUE)),
      -any_of(grep("^ob\\.", names(.), value = TRUE)),
      -any_of(c("redis", "redis.2", "redis.3", "redis.4", "c.red","weight"))
    ) %>%
    mutate(c.red=ifelse(GBD %in% ICD_x59$target, '_x59', NA)) %>%
    left_join(dados_redis, by=c('cdmun','micro','meso',  'ano', 'sexo','idade', 'uf', 'c.red'))
  
  base_final <- calc_investig (base = base_final, causa = c(trans,mat,"other_causes_all","other_causes-lri","other_desnutricao_all_ages"),
                            causa_II = road, data_weight = ICD_x59, fixed_weight = inj, prefix = "x59", obito_in = "obitos.9", obito_out = "obitos.10")
  # ============================================================
  # 3. CAUSAS Y34
  # ============================================================
  
  ICD_y34 <- ICD_pesos%>%
    filter(CG=="_y34")%>%
    select(target,weight)

  base_final <- base_final %>%
    select(
      -any_of(grep("^pr\\.", names(.), value = TRUE)),
      -any_of(grep("^ob\\.", names(.), value = TRUE)),
      -any_of(c("redis", "redis.2", "redis.3", "redis.4", "c.red","weight"))
    ) %>%
    mutate(c.red=ifelse(GBD %in% ICD_y34$target, '_y34', NA)) %>%
    left_join(dados_redis, by=c('cdmun','micro','meso',  'ano', 'sexo','idade', 'uf', 'c.red'))

  base_final <- calc_investig (base = base_final, causa = c(trans,mat,"other_causes_all","other_causes-lri","other_desnutricao_all_ages"),
                               causa_II = road, data_weight = ICD_y34, fixed_weight = c(inj,dcnt), prefix = "y34", obito_in = "obitos.10", obito_out = "obitos.11")

  # ============================================================
  # 4. CAUSAS Pneumonia e Garbage All
  # ============================================================

  ICD_pneumo <- ICD_pesos%>%
    filter(CG=="_pneumo")%>%
    select(target,age,weight)

  pneumo <- data.frame(target = c("_pneumo",dcnt,inj,trans,mat,road,"other_causes_all","other_causes-lri","other_desnutricao_all_ages"))
  pneumo <- pneumo%>%
    left_join(ICD_pneumo,by="target")%>%
    mutate(c.red=case_when(age == "<10" ~ '_pneumo_inf',
                           age == "10 a 59" ~ '_pneumo_adult',
                           age == "60 emais" ~ '_pneumo_idoso',))%>%
    select(-age)

  base.r.pneumo <- dados_redis%>%
    filter(c.red == "_pneumo")%>%
    mutate(c.red=case_when(c.red == "_pneumo" & idade %in% c("Early Neonatal","Post Neonatal","Late Neonatal","<1 year","0","5") ~ '_pneumo_inf',
                           c.red == "_pneumo" & idade %in% c("10","15","20","25","30","35","40","45","50","55") ~ '_pneumo_adult',
                           c.red == "_pneumo" & idade %in% c("60","65","70","75","80","85","90") ~ '_pneumo_idoso'))

  base_final <- base_final %>%
    select(
      -any_of(grep("^pr\\.", names(.), value = TRUE)),
      -any_of(grep("^ob\\.", names(.), value = TRUE)),
      -any_of(c("redis", "redis.2", "redis.3", "redis.4", "c.red","weight"))
    ) %>%
    mutate(c.red=case_when(GBD %in% pneumo$target & idade %in% c("Early Neonatal","Post Neonatal","Late Neonatal","<1 year","0","5") ~ '_pneumo_inf',
                           GBD %in% pneumo$target & idade %in% c("10","15","20","25","30","35","40","45","50","55") ~ '_pneumo_adult',
                           GBD %in% pneumo$target & idade %in% c("60","65","70","75","80","85","90") ~ '_pneumo_idoso')) %>%
    left_join(base.r.pneumo, by=c('cdmun','micro','meso',  'ano', 'sexo','idade', 'uf', 'c.red'))

  base_final <- calc_investig (base = base_final, causa = c(trans,mat,"other_causes_all","other_causes-lri","other_desnutricao_all_ages"),
                               causa_II = road, data_weight = pneumo, fixed_weight = c("_pneumo",inj,dcnt), prefix = "pne", obito_in = "obitos.11", obito_out = "obitos.12")

  # ============================================================
  # 5. Garbage All
  # ============================================================

  #### Casos redistribuídos entre todas as causas------
  #### Criação da variável com a causa CG para nortear a redistribuição
  base_final <- base_final %>%
    select(
      -any_of(grep("^pr\\.", names(.), value = TRUE)),
      -any_of(grep("^ob\\.", names(.), value = TRUE)),
      -any_of(c("redis", "redis.2", "redis.3", "redis.4", "c.red","weight"))
    ) %>%
    mutate(c.red=ifelse(GBD%in%"_pneumo",NA,'_all')) %>%
    left_join(dados_redis, by=c('cdmun','micro','meso',  'ano', 'sexo','idade', 'uf', 'c.red'))

  ###Proporções _all

  ###PRMUN
  muni.all <- base_final %>%
    filter(GBD!="_pneumo") %>%
    group_by(cdmun,micro,meso, GBD,idade, ano, sexo, uf) %>%
    summarise(ob=sum(obitos.2, na.rm = T))%>%
    ungroup() %>%
    group_by(cdmun,micro,meso,idade, ano, sexo, uf) %>%
    mutate(pr.mu=ob/sum(ob,na.rm=T),
           ob.mu=sum(ob,na.rm=T)) %>%
    select(-ob)

  ###PR.MICRO
  micro.all <- base_final %>%
    filter(GBD!="_pneumo") %>%
    group_by(micro,meso, GBD,idade, ano, sexo, uf) %>%
    summarise(ob=sum(obitos.2, na.rm = T))%>%
    ungroup() %>%
    group_by(micro,meso,idade, ano, sexo, uf) %>%
    mutate(pr.mi=ob/sum(ob,na.rm=T),
           ob.mi=sum(ob,na.rm=T)) %>%
    select(-ob)

  ###PR.MESO
  meso.all <- base_final %>%
    filter(GBD!="_pneumo") %>%
    group_by(meso, GBD,idade, ano, sexo, uf) %>%
    summarise(ob=sum(obitos.2, na.rm = T))%>%
    ungroup() %>%
    group_by(meso,idade, ano, sexo, uf) %>%
    mutate(pr.me=ob/sum(ob),
           ob.me=sum(ob)) %>%
    select(-ob)

  ###PR.UF
  uf.all <- base_final %>%
    filter(GBD!="_pneumo") %>%
    group_by( GBD,idade, ano, sexo, uf) %>%
    summarise(ob=sum(obitos.2, na.rm = T))%>%
    ungroup() %>%
    group_by(idade, ano, sexo, uf) %>%
    mutate(pr.uf=ob/sum(ob),
           ob.uf=sum(ob)) %>%
    select(-ob)

  ###PR.REG
  reg.all <- base_final %>%
    filter(GBD!="_pneumo") %>%
    mutate(reg=str_sub(cdmun,1,1)) %>%
    group_by( GBD,idade, ano, sexo, reg) %>%
    summarise(ob=sum(obitos.2, na.rm = T))%>%
    ungroup() %>%
    group_by(idade, ano, sexo, reg) %>%
    mutate(pr.rg=ob/sum(ob,na.rm=T),
           ob.rg=sum(ob,na.rm=T)) %>%
    select(-ob)


  base_final <- base_final %>%
    left_join(muni.all, by=c('cdmun','micro','meso', 'GBD','idade', 'ano', 'sexo', 'uf')) %>%
    left_join(micro.all, by=c('micro','meso', 'GBD','idade', 'ano', 'sexo', 'uf')) %>%
    left_join(meso.all, by=c('meso', 'GBD','idade', 'ano', 'sexo', 'uf')) %>%
    left_join(uf.all, by=c( 'GBD','idade', 'ano', 'sexo', 'uf')) %>%
    left_join(reg.all, by=c( 'GBD','idade', 'ano', 'sexo', 'reg'))

  base_final <- base_final %>%
    mutate(all.1=redis*pr.mu,
           redis.2=ifelse(is.na(all.1) & ob.mu==0, redis,NA),
           all.2=redis.2*pr.mi,
           redis.3=ifelse(is.na(all.2) & ob.mi==0, redis.2,NA),
           all.3=redis.3*pr.me,
           redis.4=ifelse(is.na(all.3) & ob.me==0, redis.3,NA),
           all.4=redis.4*pr.uf,
           redis.5=ifelse(is.na(all.4) & ob.uf==0, redis.4,NA),
           all.5=redis.5*pr.rg,
           obitos.13=ifelse(!is.na(all.1), obitos.12+all.1,
                            ifelse(!is.na(all.2), obitos.12+all.2,
                                   ifelse(!is.na(all.3), obitos.12+all.3,
                                          ifelse(!is.na(all.4), obitos.12+all.4,
                                                 ifelse(!is.na(all.5), obitos.12+all.5, obitos.12))))))


  base_covid <- base_covid %>%
    mutate(obitos.3=obitos.2,
           obitos.4=obitos.2,
           obitos.5=obitos.2,
           obitos.6=obitos.2,
           obitos.7=obitos.2,
           obitos.8=obitos.2,
           obitos.9=obitos.2,
           obitos.10=obitos.2,
           obitos.11=obitos.2,
           obitos.12=obitos.2,
           obitos.13=obitos.2
    )

  base_final <- bind_rows(base_final,base_covid)


  return(base_final)

}
