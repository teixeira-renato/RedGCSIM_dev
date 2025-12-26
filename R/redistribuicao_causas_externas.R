#' Redistribuição de Causas Externas
#'
#' Esta função redistribui as causas externas nos dados do Sistema de Informações
#' sobre Mortalidade (SIM), utilizando critérios específicos para ajuste de dados.
#'
#' @param dados_completos Data frame contendo os dados completos, com informações de causas, localidade, e outras variáveis relevantes.
#' @param dados_redis Data frame contendo os dados de causas externas que serão redistribuídas.
#' @param criterio (Opcional) Uma string que define o critério de redistribuição. O padrão é "default".
#' @return Um data frame com as causas externas redistribuídas, seguindo os critérios especificados.
#' @examples
#' \dontrun{
#' dados_completos <- data.frame(
#'   localidade = c("A", "B", "C"),
#'   causa = c("Causa1", "Causa2", "Causa3"),
#'   obitos = c(10, 20, 30)
#' )
#' dados_redis <- data.frame(
#'   localidade = c("A", "B"),
#'   causa = c("CausaX", "CausaY"),
#'   obitos = c(5, 10)
#' )
#' resultado <- redistribuicao_causas_externas(dados_completos, dados_redis)
#' print(resultado)
#' }
#' @export


redistribuicao_causas_externas <- function(dados_completos, dados_redis) {
   if (!require("pacman")) install.packages("pacman") #garantir que o pacman está instalado
  pacman::p_load(tidyverse) # pacotes necessários
  
  # ============================================================
  # 1. CAUSAS EXTERNAS
  # ============================================================
  
  causas=unique(ICD$CLASS_GPEAS_PRODUCAO)[!grepl(pattern = "^_",x = unique(ICD$CLASS_GPEAS_PRODUCAO))]
  causas=c(causas,"_pneumo")
  
  causas_inj   <- c(causas[grepl("^Injuries",causas)])
  
  causas_inj.hom.sui <-c("Injuries - Homicide", "Injuries - Suicide")
  
  causas_inj.hsf <-c("Injuries - Falls", "Injuries - Homicide", "Injuries - Suicide", "Injuries - Road - Buses and Heavy Vehicles","Injuries - Road - Cyclist",                          
                     "Injuries - Road - Four-Wheel Cars and Light Vechicles","Injuries - Road - Motocyclist",                        
                     "Injuries - Road - Other","Injuries - Road - Pedestrian")
  
  causas_inj.hst <- c("Injuries - Suicide", "Injuries - Homicide", "Injuries - Other transport injuries","Injuries - Road - Buses and Heavy Vehicles","Injuries - Road - Cyclist",                          
                     "Injuries - Road - Four-Wheel Cars and Light Vechicles","Injuries - Road - Motocyclist",                        
                     "Injuries - Road - Other","Injuries - Road - Pedestrian")
  
  causas_inj.road <-c("Injuries - Road - Buses and Heavy Vehicles","Injuries - Road - Cyclist",                          
                      "Injuries - Road - Four-Wheel Cars and Light Vechicles","Injuries - Road - Motocyclist",                        
                      "Injuries - Road - Other","Injuries - Road - Pedestrian")
  
  causas_inj.transport <-c("Injuries - Other transport injuries","Injuries - Road - Buses and Heavy Vehicles","Injuries - Road - Cyclist",                          
                           "Injuries - Road - Four-Wheel Cars and Light Vechicles","Injuries - Road - Motocyclist",                        
                           "Injuries - Road - Other","Injuries - Road - Pedestrian")
  
  causas_inj.hso <-c("Injuries - Others", "Injuries - Suicide", "Injuries - Homicide")
  
  # ============================================================
  # 1. REDISTRIBUICAO
  # ============================================================
  ##### Injuries  -----
  base_final <- dados_completos %>%
    select(
      -any_of(grep("^pr\\.", names(.), value = TRUE)),
      -any_of(grep("^ob\\.", names(.), value = TRUE)),
      -any_of(c("redis", "redis.2", "redis.3", "redis.4", "c.red"))
    ) %>%
    mutate(c.red=ifelse(GBD %in% causas_inj, '_injuries', NA)) %>%
    left_join(dados_redis, by=c('cdmun','micro','meso',  'ano', 'sexo','idade', 'uf', 'c.red'))
  
  base_final <- calc_props(base = base_final, causa = causas_inj, prefix = "inj", obito_in = "obitos.2", obito_out = "obitos.3",
                           sexo_filtro = NULL,idades_filtro = NULL)
  
  ############_Injuries-hom-sui ----
  
  base_final <- base_final %>%
    select(
      -any_of(grep("^pr\\.", names(.), value = TRUE)),
      -any_of(grep("^ob\\.", names(.), value = TRUE)),
      -any_of(c("redis", "redis.2", "redis.3", "redis.4", "c.red"))
    ) %>%
    mutate(c.red=ifelse(GBD %in% causas_inj.hom.sui, '_inj (hom,sui)',NA))%>%
    mutate(c.red=ifelse(GBD %in% "Injuries - Suicide" & idade %in% c("Early Neonatal","Post Neonatal","Late Neonatal","<1 year","0"),NA,c.red))%>%
    left_join(dados_redis, by=c('cdmun','micro','meso',  'ano', 'sexo','idade', 'uf', 'c.red'))
  
  base_final <- calc_props(base = base_final, causa = causas_inj.hom.sui, prefix = "inj.h.s", obito_in = "obitos.3", obito_out = "obitos.4",
                           sexo_filtro = NULL,idades_filtro = NULL)
  
  ########_inj (hom,suic, fall,road): -----
  base_final <- base_final %>%
    select(
      -any_of(grep("^pr\\.", names(.), value = TRUE)),
      -any_of(grep("^ob\\.", names(.), value = TRUE)),
      -any_of(c("redis", "redis.2", "redis.3", "redis.4", "c.red"))
    ) %>%
    mutate(c.red=ifelse(GBD %in% causas_inj.hsf, '_inj (hom,suic, fall,road)',NA))%>%
    mutate(c.red=ifelse(GBD %in% "Injuries - Suicide" & idade %in% c("Early Neonatal","Post Neonatal","Late Neonatal","<1 year","0"),NA,c.red))%>%
    left_join(dados_redis, by=c('cdmun','micro','meso',  'ano', 'sexo','idade', 'uf', 'c.red'))
  
  base_final <- calc_props(base = base_final, causa = causas_inj.hsf, prefix = "inj.hsf", obito_in = "obitos.4", obito_out = "obitos.5",
                           sexo_filtro = NULL,idades_filtro = NULL)
  
  ########	_inj (hom,sui,transp):-----
  base_final <- base_final %>%
    select(
      -any_of(grep("^pr\\.", names(.), value = TRUE)),
      -any_of(grep("^ob\\.", names(.), value = TRUE)),
      -any_of(c("redis", "redis.2", "redis.3", "redis.4", "c.red"))
    ) %>%
    mutate(c.red=ifelse(GBD %in% causas_inj.hst, '_inj (hom,sui,transp)', NA)) %>%
    mutate(c.red=ifelse(GBD %in% "Injuries - Suicide" & idade %in% c("Early Neonatal","Post Neonatal","Late Neonatal","<1 year","0"),NA,c.red))%>%
    left_join(dados_redis, by=c('cdmun','micro','meso',  'ano', 'sexo','idade', 'uf', 'c.red'))
  
  
  base_final <- calc_props(base = base_final, causa = causas_inj.hst, prefix = "inj.hst", obito_in = "obitos.5", obito_out = "obitos.6.0",
                           sexo_filtro = NULL,idades_filtro = NULL)
  
  ########	_gc_inj_road:-----
  base_final <- base_final %>%
    select(
      -any_of(grep("^pr\\.", names(.), value = TRUE)),
      -any_of(grep("^ob\\.", names(.), value = TRUE)),
      -any_of(c("redis", "redis.2", "redis.3", "redis.4", "c.red"))
    ) %>% 
    mutate(c.red=ifelse(GBD %in% causas_inj.road, '_gc_inj_road', NA)) %>%
    left_join(dados_redis, by=c('cdmun','micro','meso',  'ano', 'sexo','idade', 'uf', 'c.red'))
  
  base_final <- calc_props(base = base_final, causa = causas_inj.road, prefix = "inj.road", obito_in = "obitos.6.0", obito_out = "obitos.6.1",
                           sexo_filtro = NULL,idades_filtro = NULL)
  
  ########	_gc_inj_transport:-----
  base_final <- base_final %>%
    select(
      -any_of(grep("^pr\\.", names(.), value = TRUE)),
      -any_of(grep("^ob\\.", names(.), value = TRUE)),
      -any_of(c("redis", "redis.2", "redis.3", "redis.4", "c.red"))
    ) %>% 
    mutate(c.red=ifelse(GBD %in% causas_inj.transport, '_gc_inj_transport', NA)) %>%
    left_join(dados_redis, by=c('cdmun','micro','meso',  'ano', 'sexo','idade', 'uf', 'c.red'))
  
  base_final <- calc_props(base = base_final, causa = causas_inj.transport, prefix = "inj.transport", obito_in = "obitos.6.1", obito_out = "obitos.6",
                           sexo_filtro = NULL,idades_filtro = NULL)
  
  ########	_inj(hom,suic,other):-----
  base_final <- base_final %>%
    select(
      -any_of(grep("^pr\\.", names(.), value = TRUE)),
      -any_of(grep("^ob\\.", names(.), value = TRUE)),
      -any_of(c("redis", "redis.2", "redis.3", "redis.4", "c.red"))
    ) %>% 
    mutate(c.red=ifelse(GBD %in% causas_inj.hso, '_inj (hom,suic,other)', NA)) %>%
    mutate(c.red=ifelse(GBD %in% "Injuries - Suicide" & idade %in% c("Early Neonatal","Post Neonatal","Late Neonatal","<1 year","0"),NA,c.red))%>%
    left_join(dados_redis, by=c('cdmun','micro','meso',  'ano', 'sexo','idade', 'uf', 'c.red'))
  
  base_final <- calc_props(base = base_final, causa = causas_inj.hso, prefix = "inj.hso", obito_in = "obitos.6", obito_out = "obitos.7",
                           sexo_filtro = NULL,idades_filtro = NULL)
  
  return(base_final)
}
