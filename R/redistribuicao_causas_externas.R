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
  pacman::p_load(tidyverse,data.table,stringr) # pacotes necessários
  # -------------------------------------------------------------
  # 1) Função interna: prop_grupo_dt
  # -------------------------------------------------------------
    prop_grupo_dt <- function(base, causa, prefix, obito_in, obito_out) {
      
      base <- as.data.table(base)
      base[, reg := substr(cdmun, 1, 1)]
      
      df <- base[GBD %in% causa]
      
      # Município
      muni <- df[
        , .(ob = sum(get(obito_in))),
        by = .(cdmun, micro, meso, GBD, idade, ano, sexo, uf)
      ][
        , `:=`(pr.mu = ob / sum(ob), ob.mu = sum(ob)),
        by = .(cdmun, micro, meso, idade, ano, sexo, uf)
      ]
      
      # Microrregião
      micro_dt <- df[
        , .(ob = sum(get(obito_in))),
        by = .(micro, meso, GBD, idade, ano, sexo, uf)
      ][
        , `:=`(pr.mi = ob / sum(ob), ob.mi = sum(ob)),
        by = .(micro, meso, idade, ano, sexo, uf)
      ]
      
      # Mesorregião
      meso_dt <- df[
        , .(ob = sum(get(obito_in))),
        by = .(meso, GBD, idade, ano, sexo, uf)
      ][
        , `:=`(pr.me = ob / sum(ob), ob.me = sum(ob)),
        by = .(meso, idade, ano, sexo, uf)
      ]
      
      # UF
      uf_dt <- df[
        , .(ob = sum(get(obito_in))),
        by = .(GBD, idade, ano, sexo, uf)
      ][
        , `:=`(pr.uf = ob / sum(ob), ob.uf = sum(ob)),
        by = .(idade, ano, sexo, uf)
      ]
      
      # Região
      reg_dt <- df[
        , .(ob = sum(get(obito_in))),
        by = .(GBD, idade, ano, sexo, reg)
      ][
        , `:=`(pr.rg = ob / sum(ob), ob.rg = sum(ob)),
        by = .(idade, ano, sexo, reg)
      ]
      
      # Joins
      setkey(base, cdmun, micro, meso, GBD, idade, ano, sexo, uf, reg)
      base <- muni[base, on=.(cdmun, micro, meso, GBD, idade, ano, sexo, uf)]
      base <- micro_dt[base, on=.(micro, meso, GBD, idade, ano, sexo, uf)]
      base <- meso_dt[base, on=.(meso, GBD, idade, ano, sexo, uf)]
      base <- uf_dt[base, on=.(GBD, idade, ano, sexo, uf)]
      base <- reg_dt[base, on=.(GBD, idade, ano, sexo, reg)]
      
      # Redistribuição
      base[, paste0(prefix, ".1") := redis * pr.mu]
      base[, redis.2 := fifelse(is.na(get(paste0(prefix, ".1"))) & ob.mu == 0, redis, NA_real_)]
      
      base[, paste0(prefix, ".2") := redis.2 * pr.mi]
      base[, redis.3 := fifelse(is.na(get(paste0(prefix, ".2"))) & ob.mi == 0, redis.2, NA_real_)]
      
      base[, paste0(prefix, ".3") := redis.3 * pr.me]
      base[, redis.4 := fifelse(is.na(get(paste0(prefix, ".3"))) & ob.me == 0, redis.3, NA_real_)]
      
      base[, paste0(prefix, ".4") := redis.4 * pr.uf]
      base[, redis.5 := fifelse(is.na(get(paste0(prefix, ".4"))) & ob.uf == 0, redis.4, NA_real_)]
      
      base[, paste0(prefix, ".5") := redis.5 * pr.rg]
      
      # Obitos finais — VERSÃO CORRIGIDA
      base[
        , (obito_out) :=
          get(obito_in) +
          fifelse(!is.na(get(paste0(prefix, ".1"))), get(paste0(prefix, ".1")),
                  fifelse(!is.na(get(paste0(prefix, ".2"))), get(paste0(prefix, ".2")),
                          fifelse(!is.na(get(paste0(prefix, ".3"))), get(paste0(prefix, ".3")),
                                  fifelse(!is.na(get(paste0(prefix, ".4"))), get(paste0(prefix, ".4")),
                                          fifelse(!is.na(get(paste0(prefix, ".5"))), get(paste0(prefix, ".5")), 0))))) ]
  
  return(base[])
    }
  # -------------------------------------------------------------
  # 2) Preparação inicial
  # -------------------------------------------------------------
  causas <- unique(ICD$CLASS_GPEAS_PRODUCAO)
  causas <- causas[!grepl("^_", causas)]
  causas <- c(causas, "_pneumo")
  
  # Grupos:
  inj  <- causas[grepl("^Injuries", causas)]
  inj_hs <- c("Injuries - Homicide", "Injuries - Suicide")
  inj_hsf <- c("Injuries - Falls",
               "Injuries - Homicide", "Injuries - Suicide",
               "Injuries - Road - Buses and Heavy Vehicles",
               "Injuries - Road - Cyclist",
               "Injuries - Road - Four-Wheel Cars and Light Vechicles",
               "Injuries - Road - Motocyclist",
               "Injuries - Road - Other",
               "Injuries - Road - Pedestrian")
  
  inj_hst <- c("Injuries - Suicide", "Injuries - Homicide",
               "Injuries - Other transport injuries",
               "Injuries - Road - Buses and Heavy Vehicles",
               "Injuries - Road - Cyclist",
               "Injuries - Road - Four-Wheel Cars and Light Vechicles",
               "Injuries - Road - Motocyclist",
               "Injuries - Road - Other",
               "Injuries - Road - Pedestrian")
  
  inj_road <- c(
    "Injuries - Road - Buses and Heavy Vehicles",
    "Injuries - Road - Cyclist",
    "Injuries - Road - Four-Wheel Cars and Light Vechicles",
    "Injuries - Road - Motocyclist",
    "Injuries - Road - Other",
    "Injuries - Road - Pedestrian"
  )
  
  inj_transport <- c(
    "Injuries - Other transport injuries",
    inj_road
  )
  
  inj_hso <- c("Injuries - Others", "Injuries - Suicide", "Injuries - Homicide")
  
  
  # -------------------------------------------------------------
  # 3) Aplicação sequencial de todas as redistribuições
  # -------------------------------------------------------------
  
  # Etapa 1 — Injuries
  base <- dados_completos %>%
    mutate(c.red = ifelse(GBD %in% inj, "_injuries", NA)) %>%
    left_join(dados_redis, by = c("cdmun","micro","meso","ano","sexo","idade","uf","c.red"))
  
  base <- prop_grupo_dt(base, inj, "inj", "obitos.2", "obitos.3")
  
  # Etapa 2 — Injuries (Homicide + Suicide)
  base[, `:=`(c.red = ifelse(GBD %in% inj_hs, "_inj_hs", NA))]
  
  base <- prop_grupo_dt(base, inj_hs, "ihs", "obitos.3", "obitos.4")
  
  # Etapa 3 — Injuries (hom, sui, fall, road)
  base[, `:=`(c.red = ifelse(GBD %in% inj_hsf, "_inj_hsf", NA))]
  
  base <- prop_grupo_dt(base, inj_hsf, "ihsf", "obitos.4", "obitos.5")
  
  # Etapa 4 — Injuries (hom, sui, transport)
  base[, `:=`(c.red = ifelse(GBD %in% inj_hst, "_inj_hst", NA))]
  
  base <- prop_grupo_dt(base, inj_hst, "ihst", "obitos.5", "obitos.6_0")
  
  # Etapa 5 — Road
  base[, `:=`(c.red = ifelse(GBD %in% inj_road, "_inj_road", NA))]
  
  base <- prop_grupo_dt(base, inj_road, "iroad", "obitos.6_0", "obitos.6_1")
  
  # Etapa 6 — Transport
  base[, `:=`(c.red = ifelse(GBD %in% inj_transport, "_inj_transport", NA))]
  
  base <- prop_grupo_dt(base, inj_transport, "itrans", "obitos.6_1", "obitos.6")
  
  # Etapa 7 — Others (hom, sui, other)
  base[, `:=`(c.red = ifelse(GBD %in% inj_hso, "_inj_hso", NA))]
  
  base <- prop_grupo_dt(base, inj_hso, "ihso", "obitos.6", "obitos.7")
  
  return(base[])
}
