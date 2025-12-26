#' Redistribuição de Causas Maternas e Infantis
#'
#' Esta função redistribui causas de óbitos maternos e infantis (neonatais) nos dados do SIM,
#' utilizando critérios específicos baseados em proporções calculadas.
#'
#' @param dados_completos Data frame contendo os dados completos, incluindo colunas como
#'                        `cdmun`, `idade`, `sexo`, `ano`, e `GBD`.
#' @param dados_redis Data frame contendo as causas garbage a serem redistribuídas,
#'                    com as colunas necessárias para o processo.
#' @return Um data frame atualizado com as redistribuições realizadas, incluindo novas
#'         colunas de redistribuição (`obitos.8`, `obitos.9`, etc.) e validações.
#' @examples
#' \dontrun{
#' # Dados fictícios
#' dados_completos <- data.frame(
#'   cdmun = c("110001", "120001"),
#'   idade = c("25", "30"),
#'   sexo = c("Feminino", "Feminino"),
#'   ano = c(2020, 2021),
#'   GBD = c("materna_hemorragia", "infant_neonatal_prematuridade"),
#'   obitos = c(5, 10)
#' )
#'
#' dados_redis <- data.frame(
#'   cdmun = c("110001", "120001"),
#'   idade = c("25", "30"),
#'   sexo = c("Feminino", "Feminino"),
#'   ano = c(2020, 2021),
#'   c.red = c("_maternas", "_infant_neonat"),
#'   redis = c(1.5, 2.5)
#' )
#'
#' resultado <- redistribuicao_causas_mat_inf(dados_completos, dados_redis)
#' head(resultado)
#' }
#' @export

redistribuicao_causas_mat_inf = function (dados_completos, dados_redis){
  if (!require("pacman")) install.packages("pacman") #garantir que o pacman está instalado
  pacman::p_load(tidyverse) # pacotes necessários
  
  causas=unique(ICD$CLASS_GPEAS_PRODUCAO)[!grepl(pattern = "^_",x = unique(ICD$CLASS_GPEAS_PRODUCAO))]
  causas=c(causas,"_pneumo")
  
  causas_mat   <- causas[grepl("^materna", causas)]
  causas_inf   <- causas[grepl("^infant|anom_congenitas|aspiracao_pulmunar|obst_intestinal|lri_post_neo", causas)]
  
  idades_mat   <- c("10","15","20","25","30","35","40","45","50")
  idades_inf   <- c("Early Neonatal","Post Neonatal","Late Neonatal","<1 year")
  
  base_final <- dados_completos %>%
    select(-starts_with("pr."), -starts_with("ob."), 
           -redis, -redis.2, -redis.3, -redis.4, -c.red) %>%
    mutate(c.red=ifelse(GBD %in% causas_mat & sexo == "Feminino" & idade%in%c("10","15","20","25","30","35","40","45","50"), '_maternas', NA)) %>%
    left_join(dados_redis, by=c('cdmun','micro','meso',  'ano', 'sexo','idade', 'uf', 'c.red'))
  
  base_final <- calc_props(base = base_final, causa = causas_mat, prefix = "mat", obito_in = "obitos.7", obito_out = "obitos.8",
                           sexo_filtro = "Feminino",idades_filtro = idades_mat)
  
  base_final <- base_final %>%
    select(-starts_with("pr."), -starts_with("ob."), 
           -redis, -redis.2, -redis.3, -redis.4, -c.red) %>%
    mutate(c.red=ifelse(GBD %in% causas_inf & idade %in% c("Early Neonatal","Post Neonatal","Late Neonatal","<1 year"), '_infant_neonat', NA)) %>%
    left_join(dados_redis, by=c('cdmun','micro','meso',  'ano', 'sexo','idade', 'uf', 'c.red'))
  
  base_final <- calc_props(base = base_final, causa = causas_inf, prefix = "infant", obito_in = "obitos.8", obito_out = "obitos.9",sexo_filtro = NULL,idades_filtro = idades_inf)
  
  return(base_final)
}
