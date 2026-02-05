calc_props <- function(base, causa, prefix, obito_in, obito_out,
                       sexo_filtro = NULL, idades_filtro = NULL) {
  
  base_filt <- base %>%
    filter(GBD %in% causa)
  
  if (!is.null(sexo_filtro)) {
    base_filt <- base_filt %>% filter(sexo %in% sexo_filtro)
  }
  
  if (!is.null(idades_filtro)) {
    base_filt <- base_filt %>% filter(idade %in% idades_filtro)
  }
  
  base_filt <- base_filt %>%
    mutate(reg = str_sub(cdmun, 1, 1))
  
  # MUNICÍPIO
  muni <- base_filt %>%
    group_by(cdmun, micro, meso, GBD, idade, ano, sexo, uf) %>%
    summarise(ob = sum(.data[[obito_in]], na.rm = TRUE), .groups = "drop") %>%
    group_by(cdmun, micro, meso, idade, ano, sexo, uf) %>%
    mutate(
      pr.mu = ob / sum(ob, na.rm = TRUE),
      ob.mu = sum(ob, na.rm = TRUE)
    ) %>%
    ungroup()
  
  # MICRORREGIÃO
  micro_df <- base_filt %>%
    group_by(micro, meso, GBD, idade, ano, sexo, uf) %>%
    summarise(ob = sum(.data[[obito_in]], na.rm = TRUE), .groups = "drop") %>%
    group_by(micro, meso, idade, ano, sexo, uf) %>%
    mutate(
      pr.mi = ob / sum(ob, na.rm = TRUE),
      ob.mi = sum(ob, na.rm = TRUE)
    ) %>%
    ungroup()
  
  # MESORREGIÃO
  meso_df <- base_filt %>%
    group_by(meso, GBD, idade, ano, sexo, uf) %>%
    summarise(ob = sum(.data[[obito_in]], na.rm = TRUE), .groups = "drop") %>%
    group_by(meso, idade, ano, sexo, uf) %>%
    mutate(
      pr.me = ob / sum(ob, na.rm = TRUE),
      ob.me = sum(ob, na.rm = TRUE)
    ) %>%
    ungroup()
  
  # UF
  uf_df <- base_filt %>%
    group_by(GBD, idade, ano, sexo, uf) %>%
    summarise(ob = sum(.data[[obito_in]], na.rm = TRUE), .groups = "drop") %>%
    group_by(idade, ano, sexo, uf) %>%
    mutate(
      pr.uf = ob / sum(ob, na.rm = TRUE),
      ob.uf = sum(ob, na.rm = TRUE)
    ) %>%
    ungroup()
  
  # REGIÃO
  reg_df <- base_filt %>%
    group_by(GBD, idade, ano, sexo, reg) %>%
    summarise(ob = sum(.data[[obito_in]], na.rm = TRUE), .groups = "drop") %>%
    group_by(idade, ano, sexo, reg) %>%
    mutate(
      pr.rg = ob / sum(ob, na.rm = TRUE),
      ob.rg = sum(ob, na.rm = TRUE)
    ) %>%
    ungroup()
  
  # JUNTAR PROPORÇÕES
  base_alt <- base_filt %>%
    left_join(muni,     by = c("cdmun","micro","meso","GBD","idade","ano","sexo","uf")) %>%
    left_join(micro_df, by = c("micro","meso","GBD","idade","ano","sexo","uf")) %>%
    left_join(meso_df,  by = c("meso","GBD","idade","ano","sexo","uf")) %>%
    left_join(uf_df,    by = c("GBD","idade","ano","sexo","uf")) %>%
    left_join(reg_df,   by = c("GBD","idade","ano","sexo","reg"))
  
  # REDISTRIBUIÇÃO
  base_alt <- base_alt %>%
    mutate(
      "{prefix}.1" := redis * pr.mu,
      redis.2      = ifelse(is.na(.data[[paste0(prefix, ".1")]]) & ob.mu == 0, redis, NA),
      
      "{prefix}.2" := redis.2 * pr.mi,
      redis.3      = ifelse(is.na(.data[[paste0(prefix, ".2")]]) & ob.mi == 0, redis.2, NA),
      
      "{prefix}.3" := redis.3 * pr.me,
      redis.4      = ifelse(is.na(.data[[paste0(prefix, ".3")]]) & ob.me == 0, redis.3, NA),
      
      "{prefix}.4" := redis.4 * pr.uf,
      redis.5      = ifelse(is.na(.data[[paste0(prefix, ".4")]]) & ob.uf == 0, redis.4, NA),
      
      "{prefix}.5" := redis.5 * pr.rg,
      
      "{obito_out}" :=
        ifelse(!is.na(.data[[paste0(prefix, ".1")]]), .data[[obito_in]] + .data[[paste0(prefix, ".1")]],
               ifelse(!is.na(.data[[paste0(prefix, ".2")]]), .data[[obito_in]] + .data[[paste0(prefix, ".2")]],
                      ifelse(!is.na(.data[[paste0(prefix, ".3")]]), .data[[obito_in]] + .data[[paste0(prefix, ".3")]],
                             ifelse(!is.na(.data[[paste0(prefix, ".4")]]), .data[[obito_in]] + .data[[paste0(prefix, ".4")]],
                                    ifelse(!is.na(.data[[paste0(prefix, ".5")]]), .data[[obito_in]] + .data[[paste0(prefix, ".5")]],
                                           .data[[obito_in]]))))))

return(base_alt)
}
