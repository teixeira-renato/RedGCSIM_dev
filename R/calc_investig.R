calc_investig <- function(base, causa, causa_II, data_weight, fixed_weight, prefix, obito_in, obito_out) {

  base_filt <- base %>%
    mutate(reg = str_sub(cdmun, 1, 1))
  
  # MUNICÍPIO-----------
  muni <- base_filt%>%
    filter(GBD %in% causa) %>%
    group_by(cdmun, micro, meso, GBD, idade, ano, sexo, uf) %>%
    summarise(ob = sum(.data[[obito_in]], na.rm = TRUE), .groups="drop_last") %>%
    group_by(cdmun, micro, meso, idade, ano, sexo, uf) %>%
    mutate(
      pr.mu = ob / sum(ob, na.rm = TRUE),
      ob.mu = sum(ob, na.rm = TRUE)
    ) %>%
    ungroup()
  
  ###part II
  muni_II <- base_filt%>%
    filter(GBD %in% causa_II) %>%
    group_by(cdmun, micro, meso, GBD, idade, ano, sexo, uf) %>%
    summarise(ob = sum(.data[[obito_in]], na.rm = TRUE), .groups="drop_last") %>%
    group_by(cdmun, micro, meso, idade, ano, sexo, uf) %>%
    mutate(
      pr.mu = ob / sum(ob, na.rm = TRUE),
      ob.mu = sum(ob, na.rm = TRUE)
    ) %>%
    ungroup()
  
  muni <- rbind(muni, muni_II)
  rm(muni_II)
  
  # MICRORREGIÃO-------------
  micro_df <- base_filt %>%
    filter(GBD %in% causa) %>%
    group_by(micro, meso, GBD, idade, ano, sexo, uf) %>%
    summarise(ob = sum(.data[[obito_in]], na.rm = TRUE), .groups="drop_last") %>%
    group_by(micro, meso, idade, ano, sexo, uf) %>%
    mutate(
      pr.mi = ob / sum(ob, na.rm = TRUE),
      ob.mi = sum(ob, na.rm = TRUE)
    ) %>%
    ungroup()
  
  ###part II
  micro_df_II <- base_filt %>%
    filter(GBD %in% causa_II) %>%
    group_by(micro, meso, GBD, idade, ano, sexo, uf) %>%
    summarise(ob = sum(.data[[obito_in]], na.rm = TRUE), .groups="drop_last") %>%
    group_by(micro, meso, idade, ano, sexo, uf) %>%
    mutate(
      pr.mi = ob / sum(ob, na.rm = TRUE),
      ob.mi = sum(ob, na.rm = TRUE)
    ) %>%
    ungroup()
  
  micro_df <- rbind(micro_df, micro_df_II)
  rm(micro_df_II)
  
  # MESORREGIÃO----------------
  meso_df <- base_filt %>%
    filter(GBD %in% causa) %>%
    group_by(meso, GBD, idade, ano, sexo, uf) %>%
    summarise(ob = sum(.data[[obito_in]], na.rm = TRUE), .groups="drop_last") %>%
    group_by(meso, idade, ano, sexo, uf) %>%
    mutate(
      pr.me = ob / sum(ob, na.rm = TRUE),
      ob.me = sum(ob, na.rm = TRUE)
    ) %>%
    ungroup()
  
  ###part II
  meso_df_II <- base_filt %>%
    filter(GBD %in% causa_II) %>%
    group_by(meso, GBD, idade, ano, sexo, uf) %>%
    summarise(ob = sum(.data[[obito_in]], na.rm = TRUE), .groups="drop_last") %>%
    group_by(meso, idade, ano, sexo, uf) %>%
    mutate(
      pr.me = ob / sum(ob, na.rm = TRUE),
      ob.me = sum(ob, na.rm = TRUE)
    ) %>%
    ungroup()
  
  meso_df <- rbind(meso_df, meso_df_II)
  rm(meso_df_II)
  
  # UF--------------
  uf_df <- base_filt %>%
    filter(GBD %in% causa) %>%
    group_by(GBD, idade, ano, sexo, uf) %>%
    summarise(ob = sum(.data[[obito_in]], na.rm = TRUE), .groups="drop_last") %>%
    group_by(idade, ano, sexo, uf) %>%
    mutate(
      pr.uf = ob / sum(ob, na.rm = TRUE),
      ob.uf = sum(ob, na.rm = TRUE)
    ) %>%
    ungroup()
  
  ###part II
  uf_df_II <- base_filt %>%
    filter(GBD %in% causa_II) %>%
    group_by(GBD, idade, ano, sexo, uf) %>%
    summarise(ob = sum(.data[[obito_in]], na.rm = TRUE), .groups="drop_last") %>%
    group_by(idade, ano, sexo, uf) %>%
    mutate(
      pr.uf = ob / sum(ob, na.rm = TRUE),
      ob.uf = sum(ob, na.rm = TRUE)
    ) %>%
    ungroup()
  
  uf_df <- rbind(uf_df, uf_df_II)
  rm(uf_df_II)
  
  # REGIÃO------------
  reg_df <- base_filt %>%
    filter(GBD %in% causa) %>%
    group_by(GBD, idade, ano, sexo, reg) %>%
    summarise(ob = sum(.data[[obito_in]], na.rm = TRUE), .groups="drop_last") %>%
    group_by(idade, ano, sexo, reg) %>%
    mutate(
      pr.rg = ob / sum(ob, na.rm = TRUE),
      ob.rg = sum(ob, na.rm = TRUE)
    ) %>%
    ungroup()
  
  ###part II
  reg_df_II <- base_filt %>%
    filter(GBD %in% causa_II) %>%
    group_by(GBD, idade, ano, sexo, reg) %>%
    summarise(ob = sum(.data[[obito_in]], na.rm = TRUE), .groups="drop_last") %>%
    group_by(idade, ano, sexo, reg) %>%
    mutate(
      pr.rg = ob / sum(ob, na.rm = TRUE),
      ob.rg = sum(ob, na.rm = TRUE)
    ) %>%
    ungroup()
  
  reg_df <- rbind(reg_df, reg_df_II)
  rm(reg_df_II)
  
  # JUNTAR PROPORÇÕES E PESOS--------------
  base_alt <- base_filt %>%
    left_join(muni, by=c("cdmun","micro","meso","GBD","idade","ano","sexo","uf")) %>%
    left_join(micro_df, by=c("micro","meso","GBD","idade","ano","sexo","uf")) %>%
    left_join(meso_df, by=c("meso","GBD","idade","ano","sexo","uf")) %>%
    left_join(uf_df, by=c("GBD","idade","ano","sexo","uf")) %>%
    left_join(reg_df, by=c("GBD","idade","ano","sexo","reg")) %>%
    left_join(data_weight, by=c("GBD"="target"),relationship = "many-to-many") %>%
    mutate(redis = if_else(!is.na(weight), redis*weight, redis))
  
  #================ REDISTRIBUIÇÃO =================#
  p1 <- paste0(prefix,".1")
  p2 <- paste0(prefix,".2")
  p3 <- paste0(prefix,".3")
  p4 <- paste0(prefix,".4")
  p5 <- paste0(prefix,".5")
  
  base_alt <- base_alt %>%
    mutate(
      !!p1 := if_else(GBD %in% fixed_weight, redis, redis*pr.mu),
      redis.2 = if_else(is.na(.data[[p1]]) | (.data[[p1]] == 0 & ob.mu == 0), redis, NA_real_),
      
      !!p2 := if_else(GBD %in% fixed_weight, redis.2, redis.2*pr.mi),
      redis.3 = if_else(is.na(.data[[p2]]) & ob.mi == 0, redis.2, NA_real_),
      
      !!p3 := if_else(GBD %in% fixed_weight, redis.3, redis.3*pr.me),
      redis.4 = if_else(is.na(.data[[p3]]) & ob.me == 0,redis.3,NA_real_),
      
      !!p4 := if_else(GBD %in% fixed_weight, redis.4, redis.4*pr.uf),
      redis.5 = if_else(is.na(.data[[p4]]) & ob.uf == 0, redis.4, NA_real_),
      
      !!p5 := if_else(GBD %in% fixed_weight, redis.5, redis.5*pr.rg),
      
      !!obito_out :=
        if_else(!is.na(.data[[p1]]), .data[[obito_in]] + .data[[p1]],
                if_else(!is.na(.data[[p2]]), .data[[obito_in]] + .data[[p2]],
                        if_else(!is.na(.data[[p3]]), .data[[obito_in]] + .data[[p3]],
                                if_else(!is.na(.data[[p4]]), .data[[obito_in]] + .data[[p4]],
                                        if_else(!is.na(.data[[p5]]), .data[[obito_in]] + .data[[p5]],
                                                .data[[obito_in]])))))
    )
  
  return(base_alt)
}
