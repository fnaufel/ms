##' Criar df para fazer as regressões
##'
##' @param df_vetor
##' @param n_recentes
##' @return
##' @author fnaufel
##' @export
criar_df_regress <- function(df_vetor, n_recentes) {

  df_vetor %>% 
    # Criar lag da estimativa de arrecadação 
    mutate(
      est_arrec = lag(est_prox),
      .keep = 'unused'
    ) %>% 
    # A partir de `num` = 869, `est_prox` está preenchida
    # A partir de `num` = 1077, `arrec` está preenchida
    # Usar concursos a partir do ano de 2010 (concurso 1141).
    filter(num >= 1141) %>% 
    # Mudar unidade das arrecadações para milhões de reais
    mutate(
      arrec = (arrec / 1e6) %>% round(3),
      est_arrec = (est_arrec / 1e6) %>% round(3)
    ) %>% 
    # Retirar os concursos da virada, que têm valores atípicos
    filter(
      !(month(data) == 12 & day(data) == 31)
    ) %>%
    # Tomar apenas os `n_recentes` concursos
    slice_tail(n = n_recentes) %>% 
    # Escolher colunas
    select(num, data, arrec, est_arrec, starts_with(c('g_', 'r_')))

}
