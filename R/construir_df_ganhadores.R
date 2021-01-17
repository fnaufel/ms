##'
##' @title
##' @param df_vetor
##' @return
##' @author fnaufel
##' @export
construir_df_ganhadores <- function(df_vetor) {

  df_vetor %>% 
    # Coluna arrecadação só é preenchida a partir do concurso 1077
    filter(num >= 1077) %>% 
    select(
      num,
      data,
      arrec,
      starts_with('g_'),
      acumulou
    ) 
  
}
