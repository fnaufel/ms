##' Juntar colunas d1:d6 em uma coluna resultado
##'
##' @title
##' @param df
##' @return df_vetor
##' @author fnaufel
##' @export
criar_col_resultado <- function(df) {

  df %>% 
    rowwise() %>% 
    mutate(
      resultado = list(c_across(d1:d6)),
      .keep = 'unused'
    ) %>% 
    ungroup()

}
