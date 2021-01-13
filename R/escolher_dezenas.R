##' Construir df com uma linha por concurso, com os primeiros n_dezenas
##' elementos do vetor dezenas 
##' 
##'
##' @param df_raras Data frame com uma linha por concurso, com um vetor na coluna dezenas
##' @param n_dezenas Quantidade de dezenas a escolher por concurso
##' @return Data frame com uma linha por concurso, com um vetor de n_dezenas elementos na coluna dezenas 
##' @author fnaufel
##' @export
escolher_dezenas <- function(df_raras, n_dezenas = 8) {

  df_raras %>% 
    mutate(
      dezenas = map(dezenas, `[`, 1:n_dezenas)
    )
  
}
