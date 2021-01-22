##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param df_regress
##' @return
##' @author fnaufel
##' @export
computar_rsq_CEF <- function(df_regress) {

  df_regress %>% 
    metrics(truth = arrec, estimate = est_arrec) %>% 
    filter(.metric == 'rsq') %>% 
    pull(.estimate)
    
}
