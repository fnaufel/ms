##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param arrec_resultados
##' @param quina_resultados
##' @param quadra_resultados
##' @return
##' @author fnaufel
##' @export
funcao_previsao <- function(arrec_resultados, quina_resultados,
                            quadra_resultados) {

  prop_quina <- 6e4
  prop_quadra <- 8e4
  
  modelo_arrec <- arrec_resultados$modelo %>% pull_workflow_fit()
  b0_arrec <- modelo_arrec$fit$coefficients[1]
  b1_arrec <- modelo_arrec$fit$coefficients[2]

  modelo_quina <- quina_resultados$modelo %>% pull_workflow_fit()
  b0_quina <- modelo_quina$fit$coefficients[1]
  b1_quina <- modelo_quina$fit$coefficients[2]
  
  modelo_quadra <- quadra_resultados$modelo %>% pull_workflow_fit()
  b0_quadra <- modelo_quadra$fit$coefficients[1]
  b1_quadra <- modelo_quadra$fit$coefficients[2]
  
  f <- function(num, est_arrec) {
    
    tibble(
      num           = num,
      .estimativaCEF = est_arrec,
      .arrec         = b0_arrec + b1_arrec * est_arrec,
      
      .g_quina       = (b0_quina + b1_quina * est_arrec) %>% round(0),
      .total_quina   = .arrec * prop_quina,
      .r_quina       = (.total_quina / .g_quina) %>% round(2),
      
      .g_quadra      = (b0_quadra + b1_quadra * est_arrec) %>% round(0),
      .total_quadra  = .arrec * prop_quadra,
      .r_quadra      = (.total_quadra / .g_quadra) %>% round(2)
    )
    
  }
  
  f

}
