##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param df_regress
##' @param df_treino
##' @param df_teste
##' @return
##' @author fnaufel
##' @export
regredir_quina <- function(df_regress, df_treino, df_teste) {

  df_tudo <- df_regress

  receita <- df_tudo %>%
    select(data, g_quina, est_arrec) %>% 
    recipe(g_quina ~ .) %>% 
    update_role(data, new_role = 'ID')
  
  modelo <- linear_reg() %>% 
    set_engine('lm')

  wf <- workflows::workflow() %>% 
    add_model(modelo) %>% 
    add_recipe(receita)

  # Treinar
  resultados_treino <- wf %>% 
    fit(data = df_treino)

  # Validar e criar df com as previsões
  coluna_pred <- resultados_treino %>% 
    predict(new_data = df_teste) 

  colunas_intervalo <- resultados_treino %>% 
    predict(new_data = df_teste, type = 'pred_int') 

  df_pred_quina <- bind_cols(
    coluna_pred,
    colunas_intervalo,
    df_teste 
  )
  
  # Treinar com o df inteiro
  resultados_tudo <- wf %>% 
    fit(data = df_tudo)
  
  list(
    validacao  = df_pred_quina,
    modelo     = resultados_tudo
  )

}
