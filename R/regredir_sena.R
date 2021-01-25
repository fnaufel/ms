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
regredir_sena <- function(df_regress, df_treino, df_teste) {

  df_tudo <- df_regress

  receita <- df_tudo %>%
    select(data, g_sena, est_arrec) %>% 
    recipe(g_sena ~ .) %>% 
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
  # ATENÇÃO: diferente da quadra e da quina, aqui eu uso o df_tudo.
  # O objetivo é mostrar que o modelo é inútil, mesmo considerando *todos*
  # os concursos recentes.
  coluna_pred <- resultados_treino %>% 
    predict(new_data = df_tudo) 

  colunas_intervalo <- resultados_treino %>% 
    predict(new_data = df_tudo, type = 'pred_int') 

  df_pred_sena <- bind_cols(
    coluna_pred,
    colunas_intervalo,
    df_tudo
  )
  
  # Treinar com o df inteiro
  resultados_tudo <- wf %>% 
    fit(data = df_tudo)
  
  list(
    validacao  = df_pred_sena,
    modelo     = resultados_tudo
  )

}
