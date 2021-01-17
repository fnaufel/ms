##'
##' @title
##' @param df_ganhadores
##' @param inicio
##' @param min_arrec
##' @param faixa
##' @return
##' @author fnaufel
##' @export
plot_ganhadores <- function(df_ganhadores, inicio, min_arrec, faixa) {

  faixa_var <- paste0('g_', faixa)
  
  df_ganhadores %>%
    filter(data >= inicio & arrec < min_arrec) %>% 
    ggplot(aes(arrec, .data[[faixa_var]] )) +
      geom_point(alpha = .5) +
      geom_smooth(method = 'lm', formula = y ~ x) +
      scale_x_continuous(
        labels = scales::label_dollar(
          scale = 1e-6,
          suffix = 'M',
          big.mark = '.',
          decimal.mark = ','
        )
      ) +
      labs(
        title = glue::glue('Quantidade de ganhadores da {faixa}'),
        subtitle = 'de acordo com a arrecadação',
        x = NULL,
        y = NULL
      )

}
