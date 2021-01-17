the_plan <- drake_plan(

  # Ler e limpar
  df = ler_resultados(file_in('Dados/resultados.html')),
  
  # Gerar resumo
  df_summary = sumario(df),
  
  resumo = target(
    command = {
      rmarkdown::render(knitr_in("doc/resumo.Rmd"))
      file_out("doc/resumo.html")
    }
  ),
  
  # Juntar colunas d1:d6 em uma só
  # Bom para computar número de acertos em simulações
  df_vetor = criar_col_resultado(df),
  
  # Dezenas em ordem decrescente de freq. acum. até o concurso anterior
  df_raras = criar_df_raras(df_vetor),
  n_raras = 8,
  dezenas_raras = escolher_dezenas(df_raras, n_dezenas = n_raras),
  # Quantidade de acertos em cada concurso, jogando as dezenas escolhidas acima
  df_sucessos_raras = computar_acertos(df_vetor, dezenas_raras),
  
  # Jogar as mesmas 6 dezenas (escolhidas ao acaso) em todos os jogos
  mesmas_dezenas = tibble(
    num = 1:nrow(df_vetor),
    dezenas = list(sample(1:60, n_raras))
  ),
  # Quantidade de acertos em cada concurso, jogando as dezenas escolhidas acima
  df_sucessos_mesmas = computar_acertos(df_vetor, mesmas_dezenas),
  
  # Quantidade de ganhadores da quina e da quadra de acordo com a arrecadação
  df_ganhadores = construir_df_ganhadores(df_vetor),
  ano_atual = Sys.Date() %>% lubridate::ymd(),
  n_anos    = 2,
  inicio    = ano_atual - lubridate::years(n_anos),
  min_arrec = 300e6,
  
  plot_ganhadores_quadra = plot_ganhadores(
    df_ganhadores, 
    inicio, 
    min_arrec, 
    'quadra'
  ),

  plot_ganhadores_quina = plot_ganhadores(
    df_ganhadores, 
    inicio, 
    min_arrec, 
    'quina'
  ),
  
  lm_quina = lm(
    g_quina ~ arrec, 
    data = df_ganhadores %>% 
      filter(data >= inicio & arrec < min_arrec)
  ),
  
  lm_quadra = lm(
    g_quadra ~ arrec, 
    data = df_ganhadores %>% 
      filter(data >= inicio & arrec < min_arrec)
  ),
  
  # Análise exploratória
  eda = target(
    command = {
      rmarkdown::render(knitr_in("doc/eda.Rmd"))
      file_out("doc/eda.html")
    }
  )


)
