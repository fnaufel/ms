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
  df_vetor = criar_col_resultado(df),
  
  # Dezenas em ordem decrescente de freq. acum. até o concurso anterior
  df_raras = criar_df_raras(df_vetor),
  
  # Escolher n_dezenas em cada concurso
  n_raras = 8,
  dezenas_a_jogar = escolher_dezenas(df_raras, n_dezenas = n_raras),
  
  # Quantidade de acertos em cada concurso, jogando as dezenas escolhidas acima
  df_sucessos = computar_acertos(df_vetor, dezenas_a_jogar),
  
  # Análise exploratória
  eda = target(
    command = {
      rmarkdown::render(knitr_in("doc/eda.Rmd"))
      file_out("doc/eda.html")
    }
  )


)
