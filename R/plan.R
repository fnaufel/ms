the_plan <- drake_plan(

# Limpeza e resumo --------------------------------------------------------

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
  

# Jogar dezenas raras/frequentes ------------------------------------------

  # Dezenas em ordem crescente de freq. acum. até o concurso anterior
  df_raras = criar_df_raras(df_vetor),

  # Mais raras
  n_raras = 8,
  dezenas_raras = escolher_dezenas(df_raras, n_dezenas = n_raras, desc=FALSE),
  # Quantidade de acertos em cada concurso, jogando as dezenas escolhidas acima
  df_sucessos_raras = computar_acertos(df_vetor, dezenas_raras),

  # Mais frquentes
  n_freqs = 8,
  dezenas_freqs = escolher_dezenas(df_raras, n_dezenas = n_freqs, desc=TRUE),
  # Quantidade de acertos em cada concurso, jogando as dezenas escolhidas acima
  df_sucessos_freqs = computar_acertos(df_vetor, dezenas_freqs),


  

# Relatórios --------------------------------------------------------------

  # Análise exploratória
  eda = target(
    command = {
      rmarkdown::render(knitr_in("doc/eda.Rmd"))
      file_out("doc/eda.html")
    }
  ),

  # Relatório final
  final = target(
    command = {
      rmarkdown::render(knitr_in("doc/final.Rmd"))
      file_out("doc/final.html")
    }
  )


)
