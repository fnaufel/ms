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
  
  premios = ler_premios(),


# Jogar dezenas raras/frequentes ------------------------------------------

  # Dezenas em ordem crescente de freq. acum. até o concurso anterior
  df_raras = criar_df_raras(df_vetor),

  # Mais raras
  n_raras = 8,
  dezenas_raras = escolher_dezenas(df_raras, n_dezenas = n_raras, desc=FALSE),
  # Quantidade de acertos em cada concurso, jogando as dezenas escolhidas acima
  df_sucessos_raras = computar_acertos(df_vetor, dezenas_raras),

  # Mais frequentes
  n_freqs = 8,
  dezenas_freqs = escolher_dezenas(df_raras, n_dezenas = n_freqs, desc=TRUE),
  # Quantidade de acertos em cada concurso, jogando as dezenas escolhidas acima
  df_sucessos_freqs = computar_acertos(df_vetor, dezenas_freqs),


# Regressões --------------------------------------------------------------

  # Número de concursos recentes para usar nas regressões
  # Um bom valor é 300. Mais que isto, a inflação atrapalha a estimativa
  # da arrecadação
  n_recentes = 300,

  # df para usar nas regressões
  df_regress = criar_df_regress(df_vetor, n_recentes),

  # Separar em treino e teste
  data_split = initial_time_split(df_regress, prop = 5/6),
  df_treino = training(data_split),
  df_teste = testing(data_split),


  ## Regressão para estimar arrecadação -----------------------------------

  arrec_resultados = regredir_arrec(df_regress, df_treino, df_teste),
  # Calcular r^2 para as estimativas da CEF
  rsq_CEF = computar_rsq_CEF(df_regress),


  ## Regressão para estimar ganhadores da sena ----------------------------
  sena_resultados = regredir_sena(df_regress, df_treino, df_teste),


  ## Regressão para estimar ganhadores da quina ---------------------------
  quina_resultados = regredir_quina(df_regress, df_treino, df_teste),
  
  
  ## Regressão para estimar ganhadores da quadra --------------------------
  quadra_resultados = regredir_quadra(df_regress, df_treino, df_teste),


# Previsões ---------------------------------------------------------------

  # Função para calcular arrecadação, ganhadores e rateios e retornar df
  previsao = funcao_previsao(
    arrec_resultados, 
    quina_resultados, 
    quadra_resultados
  ),


# Relatórios --------------------------------------------------------------

  # Relatório final
  final = target(
    command = {
      rmarkdown::render(knitr_in("doc/final.Rmd"))
      file_out("doc/final.html")
    }
  )


)
