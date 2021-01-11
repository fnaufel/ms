the_plan <- drake_plan(

  df = ler_resultados(file_in('Dados/resultados.html')),
  
  df_summary = sumario(df),
  
  resumo = target(
    command = {
      rmarkdown::render(knitr_in("doc/resumo.Rmd"))
      file_out("doc/resumo.html")
    }
  ),
  
  eda = target(
    command = {
      rmarkdown::render(knitr_in("doc/eda.Rmd"))
      file_out("doc/eda.html")
    }
  )


)
