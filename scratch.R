


# Comportamento da arrecadação

Os dados só contêm o valor da arrecadação total sem interrupção a partir do concurso $1077$, de maio de $2009$.

```{r echo=FALSE}
df_arrec <- df %>% 
  select(num, data, arrec) %>% 
  filter(num >= 1077)
```

```{r echo=FALSE}
df_arrec %>% 
  ggplot(aes(data, arrec)) +
    geom_line() +
    scale_x_date(
      date_breaks = '1 year',
      date_labels = '%Y'
    ) +
    scale_y_continuous(
      labels = scales::label_dollar(
        scale = 1e-6,
        suffix = 'M',
        big.mark = '.',
        decimal.mark = ','
      )
    ) +
    labs(
      title = 'Arrecadação total ao longo dos anos',
      x = NULL,
      y = NULL
    )
```

Por ano:

```{r echo=FALSE}
yearly_plot <- function(this_year, df) {
  
  p <- df %>% 
    filter(lubridate::year(data) == this_year) %>% 
    ggplot(aes(data, arrec)) +
      geom_line() +
      scale_x_date(
        date_breaks = '1 month',
        date_labels = '%b'
      ) +
      scale_y_continuous(
        limits = c(0, 1200e6),
        labels = scales::label_dollar(
          scale = 1e-6,
          suffix = 'M',
          big.mark = '.',
          decimal.mark = ','
        )
      ) +
      labs(
        title = glue::glue('\n\nArrecadação total em {this_year}'),
        x = NULL,
        y = NULL
      )
  
  print(p)

}

anos <- 2009:2020

walk(anos, yearly_plot, df)
```

Observações:

* Em $2020$, a arrecadação máxima (na virada) foi mais do que o dobro da arrecadação máxima de $2009$.

* Houve picos em novembro de $2015$ e em maio de $2019$.

* Em $25/11/2015$, o prêmio estava acumulado por $10$ concursos seguidos.

* Em $11/05/2019$, o prêmio estava acumulado por $14$ concursos seguidos.


# Sequências de prêmios acumulados

```{r echo=FALSE}
rl <- rle(df$acumulou)
n_acs <- map(rl$lengths, ~c(rep(0, .x - 1), .x)) %>% unlist()

df %>% 
  mutate(acs = n_acs) %>% 
  filter(acumulou == TRUE & arrec > 0) %>% 
  select(num, data, acs, arrec) %>% 
  arrange(desc(acs)) %>% 
  head(10)
```






# Cálculo do valor de cada prêmio -----------------------------------------

loadd(df_vetor)

# Não consigo conciliar as informações do site com os valores.
# Vamos ver: quais porcentagens da arrecadação vão para a sena, quina e quadra?

percentuais <- df_vetor %>% 
  filter(arrec > 0) %>% 
  transmute(
    num = num,
    data = data,
    prop_sena   = round(100 * (r_sena * g_sena) / arrec    , 2),
    prop_quina  = round(100 * (r_quina * g_quina) / arrec  , 2),
    prop_quadra = round(100 * (r_quadra * g_quadra) / arrec, 2)
  )

percentuais %>% 
  filter(lubridate::year(data) == 2020) %>% 
  count(prop_quadra)

percentuais %>% 
  filter(lubridate::year(data) == 2020) %>% 
  count(prop_quina)
  
percentuais %>% 
  filter(lubridate::year(data) == 2020 & prop_sena > 0) %>% 
  count(prop_sena)


# Na virada ---------------------------------------------------------------
virada <- df_vetor %>% 
  filter(num == 2330) %>% 
  select(num, data, arrec, starts_with(c('g_', 'r_'))) %>% 
  mutate(
    prop_sena = (r_sena * g_sena) / arrec,
    prop_quina = (r_quina * g_quina) / arrec,
    prop_quadra = (r_quadra * g_quadra) / arrec
  ) %>% 
  select(
    arrec,
    prop_sena, g_sena, r_sena,
    prop_quina, g_quina, r_quina,
    prop_quadra, g_quadra, r_quadra
  )

virada

# O prêmio bruto corresponde a 43,35% da arrecadação. 
bruto <- virada$arrec * .4335       # 509.674.210

# Dessa porcentagem:

# 35% são distribuídos entre os acertadores dos 6 números sorteados (Sena);
bruto * .35                         # 178.385.974
virada$r_sena * virada$g_sena       # 325.250.216

# 19% entre os acertadores de 5 números (Quina);
bruto * .19                         #  96.838.100
virada$r_quina * virada$g_quina     #  67.786.673

# 19% entre os acertadores de 4 números (Quadra);
bruto * .19                         #  96.838.100
virada$r_quadra * virada$g_quadra   #  96.837.740

# 22% ficam acumulados e são distribuídos aos acertadores dos 6 números nos concursos de final 0 ou 5.
0

# 5% ficam acumulados para a primeira faixa - sena - do último concurso do ano de final 0 ou 5 (Mega da Virada).
0



# Concurso 2329 -----------------------------------------------------------

pen <- df_vetor %>% 
  filter(num == 2329) %>% 
  select(num, data, arrec, starts_with(c('g_', 'r_'))) %>% 
  mutate(
    prop_sena = (r_sena * g_sena) / arrec,
    prop_quina = (r_quina * g_quina) / arrec,
    prop_quadra = (r_quadra * g_quadra) / arrec
  ) %>% 
  select(
    arrec,
    prop_sena, g_sena, r_sena,
    prop_quina, g_quina, r_quina,
    prop_quadra, g_quadra, r_quadra
  )

pen

# O prêmio bruto corresponde a 43,35% da arrecadação. 
bruto <- pen$arrec 

# Dessa porcentagem:

# 35% são distribuídos entre os acertadores dos 6 números sorteados (Sena);
bruto * .35                         # 10.627.075
pen$r_sena * pen$g_sena             # 53.597.505

# 19% entre os acertadores de 5 números (Quina);
bruto * .19                         #  96.838.100
pen$r_quina * pen$g_quina     #  67.786.673

# 19% entre os acertadores de 4 números (Quadra);
bruto * .19                         #  96.838.100
pen$r_quadra * pen$g_quadra   #  96.837.740

# 22% ficam acumulados e são distribuídos aos acertadores dos 6 números nos concursos de final 0 ou 5.
0

# 5% ficam acumulados para a primeira faixa - sena - do último concurso do ano de final 0 ou 5 (Mega da Virada).
0



# List column from several columns ----------------------------------------


tb <- tibble(
  c1 = c(1, 2, 3),
  c2 = c(4, 5, 6),
  c3 = c(7, 8, 9)
)

tb2 <- tb %>% 
  rowwise() %>% 
  mutate(
    v = list(c_across(c1:c3))
  ) %>% 
  ungroup()

tb2 %>% view()


# 
# 
# # Primeira tentativa ------------------------------------------------------
# 
# source('packages.R')
# 
# nome <- 'Dados/resultados.html'
# 
# lido <- read_html(nome, encoding = 'UTF-8')
#   
# linhas <- lido %>% 
#     html_nodes('body > table > tbody > tr:not([bgcolor])')
# 
# ver_linha <- function(linha) {
#   
#   cols <- linha %>% 
#     html_nodes('td') %>% 
#     html_text(trim = TRUE)
#   
#   cols
# 
# }
# 
# tr <- map(linhas, ver_linha)
# 
# df <- bind_rows(
#   tr %>% 
#   map(as_tibble_row, .name_repair = 'unique')
# )
# 
# 
# # Tentando com o novo arquivo html ----------------------------------------
# 
# source('packages.R')
# 
# nome <- 'Dados/d_megasc.htm'
# 
# lido <- read_html(nome, encoding = 'UTF-8')
# 
# df <- lido %>% 
#   rvest::html_table(
#     trim = TRUE,
#     fill = TRUE,
#     dec = ','
#   )
# 
# df[[1]] %>% view()
# 
# 
# # Com funções -------------------------------------------------------------
# 
# source('packages.R')
# source('R/ler_resultados.R')
# 
# nome <- 'Dados/d_megasc.htm'
# df <- ler_resultados(nome)
# 
# df %>% view()

