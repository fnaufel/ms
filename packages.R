## library() calls go here
library(conflicted)
library(dotenv)
library(drake)

library(tidyverse)
library(lubridate)
library(tidymodels)
library(glue)
library(gglm)
library(rvest)
library(janitor)
library(kableExtra)

library(summarytools)
st_options(
  plain.ascii = FALSE,
  dfSummary.varnumbers = FALSE,
  dfSummary.style = 'grid',
  dfSummary.graph.magnif = .75
)

library(scales)

library(rmarkdown)

# Avoid scientific notation and use a comma as decimal separator
options(
  scipen = 30,
  OutDec = ','
)

conflict_prefer("filter", "dplyr")
conflict_prefer("view", "tibble")
conflict_prefer("lag", "dplyr")
