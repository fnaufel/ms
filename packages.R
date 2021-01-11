## library() calls go here
library(conflicted)
library(dotenv)
library(drake)

library(tidyverse)
library(rvest)
library(janitor)

library(summarytools)
st_options(
  plain.ascii = FALSE,
  dfSummary.varnumbers = FALSE,
  dfSummary.style = 'grid',
  dfSummary.graph.magnif = .75
)

library(rmarkdown)

conflict_prefer("filter", "dplyr")
