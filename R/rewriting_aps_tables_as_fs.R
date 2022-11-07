library(haven)
library(fst)
library(data.table)
library(tidyverse)

dir('data', ".*dta$", recursive = TRUE) |> 
  str_subset("UKDA") |> 
  walk(function(file) {
    filename <- str_extract(file, "(?<=_)a.*(?=_)")
    df <- read_dta(file.path("data", file))
    write_fst(df, paste0("data/fst_files/", filename, ".fst"))
    gc()
  })


dir('data/fst_files') |> 
  walk(function(file) {
    df <- read_fst(file.path('data', 'fst_files', file))
    df <- janitor::clean_names(df)
    y1 <- as.numeric(str_extract(file, "(?<=\\w{1,3})\\d{2}"))
    write_fst(df, glue::glue('data/fst_files/apr{y1}_mar{y1+1}.fst'))
  })