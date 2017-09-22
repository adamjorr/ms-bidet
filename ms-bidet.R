#!/usr/bin/env Rscript
# Rscript ms-bidet.R [some_dir/]

suppressPackageStartupMessages(library('plyr','tidyverse'))

arguments <- commandArgs(trailingOnly = TRUE)

if (length(arguments) > 1){
  stop("Too many arguments given. USAGE: Rscript ms-bidet.R [some_dir/]")
}

if (length(arguments) == 0){
  arguments <- "."
}

files <- list.files(arguments[1], full.names = TRUE)
dirs <- files[file_test('-d', files)]

if (length(dirs) == 0){
  stop(paste0("No directories in the given directory \"",arguments,"\""))
}

# > cdn <- read_csv("~/Dropbox (ASU)/Lake_Doug_Valley_Fever/CDN.csv")
# > cdn[1,]

load_csvs <- function(foldername){
  list.files(foldername, full.names = TRUE, pattern = "*.csv") %>%
    map(~read_csv(.)) %>%
    bind_rows() %>%
    select(1) %>% #take just first column
    rename_at(1, ~"mz") %>% #rename first column mz
    group_by(mz) %>%
    tally() %>%
    rename_at(2, ~basename(foldername)) #rename col with tally as name of folder
}

df <- map(dirs, ~load_csvs(.)) %>%
  join_all(by = "mz", type = "full") %>%
  mutate_all(~coalesce(., 0L))

res <- fisher.test(df)


