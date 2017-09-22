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
grps <- tools::file_path_sans_ext(basename(dirs))

if (length(dirs) == 0){
  stop(paste0("No directories in the given directory \"",arguments,"\""))
}

# > cdn <- read_csv("~/Dropbox (ASU)/Lake_Doug_Valley_Fever/CDN.csv")
# > cdn[1,]

mut_add_contingency <- function(df, fname){
  fname <- enquo(fname)
  sname <- quo_name(fname)
  mutate(df, !!sname := 1)
}

load_sample <- function(filename){
  sam <- tools::file_path_sans_ext(basename(filename))
  read_csv(filename) %>%
    select(1) %>% #take just first column
    rename_at(1,~"mz") %>% # rename column to mz
    mutate(!!sam := 1L) #!! and := mean use the value of filename as the new variable name
}

load_csvs <- function(foldername){
  list.files(foldername, full.names = TRUE, pattern = "*.csv") %>%
    map(~load_sample(.)) %>%
    plyr::join_all(by = "mz", type = "full") %>%
    mutate_all(~coalesce(., 0L))
}


df <- map(dirs, ~load_csvs(.)) %>%
  plyr::join_all(by = "mz", type = "full") %>%
  mutate_all(~coalesce(., 0L))

df %>% filter(test1 == 1)

sample_table <- map(d)

as.tbl(map(dirs, ~list.files(.)))

res <- fisher.test(df)


