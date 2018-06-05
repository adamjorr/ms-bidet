#!/usr/bin/Rscript
# Rscript ms-bidet.R [some_dir/]
# one sample should be named control

suppressPackageStartupMessages(require('plyr'))
suppressPackageStartupMessages(require('tidyverse'))

arguments <- commandArgs(trailingOnly = TRUE)

if (length(arguments) > 1){
  stop("Too many arguments given. USAGE: Rscript ms-bidet.R [some_dir/]")
}

if (length(arguments) == 0){
  arguments <- "."
}

get_files <- function(dir){
  list.files(dir, full.names = TRUE)
}

# ---- get list of files and save directories and groups ----
files <- get_files(arguments[1])
dirs <- files[file_test('-d', files)]
grps <- tools::file_path_sans_ext(basename(dirs))

if (length(dirs) == 0){
  stop(paste0("No directories in the given directory \"",arguments,"\""))
}

# ---- short form, again ----
load_sample <- function(filename){
  cat(filename, "\n")
  sam <- tools::file_path_sans_ext(basename(filename))
  suppressMessages(read_csv(filename)) %>%
    select(contains("m/z")) %>% #take the column that contains "m/z"
    rename_at(1,~sam) %>% # rename column to mz
    distinct() %>% #get rid of duplicated rows
    na.omit() #get rid of any rows with NAs
}

load_csvs <- function(foldername){
  grpname <- tools::file_path_sans_ext(basename(foldername))
  list.files(foldername, full.names = TRUE, pattern = "*.csv") %>% #load all csvs
    map(~load_sample(.)) %>% #load and prefilter them
    bind_rows() %>% #concatenate them
    mutate(grp = grpname) %>% #add the groupname as a variable
    mutate(val = 1L) #add column indicating presence of m/z peak
}

add_contingencies <- function(charges, newdata){
  cname <- names(newdata)[1]
  charges <- charges %>%
    transmute(!!cname := mz %in% newdata[[1]])
}

sams <- map(dirs, list.files, recursive = T) %>%
  map(tools::file_path_sans_ext) %>%
  map(basename)
names(sams) <- grps

alldata <- list.files(dirs, full.names = TRUE, pattern = '*.csv', recursive = TRUE) %>%
  map(~load_sample(.))

charges <- alldata %>%
  unlist() %>%
  unique()

df <- tibble(mz = charges)

df <- alldata %>%
  map(~add_contingencies(df, .)) %>%
  c(list(df),.) %>%
  bind_cols()

create_counts <- function(frame, sams, groupname){
  frame %>%
    select(sams[[groupname]]) %>%
    transmute(present = unlist(pmap(.,sum)), absent = length(sams[[groupname]]) - present)
}

counts <- grps[grps != 'control'] %>%
  map(~create_counts(df,sams, .))
names(counts) <- grps[grps != 'control']

pvals <- map(seq_along(charges),
             ~fisher.test(
                bind_rows(counts[['positive']][.,],counts[['negative']][.,]),
                alternative = 'g')$p) %>%
  unlist()
qvals <- p.adjust(pvals)

out <- df %>% mutate(p = pvals, q = qvals) %>%
  select(mz, p , q, everything()) %>%
  arrange(p)

longgroups <- unlist(map(grps,~rep(.,length(sams[[.]]))))
names(longgroups) <- unlist(sams)

outwithlabel <- out %>%
  transmute_all(funs(as.character)) %>%
  bind_rows(longgroups,.) %>%
  select(mz, p, q, everything())

writeLines(format_csv(outwithlabel), 'full_out.csv')

if("negative" %in% names(sams)){
  shortout <- out %>%
    filter_at(sams$negative, all_vars(. == F))

  shortoutlabeled <- shortout %>%
    transmute_all(funs(as.character)) %>%
    bind_rows(longgroups, .) %>%
    select(mz,p,q, everything())

  writeLines(format_csv(shortoutlabeled), 'short_out.csv')
}

# shortout <- out %>%
#   filter_at(sams$negative, all_vars(. == F))

# shortoutlabeled <- shortout %>%
#   transmute_all(funs(as.character)) %>%
#   bind_rows(longgroups, .) %>%
#   select(mz,p,q, everything())



