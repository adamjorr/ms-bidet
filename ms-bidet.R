#!/usr/bin/env Rscript
# Rscript ms-bidet.R [some_dir/]
# one sample should be named control

suppressPackageStartupMessages(library('plyr','tidyverse'))

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

files <- get_files(arguments[1])
dirs <- files[file_test('-d', files)]
grps <- tools::file_path_sans_ext(basename(dirs))

if (length(dirs) == 0){
  stop(paste0("No directories in the given directory \"",arguments,"\""))
}

# ---- long form ----
load_sample <- function(filename){
  sam <- tools::file_path_sans_ext(basename(filename))
  read_csv(filename) %>%
    select(1) %>% #take just first column
    rename_at(1,~"mz") %>% # rename column to mz
    distinct() %>%
    mutate(sam = sam) #!! and := mean use the value of filename as the new variable name
}

load_csvs <- function(foldername){
  grpname <- tools::file_path_sans_ext(basename(foldername))
  list.files(foldername, full.names = TRUE, pattern = "*.csv") %>%
    map(~load_sample(.)) %>%
    bind_rows() %>%
    mutate(grp = grpname) %>%
    mutate(val = 1L)
}

add_zeros <- function(df, charges, samp){
  charges %>%
    left_join(filter(df, sam == samp), by = "mz") %>%
    mutate(val = coalesce(.$val, 0L)) %>%
    mutate(sam = na.omit(.$sam)[1]) %>%
    mutate(grp = na.omit(.$grp)[1])
}

build_contingency <- function(mzd, df){
  m <- df %>%
    filter(mz == mzd) %>%
    select(-mz, -val)
  as.matrix(m)
}

control_sample <- "control"

df <- map(dirs, ~load_csvs(.)) %>%
  bind_rows()

controldf <- df %>%
  filter(sam == control_sample)

df <- df %>%
  filter(mz %in% controldf$mz)

charges <- select(df, mz) %>%
  distinct()

tables <- map(unique(df$sam), ~add_zeros(df, charges, .)) %>%
  bind_rows() %>%
  group_by(mz, grp) %>%
  summarise(present = sum(val == 1), absent = sum(val == 0)) %>%
  ungroup()

present <- tables %>%
  select(-absent) %>%
  spread(grp, present) %>%
  mutate(val = "present")
  
absent <- tables %>%
  select(-present) %>%
  spread(grp, absent) %>%
  mutate(val = "absent")

full_contingencies <- bind_rows(present, absent)

pvals <- unlist(map(charges$mz, ~fisher.test(build_contingency(.,full_contingencies))$p))
adjusted <- p.adjust(pvals)

out <- charges %>%
  mutate( p = pvals ) %>%
  mutate( q = adjusted)

writeLines(format_csv(out), stdout())

# --- short form ---
# deprecated

# > cdn <- read_csv("~/Dropbox (ASU)/Lake_Doug_Valley_Fever/CDN.csv")
# > cdn[1,]
# 
# get_samples <- function(directory){
#   tibble(fullpath = list.files(directory, full.names = T, pattern = "*.csv")) %>%
#     mutate(sample = tools::file_path_sans_ext(basename(fullpath))) %>%
#     mutate(dir = directory) %>%
#     mutate(grp = tools::file_path_sans_ext(basename(directory))) %>%
#     select(grp, dir, fullpath, sample)
# }
# 
# load_sample <- function(filename){
#   sam <- tools::file_path_sans_ext(basename(filename))
#   read_csv(filename) %>%
#     select(1) %>% #take just first column
#     rename_at(1,~"mz") %>% # rename column to mz
#     distinct() %>%
#     mutate(!!sam := 1L) #!! and := mean use the value of filename as the new variable name
# }
# 
# load_csvs <- function(foldername){
#   list.files(foldername, full.names = TRUE, pattern = "*.csv") %>%
#     map(~load_sample(.)) %>%
#     plyr::join_all(by = "mz", type = "full") %>%
#     mutate_all(~coalesce(., 0L))
# }
# 
# select_group <- function(df, sample_table, group){
#   enquo(group)
#   samples <- sample_table %>% filter(grp == group)
#   df %>% select(samples$sample)
# }
# 
# samples <- map(dirs, ~get_samples(.)) %>% bind_rows()
# 
# df <- map(dirs, ~load_csvs(.)) %>%
#   plyr::join_all(by = "mz", type = "full") %>%
#   mutate_all(~coalesce(., 0L))
# 
# df %>% filter(test1 == 1) %>%
#   select_group(samples,"grp1") %>%
#   gather("grp", "present") %>%
#   count(present)
# 





