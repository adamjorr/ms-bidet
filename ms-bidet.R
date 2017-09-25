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

# ---- long form data manipulation ----
#get a table from each file and do some filtering
load_sample <- function(filename){
  sam <- tools::file_path_sans_ext(basename(filename))
  suppressMessages(read_csv(filename)) %>%
    select(1) %>% #take just first column
    rename_at(1,~"mz") %>% # rename column to mz
    distinct() %>% #get rid of duplicated rows
    na.omit() %>% #get rid of any rows with NAs
    mutate(sam = sam) #!! and := mean use the value of filename as the new variable name
}

#load all the csvs in a folder as one group
load_csvs <- function(foldername){
  grpname <- tools::file_path_sans_ext(basename(foldername))
  list.files(foldername, full.names = TRUE, pattern = "*.csv") %>% #load all csvs
    map(~load_sample(.)) %>% #load and prefilter them
    bind_rows() %>% #concatenate them
    mutate(grp = grpname) %>% #add the groupname as a variable
    mutate(val = 1L) #add column indicating presence of m/z peak
}

#add a val = 0 row if a sample doesn't have a peak that's present in another sample
add_zeros <- function(df, charges, samp){
  charges %>% #charges should be all possible m/z peaks
    left_join(filter(df, sam == samp), by = "mz") %>% #left join with one sample to leave NAs where peak is absent
    mutate(val = coalesce(.$val, 0L)) %>% #change the NAs to 0's
    mutate(sam = na.omit(.$sam)[1]) %>% #change the samples to the first non-NA value in the group var. should be equal to samp.
    mutate(grp = na.omit(.$grp)[1]) #change the group values to the first non-NA value in group variable.
}

#look in the long form data (df) to calculate contingency table
build_contingency <- function(mzd, df){
  m <- df %>%
    filter(mz == mzd) %>% #only take correct peak
    select(-mz, -val) #only want counts in matrix
  as.matrix(m)
}

control_group <- "control"

#big table of raw data
df <- map(dirs, ~load_csvs(.)) %>%
  bind_rows()

#table just from control group
controldf <- df %>%
  filter(grp == control_group)

#remove rows in data that aren't in control group
df <- df %>%
  filter(mz %in% controldf$mz) %>%
  filter(grp != control_group)

#desired peaks possible in data
charges <- select(df, mz) %>%
  distinct()

#create table with all peaks and whether they are present for each sample
tables <- map(unique(df$sam), ~add_zeros(df, charges, .)) %>% #for every sample, add rows for peaks that are missing
  bind_rows() %>% #put them in one big table
  group_by(mz, grp) %>%
  summarise(present = sum(val == 1), absent = sum(val == 0)) %>% #count present and absent by peak and group
  ungroup() #ungroup table

#transpose so each group gets a column
present <- tables %>%
  select(-absent) %>%
  spread(grp, present) %>% #spread so each group is a column, values in table are counts of samples in group that have peak
  mutate(val = "present")
  
absent <- tables %>%
  select(-present) %>%
  spread(grp, absent) %>% #spread so each group is a column, values in table are counts of samples in group that don't have peak
  mutate(val = "absent")

#combine present + absent into one big table
full_contingencies <- bind_rows(present, absent)

#do a fisher test for each peak
pvals <- unlist(map(charges$mz, ~fisher.test(build_contingency(.,full_contingencies))$p))
adjusted <- p.adjust(pvals) #adjust p-values for multiple tests

out <- charges %>%
  mutate( p = pvals ) %>%
  mutate( q = adjusted) %>%
  arrange(q)

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





