# Alberto Rovellini
# 8/21/2022
# this code pulls terminal biomass and catch from each run, and it calculates realized F based on catch in the first year
# these variables are stored in a table with one row and written to a csv file, to be processed later
library(dplyr)
library(tidyr)

arg <- commandArgs(trailingOnly=T)
runname<- arg[1]
print(runname)

# first identify the species and the level of fishing for this run. These are unrelated from runname
folder_path <- paste('/contrib/Alberto.Rovellini/slurm_array',runname,sep='/')
# base it on the biomass at age TXT ouput file which we will need later
biomage_filename <- list.files(path = folder_path, pattern = 'AgeBiomIndx.txt')[1]
label <- unlist(strsplit(gsub('AgeBiomIndx.txt','',gsub('output_','',biomage_filename)), '_'))
sp <- label[1]
fidx <- as.numeric(label[2])
#f_vals <- c(0.00, 0.10, 0.20, 0.50, 0.75, 1.00) # F vector from script to build
#this_f <- f_vals[fidx]

#set paths to files. We will need:
grp_path <- '/contrib/Alberto.Rovellini/data/GOA_Groups.csv' # functional groups
mat_path <- '/contrib/Alberto.Rovellini/data/age_at_mat.csv' # maturity at 50%
selex_path <- '/contrib/Alberto.Rovellini/data/age_at_selex.csv' # selectivity pattern
biomage_path <- file.path('/contrib/Alberto.Rovellini/slurm_array',runname,biomage_filename, fsep = '/') # biomass by age
catch_path <- file.path('/contrib/Alberto.Rovellini/slurm_array',runname,paste0('output_',paste(label, collapse = '_'),'Catch.txt'), fsep = '/')

# read files
atlantis_fg <- read.csv(grp_path)
mat <- read.csv(mat_path, header = T) # age at maturity
selex <- read.csv(selex_path, header = T) # age at selectivity
biomage <- read.table(biomage_path, sep = ' ', header = T)
catch <- read.table(catch_path, sep = ' ', header = T)

# now extract data
#biodat_age_tmp <- read.table('../Parametrization/output_files/data/out_1342/outputGOA01342_testAgeBiomIndx.txt', sep = ' ', header = T)
# SSB to plot and report in tables
spawning_biomass <- biomage %>% 
  slice_tail(n = 5) %>% # use last xxx years
  summarise(across(-"Time", ~ mean(.x, na.rm = TRUE))) %>%
  ungroup() %>%
  pivot_longer(everything(), names_to = 'Code.Age', values_to = 'biomass_mt') %>%
  separate_wider_delim(Code.Age, delim = '.', names = c('Code', 'Age')) %>%
  filter(Code == sp) %>%
  left_join(mat, by = 'Code') %>%
  mutate(idx = as.numeric(Age) - as.numeric(age_class_mat)) %>%
  filter(is.na(idx) | idx >= 0) %>%
  group_by(Code) %>%
  summarise(biomass_mt = sum(biomass_mt)) %>%
  ungroup() %>%
  pull(biomass_mt)

# total catch
# taking mean of the last 5 years
#catchdat_tmp <- read.table('../Parametrization/output_files/data/out_1342/outputGOA01342_testCatch.txt', sep = ' ', header = T)
catch_val <- catch %>% 
  slice_tail(n = 5) %>% 
  pull(sp) %>%
  mean()

# # calculate realized F after 1 year of data
# # get initial biomass for the selected age classes
biom_age_t1 <- biomage %>% 
  filter(Time == 0) %>% 
  pivot_longer(-Time, names_to = 'Code.Age', values_to = 'biomass') %>%
  separate_wider_delim(Code.Age, delim = '.', names = c('Code', 'Age')) %>%
  left_join(selex, by = 'Code') %>%
  mutate(idx = as.numeric(Age) - as.numeric(age_class_selex)) %>%
  filter(is.na(idx) | idx >= 0) %>%
  group_by(Code) %>%
  summarise(biomass = sum(biomass)) %>%
  ungroup() %>% 
  filter(Code == sp)
# 
# # catch (one time step after biomass: how much did we catch in this time?)
catch_t1 <- catch %>% 
  select(Time, all_of(sp)) %>% 
  filter(Time == 365) %>%
  summarise(across(everything(), ~ mean(.x, na.rm = TRUE))) %>%
  pivot_longer(-Time, names_to = 'Code', values_to = 'catch') %>%
  select(-Time)
# 
# # calc realized f
f_t1 <- biom_age_t1 %>% left_join(catch_t1) %>%
  mutate(exp_rate = catch/biomass,
         f = -log(1-exp_rate)) %>%#, # this is a realized from the run
  #this_f = this_f) %>% # this is the imposed value that we'd expect to see (e.g. 0.3, etc.)
  select(Code, f)

# # bind all
f_frame <- f_t1 %>%
  mutate(biomass = spawning_biomass,
         catch = catch_val)

# method 2
# calculate F based on last 5 years of the run, averaging biomass and catch

# get initial biomass for the selected age classes

# mu_val <- catch_val / biomage_val # exploitation rate
# f_val <- -log(1-mu_val)

# f_frame <- data.frame('Code' = sp, 'biomass' = biomage_val, 'catch' = catch_val, 'f' = f_val)

# write out to be then brought together with all other runs
write.csv(f_frame, paste0('/contrib/Alberto.Rovellini/output_R/',paste(sp,fidx,'f.csv',sep='_')), row.names = F)