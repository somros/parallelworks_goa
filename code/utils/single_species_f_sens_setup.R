# 8/21/2023
# Alberto Rovellini
# Create a series of harvest.prm files where mFC for one species changes
# This is for the single-species F testing
# Do it on Tier 3 species to start
# For now, we only have one fleet, so only the first value of the mFC vector is of interest
library(readxl)
library(tidyverse)

nfleet <- 33 # how many fleets in the harvest.prm file?
select <- dplyr::select

# make mFC vectors based on a range of F:
# start from 0 and go up to 2*FOFL - test 10 values (closer together the closer to 0 we are?)
# what to do with imposed and realized f?

# Formula for mFC is mfc = 1-exp(-F / 365)

#f_vals <- c(0.00, 0.05, 0.10, 0.15, 0.20, 0.30, 0.50, 0.75, 1.00, 1.25, 1.50, 2.00)
#f_vals <- c(0.00, 0.10, 0.20, 0.50, 0.75, 1.00) # strating simple for PW testing
# read FOFL table
# read in MSY information (from FMP)
f_cap <- read_xlsx('extra/msy.xlsx', sheet = 1, range = 'A3:J19') %>%
  select(Stock, FOFL) %>%
  mutate(Code = c('POL','COD','SBF','FFS','FFS','FFS','FFS','FFD',
                  'REX','REX','ATF','FHS','POP','RFS','RFS','RFP')) %>%
  group_by(Code) %>%
  summarise(FOFL = mean(FOFL)) %>%
  ungroup() %>%
  mutate(cap = 2*FOFL) %>%
  select(-FOFL)

# add halibut (2M)
f_cap <- rbind(f_cap, data.frame('Code'='HAL', cap = 0.4)) # 0.2*2
species_to_test <- unique(f_cap$Code)

# make range of values, stored in a matrix
n_f <- 8
mfc_tab <- f_tab <- matrix(NA, nrow = length(species_to_test), ncol = n_f)
 
for(i in 1:length(species_to_test)){
  sp <- species_to_test[i]
  f <- f_cap %>% filter(Code == sp) %>% pull(cap)
  fvec <- seq(0,f,length.out=n_f)
  mfcvec <- 1-exp(-fvec / 365)
  mfcrange <- matrix(mfcvec, nrow = 1)

  # store
  mfc_tab[i,] <- mfcrange
  
  # also make df with F values, we need to store it for plotting
  frange <- matrix(fvec, nrow = 1)
  
  # store
  f_tab[i,] <- frange
}

# set names
mfc_tab <- mfc_tab %>% 
  data.frame() %>% 
  mutate(Code = species_to_test) %>%
  set_names(c(1:8, 'Code'))

f_tab <- f_tab %>% 
  data.frame() %>% 
  mutate(Code = species_to_test) %>%
  set_names(c(1:8, 'Code'))

#prm_file <- 'data/GOA_harvest_background.prm'
prm_file <- 'GOA_harvest_background.prm'
prm_vals <- readLines(prm_file)

# do for each species
for(sp in species_to_test){
  
  for(f in 1:n_f){
    
    # create file
    #newfile <- paste0('output/f_sens/GOA_harvest_', sp, '_', f, '.prm')
    newfile <- paste0('GOA_harvest_', sp, '_', f, '.prm')
    
    file.create(newfile)
    
    this_sp <- sp
    this_mfc <- mfc_tab %>% filter(Code == sp) %>% pull(as.character(f))
    
    # find the line that has the mFC vector for the species of interest
    this_line <- grep(paste0('mFC_',this_sp), prm_vals) + 1
    
    new_mFC_vector <- paste(as.character(c(this_mfc, rep(0, nfleet-1))), collapse = ' ')
    
    # make a new harvest.prm object and modify it
    prm_new <- prm_vals
    prm_new[this_line] <- new_mFC_vector
    
    # write to file
    writeLines(prm_new, con = newfile)
    
    # create corresponding bash script
    
  }
  
}

# now make the runatlantis.sh scripts pointing to the correct path and creating the correct output
patterns <- paste0('GOA_harvest_',species_to_test)
f_files <- list.files(pattern = paste0(patterns, collapse = '|'))
n_prm_files <- length(f_files)
for (ifile in 1:n_prm_files) {
  
  this_file <- f_files[ifile]
  label <- unlist(strsplit(gsub('.prm','',gsub('GOA_harvest_','',this_file)),'_'))
  # sp <- label[1]
  # f <- label[2]
  label <- paste(label, collapse = '_')
  
  # create Atlantis sh script
  filenm <- paste0("runGOA_Test_v0_",ifile,".sh")
  fileConn<-file(filenm,open="w")
  cat("#!/bin/bash\n",file=fileConn,append=T)
  cat("cd /app/model\n",file=fileConn,append=T)
  cat(paste0("atlantisMerged -i GOA_cb_summer.nc  0 -o output_", 
             label,
             ".nc -r GOA_run.prm -f GOA_force.prm -p GOA_physics.prm -b GOAbioparam_test.prm -h GOA_harvest_",
             label,
             ".prm -m GOAMigrations.csv -s GOA_Groups.csv -q GOA_fisheries.csv -d output\n"),file = fileConn,append=T)
  close(fileConn)
  system(paste0("chmod 775 ",filenm))
}
