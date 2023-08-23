# 8/21/2023
# Alberto Rovellini
# Create a series of harvest.prm files where mFC for one species changes
# This is for the single-sepcies F testing
# For now, we only have one fleet, so only the first value of the mFC vector is of interest

# make mFC vectors based on following range of F:
# F = 0, 0.05, 0.1, 0.15, 0.2, 0.3, 0.5, 0.75, 1, 1.25, 1.5, 2

# Formula for mFC is mfc = 1-exp(-F / 365)

#f_vals <- c(0.00, 0.05, 0.10, 0.15, 0.20, 0.30, 0.50, 0.75, 1.00, 1.25, 1.50, 2.00)
f_vals <- c(0.00, 0.10, 0.20, 0.50, 0.75, 1.00) # strating simple for PW testing
nfleet <- 33 # how many fleets in the harvest.prm file?

mfc_vals <- 1-exp(-f_vals / 365)
# 0.0000000000 0.0001369769 0.0002739351 0.0004108745 0.0005477951 0.0008215801 0.0013689252 0.0020526849 0.0027359764 0.0034188001 0.0041011562 0.0054644672
# 0.0000000000 0.0002739351 0.0005477951 0.0013689252 0.0020526849 0.0027359764 # for PW testing
#prm_file <- 'data/GOA_harvest_background.prm'
prm_file <- 'GOA_harvest_background.prm'
prm_vals <- readLines(prm_file)

species_to_test <- c('COD', 'POL')

# do for each species
for(sp in species_to_test){
  
  for(f in 1:length(mfc_vals)){
    
    # create file
    #newfile <- paste0('output/f_sens/GOA_harvest_', sp, '_', f, '.prm')
    newfile <- paste0('GOA_harvest_', sp, '_', f, '.prm')
    
    file.create(newfile)
    
    this_sp <- sp
    
    # find the line that has the mFC vector for the species of interest
    this_line <- grep(paste0('mFC_',this_sp), prm_vals) + 1
    
    new_mFC_vector <- paste(as.character(c(mfc_vals[f], rep(0, nfleet-1))), collapse = ' ')
    
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
