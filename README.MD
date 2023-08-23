
`nfolders <- 2
for (ifolder in 1:nfolders) {
  
  # create Atlantis sh script
  filenm <- paste0("/contrib/atlantis_goa/currentVersion/runGOA_Test_v0_",ifolder,".sh")
  fileConn<-file(filenm,open="w")
  cat("#!/bin/bash\n",file=fileConn,append=T)
  cat("cd /app/model\n",file=fileConn,append=T)
  cat(paste0("atlantisMerged -i GOA_cb_summer.nc  0 -o outputGOA01328_test.nc -r GOA_run", ifolder, ".prm -f GOA_force.prm -p GOA_physics.prm -b GOAbioparam_test.prm -h GOA_harvest_background.prm -m GOAMigrations.csv -s GOA_Groups.csv -q GOA_fisheries.csv -d outputFolder\n"),file = fileConn,append=T)
  close(fileConn)
  system(paste0("chmod 775 ",filenm))
}`

`#!/bin/bash
#SBATCH --mail-type=ALL
#SBATCH --mail-user=alberto.rovellini@noaa.gov
#SBATCH --nodes=1
#SBATCH --partition=compute
#SBATCH --array=1-2

sudo mkdir -p /contrib/atlantis_outputs_goa/slurm_array/out$SLURM_ARRAY_TASK_ID

sudo singularity exec --bind /contrib/atlantis_goa/currentVersion:/app/model,/contrib/atlantis_outputs_goa/slurm_array/out$SLURM_ARRAY_TASK_ID:/app/model/output /contrib/atlantisCode/atlantis6665.sif /app/model/runGOA_Test_v0_$SLURM_ARRAY_TASK_ID.sh`