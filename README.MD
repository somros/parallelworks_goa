# Atlantis GOA on Parallel Works

These are notes to set up a Parallel Works environment run the Atlantis GOA model. This is based on notes from [Andy Beet and Joe Caracappa](https://github.com/NOAA-EDAB/neus-atlantis/wiki/Cloud-setup) and from [Hem Nalini Morzaria-Luna](https://github.com/hmorzaria/psatlantismodelupdates/wiki/Parallel-works), with modifications.

## Configuration

For the controller node, a low-spec machine like Standard_D4_v3 (4 vCPUs, 16 GB Memory, amd64) will be fine. For the compute and process partitions, we need to request an instance that can accommodate the job at hand. For 96 runs, an option can be Standard_HB120rs_v2 (120 vCPUs, 456 GB Memory, amd64). 1 node would be fine for 96 runs (Max Nodes is the parameter for how many of these machines you want, not the number of cores). For more runs you'd need to request more nodes accordingly in the Definition page of the resource. 

The parameters in the call to the slurm job are also important for correct allocation (see below).

## Snapshot

What worked on my cluster was the following snapshot:

```
#install libraries
sudo yum -y install git;
sudo yum -y install netcdf-devel;
sudo dnf -y install nco;

# repo is cloned on contrib
# can not do this in snapshot since /contrib isn't available at this time in boot
# sudo git clone -b dev_branch --single-branch https://github.com/NOAA-EDAB/neus-atlantis.git /usr/local/neus-atlantis;

sudo yum-config-manager --add-repo https://developer.download.nvidia.com/compute/cuda/repos/rhel7/x86_64/cuda-rhel7.repo;
sudo -n yum install tigervnc-server -y;
sudo -n yum install python3 -y;
sudo -n yum groupinstall "Server with GUI" -y;
sudo -n yum install epel-release -y;

# install custom R and symlink where RPM package would go
export R_VERSION=4.2.2;
# download and install R packages
curl -O https://cdn.rstudio.com/r/centos-7/pkgs/R-${R_VERSION}-1-1.x86_64.rpm;
yum -y install R-${R_VERSION}-1-1.x86_64.rpm;
ln -s /opt/R/${R_VERSION}/bin/R /usr/local/bin/R;
ln -s /opt/R/${R_VERSION}/bin/Rscript   /usr/local/bin/Rscript;

# required for some other R packages
sudo yum -y install fontconfig-devel
# install package manager for gdal, geos, proj4
sudo yum -y install https://download.postgresql.org/pub/repos/yum/reporpms/EL-7-x86_64/pgdg-redhat-repo-latest.noarch.rpm
sudo yum -y install proj-devel
sudo yum -y install gdal34-devel geos311-devel proj81-developer
sudo yum -y install harfbuzz-devel
sudo yum -y install fribidi-devel
sudo yum -y install libjpeg-devel

# install packages ahead of time
# used in atlantistools
sudo /usr/local/bin/Rscript -e "install.packages('remotes', repos='http://cran.rstudio.com/')";
sudo /usr/local/bin/Rscript -e "remotes::install_github('NOAA-EDAB/atlantisprocessing')";
sudo /usr/local/bin/Rscript -e "remotes::install_github('NOAA-EDAB/atlantisdiagnostics')";

# old Rstudio version
wget https://download1.rstudio.org/desktop/centos7/x86_64/rstudio-2022.07.2-576-x86_64.rpm;
sudo -n yum install rstudio-2022.07.2-576-x86_64.rpm -y;
# new rstudio has issues with centos7
#wget https://download1.rstudio.org/electron/centos7/x86_64/rstudio-2023.03.0-386-x86_64.rpm;
#sudo -n yum install rstudio-2023.03.0-386-x86_64.rpm -y;

#Add azcopy
wget -O azcopy_linux_amd64_10.18.1.tar https://aka.ms/downloadazcopy-v10-linux
tar -xzf azcopy_linux_amd64_10.18.1.tar
sudo cp ./azcopy_linux_amd64_*/azcopy /usr/bin/

```

## Singularity recipe

The Singularity recipe can be found in `/contrib/atlantisCode/` and it looks like this:

```
Bootstrap: docker
From: ubuntu:18.04

%help
Atlantis v6665 model

%labels
Author alberto.rovellini@gmail.com, modified from Andrew Beet & Hem Nalini Morzaria Luna

%environment
TZ=UTC
DEBIAN_FRONTEND=noninteractive
export PATH=/usr/lib/rstudio-server/bin:${PATH}
  
%files
/contrib/atlantisCode/trunk-6665/for_pw/trunk/atlantis /app/atlantis
/contrib/atlantisCode/trunk-6665/for_pw/trunk/.svn /app/.svn

#%setup
#install -Dv \
# rstudio_auth.sh \
# ${SINGULARITY_ROOTFS}/usr/lib/rstudio-server/bin/rstudio_auth
#install -Dv \
# ldap_auth.py \
# ${SINGULARITY_ROOTFS}/usr/lib/rstudio-server/bin/ldap_auth

%post
ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone
apt-get update && apt-get install -yq build-essential autoconf libnetcdf-dev libxml2-dev libproj-dev subversion valgrind dos2unix nano r-base
cd /app/atlantis
aclocal && autoheader && autoconf && automake -a && ./configure --enable-rassesslink && make CFLAGS="-DACCEPT_USE_OF_DEPRECATED_PROJ_API_H -Wno-misleading-indentation -Wno-format -Wno-implicit-fallthrough" && make install
mkdir /app/model
 
%runscript
cd /app/model 
./runGOA_Test_v0.sh
  
%startscript
cd /app/model
./runGOA_Test_v0.sh

```

This recipe is used to create the container `atlantis6665.sif` by running `sudo singularity build atlantis6665.sif Singularity` in the `/contrib/atlantisCode` folder, where both the container and the recipe are. 

## Slurm job

I set up a slurm batch job named `batchjob.sh` which is hosted in `/contrib/atlantisCode` and has the following shape (for 96 runs):

```
#!/bin/bash
#SBATCH --mail-type=ALL
#SBATCH --mail-user=alberto.rovellini@noaa.gov
#SBATCH --nodes=1
#SBATCH --partition=compute
#SBATCH --array=1-96

sudo mkdir -p /contrib/$USER/slurm_array/out$SLURM_ARRAY_TASK_ID

sudo singularity exec --bind /contrib/atlantis_goa/currentVersion:/app/model,/contrib/$USER/slurm_array/out$SLURM_ARRAY_TASK_ID:/app/model/output /contrib/atlantisCode/atlantis6665.sif /app/model/runGOA_Test_v0_$SLURM_ARRAY_TASK_ID.sh

```
If we want to run more simulations or if the instance requested for the compute partition has fewer cores than 96, we'd need to call more nodes or else jobs will be queued until cores are becoming free.

This slurm call points to individual `runGOA_Test_v0_xxx.sh` bash scripts to run Atlantis, one per run. They are all in `/contrib/atlantis_goa/currentVersion/` and they have this shape:

```
#!/bin/bash

cd /app/model

atlantisMerged -i GOA_cb_summer.nc  0 -o output_COD_1.nc -r GOA_run.prm -f GOA_force.prm -p GOA_physics.prm -b GOAbioparam_test.prm -h GOA_harvest_COD_1.prm -m GOAMigrations.csv -s GOA_Groups.csv -q GOA_fisheries.csv -d output

```
Each of these 96 points to a different `harvest.prm` file and names the output files differently, other than that they are identical. They are all created automatically (see code in `code/utils/single_species_f_sens_setup.R`).

## Outputs

Output of the Atlantis runs are stored in the individual containers in the `output` folder, but slurm copies them when the run is done to `/contrib/Alberto.Rovellini/slurm_array/outxxx`, where `outxxx` are individual output folders that are created when the slurm job is run. 
