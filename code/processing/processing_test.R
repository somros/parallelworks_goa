# test script to take Atlantis output and do something with it
library(dplyr)
library(tidyr)

arg <- commandArgs(trailingOnly=T)
runname<- arg[1]
print(runname)

#set path
atl.dir <- file.path('/contrib/Alberto.Rovellini/slurm_array',runname,'outputGOA_outBiomIndx.txt', fsep = '/')

# read file
dat <- read.table(atl.dir, header = T)

# do something
dat1 <- dat %>%
  select(Time:DR) %>%
  filter(Time == max(Time)) %>%
  pivot_longer(-Time, values_to = 'Biomass', names_to = 'Species') %>%
  group_by(Species) %>%
  summarize(TotBiom = sum(Biomass)) %>%
  ungroup() %>%
  mutate(run = runname)

# write out
write.csv(dat1, paste0('/contrib/Alberto.Rovellini/terminal_biomass', runname, '.csv'), row.names = F)
