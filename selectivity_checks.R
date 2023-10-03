# Alberto Rovellini
# 10/2/2023
# Plotting the original fishery selectivity ogives versus the resulting knife edges for Tier 3
# There is a mismatch in the age class at selectivity versus the size at 50% selex of annual cohorts in the original ogives
# See below for details, but it is due to loss of resolution when aggregating cohorts into age classes
# A revision of that for the catch.ts removals will be needed

library(tidyverse)
library(readxl)

# read data
ogives <- read_excel('../Catch_data/data/Selectivity patterns tier 3 GOA stocks.xlsx', sheet = 2, skip = 1)
grps <- read.csv('data/GOA_Groups.csv')
startage <- read.csv('data/age_at_selex.csv')

which_ogives <- grep('\\...',colnames(ogives))
these_species <- colnames(ogives)[!grepl('\\...',colnames(ogives))]

ogives <- ogives[,which_ogives] %>%
  set_names(these_species) %>%
  mutate(age = 1:nrow(.))

# pivot
ogives_long <- ogives %>% 
  pivot_longer(-age, names_to = 'species', values_to = 'selex') %>%
  arrange(species, age) %>%
  drop_na()

# match to Atlantis names
t3_codes <- c('POL','COD','SBF','FFS','FHS','FFD','RFS','POP','RFP','RFS','REX','ATF')
tier3_key <- data.frame(species = 
                          c("Pollock",
                            "Pacific cod",
                            "Sablefish",
                            "Northern and Southern rock sole",
                            "Flathead sole",
                            "Dover",
                            "Northern Rockfish",
                            "POP",
                            "Dusky rockfish",
                            "Rougheye Blackspotted rockfish",
                            "Rex Sole",
                            "Arrowtooth flounder"),
                        Code = t3_codes)

ogives_long <- ogives_long %>% left_join(tier3_key) %>% left_join(grps %>% select(Code, LongName))

# get years per age class
ypa <- grps %>% select(Code, LongName, NumAgeClassSize) %>% filter(Code %in% t3_codes)

# make the knife edges based on startage
# this is the fist age class geting fished. We also start counting from 0
# so the formula to get the first annual cohort getting fished is:
# (age_class * ypa) + 1 (the +1 means that we are getting the first year after the last age not getting fished)
knife_edge <- startage %>% 
  filter(Code %in% t3_codes) %>% 
  left_join(ypa, by = 'Code') %>%
  mutate(edge = (age_class*NumAgeClassSize) + 1) %>%
  select(Code, LongName, edge)
  
# now view
max_val <- max(ogives_long$age)
ogives_long %>%
  ggplot(aes(x = age, y = selex, group = LongName))+
  geom_line()+
  geom_vline(data = knife_edge, aes(xintercept = edge), color = 'red')+
  scale_x_continuous(breaks = seq(1, max_val, by = 1), limits=c(0, NA))+
  theme_bw()+
  facet_wrap(~LongName, scales= 'free', nrow = 2)

# bad. Test the ogives by age class vs startage to see where the issue arises
# Here's why some groups are super resistent to fishing though...

ogives_10 <- read.csv('../Catch_data/data/selex_10_age_classes.csv')
startage_mod <- startage %>% filter(Code %in% t3_codes) %>% mutate(edge = age_class + 1) # because in the other data frame we start from 1

# view
ogives_10 %>%
  ggplot(aes(x = age_class, y = selex_age_class, group = Code))+
  geom_line()+
  geom_vline(data = startage_mod, aes(xintercept = edge), color = 'red')+
  scale_x_continuous(breaks = seq(1, max_val, by = 1), limits=c(0, NA))+
  theme_bw()+
  facet_wrap(~Code, scales= 'free', nrow = 2)

# RFD is plain WRONG (mapped the wrong species to the wrong selectivity)
# The rest seems an artefact of one  or more of these operations:
# 1. Inconsistency between maximum age and stock assessment maximum age
# 2. Rounding the maximum age to a multiple of 10, so that the age class size is an integer
# 3. Aggregating many annual cohorts into one age class (there was also weighting by number of individuals that fed into these calculations)
# 4. Selecting the age class that >50%. In most cases (and more so for age classes comprising many years), the jump may be 0.1-0.8 or the likes, and the actual selected age class sits in the middle

# review algebra to get startage, this time compare to original

# for startage we can get away with something more intuitive, for catch we will have to rework the proportions
# this only applies to Tier 3
new_startage <- ogives_long %>%
  filter(species != 'Rougheye Blackspotted rockfish') %>% # let's drop the longer-lived rockfish, most of the biomass in RFS is northern
  group_by(Code) %>%
  filter(selex > 0.5) %>%
  arrange(age,selex) %>%
  slice_head() %>%
  ungroup()

# now join with ypa
new_startage <- new_startage %>% left_join(ypa, by = c('Code','LongName')) %>%
  mutate(startage = floor(age / NumAgeClassSize)) # floor to round down

# adapt to Atlantis
new_startage <- new_startage %>%
  mutate(mod = age%%NumAgeClassSize) %>%
  rowwise() %>%
  mutate(prm_startage = ifelse(mod>0, startage + 1 -1, startage - 1)) %>%
  ungroup()

# do not allow 0's (do not fish the first age class)
new_startage[new_startage$prm_startage==0,]$prm_startage <- 1

# add edge field for plotting
new_startage <- new_startage %>%
  mutate

# view
ogives_long %>%
  ggplot(aes(x = age, y = selex, group = Code))+
  geom_line()+
  geom_vline(data = new_startage, aes(xintercept = (prm_startage*NumAgeClassSize)+1), color = 'red', size = 2)+
  scale_x_continuous(breaks = seq(1, max_val, by = 1), limits=c(0, NA))+
  theme_bw()+
  facet_wrap(~Code, scales= 'free', nrow = 2)

# write out
write.csv(new_startage %>% select(Code, prm_startage), 'new_startage.csv', row.names = F)
