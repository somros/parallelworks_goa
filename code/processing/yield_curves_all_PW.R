# Alberto Rovellini
# 9/25/2023
# This script produces yield curves from the individual tables produced in the parallel runs
# This script is not run in parallel, just once after the batch that extracted catch and biomass and F etc.
# This is to create panels with three curves each: 
# 1. original
# 2. reduced age at first selex for POP, HAL, RFP, RFS
# 3. reduced steepness for POP, HAL, SBF, RFP, RFS

# loop over batches of output csv files depending on label ('', 'v2', 'v3')

library(tidyverse)
library(readxl)
library(ggh4x)
library(viridis)

# read in Groups.csv file
grps <- read.csv('data/GOA_Groups.csv')

# list all csv files we need to read
f_files_v2 <- list.files('../PW_output/', pattern = 'v2.csv', recursive = T, full.names = T)
f_files_v3 <- list.files('../PW_output/', pattern = 'v3.csv', recursive = T, full.names = T)
f_files_v1 <- setdiff(list.files('../PW_output/', pattern = '.csv', recursive = T, full.names = T),
                   c(f_files_v2, f_files_v3))

# bind all files into one list
f_files <- list(f_files_v1, f_files_v2, f_files_v3)

# create empty list to fill with data frame for the yield curve
f_df_ls <- list()

for(i in 1:length(f_files)){
  
  # what run is this
  this_run <- paste0('v',i)
  
  this_f_files <- f_files[[i]]
  
  # read all csv files
  f_ls <- list()
  for(j in 1:length(this_f_files)){
    this_file <- this_f_files[j]
    f_ls[[j]] <- read.csv(this_file)
  }
  
  # bind into a data frame
  f_df <- f_ls %>% bind_rows() %>% rename(Biomass = biomass, Catch = catch)
  
  # clean up and format
  f_df <- f_df %>%
    pivot_longer(-c(Code, f), values_to = 'mt', names_to = 'type') %>%
    left_join(grps %>% select(Code, LongName), by = 'Code') %>%
    mutate(run = this_run)
  
  f_df_ls[[i]] <- f_df

}

f_df <- bind_rows(f_df_ls)

# produce a dataset of 35% B0, to be used to plot horizontal lines that will intersect the yield curve
# but B35% will now be different between runs with different steepness? Not between runs with smaller selex in theory
b35 <- f_df %>%
  filter(run == 'v1', f == 0) %>% # producing it off of v1 (v2 should be the same, v3 should be similar)
  rowwise() %>%
  mutate(b35 = ifelse(type== 'Biomass', mt * 0.35, NA)) %>%
  ungroup() %>%
  select(LongName, Code, type, b35)

# read in MSY information (from FMP)
tier3 <- read_xlsx('data/msy.xlsx', sheet = 1, range = 'A3:J19') %>%
  select(Stock, FOFL) %>%
  set_names(c('Stock', 'FMSY'))

tier4_5 <- read_xlsx('data/msy.xlsx', sheet = 2, range = 'A3:I10') %>%
  select(`Stock/Stock complex`, `M or FMSY`)%>%
  set_names(c('Stock', 'FMSY'))

tier_3_4_5 <- rbind(tier3, tier4_5)

# make key
tier_3_4_5 <- tier_3_4_5 %>%
  mutate(Code = c('POL','COD','SBF','FFS','FFS','FFS','FFS','FFD',
                  'REX','REX','ATF','FHS','POP','RFS','RFS','RFP',
                  'FFS','RFD','RFD','RFD','RFD','THO','DOG')) %>%
  group_by(Code) %>%
  summarise(FMSY = mean(FMSY)) %>%
  set_names('Code','FMSY')

# as soon as we introduce species that are not in the FMP, including forage species, we will need estimates of M from the parameters file
all_f <- tier_3_4_5 # placeholder for now

# find groups to plot
to_plot <- unique(f_df$Code)

# bind FMSY information
fmsy <- data.frame('Code' = to_plot) %>%
  left_join(all_f) %>%
  left_join(grps %>% select(Code, LongName))

# add halibut (M from IPHC assessment)
fmsy[fmsy$Code=='HAL',]$FMSY <- 0.2 # this is M

# make key from v to what actually goes on in the run
key <- data.frame('run' = c('v1','v2','v3'),
                  'Run' = c('Base', 'Lower first selected\nage class', 'BH Beta x2\n(lower steepness)'))

# plot
# only focus on POP, HAL, SBF, RFP, RFS
f_plot <- f_df %>%
  filter(Code %in% c('POP','HAL','RFP','RFS')) %>%
  left_join(key) %>%
  ggplot(aes(x = f, y = mt/1000, color = Run))+
  geom_line()+
  geom_point(size = 2)+
  scale_colour_manual(values = c('black','red','blue'))+
  geom_vline(data = fmsy %>% filter(Code %in% c('POP','HAL','RFP','RFS')), 
             aes(xintercept = FMSY, group = LongName), linetype = 'dashed', color = 'orange')+
  geom_hline(data = b35  %>% filter(Code %in% c('POP','HAL','RFP','RFS')), 
             aes(yintercept = b35/1000, group = LongName), linetype = 'dashed', color = 'red')+
  theme_bw()+
  labs(x = 'F as perceived by the model', y = '1000\'s of tons', color = 'Run')+
  facet_grid2(LongName~type, scales = 'free', independent = 'all')+
  theme(strip.text.y = element_text(angle=0))
f_plot

# # make a figure
t <- format(Sys.time(),'%Y-%m-%d %H-%M-%S')
ggsave(paste0('../PW_output/yield_curves',t,'.png'), f_plot, width = 8, height = 5)
