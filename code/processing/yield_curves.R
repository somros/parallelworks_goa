# Alberto Rovellini
# 8/2//2023
# This script produces yield curves from the individual tables produced in the parallel runs
# This script is not run in parallel, just once after the batch that extracted catch and biomass and F etc.

library(tidyverse)
library(readxl)
library(ggh4x)
library(viridis)

# read in Groups.csv file
grps <- read.csv('../data/GOA_Groups.csv')

# list all csv files we need to read
f_files <- list.files('../output_R/', pattern = '.csv', recursive = T, full.names = T)

# read all csv files
f_ls <- list()
for(i in 1:length(f_files)){
  this_file <- f_files[i]
  f_ls[[i]] <- read.csv(this_file)
}

# bind into a data frame
f_df <- f_ls %>% bind_rows() %>% rename(Biomass = biomass, Catch = catch)

# clean up and format
f_df <- f_df %>%
  pivot_longer(-c(Code, this_f, f), values_to = 'mt', names_to = 'type') %>%
  left_join(grps %>% select(Code, LongName), by = 'Code')

# produce a dataset of 35% B0, to be used to plot horizontal lines that eill intersect the yield curve
b35 <- f_df %>%
  filter(f == 0) %>%
  rowwise() %>%
  mutate(b35 = ifelse(type== 'Biomass', mt * 0.35, NA)) %>%
  ungroup() %>%
  select(LongName, type, b35)

# find groups to plot
to_plot <- unique(f_df$Code)

# read in MSY information (from FMP)
tier3 <- read_xlsx('../data/msy.xlsx', sheet = 1, range = 'A3:J19') %>%
  select(Stock, FOFL) %>%
  set_names(c('Stock', 'FMSY'))

tier4_5 <- read_xlsx('../data/msy.xlsx', sheet = 2, range = 'A3:I10') %>%
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

fmsy <- data.frame('Code' = to_plot) %>%
  left_join(all_f) %>%
  left_join(grps %>% select(Code, LongName))

# plot
f_plot <- f_df %>%
  ggplot(aes(x = f, y = mt/1000))+
  geom_line()+
  geom_point(aes(color = factor(this_f)), size = 2)+
  scale_colour_viridis_d(begin = 0.1, end = 0.9)+
  geom_vline(data = fmsy, aes(xintercept = FMSY, group = LongName), linetype = 'dashed', color = 'orange')+
  geom_hline(data = b35, aes(yintercept = b35/1000, group = LongName), linetype = 'dashed', color = 'red')+
  theme_bw()+
  labs(x = 'F as perceived by the model', y = '1000\'s of tons', color = 'F as model input')+
  facet_grid2(LongName~type, scales = 'free', independent = 'all')+
  theme(strip.text.y = element_text(angle=0))
f_plot

# make a figure
t <- format(Sys.time(),'%Y-%m-%d %H-%M-%S')
ggsave(paste0('../output_R/yield_curves',t,'.png'), f_plot, width = 8, height = 6)