# Alberto Rovellini
# 9/25/2023
# This script produces yield curves from the individual tables produced in the parallel runs
# This script is not run in parallel, just once after the batch that extracted catch and biomass and F etc.

library(tidyverse)
library(readxl)
library(ggh4x)
library(viridis)

# read in Groups.csv file
#grps <- read.csv('../data/GOA_Groups.csv')
grps <- read.csv('data/GOA_Groups.csv')

# list all csv files we need to read
#f_files <- list.files('../output_R/', pattern = '.csv', recursive = T, full.names = T)
f_files_v2 <- list.files('../PW_output/', pattern = 'v2.csv', recursive = T, full.names = T)
f_files_v3 <- list.files('../PW_output/', pattern = 'v3.csv', recursive = T, full.names = T)
f_files_v1 <- setdiff(list.files('../PW_output/', pattern = '.csv', recursive = T, full.names = T),
                      c(f_files_v2, f_files_v3))

f_files <- f_files_v2

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
  pivot_longer(-c(Code, f), values_to = 'mt', names_to = 'type') %>%
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
#tier3 <- read_xlsx('../data/msy.xlsx', sheet = 1, range = 'A3:J19') %>%
tier3 <- read_xlsx('data/msy.xlsx', sheet = 1, range = 'A3:J19') %>%
  select(Stock, FOFL) %>%
  set_names(c('Stock', 'FMSY'))

#tier4_5 <- read_xlsx('../data/msy.xlsx', sheet = 2, range = 'A3:I10') %>%
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

fmsy <- data.frame('Code' = to_plot) %>%
  left_join(all_f) %>%
  left_join(grps %>% select(Code, LongName))

# add halibut (M from IPHC assessment)
fmsy[fmsy$Code=='HAL',]$FMSY <- 0.2 # this is M

# get f that returned the highest yield, and level of depletion for that F

sp <- unique(f_df$LongName)

atlantis_fmsy_ls <- list()

for(i in 1:length(sp)){
  this_f_df <- f_df %>% filter(LongName == sp[i])
  
  atlantis_fmsy <- this_f_df %>% filter(type == 'Catch') %>%
    slice_max(mt) %>%
    pull(f)
  
  b0 <- this_f_df %>%
    filter(f == 0, type == 'Biomass') %>%
    pull(mt)
  
  b_fmsy <- this_f_df %>%
    filter(f == atlantis_fmsy, type == 'Biomass') %>%
    pull(mt)
  
  depletion_fmsy <- b_fmsy / b0
  
  atlantis_fmsy_ls[[i]] <- data.frame('LongName' = sp[i], 
                                      'atlantis_fmsy' = atlantis_fmsy,
                                      'b_fmsy' = b_fmsy,
                                      'depletion' = depletion_fmsy)
}

atlantis_fmsy <- bind_rows(atlantis_fmsy_ls)

# annotations for the plots (atlantis depletion)
annotations <- atlantis_fmsy %>% 
  mutate(depletion=round(depletion,digits=2), atlantis_fmsy=round(atlantis_fmsy,digits = 2))

# plot
f_plot <- f_df %>%
  ggplot(aes(x = f, y = mt/1000))+
  geom_line()+
  geom_point(size = 2)+
  geom_vline(data = fmsy, aes(xintercept = FMSY, group = LongName), linetype = 'dashed', color = 'orange')+
  geom_vline(data = atlantis_fmsy, aes(xintercept = atlantis_fmsy, group = LongName), linetype = 'dashed', color = 'blue')+
  geom_hline(data = b35, aes(yintercept = b35/1000, group = LongName), linetype = 'dashed', color = 'red')+
  geom_hline(data = atlantis_fmsy %>% mutate(type = 'Biomass'), 
             aes(yintercept = b_fmsy/1000, group = LongName), linetype = 'dashed', color = 'blue')+
  geom_text(data = annotations %>% mutate(type = 'Biomass'), 
            aes(x=Inf,y=Inf,hjust=1,vjust=1,label=paste0('Depletion=',depletion)), color = 'blue')+
  theme_bw()+
  scale_y_continuous(limits = c(0, NA))+
  labs(x = 'F (as perceived by the model)', y = '1000\'s of tons')+
  facet_grid2(LongName~type, scales = 'free', independent = 'all')+
  theme(strip.text.y = element_text(angle=0))
f_plot

# make a figure
t <- format(Sys.time(),'%Y-%m-%d %H-%M-%S')
ggsave(paste0('../PW_output/yield_curves',t,'.png'), f_plot, width = 8, height = 12)

# make figures for slides (break into two columns)
# plot
grp1 <- unique(f_df$LongName)[1:6]
f_plot1 <- f_df %>%
  filter(LongName %in% grp1) %>%
  ggplot(aes(x = f, y = mt/1000))+
  geom_line()+
  geom_point(size = 2)+
  geom_vline(data = fmsy %>% filter(LongName %in% grp1), aes(xintercept = FMSY, group = LongName), linetype = 'dashed', color = 'orange')+
  geom_vline(data = atlantis_fmsy %>% filter(LongName %in% grp1), aes(xintercept = atlantis_fmsy, group = LongName), linetype = 'dashed', color = 'blue')+
  geom_hline(data = b35 %>% filter(LongName %in% grp1), aes(yintercept = b35/1000, group = LongName), linetype = 'dashed', color = 'red')+
  geom_hline(data = atlantis_fmsy %>% filter(LongName %in% grp1) %>% mutate(type = 'Biomass'), 
             aes(yintercept = b_fmsy/1000, group = LongName), linetype = 'dashed', color = 'blue')+
  geom_text(data = annotations %>% filter(LongName %in% grp1) %>% mutate(type = 'Biomass'), 
            aes(x=Inf,y=Inf,hjust=1,vjust=1,label=paste0('Depletion=',depletion)), color = 'blue')+
  theme_bw()+
  scale_y_continuous(limits = c(0, NA))+
  labs(x = 'F (as perceived by the model)', y = '1000\'s of tons')+
  facet_grid2(LongName~type, scales = 'free', independent = 'all')+
  theme(strip.text.y = element_text(angle=0))

grp2 <- unique(f_df$LongName)[7:12]
f_plot2 <- f_df %>%
  filter(LongName %in% grp2) %>%
  ggplot(aes(x = f, y = mt/1000))+
  geom_line()+
  geom_point(size = 2)+
  geom_vline(data = fmsy %>% filter(LongName %in% grp2), aes(xintercept = FMSY, group = LongName), linetype = 'dashed', color = 'orange')+
  geom_vline(data = atlantis_fmsy %>% filter(LongName %in% grp2), aes(xintercept = atlantis_fmsy, group = LongName), linetype = 'dashed', color = 'blue')+
  geom_hline(data = b35 %>% filter(LongName %in% grp2), aes(yintercept = b35/1000, group = LongName), linetype = 'dashed', color = 'red')+
  geom_hline(data = atlantis_fmsy %>% filter(LongName %in% grp2) %>% mutate(type = 'Biomass'), 
             aes(yintercept = b_fmsy/1000, group = LongName), linetype = 'dashed', color = 'blue')+
  geom_text(data = annotations %>% filter(LongName %in% grp2) %>% mutate(type = 'Biomass'), 
            aes(x=Inf,y=Inf,hjust=1,vjust=1,label=paste0('Depletion=',depletion)), color = 'blue')+
  theme_bw()+
  scale_y_continuous(limits = c(0, NA))+
  labs(x = 'F (as perceived by the model)', y = '1000\'s of tons')+
  facet_grid2(LongName~type, scales = 'free', independent = 'all')+
  theme(strip.text.y = element_text(angle=0))

# make a figure
t <- format(Sys.time(),'%Y-%m-%d %H-%M-%S')
ggsave(paste0('../PW_output/yield_curves',t,'_1.png'), f_plot1, width = 6, height = 6)
ggsave(paste0('../PW_output/yield_curves',t,'_2.png'), f_plot2, width = 6, height = 6)


