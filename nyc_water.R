# NYC Drinking Water Quality Distribution Monitoring Data Analysis 
# Oct 8 2020
# August Guo

# Packages 
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)

# Load data 
setwd('~/Desktop/r_projects/practice')
nyc_water <- read.csv('drinking-water-quality-distribution-monitoring-data-1.csv')

# Data cleaning 
water <- rename(nyc_water, 
                Date = Sample.Date, 
                Time = Sample.Time, 
                Site = Sample.Site, 
                Class = Sample.class,
                Chlorine = Residual.Free.Chlorine..mg.L.,
                Turb = Turbidity..NTU.,
                Ecoli = E.coli.Quanti.Tray...MPN.100mL.,
                Coli = Coliform..Quanti.Tray...MPN..100mL.,
                Fluoride = Fluoride..mg.L.) %>% 
  select(-Sample.Number)

str(water)

# transform date and time
# convert date and time to POSIX format using lubridate 
water1 <- water %>% 
  mutate(date_time = mdy_hm(paste(Date, Time, sep = ' ')))

# check missing value in date_time 
filter(water1, !is.na(water1$date_time))

# not much valuable info in this observation of missing so remove it
water1 <- water1 %>% 
  filter(!is.na(date_time)) %>% 
  select(date_time, everything()) %>% 
  rename(Date_time = date_time)

# clean coli variable 
class(water1$Coli)
unique(water1$Coli)
water1$Coli <- case_when(
  water1$Coli %in% c('<1', '<1 ') ~ '0',
  water1$Coli == '>200.5' ~ '200.5',
  water1$Coli == '-' ~ 'NA',
  TRUE ~ as.character(water1$Coli))
unique(water1$Coli)

water1$Coli <- as.numeric(water1$Coli)
unique(water1$Coli)

# clean Ecoli variable 
unique(water1$Ecoli)

water1$Ecoli <- case_when(
  water1$Ecoli %in% c('<1', '<1 ') ~ '0',
  water1$Ecoli == '-' ~ 'NA',
  TRUE ~ as.character(water1$Ecoli))
unique(water1$Ecoli)

water1$Ecoli <- as.numeric(water1$Ecoli)
unique(water1$Ecoli)

# check NA for coli and Ecoli 
water1 %>%  
  filter(is.na(Ecoli) | is.na(Coli))

# remove NA row of coli and Ecoli
water1 <- water1 %>%  
  filter(!is.na(Ecoli) | !is.na(Coli))

water1 %>%  
  filter(is.na(Ecoli))

# Create year, month, day columns 
water1 <- water1 %>% 
  mutate(Year = year(Date_time),
         Month = month(Date_time),
         Day = day(Date_time))

# Exploratory data analysis ----
# chlorine 
summary(water1$Chlorine)

filter(water1, is.na(Chlorine))

filter(water1, Chlorine < 0)

water1 <- water1 %>% 
  filter(!is.na(Chlorine)) %>% 
  filter(Chlorine >= 0)

summary(water1$Chlorine)

water1 %>% 
  group_by(Month) %>% 
  summarise(mean.chlorine = mean(Chlorine, na.rm = T),
            mean.coli = mean(Coli),
            mean.ecoli = mean(Ecoli))

# seperate chlorine into 3 baskets 

cutpoints <- quantile(water1$Chlorine, 
                      seq(0, 1, length=4), 
                      na.rm=T)
water1$Chlorine_range <- cut(water1$Chlorine, cutpoints, include.lowest = T)

table(water1$Chlorine_range)
  
water1 %>% 
  group_by(Month) %>% 
  filter(Site == '1S07') %>% 
  ggplot(.) +
    geom_point(aes(x = Month, y = Chlorine))
  
water1 %>%
  filter(Site == '1S07')

water1 %>% 
  group_by(Month) %>%
  ggplot(.) +
    geom_bar(aes(x = Site))

water1 %>% 
  group_by(Day) %>% 
  ggplot(.) +
    geom_violin(aes(x = Month, y = Chlorine, group = Month))



# convert chlorine to levels 
water <- water %>% 
  mutate(Chlorine_level = case_when(
    Chlorine <= 0.5 ~ '0-0.5', 
    Chlorine > 0.5 & Chlorine <= 1 ~ '0.5-1', 
    Chlorine > 1  ~ '> 1')
  )

class(water$Chlorine_level)
water$Chlorine_level <- factor(water$Chlorine_level, 
                               levels = c('0-0.5', '0.5-1', '> 1'))
class(water$Chlorine_level)
levels(water$Chlorine_level)

ggplot(data = water) +
  geom_histogram(aes(x = Chlorine_level))

ggplot(data = water, mapping = aes(x= Chlorine_level)) +
  geom_bar()

# Coli number & chlorine level 

ggplot(data = water) +
  geom_bar(mapping = aes(x = Coli))



ggplot(data = water, aes(x = Date, y = Coli)) +
  geom_point()



