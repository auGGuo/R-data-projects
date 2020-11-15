# Data Transformation
# Oct 6 2020

library(tidyverse)
library(dplyr)
library(nycflights13)

flights
View(flights)

# working directory
setwd('~/Desktop/r_projects/r_for_datasci')

# select all flights on January 1st with
Jan1_flights <- filter(flights, month == 1, day == 1)
View(Jan1_flights)

# filter exercise ----
# Had an arrival delay of two or more hours
over_2hr_delay <- filter(flights, arr_delay >= 120)

# Flew to Houston (IAH or HOU)
houston <- filter(flights, dest %in% c('IAH', 'HOU'))

# Departed in summer (July, August, and September
summer_flights <- filter(flights, between(month, 7, 9))

# Arrived more than two hours late, but didn’t leave late
flights1 <- filter(flights, arr_delay > 120 & dep_delay <= 0)
filter(flights, arr_delay > 120, dep_delay <= 0)

# Were delayed by at least an hour, but made up over 30 minutes in flight
flights2 <- filter(flights, dep_delay >= 60,  dep_delay > arr_delay + 30)

# Departed between midnight and 6am (inclusive)
flights3 <- filter(flights, between(dep_time, 0, 600))

# check na row in dep_time
sum(is.na(flights$dep_time))
dep_time_na <- filter(flights, is.na(dep_time))

# arrange exercise ----
# 1. use arrange() to sort all missing values to the start? (Hint: use is.na()).
df <- tibble(x = c(5, 2, 1, 3, 6, NA))
arrange(df, desc(!is.na((x))))
arrange(df, desc(x))

dep_time_arranged <- arrange(flights, !is.na(desc(dep_time)))
dep_time_arranged2 <- arrange(flights, desc(is.na(dep_time)))
View(dep_time_arranged2)

# 2. Sort flights to find the most delayed flights. Find the flights that left earliest.
most_delay <- arrange(flights, desc(arr_delay), dep_time)

# 3. Sort flights to find the fastest (highest speed) flights.
fastest_flight <- arrange(flights, desc(distance / air_time))

# 4. Which flights travelled the farthest? Which travelled the shortest?
farthest_flight <- arrange(flights, desc(distance))
shortest_flight <- arrange(flights, distance)

# select exercise ----
# 1. Brainstorm as many ways as possible to select dep_time, dep_delay, arr_time, and arr_delay
select(flights, starts_with('dep'), starts_with('arr'))

# 2. What happens if you include the name of a variable multiple times in a select() call?
select(flights, dep_time, dep_time)

# 3. What does the any_of() function do? Why might it be helpful in conjunction with this vector?
vars <- c("year", "month", "day", "dep_delay", "arr_delay")
select(flights, any_of(vars))


# 4. Does the result of running the following code surprise you? How do the select helpers deal 
# with case by default? How can you change that default?
select(flights, contains("TIME"))
select(flights, contains("TIME", ignore.case = F))

# Mutate
flights_sml <- select(flights, 
                      year:day, 
                      ends_with('delay'),
                      distance, 
                      air_time)

flights_sml <- flights_sml %>% 
  mutate(gain = dep_delay - arr_delay,
         speed = distance / air_time * 60)

flights_sml %>% 
  mutate(gain_per_hour = gain / 60)

flights_sml %>% 
  transmute(gain_per_hour = gain / 60)

flights %>% 
  transmute(dep_time, 
            hour = dep_time %/% 100, 
            min = dep_time %% 100)

# lag & lead ----
x <- c(1:10)
lag(x)
x == lag(x)

y <- c(2, 3, 3, 6, 6)
lead(y)
y != lead(y)

# cumsum, cumprod cummin, cummax, cummean ----
x
cumsum(x)
cummean(x)
cumprod(x)

# mutate exerise
# 1. Currently dep_time and sched_dep_time are convenient to look at, 
# but hard to compute with because they’re not really continuous numbers. 
# Convert them to a more convenient representation of number of minutes since midnight.
dep_time <- select(flights, contains('dep_time'))
dep_time %>% 
  mutate(dep_min  = dep_time %/% 100 * 60 + dep_time %% 100, 
         sched_dep_min = sched_dep_time %/% 100 * 60 + sched_dep_time %% 100)

# 2. Compare air_time with arr_time - dep_time. What do you expect to see? 
# What do you see? What do you need to do to fix it?
flights <- mutate(flights, air_time, arr_time, dep_time,
            arr_time_min = arr_time %/% 100 * 60 + arr_time %% 100,
            dep_time_min = dep_time %/% 100 * 60 + dep_time %% 100, 
            flight_time = arr_time_min - dep_time_min)

sum(flights$air_time == flights$flight_time)
sum(flights$air_time == flights$flight_time, na.rm = T)

# 3. Compare dep_time, sched_dep_time, and dep_delay. 
# How would you expect those three numbers to be related?
flights %>% 
  select(contains('dep'))

# 4. Find the 10 most delayed flights using a ranking function. 
# How do you want to handle ties? Carefully read the documentation for min_rank().
flights %>%  
  arrange(min_rank(desc(dep_delay))) %>% 
  select(year:day, dep_delay, tailnum) %>% 
  head(n = 20)

# 5. What does 1:3 + 1:10 return? Why?
1:3 + 1:10
1:2 + 1:4

# summarise ----
by_day <- flights %>% 
  group_by(year, month, day) %>% 
  summarise(avg_delay = mean(dep_delay, na.rm = T))

delays <- flights %>% 
  group_by(dest) %>% 
  summarise(
  count = n(),
  dist = mean(distance, na.rm = TRUE),
  delay = mean(arr_delay, na.rm = TRUE)
  )

ggplot(data = delays, mapping=aes(x = dist, y = delay)) +
  geom_point(aes(size = count), alpha = 1/3) +
  geom_smooth(se = FALSE)

# missing values ----
not_cancelled <- flights %>% 
  filter(!is.na(arr_time), !is.na(dep_time))
  
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(mean_delay = mean(dep_delay))
  
# counts
# compare the distribution of delay 
delays <- not_cancelled %>% 
  group_by(tailnum) %>% 
  summarise(
    delay = mean(arr_delay,)
  )
ggplot(data = delays, mapping = aes(x = delay)) +
  geom_freqpoly(binwidth = 10)

# comapare the distributionb of delay across the count 
delays <- not_cancelled %>% 
  group_by(tailnum) %>% 
  summarise(
    delay = mean(arr_delay, na.rm = T),
    n =n()
  )

ggplot(data = delays, mapping = aes(x = n, y = delay)) +
  geom_point(aes(alpha = 0.05), show.legend = F)

# filter out the noise setting count > 25
delays %>% 
  filter(n > 25) %>% 
  ggplot(mapping = aes(x = n, y = delay)) +
    geom_point(aes(alpha = 0.05), show.legend = F)

# Convert to a tibble so it prints nicely
batting <- as_tibble(Lahman::Batting)

batters <- batting %>% 
  group_by(playerID) %>% 
  summarise(
    ba = sum(H, na.rm = TRUE) / sum(AB, na.rm = TRUE),
    ab = sum(AB, na.rm = TRUE)
  )

batters %>% 
  filter(ab > 100) %>% 
  ggplot(mapping = aes(x = ab, y = ba)) +
  geom_point() + 
  geom_smooth(se = FALSE)

not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(
    avg_delay1 = mean(arr_delay, na.rm = T),
    avg_delay2 = mean(arr_delay[arr_delay > 0], na.rm = T) # the average positive delay
  )

not_cancelled %>% 
  group_by(dest) %>% 
  summarise(distance_sd = sd(distance)) %>% 
  arrange(desc(distance_sd))

# When do the first and last flights leave each day?
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(
    first = min(dep_time),
    last = max(dep_time)
  )

# Measures of position
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(
    first_dep = first(dep_time),
    third_dep = nth(dep_time, 3),
    last_dep = last(dep_time)
  )

# find out rank of 1st and latest dep_time for each day 
not_cancelled %>% 
  group_by(year, month, day) %>% 
  mutate(r = min_rank(desc(dep_time))) %>%
  select(year:day, r, dep_time) %>% 
  filter(r %in% range(r))

# Which destinations have the most carriers?
not_cancelled %>% 
  group_by(dest) %>% 
  summarise(carriers = n_distinct(carrier)) %>% 
  arrange(desc(carriers))

# “count” (sum) the total number of miles a plane flew
not_cancelled %>% 
  count(tailnum, wt = distance)

# How many flights left before 5am?
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(n_early = sum(dep_time < 500))

# What proportion of flights are delayed by more than an hour?
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(hour_prop = mean(arr_delay > 60, na.rm = T))

# 5.6.7 exercise



