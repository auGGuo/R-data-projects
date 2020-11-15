# R for Data Sci: Data Visualization 
# Oct 5 2020

library(tidyverse)

# working directory 
setwd('~/Desktop/r_projects/r_for_datasci/')

# load data set 
car <- mpg

# scatter plot ---- 
ggplot(data = car) + 
  geom_point(mapping = aes(x = displ, y = hwy))

ggplot(car) +
  geom_point(mapping = aes(x = cyl, y = hwy))

ggplot(car) + 
  geom_point(aes(x = class, y = drv))

# assign diff color to diff class 
ggplot(car) + 
  geom_point(aes(x = displ, y = hwy, color = class))

# assign diff size to diff class 
ggplot(car) + 
  geom_point(aes(x = displ, y = hwy, size = class))

# assign diff shape to diff class 
ggplot(car) + 
  geom_point(mapping = aes(x = displ, y = hwy, shape = class))

# assigh diff transparancy to diff class 
ggplot(car) + 
  geom_point(aes(x = displ, y = hwy, alpha = class))

# manually set a color for the points
ggplot(car) +
  geom_point(aes(x = displ, y = hwy), color = 'blue')

# map color to a continous varibale
ggplot(car) + 
  geom_point(aes(x = fl, y = class, color = hwy))

# map shape to a continous varibale 
ggplot(car) +
  geom_point(aes(x = fl, y = class, size = hwy))

# map alpha to a continuous variable 
ggplot(car) +
  geom_point(aes(x = fl, y = class, alpha = hwy))

# map color and size to the same varibale 
ggplot(car) +
  geom_point(aes(x = fl, y = hwy, size = class, color = class, shape = class))

# map stroke to a continous var
ggplot(car) +
  geom_point(aes(x = displ, y = fl, stroke = hwy))

# avoid overlapping data using jitter 
ggplot(data = car) +
  geom_point(mapping = aes(x = displ, y = hwy), position = 'jitter')
 
# create facets ----
# facetting on a var 
ggplot(car) + 
  geom_point(aes(x = displ, y = hwy)) + 
  facet_wrap(~ class, nrow = 3)

# facetting on two vars 
ggplot(car) + 
  geom_point(aes(x = displ, y = hwy)) + 
  facet_grid(fl ~ class)

# facetting in 1 dimension in horizontal direction 
ggplot(car) + 
  geom_point(aes(x = displ, y = hwy)) + 
  facet_grid(. ~ class)

# facetting in 1 dimension in vertical direction 
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(drv ~ .)

# compare scatter plot with geom_smooth 
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) 

ggplot(data = mpg) +   
  geom_smooth(aes(x = displ, y = hwy), se = TRUE)

# assign diff linetype and color to diff drv
ggplot(data = mpg) +   
  geom_smooth(aes(x = displ, y = hwy, linetype = drv, color = drv))

# display both point and smooth on the same plot by global mapping under ggplot
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point() +
  geom_smooth()

# assign diff color to point layer 
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(aes(color = class)) +
  geom_smooth()

# only show geom_smooth for subcompact 
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(aes(color = class)) +
  geom_smooth(data = filter(mpg, class == 'subcompact'), se = FALSE)

# global mapping color to drv
ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color = drv)) + 
  geom_point() + 
  geom_smooth(se = T)

# exerise
ggplot(data = car, mapping = aes(x = displ, y = hwy)) + 
  geom_point() + 
  geom_smooth()

ggplot(data = car, mapping = aes(x = displ, y = hwy)) + 
  geom_point() + 
  geom_smooth(data = filter(car, drv == '4'), se = F) +
  geom_smooth(data = filter(car, drv == 'f'), se = F) +
  geom_smooth(data = filter(car, drv == 'r'), se = F)


ggplot(data = car, mapping = aes(x = displ, y = hwy, colour = drv)) +
  geom_point() +
  geom_smooth(se = F)

ggplot(data = car, mapping = aes(x = displ, y = hwy)) +
  geom_point(mapping = aes(color = drv)) +
  geom_smooth(se = F)

ggplot(data = car, mapping = aes(x = displ, y = hwy)) +
  geom_point(mapping = aes(color = drv)) +
  geom_smooth(mapping = aes(linetype = drv), se = F)

ggplot(data = car, mapping = aes(x = displ, y =hwy)) +
  geom_point(mapping = aes(colour = drv))

# bar chart ----

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut))

ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, y = stat(prop), group = 1))

ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, fill = cut))

ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, fill = clarity))

ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, fill = clarity), position = 'dodge2')

# exerise 
ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) + 
  geom_point(position = 'jitter')