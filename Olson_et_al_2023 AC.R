library(readxl)
library(dplyr)
library(tidyverse)


Membrane_Variability <- read_excel(
  "Hannah/mcf2lb paper/Membrane Variability_outliers.xlsx")


Membrane_Variability <- pivot_longer(Membrane_Variability, cols = c('1', '2', '3',
'4', '5', '6'), names_to = "time", values_to = "AC" ) %>% transform(time = as.numeric(time))

gd <- Membrane_Variability %>% 
  group_by(genotype, time) %>% 
  summarise(AC = mean(AC, na.rm=TRUE))

ggplot(Membrane_Variability, aes(x = time, y = AC, color = genotype)) +
  geom_point(aes(group = sampleN), alpha = .5) +
  geom_line(data = gd, alpha = .5, size = 1.5) + 
  geom_point(data = gd, alpha = .7, size = 8, shape = 18) +
  geom_smooth(linetype = 0) +
  theme_bw()



