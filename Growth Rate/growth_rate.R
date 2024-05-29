library(tidyverse)
library(scales)

growth_rate <- read_csv('growth_rate.csv')
growth_rate_longer <- growth_rate %>% 
  pivot_longer(cols = -Year,
               names_to = 'category',
               values_to = 'value')

growth_rate_longer %>% 
  ggplot(aes(x = Year,
             y = value,
             color = category)) +
  geom_line() +
  labs(x = 'Year',
       y = 'Growth Rate',
       title = 'Growth Rates',
       subtitle = 'Dividends vs EPS') +
  scale_x_continuous(breaks = seq(2013, 2023, by = 1)) +
  scale_y_continuous(labels = label_percent(scale = 100)) + 
  theme_bw(base_family = 'Ubuntu')
