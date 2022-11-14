library(tidyverse)
library(sf)

dat <- readxl::read_excel("data.xlsx", sheet = "2015")

dat <- dat %>% 
  mutate_at(-1, function(x) as.numeric(x))

dat <- dat[apply(dat, 1, function(x) sum(!is.na(x)) > 1), ] %>% 
  select(1, 3) %>% # adatsor oszlopa
  set_names(c('NAME', 'value')) %>% 
  mutate(NAME = ifelse(!str_detect(NAME, 'kerület'), paste(NAME, 'járás'), NAME)) 
  
p <-  merge(dat, read_sf('kozighatarok/admin7.shp'), all.y = T) %>% 
  ggplot() +
  geom_sf(aes(fill = value, geometry = geometry, text = paste0(NAME, ':\n', value)), color ='black') + 
  geom_sf(data = merge(read_sf('kozighatarok/admin9.shp'), dat, all.x = T), 
          mapping = aes(fill = value, geometry = geometry, text = paste0(NAME, ':\n', value)), color = 'black') +
  theme_void() + 
  scale_fill_viridis_c(guide = guide_colorbar(ticks.colour = 'black', frame.colour = 'black')) + 
  labs(fill = NULL, title = 'Lakónépesség 2019 jan 1-én (fő)')

p

library(plotly)

ggplotly(p, tooltip = c('text')) %>% 
  config(displayModeBar = F)
