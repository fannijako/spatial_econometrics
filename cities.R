load("d:/Users/Fanni/Desktop/Területi autokorreláció/data_shp.RData")
LoadLibrary()
load("d:/Users/Fanni/Desktop/Területi autokorreláció/matrix_NUTS_3.RData")
#-------------------------------------------------------------------------------
# cities <- read_excel("d:/Users/Fanni/Downloads/urb_cpopstr_page_spreadsheet.xlsx", 
#                       sheet = "Structure", skip = 4)
# cities <- cities[1:1075,2]
# colnames(cities) <- "name"

#lon <- vector()
#lat <- vector()
#city <- vector()

#for (i in 901:1075){
#  register_google(key = "AIzaSyAJRy3WV69mtWNsFFDf-pfBNHZmN8OAGWI")
#  code <- geocode(cities$name[i])
#  city[i] <- cities$name[i]
#  lon[i] <- code$lon
#  lat[i] <- code$lat
#}

# df <- data.frame(name = city, lat = lat, long = lon)
# write_xlsx(df,"d:/Users/Fanni/Downloads/cities.xlsx")

#------------------------------------------------------------------------------
# cities_2 <- read_excel("d:/Users/Fanni/Downloads/urb_cpopstr_page_spreadsheet.xlsx", 
#                       sheet = "Structure")

#lon <- vector()
#lat <- vector()
#city <- vector()

#for (i in 1:1006){
#  register_google(key = "AIzaSyD043L4r5yqacnCwv465zE0muf6vnTr4s0")
#  code <- geocode(cities_2$whole[i])
#  city[i] <- cities_2$whole[i]
#  lon[i] <- code$lon
#  lat[i] <- code$lat
#}

# df <- data.frame(name = city, lat = lat, long = lon)
# write_xlsx(df,"d:/Users/Fanni/Downloads/cities_long_lat, 2.xlsx")

png("cities.png", width = 2500, height = 700, units = "px")
NUTS_3 %>% ggplot() + ggtitle("Városi területek és a terület fő városai\n") + 
  geom_sf(aes(fill = factor(URBN_TYPE), geometry = geometry))  +
  geom_point(data = cities, aes(x = long, y = lat), size = 1, 
             shape = 23, fill = "red") +
  theme_minimal()+
  theme(plot.title = element_text(color="black", size=25, hjust=0.5, face = "bold"),
        axis.text.y = element_text(color="black", size=16, hjust=0.5, face = "bold"),
        axis.text.x = element_text(color="black", size=16, face = "bold"),  
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),  
        axis.ticks.x=element_blank(),
        text = element_text(family = "serif"),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 16, family = "serif"),
        legend.background = element_rect( size=.5)) +
  paletteer::scale_fill_paletteer_d("nord::algoma_forest")
dev.off()

gc_sfg <- vector()
for (i in 1:993){
  gc_sfg[i] <- st_transform(st_sfc(st_point(c(cities$long[i], cities$lat[i])), 
                                   crs = 4326), 
                            crs = "+init=epsg:4326")
}
cities$geocode <- gc_sfg
rm(i)
save.image("d:/Users/Fanni/Desktop/Területi autokorreláció/data_shp.RData")