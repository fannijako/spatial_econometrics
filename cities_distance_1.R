load("d:/Users/Fanni/Desktop/Területi autokorreláció/data_shp.RData")
LoadLibrary()

#-------------------------------------------------------------------------------
codes <- NUTS_1$NUTS_ID
region_cities_df_1 <- data.frame(NUTS_ID = codes, 
                                 cities = vector(length = length(codes)))

for (code in 1:length(codes)){
  seged <- as.vector(cities$name[which(cities$NUTS_1 == codes[code])])
  if (length(seged) == 0){region_cities_df_1$cities[code] <- c("")
  } else {
    osszes <- vector()
    for (i in seged){osszes <- paste(osszes, i, sep = "; ")}
    region_cities_df_1$cities[code] <- substring(osszes,3)
  }
}
rm(codes, code, seged, osszes, i)
#-------------------------------------------------------------------------------
codes <- NUTS_1$NUTS_ID
region_neighbor_df_1 <- data.frame(NUTS_ID = codes,
                                   neighbor = vector(length = length(codes)))

for (code in 1:length(codes)){
  seged <- NUTS_1$NUTS_ID[which(as.numeric(distance_1[,code]) == 0)]
  if (length(seged) < 2){region_neighbor_df_1$neighbor[code] <- c("")
  } else {
    osszes <- vector()
    for (i in 1:length(seged)){
      if (seged[i] != codes[code]){osszes <- paste(osszes, seged[i], sep = "; ")}}
    region_neighbor_df_1$neighbor[code] <- substring(osszes,3)
  }
}
rm(codes, code, seged, osszes, i)

# Csalagút
index <- which(region_neighbor_df_1$NUTS_ID == "UKJ")
value <- region_neighbor_df_1$neighbor[index]
region_neighbor_df_1$neighbor[index] <- paste(value, "FRE", sep = "; ")

index <- which(region_neighbor_df_1$NUTS_ID == "FRE")
value <- region_neighbor_df_1$neighbor[index]
region_neighbor_df_1$neighbor[index] <- paste(value, "UKJ", sep = "; ")

# Koppenhága - Malmö
index <- which(region_neighbor_df_1$NUTS_ID == "DK0")
value <- region_neighbor_df_1$neighbor[index]
region_neighbor_df_1$neighbor[index] <- paste(value, "SE2", sep = "; ")

index <- which(region_neighbor_df_1$NUTS_ID == "SE2")
value <- region_neighbor_df_1$neighbor[index]
region_neighbor_df_1$neighbor[index] <- paste(value, "DK0", sep = "; ")

# Szicília
index <- which(region_neighbor_df_1$NUTS_ID == "ITF")
value <- region_neighbor_df_1$neighbor[index]
region_neighbor_df_1$neighbor[index] <- paste(value, "ITG", sep = "; ")

index <- which(region_neighbor_df_1$NUTS_ID == "ITG")
value <- region_neighbor_df_1$neighbor[index]
region_neighbor_df_1$neighbor[index] <- paste(value, "ITF", sep = "; ")

rm(index, value)
#-------------------------------------------------------------------------------
codes <- NUTS_1$NUTS_ID
region_neighbor_df_1_2 <- data.frame(NUTS_ID = codes,
                                     neighbor = vector(length = length(codes)))

for (code in 1:length(codes)){
  
  if (region_neighbor_df_1$neighbor[code] == "") {
    region_neighbor_df_1_2$neighbor[code] <- ""
    next
  }
  
  seged <- vector()
  first_neighbor <- strsplit(region_neighbor_df_1$neighbor[code], split = "; ")[[1]]
  for (i in 1:length(first_neighbor)){
    seged <- paste(seged, region_neighbor_df_1$neighbor[which(region_neighbor_df_1$NUTS_ID == first_neighbor[i])], sep = "; ")
  }
  seged <- substring(seged, 3)
  seged <- unique(strsplit(seged, split = "; ")[[1]])
  
  if (length(seged) == 1){
    region_neighbor_df_1_2$neighbor[code] <- ""
    next
  }
  
  seged <- seged[which(seged != codes[code])]
  
  seged_2 <- ""
  for (i in 1:length(seged)){
    if (!any(seged[i] == first_neighbor)){
      seged_2 <- paste(seged_2, seged[i], sep = "; ")
    }
  }
  
  region_neighbor_df_1_2$neighbor[code] <- substring(seged_2, 3)
}

rm(codes, code, seged, first_neighbor, i, seged_2)

#-------------------------------------------------------------------------------
cities_distance_df_1 <- data.frame(city_1 = vector(), city_2 = vector())
for (i in 1:120){
  region_1 <- region_neighbor_df_1$NUTS_ID[i]
  cities_1 <- strsplit(region_cities_df_1$cities[i], split = "; ")[[1]]
  neighbor <- strsplit(region_neighbor_df_1$neighbor[i], split = "; ")[[1]]
  if (length(neighbor) != 0){
    for (j in 1:length(neighbor)){
      neighbor_cities <- cities$name[which(cities$NUTS_1 == neighbor[j])]
      cities_distance_df_1 <- rbind(cities_distance_df_1, 
                                    setNames(expand.grid(cities_1, neighbor_cities), 
                                             names(cities_distance_df_1)))
    }
  }
}
rm(region_1, cities_1, neighbor, j, i, neighbor_cities)

cities_distance_df_1$save <- vector(length = length(cities_distance_df_1$city_1))
for (i in 1:length(cities_distance_df_1$city_1)){
  first <- as.character(cities_distance_df_1$city_1[i])
  second <- as.character(cities_distance_df_1$city_2[i])
  if (first < second){cities_distance_df_1$save[i] <- TRUE}
}

rm(i, first, second)

cities_distance_df_1 <- cities_distance_df_1 %>%  filter(save == TRUE)
cities_distance_df_1 <- cities_distance_df_1 %>% select(-save)

region_1_l <-  c("UKJ", "FRE", "DK0", "SE2", "ITG", "ITF")
region_2_l <- c("FRE", "UKJ", "SE2", "DK0", "ITF", "ITG")
for (i in 1:6){
  region_1 <- region_1_l[i]
  index <- which(region_cities_df_1$NUTS_ID == region_1)
  cities_1 <- strsplit(region_cities_df_1$cities[index], split = "; ")[[1]]
  neighbor <- region_2_l[i]

  neighbor_cities <- cities$name[which(cities$NUTS_1 == neighbor)]
  new_rows <- as.data.frame(expand.grid(cities_1, neighbor_cities))
  colnames(new_rows) <- colnames(cities_distance_df_1)[1:2]
  new_rows$km <- vector(length = length(new_rows$city_1))
  new_rows$hour <- vector(length = length(new_rows$city_1))
  new_rows$lat_1 <- vector(length = length(new_rows$city_1))
  new_rows$long_1 <- vector(length = length(new_rows$city_1))
  new_rows$geocode_1 <- vector(length = length(new_rows$city_1))
  new_rows$NUTS_1_1 <- vector(length = length(new_rows$city_1))
  new_rows$NUTS_2_1 <- vector(length = length(new_rows$city_1))
  new_rows$NUTS_3_1 <- vector(length = length(new_rows$city_1))
  new_rows$lat_2 <- vector(length = length(new_rows$city_1))
  new_rows$long_2 <- vector(length = length(new_rows$city_1))
  new_rows$geocode_2 <- vector(length = length(new_rows$city_1))
  new_rows$NUTS_1_2 <- vector(length = length(new_rows$city_1))
  new_rows$NUTS_2_2 <- vector(length = length(new_rows$city_1))
  new_rows$NUTS_3_2 <- vector(length = length(new_rows$city_1))
  
  cities_distance_df_1 <- rbind(cities_distance_df_1, new_rows)
}
rm(new_rows, region_1_l, region_2_l, i, region_1, index, cities_1, neighbor, neighbor_cities)

#-------------------------------------------------------------------------------
cities_distance_df_1_2 <- data.frame(city_1 = vector(), city_2 = vector())
for (i in 1:100){
  region_1 <- region_neighbor_df_1_2$NUTS_ID[i]
  cities_1 <- strsplit(region_cities_df_1$cities[i], split = "; ")[[1]]
  neighbor <- strsplit(region_neighbor_df_1_2$neighbor[i], split = "; ")[[1]]
  if (length(neighbor) != 0){
    for (j in 1:length(neighbor)){
      neighbor_cities <- cities$name[which(cities$NUTS_1 == neighbor[j])]
      cities_distance_df_1_2 <- rbind(cities_distance_df_1_2, 
                                      setNames(expand.grid(cities_1, neighbor_cities), 
                                               names(cities_distance_df_1_2)))
    }
  }
}
rm(region_1, cities_1, neighbor, j, i, neighbor_cities)

cities_distance_df_1_2$save <- vector(length = length(cities_distance_df_1_2$city_1))
for (i in 1:length(cities_distance_df_1_2$city_1)){
  first <- as.character(cities_distance_df_1_2$city_1[i])
  second <- as.character(cities_distance_df_1_2$city_2[i])
  if (first < second){cities_distance_df_1_2$save[i] <- TRUE}
}

rm(i, first, second)

cities_distance_df_1_2 <- cities_distance_df_1_2 %>%  filter(save == TRUE)
cities_distance_df_1_2 <- cities_distance_df_1_2 %>% select(-save)

region_1_l <-  c("UKJ", "FRE", "DK0", "SE2", "ITG", "ITF")
region_2_l <- c("FRE", "UKJ", "SE2", "DK0", "ITF", "ITG")

for (i in 1:6){
  region_1 <- region_1_l[i]
  index <- which(region_cities_df_1$NUTS_ID == region_1)
  cities_1 <- strsplit(region_cities_df_1$cities[index], split = "; ")[[1]]
  
  neighbor <- region_2_l[i]
  neighbor_2 <- region_neighbor_df_1$neighbor[which(region_neighbor_df_1$NUTS_ID == neighbor)]
  neighbor_2 <- strsplit(neighbor_2, split = "; ")[[1]]
  neighbor_2 <- neighbor_2[which(neighbor_2 != neighbor)]
  
  if (length(neighbor_2) == 0) next
  neighbor_cities <- vector()
  for (j in 1:length(neighbor_2)){
    neighbor_cities <- c(neighbor_cities, 
                         cities$name[which(cities$NUTS_1 == neighbor_2[j])])
  }
  
  new_rows <- as.data.frame(expand.grid(cities_1, neighbor_cities))
  colnames(new_rows) <- colnames(cities_distance_df_1_2)[1:2]
  new_rows$km <- vector(length = length(new_rows$city_1))
  new_rows$hour <- vector(length = length(new_rows$city_1))
  new_rows$lat_1 <- vector(length = length(new_rows$city_1))
  new_rows$long_1 <- vector(length = length(new_rows$city_1))
  new_rows$geocode_1 <- vector(length = length(new_rows$city_1))
  new_rows$NUTS_1_1 <- vector(length = length(new_rows$city_1))
  new_rows$NUTS_2_1 <- vector(length = length(new_rows$city_1))
  new_rows$NUTS_3_1 <- vector(length = length(new_rows$city_1))
  new_rows$lat_2 <- vector(length = length(new_rows$city_1))
  new_rows$long_2 <- vector(length = length(new_rows$city_1))
  new_rows$geocode_2 <- vector(length = length(new_rows$city_1))
  new_rows$NUTS_1_2 <- vector(length = length(new_rows$city_1))
  new_rows$NUTS_2_2 <- vector(length = length(new_rows$city_1))
  new_rows$NUTS_3_2 <- vector(length = length(new_rows$city_1))
  
  cities_distance_df_1_2 <- rbind(cities_distance_df_1_2, new_rows)
}
rm(new_rows, region_1_l, region_2_l, i,j, region_1, index, cities_1, 
   neighbor, neighbor_2, neighbor_cities)

save.image("d:/Users/Fanni/Desktop/Területi autokorreláció/data_shp.RData")

#-------------------------------------------------------------------------------

register_google(key = "AIzaSyD043L4r5yqacnCwv465zE0muf6vnTr4s0")

cities_distance_df_1$km <- vector(length = length(cities_distance_df_1$city_1))
cities_distance_df_1$hour <- vector(length = length(cities_distance_df_1$city_1))

for (i in 1:24646){
  result <- data.frame(mapdist(as.character(cities_distance_df_1$city_1[i]), 
                               as.character(cities_distance_df_1$city_2[i]), 
                               override_limit = TRUE))
  
  if (length(result$km) == 0) next
  cities_distance_df_1$km[i] <- result$km
  cities_distance_df_1$hour[i] <- result$hours
  
  if (i %% 1000 == 0){
    save.image("d:/Users/Fanni/Desktop/Területi autokorreláció/data_shp.RData")
    print(i)}
}

cities_distance_df_1 <- cities_distance_df_1 %>% filter(km > 0)
rm(result, i)
save.image("d:/Users/Fanni/Desktop/Területi autokorreláció/data_shp.RData")

#-------------------------------------------------------------------------------

register_google(key = "AIzaSyD043L4r5yqacnCwv465zE0muf6vnTr4s0")

index <- which(cities_distance_df_1$km == 0)

for (i in index){
  result <- data.frame(mapdist(as.character(cities_distance_df_1$city_1[i]), 
                               as.character(cities_distance_df_1$city_2[i]), 
                               override_limit = TRUE))
  
  if (length(result$km) == 0) next
  cities_distance_df_1$km[i] <- result$km
  cities_distance_df_1$hour[i] <- result$hours
  
}

cities_distance_df_1 <- cities_distance_df_1 %>% filter(km > 0)
rm(result, i, index)
save.image("d:/Users/Fanni/Desktop/Területi autokorreláció/data_shp.RData")

#-------------------------------------------------------------------------------

register_google(key = "AIzaSyD043L4r5yqacnCwv465zE0muf6vnTr4s0")

cities_distance_df_1_2$km <- vector(length = length(cities_distance_df_1_2$city_1))
cities_distance_df_1_2$hour <- vector(length = length(cities_distance_df_1_2$city_1))

for (i in 1:36800){
  result <- data.frame(mapdist(as.character(cities_distance_df_1_2$city_1[i]), 
                               as.character(cities_distance_df_1_2$city_2[i]), 
                               override_limit = TRUE))
  
  if (length(result$km) == 0) next
  cities_distance_df_1_2$km[i] <- result$km
  cities_distance_df_1_2$hour[i] <- result$hours
  
  if (i %% 1000 == 0){
    save.image("d:/Users/Fanni/Desktop/Területi autokorreláció/data_shp.RData")
    print(i)}
}

cities_distance_df_1_2 <- cities_distance_df_1_2 %>% filter(km > 0)
rm(result, i)
save.image("d:/Users/Fanni/Desktop/Területi autokorreláció/data_shp.RData")

#-------------------------------------------------------------------------------

register_google(key = "AIzaSyD043L4r5yqacnCwv465zE0muf6vnTr4s0")

index <- which(cities_distance_df_1_2$km == 0)

for (i in index){
  result <- data.frame(mapdist(as.character(cities_distance_df_1_2$city_1[i]), 
                               as.character(cities_distance_df_1_2$city_2[i]), 
                               override_limit = TRUE))
  
  if (length(result$km) == 0) next
  cities_distance_df_1_2$km[i] <- result$km
  cities_distance_df_1_2$hour[i] <- result$hours
  
}

cities_distance_df_1_2 <- cities_distance_df_1_2 %>% filter(km > 0)
rm(result, i, index)
save.image("d:/Users/Fanni/Desktop/Területi autokorreláció/data_shp.RData")

#-------------------------------------------------------------------------------

colnames(cities)[1] <- "city_1"
cities_distance_df_1 <- left_join(cities_distance_df_1, cities, "city_1", suffix = c("_1", "_2"))
colnames(cities_distance_df_1)[5:10] <- c("lat_1", "long_1", "geocode_1", 
                                          "NUTS_1_1", "NUTS_2_1", "NUTS_3_1")

colnames(cities)[1] <- "city_2"
cities_distance_df_1 <- left_join(cities_distance_df_1, cities, "city_2", suffix = c("_1", "_2"))
colnames(cities_distance_df_1)[11:16] <- c("lat_2", "long_2", "geocode_2", 
                                           "NUTS_1_2", "NUTS_2_2", "NUTS_3_2")

colnames(cities)[1] <- "name"

save.image("d:/Users/Fanni/Desktop/Területi autokorreláció/data_shp.RData")

#-------------------------------------------------------------------------------

colnames(cities)[1] <- "city_1"
cities_distance_df_1_2 <- left_join(cities_distance_df_1_2, cities, "city_1", suffix = c("_1", "_2"))
colnames(cities_distance_df_1_2)[5:10] <- c("lat_1", "long_1", "geocode_1", 
                                            "NUTS_1_1", "NUTS_2_1", "NUTS_3_1")

colnames(cities)[1] <- "city_2"
cities_distance_df_1_2 <- left_join(cities_distance_df_1_2, cities, "city_2", suffix = c("_1", "_2"))
colnames(cities_distance_df_1_2)[11:16] <- c("lat_2", "long_2", "geocode_2", 
                                            "NUTS_1_2", "NUTS_2_2", "NUTS_3_2")

colnames(cities)[1] <- "name"

save.image("d:/Users/Fanni/Desktop/Területi autokorreláció/data_shp.RData")

#-------------------------------------------------------------------------------

index <- which(cities_distance_df_1$lat_1 == FALSE)

for (i in index){
  city_1 <- cities_distance_df_1$city_1[i]
  city_2 <- cities_distance_df_1$city_2[i]
  index_1 <- which(cities$name == city_1)
  index_2 <- which(cities$name == city_2)
  
  cities_distance_df_1$lat_1[index] <- cities$lat[index_1]
  cities_distance_df_1$long_1[index] <- cities$long[index_1]
  cities_distance_df_1$geocode_1[index] <- cities$geocode[index_1]
  cities_distance_df_1$NUTS_1_1[index] <- cities$NUTS_1[index_1]
  cities_distance_df_1$NUTS_2_1[index] <- cities$NUTS_2[index_1]
  cities_distance_df_1$NUTS_3_1[index] <- cities$NUTS_3[index_1]
  
  cities_distance_df_1$lat_2[index] <- cities$lat[index_2]
  cities_distance_df_1$long_2[index] <- cities$long[index_2]
  cities_distance_df_1$geocode_2[index] <- cities$geocode[index_2]
  cities_distance_df_1$NUTS_1_2[index] <- cities$NUTS_1[index_2]
  cities_distance_df_1$NUTS_2_2[index] <- cities$NUTS_2[index_2]
  cities_distance_df_1$NUTS_3_2[index] <- cities$NUTS_3[index_2]
  
}

rm(i, city_1, city_2, index_1, index_2)
save.image("d:/Users/Fanni/Desktop/Területi autokorreláció/data_shp.RData")

#-------------------------------------------------------------------------------

index <- which(cities_distance_df_1_2$lat_1 == FALSE)

for (i in index){
  city_1 <- cities_distance_df_1_2$city_1[i]
  city_2 <- cities_distance_df_1_2$city_2[i]
  index_1 <- which(cities$name == city_1)
  index_2 <- which(cities$name == city_2)
  
  cities_distance_df_1_2$lat_1[index] <- cities$lat[index_1]
  cities_distance_df_1_2$long_1[index] <- cities$long[index_1]
  cities_distance_df_1_2$geocode_1[index] <- cities$geocode[index_1]
  cities_distance_df_1_2$NUTS_1_1[index] <- cities$NUTS_1[index_1]
  cities_distance_df_1_2$NUTS_2_1[index] <- cities$NUTS_2[index_1]
  cities_distance_df_1_2$NUTS_3_1[index] <- cities$NUTS_3[index_1]
  
  cities_distance_df_1_2$lat_2[index] <- cities$lat[index_2]
  cities_distance_df_1_2$long_2[index] <- cities$long[index_2]
  cities_distance_df_1_2$geocode_2[index] <- cities$geocode[index_2]
  cities_distance_df_1_2$NUTS_1_2[index] <- cities$NUTS_1[index_2]
  cities_distance_df_1_2$NUTS_2_2[index] <- cities$NUTS_2[index_2]
  cities_distance_df_1_2$NUTS_3_2[index] <- cities$NUTS_3[index_2]
}

rm(i, city_1, city_2, index_1, index_2)
save.image("d:/Users/Fanni/Desktop/Területi autokorreláció/data_shp.RData")

#------------------------------------------------------------------------------
# ITF - ITG probléma
region_1_l <- "ITF"
region_2_l <- "ITG"
for (i in 1:1){
  region_1 <- region_1_l[i]
  index <- which(region_cities_df_1$NUTS_ID == region_1)
  cities_1 <- strsplit(region_cities_df_1$cities[index], split = "; ")[[1]]
  neighbor <- region_2_l[i]
  
  neighbor_cities <- cities$name[which(cities$NUTS_1 == neighbor)]
  new_rows <- as.data.frame(expand.grid(cities_1, neighbor_cities))
  colnames(new_rows) <- colnames(cities_distance_df_1)[1:2]
  new_rows$km <- vector(length = length(new_rows$city_1))
  new_rows$hour <- vector(length = length(new_rows$city_1))
  new_rows$lat_1 <- vector(length = length(new_rows$city_1))
  new_rows$long_1 <- vector(length = length(new_rows$city_1))
  new_rows$geocode_1 <- vector(length = length(new_rows$city_1))
  new_rows$NUTS_1_1 <- vector(length = length(new_rows$city_1))
  new_rows$NUTS_2_1 <- vector(length = length(new_rows$city_1))
  new_rows$NUTS_3_1 <- vector(length = length(new_rows$city_1))
  new_rows$lat_2 <- vector(length = length(new_rows$city_1))
  new_rows$long_2 <- vector(length = length(new_rows$city_1))
  new_rows$geocode_2 <- vector(length = length(new_rows$city_1))
  new_rows$NUTS_1_2 <- vector(length = length(new_rows$city_1))
  new_rows$NUTS_2_2 <- vector(length = length(new_rows$city_1))
  new_rows$NUTS_3_2 <- vector(length = length(new_rows$city_1))
  
  cities_distance_df_1 <- rbind(cities_distance_df_1, new_rows)
}
rm(new_rows, region_1_l, region_2_l, i, region_1, index, cities_1, neighbor, neighbor_cities)

register_google(key = "AIzaSyD043L4r5yqacnCwv465zE0muf6vnTr4s0")

index <- which(cities_distance_df_1$km == 0)

for (i in index){
  result <- data.frame(mapdist(as.character(cities_distance_df_1$city_1[i]), 
                               as.character(cities_distance_df_1$city_2[i]), 
                               override_limit = TRUE))
  
  if (length(result$km) == 0) next
  cities_distance_df_1$km[i] <- result$km
  cities_distance_df_1$hour[i] <- result$hours
  
}

cities_distance_df_1 <- cities_distance_df_1 %>% filter(km > 0)
rm(result, i, index)
save.image("d:/Users/Fanni/Desktop/Területi autokorreláció/data_shp.RData")

index <- which(cities_distance_df_1$lat_1 == FALSE)

for (i in index){
  city_1 <- cities_distance_df_1$city_1[i]
  city_2 <- cities_distance_df_1$city_2[i]
  index_1 <- which(cities$name == city_1)
  index_2 <- which(cities$name == city_2)
  
  cities_distance_df_1$lat_1[index] <- cities$lat[index_1]
  cities_distance_df_1$long_1[index] <- cities$long[index_1]
  cities_distance_df_1$geocode_1[index] <- cities$geocode[index_1]
  cities_distance_df_1$NUTS_1_1[index] <- cities$NUTS_1[index_1]
  cities_distance_df_1$NUTS_2_1[index] <- cities$NUTS_2[index_1]
  cities_distance_df_1$NUTS_3_1[index] <- cities$NUTS_3[index_1]
  
  cities_distance_df_1$lat_2[index] <- cities$lat[index_2]
  cities_distance_df_1$long_2[index] <- cities$long[index_2]
  cities_distance_df_1$geocode_2[index] <- cities$geocode[index_2]
  cities_distance_df_1$NUTS_1_2[index] <- cities$NUTS_1[index_2]
  cities_distance_df_1$NUTS_2_2[index] <- cities$NUTS_2[index_2]
  cities_distance_df_1$NUTS_3_2[index] <- cities$NUTS_3[index_2]
  
}

rm(i, city_1, city_2, index_1, index_2)
save.image("d:/Users/Fanni/Desktop/Területi autokorreláció/data_shp.RData")

seged <- cities_distance_df_1 %>% filter((NUTS_1_1 == "ITG" & NUTS_1_2 == "ITF") | (NUTS_1_2 == "ITG" & NUTS_1_1 == "ITF") )
hist(seged$hour)
distance_mean_hour_1[99, 58] <- mean(seged$hour)
distance_mean_hour_1[58, 99] <- mean(seged$hour)
distance_mean_hour_1_2[99, 58] <- mean(seged$hour)
distance_mean_hour_1_2[58, 99] <- mean(seged$hour)

distance_mean_km_1[99, 58] <- mean(seged$km)
distance_mean_km_1[58, 99] <- mean(seged$km)
distance_mean_km_1_2[99, 58] <- mean(seged$km)
distance_mean_km_1_2[58, 99] <- mean(seged$km)

distance_sd_km_1[99, 58] <- sd(seged$km)
distance_sd_km_1[58, 99] <- sd(seged$km)
distance_sd_km_1_2[99, 58] <- sd(seged$km)
distance_sd_km_1_2[58, 99] <- sd(seged$km)

distance_sd_hour_1[99, 58] <- sd(seged$hour)
distance_sd_hour_1[58, 99] <- sd(seged$hour)
distance_sd_hour_1_2[99, 58] <- sd(seged$hour)
distance_sd_hour_1_2[58, 99] <- sd(seged$hour)

#-----------------------------------------------------------------------------
seged <- cities_distance_df_1 %>% filter((NUTS_1_1 == "DE3") | (NUTS_1_2 == "DE3"))
distance_mean_hour_1[12, 19] <- mean(seged$hour)
distance_mean_hour_1[19, 12] <- mean(seged$hour)
distance_mean_hour_1_2[12, 19] <- mean(seged$hour)
distance_mean_hour_1_2[19, 12] <- mean(seged$hour)

distance_mean_km_1[12, 19] <- mean(seged$km)
distance_mean_km_1[19, 12] <- mean(seged$km)
distance_mean_km_1_2[12, 19] <- mean(seged$km)
distance_mean_km_1_2[19, 12] <- mean(seged$km)

distance_sd_km_1[12, 19] <- sd(seged$km)
distance_sd_km_1[19, 12] <- sd(seged$km)
distance_sd_km_1_2[12, 19] <- sd(seged$km)
distance_sd_km_1_2[19, 12] <- sd(seged$km)

distance_sd_hour_1[12, 19] <- sd(seged$hour)
distance_sd_hour_1[19, 12] <- sd(seged$hour)
distance_sd_hour_1_2[12, 19] <- sd(seged$hour)
distance_sd_hour_1_2[19, 12] <- sd(seged$hour)