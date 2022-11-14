load("d:/Users/Fanni/Desktop/Területi autokorreláció/data_shp.RData")
LoadLibrary()

#-------------------------------------------------------------------------------
codes <- NUTS_2$NUTS_ID
region_cities_df_2 <- data.frame(NUTS_ID = codes, 
                                 cities = vector(length = length(codes)))

for (code in 1:length(codes)){
  seged <- as.vector(cities$name[which(cities$NUTS_2 == codes[code])])
  if (length(seged) == 0){region_cities_df_2$cities[code] <- c("")
  } else {
    osszes <- vector()
    for (i in seged){osszes <- paste(osszes, i, sep = "; ")}
    region_cities_df_2$cities[code] <- substring(osszes,3)
  }
}
rm(codes, code, seged, osszes, i)
#-------------------------------------------------------------------------------
codes <- NUTS_2$NUTS_ID
region_neighbor_df_2 <- data.frame(NUTS_ID = codes,
                                   neighbor = vector(length = length(codes)))

for (code in 1:length(codes)){
  seged <- NUTS_2$NUTS_ID[which(as.numeric(distance_2[,code]) == 0)]
  if (length(seged) < 2){region_neighbor_df_2$neighbor[code] <- c("")
  } else {
    osszes <- vector()
    for (i in 1:length(seged)){
      if (seged[i] != codes[code]){osszes <- paste(osszes, seged[i], sep = "; ")}}
    region_neighbor_df_2$neighbor[code] <- substring(osszes,3)
  }
}
rm(codes, code, seged, osszes, i)

# Csalagút
index <- which(region_neighbor_df_2$NUTS_ID == "UKJ4")
value <- region_neighbor_df_2$neighbor[index]
region_neighbor_df_2$neighbor[index] <- paste(value, "FRE1", sep = "; ")

index <- which(region_neighbor_df_2$NUTS_ID == "FRE1")
value <- region_neighbor_df_2$neighbor[index]
region_neighbor_df_2$neighbor[index] <- paste(value, "UKJ4", sep = "; ")

# Koppenhága - Malmö
index <- which(region_neighbor_df_2$NUTS_ID == "DK01")
value <- region_neighbor_df_2$neighbor[index]
region_neighbor_df_2$neighbor[index] <- paste(value, "SE22", sep = "; ")

index <- which(region_neighbor_df_2$NUTS_ID == "SE22")
value <- region_neighbor_df_2$neighbor[index]
region_neighbor_df_2$neighbor[index] <- paste(value, "DK01", sep = "; ")

# Szicília
index <- which(region_neighbor_df_2$NUTS_ID == "ITF6")
value <- region_neighbor_df_2$neighbor[index]
region_neighbor_df_2$neighbor[index] <- paste(value, "ITG1", sep = "; ")

index <- which(region_neighbor_df_2$NUTS_ID == "ITG1")
value <- region_neighbor_df_2$neighbor[index]
region_neighbor_df_2$neighbor[index] <- paste(value, "ITF6", sep = "; ")

rm(index, value)

#-------------------------------------------------------------------------------
codes <- NUTS_2$NUTS_ID
region_neighbor_df_2_2 <- data.frame(NUTS_ID = codes,
                                     neighbor = vector(length = length(codes)))

for (code in 1:length(codes)){
  
  if (region_neighbor_df_2$neighbor[code] == "") {
    region_neighbor_df_2_2$neighbor[code] <- ""
    next
  }
  
  seged <- vector()
  first_neighbor <- strsplit(region_neighbor_df_2$neighbor[code], split = "; ")[[1]]
  for (i in 1:length(first_neighbor)){
    seged <- paste(seged, region_neighbor_df_2$neighbor[which(region_neighbor_df_2$NUTS_ID == first_neighbor[i])], sep = "; ")
  }
  seged <- substring(seged, 3)
  seged <- unique(strsplit(seged, split = "; ")[[1]])
  
  if (length(seged) == 1){
    region_neighbor_df_2_2$neighbor[code] <- ""
    next
  }
  
  seged <- seged[which(seged != codes[code])]
  
  seged_2 <- ""
  for (i in 1:length(seged)){
    if (!any(seged[i] == first_neighbor)){
      seged_2 <- paste(seged_2, seged[i], sep = "; ")
    }
  }
  
  region_neighbor_df_2_2$neighbor[code] <- substring(seged_2, 3)
}

rm(codes, code, seged, first_neighbor, i, seged_2)
#-------------------------------------------------------------------------------
cities_distance_df_2 <- data.frame(city_1 = vector(), city_2 = vector())

for (i in 1:324){
  region_1 <- region_neighbor_df_2$NUTS_ID[i]
  cities_1 <- strsplit(region_cities_df_2$cities[i], split = "; ")[[1]]
  neighbor <- strsplit(region_neighbor_df_2$neighbor[i], split = "; ")[[1]]
  if (length(neighbor) != 0){
    for (j in 1:length(neighbor)){
      neighbor_cities <- cities$name[which(cities$NUTS_2 == neighbor[j])]
      cities_distance_df_2 <- rbind(cities_distance_df_2, 
                                  setNames(expand.grid(cities_1, neighbor_cities), 
                                           names(cities_distance_df_2)))
    }
  }
}
rm(region_1, cities_1, neighbor, j, i, neighbor_cities)

cities_distance_df_2$save <- vector(length = length(cities_distance_df_2$city_1))
for (i in 1:length(cities_distance_df_2$city_1)){
  first <- as.character(cities_distance_df_2$city_1[i])
  second <- as.character(cities_distance_df_2$city_2[i])
  if (first < second){cities_distance_df_2$save[i] <- TRUE}
}

rm(i, first, second)

cities_distance_df_2 <- cities_distance_df_2 %>%  filter(save == TRUE)
cities_distance_df_2 <- cities_distance_df_2 %>% select(-save)

region_1_l <-  c("UKJ4", "FRE1", "DK01", "SE22", "ITG1", "ITF6")
region_2_l <- c("FRE1", "UKJ4", "SE22", "DK01", "ITF6", "ITG1")
for (i in 1:6){
  region_1 <- region_1_l[i]
  index <- which(region_cities_df_2$NUTS_ID == region_1)
  cities_1 <- strsplit(region_cities_df_2$cities[index], split = "; ")[[1]]
  neighbor <- region_2_l[i]
  
  neighbor_cities <- cities$name[which(cities$NUTS_2 == neighbor)]
  new_rows <- as.data.frame(expand.grid(cities_1, neighbor_cities))
  colnames(new_rows) <- colnames(cities_distance_df_2)[1:2]
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
  
  cities_distance_df_2 <- rbind(cities_distance_df_2, new_rows)
}
rm(new_rows, region_1_l, region_2_l, i, region_1, index, cities_1, neighbor, neighbor_cities)

#-------------------------------------------------------------------------------
cities_distance_df_2_2 <- data.frame(city_1 = vector(), city_2 = vector())
for (i in 1:324){
  region_1 <- region_neighbor_df_2_2$NUTS_ID[i]
  cities_1 <- strsplit(region_cities_df_2$cities[i], split = "; ")[[1]]
  neighbor <- strsplit(region_neighbor_df_2_2$neighbor[i], split = "; ")[[1]]
  if (length(neighbor) != 0){
    for (j in 1:length(neighbor)){
      neighbor_cities <- cities$name[which(cities$NUTS_2 == neighbor[j])]
      cities_distance_df_2_2 <- rbind(cities_distance_df_2_2, 
                                      setNames(expand.grid(cities_1, neighbor_cities), 
                                               names(cities_distance_df_2_2)))
    }
  }
}
rm(region_1, cities_1, neighbor, j, i, neighbor_cities)

cities_distance_df_2_2$save <- vector(length = length(cities_distance_df_2_2$city_1))
for (i in 1:length(cities_distance_df_2_2$city_1)){
  first <- as.character(cities_distance_df_2_2$city_1[i])
  second <- as.character(cities_distance_df_2_2$city_2[i])
  if (first < second){cities_distance_df_2_2$save[i] <- TRUE}
}

rm(i, first, second)
cities_distance_df_2_2 <- cities_distance_df_2_2 %>%  filter(save == TRUE)
cities_distance_df_2_2 <- cities_distance_df_2_2 %>% select(-save)

region_1_l <-  c("UKJ4", "FRE1", "DK01", "SE22", "ITG1", "ITF6")
region_2_l <- c("FRE1", "UKJ4", "SE22", "DK01", "ITF6", "ITG1")

for (i in 1:6){
  region_1 <- region_1_l[i]
  index <- which(region_cities_df_2$NUTS_ID == region_1)
  cities_1 <- strsplit(region_cities_df_2$cities[index], split = "; ")[[1]]
  
  neighbor <- region_2_l[i]
  neighbor_2 <- region_neighbor_df_2$neighbor[which(region_neighbor_df_2$NUTS_ID == neighbor)]
  neighbor_2 <- strsplit(neighbor_2, split = "; ")[[1]]
  neighbor_2 <- neighbor_2[which(neighbor_2 != neighbor)]
  
  if (length(neighbor_2) == 0) next
  neighbor_cities <- vector()
  for (j in 1:length(neighbor_2)){
    neighbor_cities <- c(neighbor_cities, 
                         cities$name[which(cities$NUTS_2 == neighbor_2[j])])
  }
  
  new_rows <- as.data.frame(expand.grid(cities_1, neighbor_cities))
  colnames(new_rows) <- colnames(cities_distance_df_2_2)[1:2]
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
  
  cities_distance_df_2_2 <- rbind(cities_distance_df_2_2, new_rows)
}
rm(new_rows, region_1_l, region_2_l, i,j, region_1, index, cities_1, 
   neighbor, neighbor_2, neighbor_cities)

save.image("d:/Users/Fanni/Desktop/Területi autokorreláció/data_shp.RData")
#-------------------------------------------------------------------------------

register_google(key = "AIzaSyD043L4r5yqacnCwv465zE0muf6vnTr4s0")

cities_distance_df_2$km <- vector(length = length(cities_distance_df_2$city_1))
cities_distance_df_2$hour <- vector(length = length(cities_distance_df_2$city_1))

for (i in 1:9603){
  result <- data.frame(mapdist(as.character(cities_distance_df_2$city_1[i]), 
                               as.character(cities_distance_df_2$city_2[i]), 
                               override_limit = TRUE))
  cities_distance_df_2$km[i] <- result$km
  cities_distance_df_2$hour[i] <- result$hours
  
  if (i %% 1000 == 0){
    save.image("d:/Users/Fanni/Desktop/Területi autokorreláció/data_shp.RData")
    print(i)}
}

cities_distance_df_2 <- cities_distance_df_2 %>% filter(km > 0)
rm(result, i)
save.image("d:/Users/Fanni/Desktop/Területi autokorreláció/data_shp.RData")

#-------------------------------------------------------------------------------

register_google(key = "AIzaSyD043L4r5yqacnCwv465zE0muf6vnTr4s0")

index <- which(cities_distance_df_2$km == 0)

for (i in index){
  result <- data.frame(mapdist(as.character(cities_distance_df_2$city_1[i]), 
                               as.character(cities_distance_df_2$city_2[i]), 
                               override_limit = TRUE))
  
  if (length(result$km) == 0) next
  cities_distance_df_2$km[i] <- result$km
  cities_distance_df_2$hour[i] <- result$hours
  
}

cities_distance_df_2 <- cities_distance_df_2 %>% filter(km > 0)
rm(result, i, index)
save.image("d:/Users/Fanni/Desktop/Területi autokorreláció/data_shp.RData")

#-------------------------------------------------------------------------------

register_google(key = "AIzaSyD043L4r5yqacnCwv465zE0muf6vnTr4s0")

cities_distance_df_2_2$km <- vector(length = length(cities_distance_df_2_2$city_1))
cities_distance_df_2_2$hour <- vector(length = length(cities_distance_df_2_2$city_1))

for (i in 1:18086){
  result <- data.frame(mapdist(as.character(cities_distance_df_2_2$city_1[i]), 
                               as.character(cities_distance_df_2_2$city_2[i]), 
                               override_limit = TRUE))
  
  if (length(result$km) == 0) next
  cities_distance_df_2_2$km[i] <- result$km
  cities_distance_df_2_2$hour[i] <- result$hours
  
  if (i %% 1000 == 0){
    save.image("d:/Users/Fanni/Desktop/Területi autokorreláció/data_shp.RData")
    print(i)}
}

cities_distance_df_2_2 <- cities_distance_df_2_2 %>% filter(km > 0)
rm(result, i)
save.image("d:/Users/Fanni/Desktop/Területi autokorreláció/data_shp.RData")

#-------------------------------------------------------------------------------

register_google(key = "AIzaSyD043L4r5yqacnCwv465zE0muf6vnTr4s0")

index <- which(cities_distance_df_2_2$km == 0)

for (i in index){
  result <- data.frame(mapdist(as.character(cities_distance_df_2_2$city_1[i]), 
                               as.character(cities_distance_df_2_2$city_2[i]), 
                               override_limit = TRUE))
  
  if (length(result$km) == 0) next
  cities_distance_df_2_2$km[i] <- result$km
  cities_distance_df_2_2$hour[i] <- result$hours
  
}

cities_distance_df_2_2 <- cities_distance_df_2_2 %>% filter(km > 0)
rm(result, i, index)
save.image("d:/Users/Fanni/Desktop/Területi autokorreláció/data_shp.RData")

#-------------------------------------------------------------------------------
colnames(cities)[1] <- "city_1"
cities_distance_df_2 <- left_join(cities_distance_df_2, cities, "city_1", suffix = c("_1", "_2"))
colnames(cities_distance_df_2)[5:10] <- c("lat_1", "long_1", "geocode_1", 
                                          "NUTS_1_1", "NUTS_2_1", "NUTS_3_1")

colnames(cities)[1] <- "city_2"
cities_distance_df_2 <- left_join(cities_distance_df_2, cities, "city_2", suffix = c("_1", "_2"))
colnames(cities_distance_df_2)[11:16] <- c("lat_2", "long_2", "geocode_2", 
                                           "NUTS_1_2", "NUTS_2_2", "NUTS_3_2")

colnames(cities)[1] <- "name"

save.image("d:/Users/Fanni/Desktop/Területi autokorreláció/data_shp.RData")

#-------------------------------------------------------------------------------

colnames(cities)[1] <- "city_1"
cities_distance_df_2_2 <- left_join(cities_distance_df_2_2, cities, "city_1", suffix = c("_1", "_2"))
colnames(cities_distance_df_2_2)[5:10] <- c("lat_1", "long_1", "geocode_1", 
                                            "NUTS_1_1", "NUTS_2_1", "NUTS_3_1")

colnames(cities)[1] <- "city_2"
cities_distance_df_2_2 <- left_join(cities_distance_df_2_2, cities, "city_2", suffix = c("_1", "_2"))
colnames(cities_distance_df_2_2)[11:16] <- c("lat_2", "long_2", "geocode_2", 
                                             "NUTS_1_2", "NUTS_2_2", "NUTS_3_2")

colnames(cities)[1] <- "name"

save.image("d:/Users/Fanni/Desktop/Területi autokorreláció/data_shp.RData")

#-------------------------------------------------------------------------------

index <- which(cities_distance_df_2$lat_1 == FALSE)

for (i in index){
  city_1 <- cities_distance_df_2$city_1[i]
  city_2 <- cities_distance_df_2$city_2[i]
  index_1 <- which(cities$name == city_1)
  index_2 <- which(cities$name == city_2)
  
  cities_distance_df_2$lat_1[index] <- cities$lat[index_1]
  cities_distance_df_2$long_1[index] <- cities$long[index_1]
  cities_distance_df_2$geocode_1[index] <- cities$geocode[index_1]
  cities_distance_df_2$NUTS_1_1[index] <- cities$NUTS_1[index_1]
  cities_distance_df_2$NUTS_2_1[index] <- cities$NUTS_2[index_1]
  cities_distance_df_2$NUTS_3_1[index] <- cities$NUTS_3[index_1]
  
  cities_distance_df_2$lat_2[index] <- cities$lat[index_2]
  cities_distance_df_2$long_2[index] <- cities$long[index_2]
  cities_distance_df_2$geocode_2[index] <- cities$geocode[index_2]
  cities_distance_df_2$NUTS_1_2[index] <- cities$NUTS_1[index_2]
  cities_distance_df_2$NUTS_2_2[index] <- cities$NUTS_2[index_2]
  cities_distance_df_2$NUTS_3_2[index] <- cities$NUTS_3[index_2]
  
}

rm(i, city_1, city_2, index_1, index_2)
save.image("d:/Users/Fanni/Desktop/Területi autokorreláció/data_shp.RData")

#-------------------------------------------------------------------------------

index <- which(cities_distance_df_2_2$lat_1 == FALSE)

for (i in index){
  city_1 <- cities_distance_df_2_2$city_1[i]
  city_2 <- cities_distance_df_2_2$city_2[i]
  index_1 <- which(cities$name == city_1)
  index_2 <- which(cities$name == city_2)
  
  cities_distance_df_2_2$lat_1[index] <- cities$lat[index_1]
  cities_distance_df_2_2$long_1[index] <- cities$long[index_1]
  cities_distance_df_2_2$geocode_1[index] <- cities$geocode[index_1]
  cities_distance_df_2_2$NUTS_1_1[index] <- cities$NUTS_1[index_1]
  cities_distance_df_2_2$NUTS_2_1[index] <- cities$NUTS_2[index_1]
  cities_distance_df_2_2$NUTS_3_1[index] <- cities$NUTS_3[index_1]
  
  cities_distance_df_2_2$lat_2[index] <- cities$lat[index_2]
  cities_distance_df_2_2$long_2[index] <- cities$long[index_2]
  cities_distance_df_2_2$geocode_2[index] <- cities$geocode[index_2]
  cities_distance_df_2_2$NUTS_1_2[index] <- cities$NUTS_1[index_2]
  cities_distance_df_2_2$NUTS_2_2[index] <- cities$NUTS_2[index_2]
  cities_distance_df_2_2$NUTS_3_2[index] <- cities$NUTS_3[index_2]
}

rm(i, city_1, city_2, index_1, index_2)
save.image("d:/Users/Fanni/Desktop/Területi autokorreláció/data_shp.RData")