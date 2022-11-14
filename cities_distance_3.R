load("d:/Users/Fanni/Desktop/Területi autokorreláció/data_shp.RData")
LoadLibrary()

#-------------------------------------------------------------------------------
codes <- NUTS_3$NUTS_ID
region_cities_df_3 <- data.frame(NUTS_ID = codes, 
                                 cities = vector(length = length(codes)))

for (code in 1:length(codes)){
  seged <- as.vector(cities$name[which(cities$NUTS_3 == codes[code])])
  if (length(seged) == 0){region_cities_df_3$cities[code] <- c("")
  } else {
    osszes <- vector()
    for (i in seged){osszes <- paste(osszes, i, sep = "; ")}
    region_cities_df_3$cities[code] <- substring(osszes,3)
  }
}
rm(codes, code, seged, osszes, i)
#-------------------------------------------------------------------------------
codes <- NUTS_3$NUTS_ID
region_neighbor_df_3 <- data.frame(NUTS_ID = codes,
                                   neighbor = vector(length = length(codes)))

for (code in 1:length(codes)){
  seged <- NUTS_3$NUTS_ID[which(as.numeric(distance_3[,code]) == 0)]
  if (length(seged) < 2){region_neighbor_df_3$neighbor[code] <- c("")
  } else {
    osszes <- vector()
    for (i in 1:length(seged)){
      if (seged[i] != codes[code]){osszes <- paste(osszes, seged[i], sep = "; ")}}
    region_neighbor_df_3$neighbor[code] <- substring(osszes,3)
  }
}
rm(codes, code, seged, osszes, i)

# Csalagút
index <- which(region_neighbor_df_3$NUTS_ID == "UKJ44")
value <- region_neighbor_df_3$neighbor[index]
region_neighbor_df_3$neighbor[index] <- paste(value, "FRE12", sep = "; ")

index <- which(region_neighbor_df_3$NUTS_ID == "FRE12")
value <- region_neighbor_df_3$neighbor[index]
region_neighbor_df_3$neighbor[index] <- paste(value, "UKJ44", sep = "; ")

# Koppenhága - Malmö
index <- which(region_neighbor_df_3$NUTS_ID == "DK011")
value <- region_neighbor_df_3$neighbor[index]
region_neighbor_df_3$neighbor[index] <- paste(value, "SE224", sep = "; ")

index <- which(region_neighbor_df_3$NUTS_ID == "SE224")
value <- region_neighbor_df_3$neighbor[index]
region_neighbor_df_3$neighbor[index] <- paste(value, "DK011", sep = "; ")

# Szicília
index <- which(region_neighbor_df_3$NUTS_ID == "ITF65")
value <- region_neighbor_df_3$neighbor[index]
region_neighbor_df_3$neighbor[index] <- paste(value, "ITG13", sep = "; ")

index <- which(region_neighbor_df_3$NUTS_ID == "ITG13")
value <- region_neighbor_df_3$neighbor[index]
region_neighbor_df_3$neighbor[index] <- paste(value, "ITF65", sep = "; ")

rm(index, value)
#-------------------------------------------------------------------------------
codes <- NUTS_3$NUTS_ID
region_neighbor_df_3_2 <- data.frame(NUTS_ID = codes,
                                     neighbor = vector(length = length(codes)))

for (code in 1:length(codes)){
  
  if (region_neighbor_df_3$neighbor[code] == "") {
    region_neighbor_df_3_2$neighbor[code] <- ""
    next
  }
  
  seged <- vector()
  first_neighbor <- strsplit(region_neighbor_df_3$neighbor[code], split = "; ")[[1]]
  for (i in 1:length(first_neighbor)){
    seged <- paste(seged, region_neighbor_df_3$neighbor[which(region_neighbor_df_3$NUTS_ID == first_neighbor[i])], sep = "; ")
  }
  seged <- substring(seged, 3)
  seged <- unique(strsplit(seged, split = "; ")[[1]])
  
  if (length(seged) == 1){
    region_neighbor_df_3_2$neighbor[code] <- ""
    next
  }
  
  seged <- seged[which(seged != codes[code])]
  
  seged_2 <- ""
  for (i in 1:length(seged)){
    if (!any(seged[i] == first_neighbor)){
      seged_2 <- paste(seged_2, seged[i], sep = "; ")
    }
  }
  
  region_neighbor_df_3_2$neighbor[code] <- substring(seged_2, 3)
}

rm(codes, code, seged, first_neighbor, i, seged_2)

#-------------------------------------------------------------------------------
cities_distance_df_3 <- data.frame(city_1 = vector(), city_2 = vector())

for (i in 1:1496){
  region_1 <- region_neighbor_df_3$NUTS_ID[i]
  cities_1 <- strsplit(region_cities_df_3$cities[i], split = "; ")[[1]]
  neighbor <- strsplit(region_neighbor_df_3$neighbor[i], split = "; ")[[1]]
  if (length(neighbor) != 0){
    for (j in 1:length(neighbor)){
      neighbor_cities <- cities$name[which(cities$NUTS_3 == neighbor[j])]
      cities_distance_df_3 <- rbind(cities_distance_df_3, 
                                  setNames(expand.grid(cities_1, neighbor_cities), 
                                           names(cities_distance_df_3)))
    }
  }
}
rm(region_1, cities_1, neighbor, j, i, neighbor_cities)

cities_distance_df_3$save <- vector(length = length(cities_distance_df_3$city_1))
for (i in 1:length(cities_distance_df_3$city_1)){
  first <- as.character(cities_distance_df_3$city_1[i])
  second <- as.character(cities_distance_df_3$city_2[i])
  if (first < second){cities_distance_df_3$save[i] <- TRUE}
}

rm(i, first, second)

cities_distance_df_3 <- cities_distance_df_3 %>%  filter(save == TRUE)
cities_distance_df_3 <- cities_distance_df_3 %>% select(-save)

region_1_l <-  c("UKJ44", "FRE12", "DK011", "SE224", "ITG13", "ITF65")
region_2_l <- c("FRE12", "UKJ44", "SE224", "DK011", "ITF65", "ITG13")
for (i in 1:6){
  region_1 <- region_1_l[i]
  index <- which(region_cities_df_3$NUTS_ID == region_1)
  cities_1 <- strsplit(region_cities_df_3$cities[index], split = "; ")[[1]]
  neighbor <- region_2_l[i]
  
  neighbor_cities <- cities$name[which(cities$NUTS_3 == neighbor)]
  new_rows <- as.data.frame(expand.grid(cities_1, neighbor_cities))
  colnames(new_rows) <- colnames(cities_distance_df_3)[1:2]
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
  
  cities_distance_df_3 <- rbind(cities_distance_df_3, new_rows)
}
rm(new_rows, region_1_l, region_2_l, i, region_1, index, cities_1, neighbor, neighbor_cities)

#-------------------------------------------------------------------------------
cities_distance_df_3_2 <- data.frame(city_1 = vector(), city_2 = vector())
for (i in 1:1496){
  region_1 <- region_neighbor_df_3_2$NUTS_ID[i]
  cities_1 <- strsplit(region_cities_df_3$cities[i], split = "; ")[[1]]
  neighbor <- strsplit(region_neighbor_df_3_2$neighbor[i], split = "; ")[[1]]
  if (length(neighbor) != 0){
    for (j in 1:length(neighbor)){
      neighbor_cities <- cities$name[which(cities$NUTS_3 == neighbor[j])]
      cities_distance_df_3_2 <- rbind(cities_distance_df_3_2, 
                                      setNames(expand.grid(cities_1, neighbor_cities), 
                                               names(cities_distance_df_3_2)))
    }
  }
}
rm(region_1, cities_1, neighbor, j, i, neighbor_cities)

cities_distance_df_3_2$save <- vector(length = length(cities_distance_df_3_2$city_1))
for (i in 1:length(cities_distance_df_3_2$city_1)){
  first <- as.character(cities_distance_df_3_2$city_1[i])
  second <- as.character(cities_distance_df_3_2$city_2[i])
  if (first < second){cities_distance_df_3_2$save[i] <- TRUE}
}

rm(i, first, second)

cities_distance_df_3_2 <- cities_distance_df_3_2 %>%  filter(save == TRUE)
cities_distance_df_3_2 <- cities_distance_df_3_2 %>% select(-save)

region_1_l <-  c("UKJ44", "FRE12", "DK011", "SE224", "ITG13", "ITF65")
region_2_l <- c("FRE12", "UKJ44", "SE224", "DK011", "ITF65", "ITG13")

for (i in 1:6){
  region_1 <- region_1_l[i]
  index <- which(region_cities_df_3$NUTS_ID == region_1)
  cities_1 <- strsplit(region_cities_df_3$cities[index], split = "; ")[[1]]
  
  neighbor <- region_2_l[i]
  neighbor_2 <- region_neighbor_df_3$neighbor[which(region_neighbor_df_3$NUTS_ID == neighbor)]
  neighbor_2 <- strsplit(neighbor_2, split = "; ")[[1]]
  neighbor_2 <- neighbor_2[which(neighbor_2 != neighbor)]
  
  if (length(neighbor_2) == 0) next
  neighbor_cities <- vector()
  for (j in 1:length(neighbor_2)){
    neighbor_cities <- c(neighbor_cities, 
                         cities$name[which(cities$NUTS_3 == neighbor_2[j])])
  }
  
  new_rows <- as.data.frame(expand.grid(cities_1, neighbor_cities))
  colnames(new_rows) <- colnames(cities_distance_df_3_2)[1:2]
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
  
  cities_distance_df_3_2 <- rbind(cities_distance_df_3_2, new_rows)
}
rm(new_rows, region_1_l, region_2_l, i,j, region_1, index, cities_1, 
   neighbor, neighbor_2, neighbor_cities)

save.image("d:/Users/Fanni/Desktop/Területi autokorreláció/data_shp.RData")

#-------------------------------------------------------------------------------

register_google(key = "AIzaSyD043L4r5yqacnCwv465zE0muf6vnTr4s0")

cities_distance_df_3$km <- vector(length = length(cities_distance_df_3$city_1))
cities_distance_df_3$hour <- vector(length = length(cities_distance_df_3$city_1))

for (i in 1:2418){
  result <- data.frame(mapdist(as.character(cities_distance_df_3$city_1[i]), 
                               as.character(cities_distance_df_3$city_2[i]), 
                               override_limit = TRUE))
  cities_distance_df_3$km[i] <- result$km
  cities_distance_df_3$hour[i] <- result$hours
  
  if (i %% 1000 == 0){
    save.image("d:/Users/Fanni/Desktop/Területi autokorreláció/data_shp.RData")
    print(i)}
}

cities_distance_df_3 <- cities_distance_df_3 %>% filter(km > 0)
rm(result, i)
save.image("d:/Users/Fanni/Desktop/Területi autokorreláció/data_shp.RData")

#-------------------------------------------------------------------------------

register_google(key = "AIzaSyD043L4r5yqacnCwv465zE0muf6vnTr4s0")

index <- which(cities_distance_df_3$km == 0)

for (i in index){
  result <- data.frame(mapdist(as.character(cities_distance_df_3$city_1[i]), 
                               as.character(cities_distance_df_3$city_2[i]), 
                               override_limit = TRUE))
  
  if (length(result$km) == 0) next
  cities_distance_df_3$km[i] <- result$km
  cities_distance_df_3$hour[i] <- result$hours
  
}

cities_distance_df_3 <- cities_distance_df_3 %>% filter(km > 0)
rm(result, i, index)
save.image("d:/Users/Fanni/Desktop/Területi autokorreláció/data_shp.RData")

#-------------------------------------------------------------------------------

register_google(key = "AIzaSyD043L4r5yqacnCwv465zE0muf6vnTr4s0")

cities_distance_df_3_2$km <- vector(length = length(cities_distance_df_3_2$city_1))
cities_distance_df_3_2$hour <- vector(length = length(cities_distance_df_3_2$city_1))

for (i in 1:4608){
  result <- data.frame(mapdist(as.character(cities_distance_df_3_2$city_1[i]), 
                               as.character(cities_distance_df_3_2$city_2[i]), 
                               override_limit = TRUE))
  
  if (length(result$km) == 0) next
  cities_distance_df_3_2$km[i] <- result$km
  cities_distance_df_3_2$hour[i] <- result$hours
  
  if (i %% 1000 == 0){
    save.image("d:/Users/Fanni/Desktop/Területi autokorreláció/data_shp.RData")
    print(i)}
}

cities_distance_df_3_2 <- cities_distance_df_3_2 %>% filter(km > 0)
rm(result, i)
save.image("d:/Users/Fanni/Desktop/Területi autokorreláció/data_shp.RData")

#-------------------------------------------------------------------------------

register_google(key = "AIzaSyD043L4r5yqacnCwv465zE0muf6vnTr4s0")

index <- which(cities_distance_df_3_2$km == 0)

for (i in index){
  result <- data.frame(mapdist(as.character(cities_distance_df_3_2$city_1[i]), 
                               as.character(cities_distance_df_3_2$city_2[i]), 
                               override_limit = TRUE))
  
  if (length(result$km) == 0) next
  cities_distance_df_3_2$km[i] <- result$km
  cities_distance_df_3_2$hour[i] <- result$hours
  
}

cities_distance_df_3_2 <- cities_distance_df_3_2 %>% filter(km > 0)
rm(result, i, index)
save.image("d:/Users/Fanni/Desktop/Területi autokorreláció/data_shp.RData")
#-------------------------------------------------------------------------------
colnames(cities)[1] <- "city_1"
cities_distance_df_3 <- left_join(cities_distance_df_3, cities, "city_1", suffix = c("_1", "_2"))
colnames(cities_distance_df_3)[5:10] <- c("lat_1", "long_1", "geocode_1", 
                                          "NUTS_1_1", "NUTS_2_1", "NUTS_3_1")

colnames(cities)[1] <- "city_2"
cities_distance_df_3 <- left_join(cities_distance_df_3, cities, "city_2", suffix = c("_1", "_2"))
colnames(cities_distance_df_3)[11:16] <- c("lat_2", "long_2", "geocode_2", 
                                           "NUTS_1_2", "NUTS_2_2", "NUTS_3_2")

colnames(cities)[1] <- "name"

save.image("d:/Users/Fanni/Desktop/Területi autokorreláció/data_shp.RData")

#-------------------------------------------------------------------------------

colnames(cities)[1] <- "city_1"
cities_distance_df_3_2 <- left_join(cities_distance_df_3_2, cities, "city_1", suffix = c("_1", "_2"))
colnames(cities_distance_df_3_2)[5:10] <- c("lat_1", "long_1", "geocode_1", 
                                            "NUTS_1_1", "NUTS_2_1", "NUTS_3_1")

colnames(cities)[1] <- "city_2"
cities_distance_df_3_2 <- left_join(cities_distance_df_3_2, cities, "city_2", suffix = c("_1", "_2"))
colnames(cities_distance_df_3_2)[11:16] <- c("lat_2", "long_2", "geocode_2", 
                                             "NUTS_1_2", "NUTS_2_2", "NUTS_3_2")

colnames(cities)[1] <- "name"

save.image("d:/Users/Fanni/Desktop/Területi autokorreláció/data_shp.RData")
#-------------------------------------------------------------------------------

index <- which(cities_distance_df_3$lat_1 == FALSE)

for (i in index){
  city_1 <- cities_distance_df_3$city_1[i]
  city_2 <- cities_distance_df_3$city_2[i]
  index_1 <- which(cities$name == city_1)
  index_2 <- which(cities$name == city_2)
  
  cities_distance_df_3$lat_1[index] <- cities$lat[index_1]
  cities_distance_df_3$long_1[index] <- cities$long[index_1]
  cities_distance_df_3$geocode_1[index] <- cities$geocode[index_1]
  cities_distance_df_3$NUTS_1_1[index] <- cities$NUTS_1[index_1]
  cities_distance_df_3$NUTS_2_1[index] <- cities$NUTS_2[index_1]
  cities_distance_df_3$NUTS_3_1[index] <- cities$NUTS_3[index_1]
  
  cities_distance_df_3$lat_2[index] <- cities$lat[index_2]
  cities_distance_df_3$long_2[index] <- cities$long[index_2]
  cities_distance_df_3$geocode_2[index] <- cities$geocode[index_2]
  cities_distance_df_3$NUTS_1_2[index] <- cities$NUTS_1[index_2]
  cities_distance_df_3$NUTS_2_2[index] <- cities$NUTS_2[index_2]
  cities_distance_df_3$NUTS_3_2[index] <- cities$NUTS_3[index_2]
  
}

rm(i, city_1, city_2, index_1, index_2)
save.image("d:/Users/Fanni/Desktop/Területi autokorreláció/data_shp.RData")

#-------------------------------------------------------------------------------

index <- which(cities_distance_df_3_2$lat_1 == FALSE)

for (i in index){
  city_1 <- cities_distance_df_3_2$city_1[i]
  city_2 <- cities_distance_df_3_2$city_2[i]
  index_1 <- which(cities$name == city_1)
  index_2 <- which(cities$name == city_2)
  
  cities_distance_df_3_2$lat_1[index] <- cities$lat[index_1]
  cities_distance_df_3_2$long_1[index] <- cities$long[index_1]
  cities_distance_df_3_2$geocode_1[index] <- cities$geocode[index_1]
  cities_distance_df_3_2$NUTS_1_1[index] <- cities$NUTS_1[index_1]
  cities_distance_df_3_2$NUTS_2_1[index] <- cities$NUTS_2[index_1]
  cities_distance_df_3_2$NUTS_3_1[index] <- cities$NUTS_3[index_1]
  
  cities_distance_df_3_2$lat_2[index] <- cities$lat[index_2]
  cities_distance_df_3_2$long_2[index] <- cities$long[index_2]
  cities_distance_df_3_2$geocode_2[index] <- cities$geocode[index_2]
  cities_distance_df_3_2$NUTS_1_2[index] <- cities$NUTS_1[index_2]
  cities_distance_df_3_2$NUTS_2_2[index] <- cities$NUTS_2[index_2]
  cities_distance_df_3_2$NUTS_3_2[index] <- cities$NUTS_3[index_2]
}

rm(i, city_1, city_2, index_1, index_2)
save.image("d:/Users/Fanni/Desktop/Területi autokorreláció/data_shp.RData")