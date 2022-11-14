load("d:/Users/Fanni/Desktop/Területi autokorreláció/data_shp.RData")
LoadLibrary()

#-------------------------------------------------------------------------------
codes <- NUTS_1$NUTS_ID

distance_mean_km_1_2 <- matrix(nrow = 100, ncol = 100)
distance_sd_km_1_2 <- matrix(nrow = 100, ncol = 100)
distance_mean_hour_1_2 <- matrix(nrow = 100, ncol = 100)
distance_sd_hour_1_2 <- matrix(nrow = 100, ncol = 100)

for (i in 1:100){
  code_1 <- codes[i]
  neighbor <- region_neighbor_df_1_2 %>% filter(NUTS_ID == code_1)
  if (neighbor$neighbor == ""){next}
  neighbor <- strsplit(neighbor$neighbor, split = "; ")[[1]]
  
  for (j in 1:100){
    code_2 <- codes[j]
    
    if (!any(code_2 == neighbor)){next}
    
    if (i > j){
      proba <- cities_distance_df_1_2 %>% filter((NUTS_1_1 == codes[i] & NUTS_1_2 == codes[j])
                                                  | NUTS_1_2 == codes[j] & NUTS_1_1 == codes[i])
      distance_mean_km_1_2[i, j] <- mean(proba$km)
      distance_sd_km_1_2[i, j] <- sd(proba$km)
      distance_mean_hour_1_2[i, j] <- mean(proba$hour)
      distance_sd_hour_1_2[i, j] <- sd(proba$hour)
      
      distance_mean_km_1_2[j, i] <- mean(proba$km)
      distance_sd_km_1_2[j, i] <- sd(proba$km)
      distance_mean_hour_1_2[j, i] <- mean(proba$hour)
      distance_sd_hour_1_2[j, i] <- sd(proba$hour)
    }
  }
}

distance_mean_km_1_2[is.na(distance_mean_km_1_2)] <- 0
distance_sd_km_1_2[is.na(distance_sd_km_1_2)] <- 0
distance_mean_hour_1_2[is.na(distance_mean_hour_1_2)] <- 0
distance_sd_hour_1_2[is.na(distance_sd_hour_1_2)] <- 0

rm(i, j, proba, code_1, code_2, neighbor, codes)

index <- which(distance_mean_km_1_2 == 0)
distance_mean_km_1_2[index] <- distance_mean_km_1[index]
index <- which(distance_sd_km_1_2 == 0)
distance_sd_km_1_2[index] <- distance_sd_km_1[index]
index <- which(distance_mean_hour_1_2 == 0)
distance_mean_hour_1_2[index] <- distance_mean_hour_1[index]
index <- which(distance_sd_hour_1_2 == 0)
distance_sd_hour_1_2[index] <- distance_sd_hour_1[index]
rm(index)

save.image("d:/Users/Fanni/Desktop/Területi autokorreláció/data_shp.RData")