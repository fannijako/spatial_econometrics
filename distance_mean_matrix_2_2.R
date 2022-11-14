load("d:/Users/Fanni/Desktop/Területi autokorreláció/data_shp.RData")
LoadLibrary()

#-------------------------------------------------------------------------------
codes <- NUTS_2$NUTS_ID

distance_mean_km_2_2 <- matrix(nrow = 281, ncol = 281)
distance_sd_km_2_2 <- matrix(nrow = 281, ncol = 281)
distance_mean_hour_2_2 <- matrix(nrow = 281, ncol = 281)
distance_sd_hour_2_2 <- matrix(nrow = 281, ncol = 281)

for (i in 1:281){
  code_1 <- codes[i]
  neighbor <- region_neighbor_df_2_2 %>% filter(NUTS_ID == code_1)
  if (neighbor$neighbor == ""){next}
  neighbor <- strsplit(neighbor$neighbor, split = "; ")[[1]]
  
  for (j in 1:281){
    code_2 <- codes[j]
    
    if (!any(code_2 == neighbor)){next}
    
    if (i > j){
      proba <- cities_distance_df_2_2 %>% filter((NUTS_2_1 == codes[i] & NUTS_2_2 == codes[j])
                                                 | NUTS_2_2 == codes[j] & NUTS_2_1 == codes[i])
      distance_mean_km_2_2[i, j] <- mean(proba$km)
      distance_sd_km_2_2[i, j] <- sd(proba$km)
      distance_mean_hour_2_2[i, j] <- mean(proba$hour)
      distance_sd_hour_2_2[i, j] <- sd(proba$hour)
      
      distance_mean_km_2_2[j, i] <- mean(proba$km)
      distance_sd_km_2_2[j, i] <- sd(proba$km)
      distance_mean_hour_2_2[j, i] <- mean(proba$hour)
      distance_sd_hour_2_2[j, i] <- sd(proba$hour)
    }
  }
}

distance_mean_km_2_2[is.na(distance_mean_km_2_2)] <- 0
distance_sd_km_2_2[is.na(distance_sd_km_2_2)] <- 0
distance_mean_hour_2_2[is.na(distance_mean_hour_2_2)] <- 0
distance_sd_hour_2_2[is.na(distance_sd_hour_2_2)] <- 0

rm(i, j, proba, code_1, code_2, neighbor, codes)

index <- which(distance_mean_km_2_2 == 0)
distance_mean_km_2_2[index] <- distance_mean_km_2[index]
index <- which(distance_sd_km_2_2 == 0)
distance_sd_km_2_2[index] <- distance_sd_km_2[index]
index <- which(distance_mean_hour_2_2 == 0)
distance_mean_hour_2_2[index] <- distance_mean_hour_2[index]
index <- which(distance_sd_hour_2_2 == 0)
distance_sd_hour_2_2[index] <- distance_sd_hour_2[index]
rm(index)

save.image("d:/Users/Fanni/Desktop/Területi autokorreláció/data_shp.RData")