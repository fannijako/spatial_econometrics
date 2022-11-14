load("d:/Users/Fanni/Desktop/Területi autokorreláció/data_shp.RData")
LoadLibrary()

#-------------------------------------------------------------------------------
codes <- NUTS_3$NUTS_ID

distance_mean_km_3 <- matrix(nrow = 1355, ncol = 1355)
distance_sd_km_3 <- matrix(nrow = 1355, ncol = 1355)
distance_mean_hour_3 <- matrix(nrow = 1355, ncol = 1355)
distance_sd_hour_3 <- matrix(nrow = 1355, ncol = 1355)

for (i in 1:1355){
  code_1 <- codes[i]
  neighbor <- region_neighbor_df_3 %>% filter(NUTS_ID == code_1)
  if (neighbor$neighbor == ""){next}
  neighbor <- strsplit(neighbor$neighbor, split = "; ")[[1]]
  
  for (j in 1:1355){
    code_2 <- codes[j]
    
    if (!any(code_2 == neighbor)){next}
    
    if (i > j){
      proba <- cities_distance_df_3 %>% filter((NUTS_3_1 == codes[i] & NUTS_3_2 == codes[j])
                                               | NUTS_3_2 == codes[j] & NUTS_3_1 == codes[i])
      distance_mean_km_3[i, j] <- mean(proba$km)
      distance_sd_km_3[i, j] <- sd(proba$km)
      distance_mean_hour_3[i, j] <- mean(proba$hour)
      distance_sd_hour_3[i, j] <- sd(proba$hour)
      
      distance_mean_km_3[j, i] <- mean(proba$km)
      distance_sd_km_3[j, i] <- sd(proba$km)
      distance_mean_hour_3[j, i] <- mean(proba$hour)
      distance_sd_hour_3[j, i] <- sd(proba$hour)
    }
  }
}

distance_mean_km_3[is.na(distance_mean_km_3)] <- 0
distance_sd_km_3[is.na(distance_sd_km_3)] <- 0
distance_mean_hour_3[is.na(distance_mean_hour_3)] <- 0
distance_sd_hour_3[is.na(distance_sd_hour_3)] <- 0

rm(i, j, proba, code_1, code_2, neighbor, codes)

save.image("d:/Users/Fanni/Desktop/Területi autokorreláció/data_shp.RData")