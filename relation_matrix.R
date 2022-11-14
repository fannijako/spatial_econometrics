load("d:/Users/Fanni/Desktop/Területi autokorreláció/data_shp.RData")
LoadLibrary()
#-------------------------------------------------------------------------------
relation_matrix <- matrix(nrow = 959, ncol = 100)
NUTS_1_category <- vector(length = 959)

for (i in 1:959){
  for (j in 1:100){
    proba <- st_relate(cities$geocode[[i]], NUTS_1$geometry[j])
    relation_matrix[i, j] <- ifelse(proba == "FF0FFF212", 0, 
                                    ifelse(proba == "0FFFFF212", 1, 2))
    if (proba ==  "0FFFFF212"){
      NUTS_1_category[i] <- NUTS_1$NUTS_ID[j]
    }
  }
}

relation_matrix[217,93] <- 1
NUTS_1_category[217] <- NUTS_1$NUTS_ID[93]
cities$NUTS_1 <- NUTS_1_category

save.image("d:/Users/Fanni/Desktop/Területi autokorreláció/data_shp.RData")

#-------------------------------------------------------------------------------
relation_matrix_2 <- matrix(nrow = 959, ncol = 324)
NUTS_2_category <- vector(length = 959)

for (i in 1:959){
  for (j in 1:324){
    proba <- st_relate(cities$geocode[[i]], NUTS_2$geometry[j])
    relation_matrix_2[i, j] <- ifelse(proba == "FF0FFF212", 0, 
                                    ifelse(proba == "0FFFFF212", 1, 2))
    if (proba ==  "0FFFFF212"){
      NUTS_2_category[i] <- NUTS_2$NUTS_ID[j]
    }
  }
}

rm(i, j, proba)

relation_matrix_2[217, 252] <- 1
NUTS_2_category[217] <- NUTS_2$NUTS_ID[252]

cities$NUTS_2 <- NUTS_2_category

save.image("d:/Users/Fanni/Desktop/Területi autokorreláció/data_shp.RData")

#-------------------------------------------------------------------------------
relation_matrix_3 <- matrix(nrow = 959, ncol = 1496)
NUTS_3_category <- vector(length = 959)

for (i in 1:959){
  for (j in 1:1496){
    proba <- st_relate(cities$geocode[[i]], NUTS_3$geometry[j])
    relation_matrix_3[i, j] <- ifelse(proba == "FF0FFF212", 0, 
                                      ifelse(proba == "0FFFFF212", 1, 2))
    if (proba ==  "0FFFFF212"){
      NUTS_3_category[i] <- NUTS_3$NUTS_ID[j]
    }
  }
}

rm(i, j, proba)
rm(rowsum)

relation_matrix_3[217, 1238] <- 1
NUTS_3_category[217] <- NUTS_3$NUTS_ID[1238]

cities$NUTS_3 <- NUTS_3_category

rm(NUTS_1_category, NUTS_2_category, NUTS_3_category)
save.image("d:/Users/Fanni/Desktop/Területi autokorreláció/data_shp.RData")
