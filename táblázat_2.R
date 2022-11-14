load("d:/Users/Fanni/Desktop/Területi autokorreláció/data_shp.RData")
LoadLibrary()
rm(list = ls())
load("d:/Users/Fanni/Desktop/Területi autokorreláció/data_nuts_2.RData")

colnames(intersection_2) <- NUTS_2$NUTS_ID
rownames(intersection_2) <- NUTS_2$NUTS_ID
diag(intersection_2) <- 0
intersection_2[278, 258] <- 1
intersection_2[258, 278] <- 1
intersection_2[109, 272] <- 1
intersection_2[272, 109] <- 1
intersection_2[94, 249] <- 1
intersection_2[249, 94] <- 1

elso_szomszed_2 <- intersection_2

masod_szomszed_2 <- elso_szomszed_2 + elso_szomszed_2 %*% elso_szomszed_2
diag(masod_szomszed_2) <- 0
masod_szomszed_2[which(masod_szomszed_2 > 0)] <- 1

regio_kozep_tav_2 <- apply(center_distance_2, 1, as.numeric)
colnames(regio_kozep_tav_2) <- NUTS_2$NUTS_ID
rownames(regio_kozep_tav_2) <- NUTS_2$NUTS_ID
regio_kozep_tav_2 <- regio_kozep_tav_2 / 1000
regio_kozep_tav_2[which(regio_kozep_tav_2 > 0)] <- 1/regio_kozep_tav_2[which(regio_kozep_tav_2 > 0)]

kozut_elsodleges_km_2 <- distance_mean_km_2
colnames(kozut_elsodleges_km_2) <- NUTS_2$NUTS_ID
rownames(kozut_elsodleges_km_2) <- NUTS_2$NUTS_ID
kozut_elsodleges_km_2[which(kozut_elsodleges_km_2 == 0 & elso_szomszed_2 > 0)] <- center_distance_2[which(kozut_elsodleges_km_2 == 0 & elso_szomszed_2 > 0)]
kozut_elsodleges_km_2[which(kozut_elsodleges_km_2 > 0)] <- 1/kozut_elsodleges_km_2[which(kozut_elsodleges_km_2 > 0)]

kozut_elsodleges_ora_2 <- distance_mean_hour_2
colnames(kozut_elsodleges_ora_2) <- NUTS_2$NUTS_ID
rownames(kozut_elsodleges_ora_2) <- NUTS_2$NUTS_ID
kozut_elsodleges_ora_2[which(kozut_elsodleges_ora_2 == 0 & elso_szomszed_2 > 0)] <- center_distance_2[which(kozut_elsodleges_ora_2 == 0 & elso_szomszed_2 > 0)]*0.01152
kozut_elsodleges_ora_2[which(kozut_elsodleges_ora_2 > 0)] <- 1/kozut_elsodleges_ora_2[which(kozut_elsodleges_ora_2 > 0)]

kozut_masodlagos_km_2 <- distance_mean_km_2_2
colnames(kozut_masodlagos_km_2) <- NUTS_2$NUTS_ID
rownames(kozut_masodlagos_km_2) <- NUTS_2$NUTS_ID
kozut_masodlagos_km_2[which(kozut_masodlagos_km_2 == 0 & masod_szomszed_2 > 0)] <- center_distance_2[which(kozut_masodlagos_km_2 == 0 & masod_szomszed_2 > 0)]
kozut_masodlagos_km_2[which(kozut_masodlagos_km_2 > 0)] <- 1/kozut_masodlagos_km_2[which(kozut_masodlagos_km_2 > 0)]

kozut_masodlagos_ora_2 <- distance_mean_hour_2_2
colnames(kozut_masodlagos_ora_2) <- NUTS_2$NUTS_ID
rownames(kozut_masodlagos_ora_2) <- NUTS_2$NUTS_ID
kozut_masodlagos_ora_2[which(kozut_masodlagos_ora_2 == 0 & masod_szomszed_2 > 0)] <- center_distance_2[which(kozut_masodlagos_ora_2 == 0 & masod_szomszed_2 > 0)]*0.01152
kozut_masodlagos_ora_2[which(kozut_masodlagos_ora_2 > 0)] <- 1/kozut_masodlagos_ora_2[which(kozut_masodlagos_ora_2 > 0)]

min_tav_2 <- apply(distance_2, 2, as.numeric)
min_tav_2 <- min_tav_2 / 1000
min_tav_2[which(min_tav_2 > 500)] <- 0
min_tav_2[which(min_tav_2 != 0)] <- 1/min_tav_2[which(min_tav_2 != 0)]
min_tav_2[which(elso_szomszed_2 == 1 & min_tav_2 == 0)] <- 0.5

legkozelebbi_negy_2 <- nb2mat(knn2nb(knearneigh(coordinates(as(NUTS_2, "Spatial")), k = 4)))

rm(distance_mean_hour_2, distance_mean_hour_2_2, distance_mean_km_2, distance_mean_km_2_2, distance_sd_hour_2, distance_sd_hour_2_2, distance_sd_km_2, distance_sd_km_2_2, distance_2, intersection_2, masod_szomszed, relation_2, center_distance_2)

save.image("d:/Users/Fanni/Desktop/Területi autokorreláció/matrix_nuts_2.RData")

#-------------------------------------------------------------------------------
load("d:/Users/Fanni/Desktop/Területi autokorreláció/matrix_nuts_2.RData")

det_1 <- det(elso_szomszed_2)
det_2 <- det(masod_szomszed_2)
det_3 <- det(regio_kozep_tav_2)
det_4 <- det(kozut_elsodleges_km_2)
det_5 <- det(kozut_elsodleges_ora_2)
det_6 <- det(kozut_masodlagos_km_2)
det_7 <- det(kozut_masodlagos_ora_2)
det_8 <- det(min_tav_2)
det_9 <- det(legkozelebbi_negy_2)

eigen_1 <- eigen(elso_szomszed_2)$values
eigen_2 <- eigen(masod_szomszed_2)$values
eigen_3 <- eigen(regio_kozep_tav_2)$values
eigen_4 <- eigen(kozut_elsodleges_km_2)$values
eigen_5 <- eigen(kozut_elsodleges_ora_2)$values
eigen_6 <- eigen(kozut_masodlagos_km_2)$values
eigen_7 <- eigen(kozut_masodlagos_ora_2)$values
eigen_8 <- eigen(min_tav_2)$values
eigen_9 <- eigen(legkozelebbi_negy_2)$values

eigen_df <- data.frame(eigen = c(eigen_1, eigen_2, eigen_3, eigen_4, eigen_5, 
                                 eigen_6, eigen_7),
                       matrix = rep(c("elsődleges szomszéd",
                                      "másodlagos szomszéd",
                                      "régióközép távolság",
                                      "közút - első - km",
                                      "közút - első - h",
                                      "közút - másod - km",
                                      "közút - másod - h"), each = 281),
                       index = rep(1:281, 7))

eigen_df %>% ggplot(aes(x = index, y = eigen, color = as.factor(matrix))) + geom_point()+
  ggtitle("Mátrix sajátértékek NUTS 2 szinten\nvalós sajátértékek esetén\n")+
  labs(x = "", y = "sajátérték\n")+
  theme_minimal()+
  theme(plot.title = element_text(color="black", size=25, hjust=0.5, face = "bold"),
        axis.text.y = element_text(color="black", size=16, hjust=0.5, face = "bold"),
        axis.text.x = element_blank(),  
        axis.title.y = element_text(color="black", size=16),  
        axis.ticks.x=element_blank(),
        text = element_text(family = "serif"),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 14, family = "serif"),
        legend.background = element_rect( size=.5)) +
  
  paletteer::scale_fill_paletteer_d("colorBlindness::paletteMartin")


R_1 <- R(elso_szomszed_2)
R_2 <- R(masod_szomszed_2)
R_3 <- R(regio_kozep_tav_2)
R_4 <- R(kozut_elsodleges_km_2)
R_5 <- R(kozut_elsodleges_ora_2)
R_6 <- R(kozut_masodlagos_km_2)
R_7 <- R(kozut_masodlagos_ora_2)
R_8 <- R(min_tav_2)
R_9 <- R(legkozelebbi_negy_2)

matrix_modification <- function(matrix){
  columnsum <- apply(matrix, 2, sum)
  index <- which(columnsum != 0)
  matrix <- matrix[index, index]
  return(matrix)
}

elso_szomszed_2 <- matrix_modification(elso_szomszed_2)
masod_szomszed_2 <- matrix_modification(masod_szomszed_2)
regio_kozep_tav_2 <- matrix_modification(regio_kozep_tav_2)
kozut_elsodleges_km_2 <- matrix_modification(kozut_elsodleges_km_2)
kozut_elsodleges_ora_2 <- matrix_modification(kozut_elsodleges_ora_2)
kozut_masodlagos_km_2 <- matrix_modification(kozut_masodlagos_km_2)
kozut_masodlagos_ora_2 <- matrix_modification(kozut_masodlagos_ora_2)
min_tav_2 <- matrix_modification(min_tav_2)
legkozelebbi_negy_2 <- matrix_modification(legkozelebbi_negy_2)

m_det_1 <- det(elso_szomszed_2)
m_det_2 <- det(masod_szomszed_2)
m_det_3 <- det(regio_kozep_tav_2)
m_det_4 <- det(kozut_elsodleges_km_2)
m_det_5 <- det(kozut_elsodleges_ora_2)
m_det_6 <- det(kozut_masodlagos_km_2)
m_det_7 <- det(kozut_masodlagos_ora_2)
m_det_8 <- det(min_tav_2)
m_det_9 <- det(legkozelebbi_negy_2)

m_eigen_1 <- length(eigen(elso_szomszed_2)$values)
m_eigen_2 <- length(eigen(masod_szomszed_2)$values)
m_eigen_3 <- length(eigen(regio_kozep_tav_2)$values)
m_eigen_4 <- length(eigen(kozut_elsodleges_km_2)$values)
m_eigen_5 <- length(eigen(kozut_elsodleges_ora_2)$values)
m_eigen_6 <- length(eigen(kozut_masodlagos_km_2)$values)
m_eigen_7 <- length(eigen(kozut_masodlagos_ora_2)$values)
m_eigen_8 <- length(eigen(min_tav_2)$values)
m_eigen_9 <- length(eigen(legkozelebbi_negy_2)$values)

nemzerus <- function(matrix)return(length(which(matrix != 0)) / length(matrix)*100)

m_nemzerus_1 <- nemzerus(elso_szomszed_2)
m_nemzerus_2 <- nemzerus(masod_szomszed_2)
m_nemzerus_3 <- nemzerus(regio_kozep_tav_2)
m_nemzerus_4 <- nemzerus(kozut_elsodleges_km_2)
m_nemzerus_5 <- nemzerus(kozut_elsodleges_ora_2)
m_nemzerus_6 <- nemzerus(kozut_masodlagos_km_2)
m_nemzerus_7 <- nemzerus(kozut_masodlagos_ora_2)
m_nemzerus_8 <- nemzerus(min_tav_2)
m_nemzerus_9 <- nemzerus(legkozelebbi_negy_2)