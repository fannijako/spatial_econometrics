load("d:/Users/Fanni/Desktop/Területi autokorreláció/data_shp.RData")
LoadLibrary()
rm(list = ls())
rm(NUTS_3)
load("d:/Users/Fanni/Desktop/Területi autokorreláció/simul_nuts_1.RData")
colnames(simul)[2] <- "direction"
colnames(simul)[4] <- "test_matrix"
colnames(simul)[5] <- "p"
colnames(simul)[6] <- "c"

simul_1 <- simul %>% select(-matrix_test)
rm(simul)
save.image("d:/Users/Fanni/Desktop/Területi autokorreláció/simul_nuts_1.RData")
#-------------------------------------------------------------------------------
NUTS_3$proba <- rnorm(1355, mean = 100, sd = 10)
relation_new <- legkozelebbi_negy_3

for (i in 1:400){
  NUTS_3$proba <- NUTS_3$proba + 0.1*(relation_new %*% NUTS_3$proba) 
  + rnorm(1355, 10, 2)
}

NUTS_3 %>% ggplot() + ggtitle("Szimuláció bemutatása NUTS 1 szinten\nRégióközép távolság mátrix\n") + 
  geom_sf(aes(fill = proba, geometry = geometry))  +
  theme(plot.title = element_text(color="black", size=18, hjust=0.5, face = "bold"),
        axis.text.y = element_text(color="black", size=10, hjust=0.5, face = "bold"),
        axis.text.x = element_text(color="black", size=10, face = "bold"),                
        axis.ticks.x=element_blank(),
        text = element_text(family = "serif"),
        legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(size = 12, family = "serif"),
        legend.background = element_rect( size=.5))

elso_szomszed_3 <- elso_szomszed_3 / apply(elso_szomszed_3, 1, sum)
kozut_elsodleges_km_3 <- kozut_elsodleges_km_3 / apply(kozut_elsodleges_km_3, 1, sum)
kozut_elsodleges_ora_3 <- kozut_elsodleges_ora_3 / apply(kozut_elsodleges_ora_3, 1, sum)
kozut_masodlagos_km_3 <- kozut_masodlagos_km_3 / apply(kozut_masodlagos_km_3, 1, sum)
kozut_masodlagos_ora_3 <- kozut_masodlagos_ora_3 / apply(kozut_masodlagos_ora_3, 1, sum)
masod_szomszed_3 <- masod_szomszed_3 / apply(masod_szomszed_3, 1, sum)
legkozelebbi_negy_3 <- legkozelebbi_negy_3 / apply(legkozelebbi_negy_3, 1, sum)
min_tav_3 <- min_tav_3 / apply(min_tav_3, 1, sum)
regio_kozep_tav_3 <- regio_kozep_tav_3 / apply(regio_kozep_tav_3, 1, sum)

elso_szomszed_3[which(is.na(elso_szomszed_3))] <- 0
kozut_elsodleges_km_3[which(is.na(kozut_elsodleges_km_3))] <- 0
kozut_elsodleges_ora_3[which(is.na(kozut_elsodleges_ora_3))] <- 0
kozut_masodlagos_km_3[which(is.na(kozut_masodlagos_km_3))] <- 0
kozut_masodlagos_ora_3[which(is.na(kozut_masodlagos_ora_3))] <- 0
masod_szomszed_3[which(is.na(masod_szomszed_3))] <- 0
legkozelebbi_negy_3[which(is.na(legkozelebbi_negy_3))] <- 0
min_tav_3[which(is.na(min_tav_3))] <- 0
regio_kozep_tav_3[which(is.na(regio_kozep_tav_3))] <- 0

p_value <- vector()
irany <- vector()
original_matrix <- vector()
matrix_list <- vector()
m_list <- vector()
z_list <- vector()
count <- 0

for (m in 1:4){
  for (z in seq(0.01, 0.5, 0.05)){
    for (j in 1:50){
      for (relation_new in c("elso_szomszed_3", "kozut_elsodleges_km_3", "kozut_elsodleges_ora_3",
                             "kozut_masodlagos_km_3", "kozut_masodlagos_ora_3", "legkozelebbi_negy_3",
                             "masod_szomszed_3", "min_tav_3", "regio_kozep_tav_3")){
      
        NUTS_3$proba <- rnorm(1355, mean = 100, sd = 10)
        for (i in 1:m){ NUTS_3$proba <- NUTS_3$proba + 
                                        z*(get(relation_new) %*% NUTS_3$proba) 
                                        + rnorm(1355, 10, 2)}
      
        for (matrix in c("elso_szomszed_3", "kozut_elsodleges_km_3", "kozut_elsodleges_ora_3",
                         "kozut_masodlagos_km_3", "kozut_masodlagos_ora_3", "legkozelebbi_negy_3",
                         "masod_szomszed_3", "min_tav_3", "regio_kozep_tav_3")){
          
          count <- count + 1
          moran <- Moran.I(as.numeric(NUTS_3$proba), get(matrix))
          p_value[count] <- moran$p.value
          irany[count] <- ifelse(moran$observed > moran$expected, 1, -1)
          original_matrix[count] <- relation_new
          matrix_list[count] <- matrix
          m_list[count] <- m
          z_list[count] <- z
        
        if (count %% 10000 == 0)print(count)
}}}}}

simul <- data.frame(p_value = p_value, irany = irany, original_matrix = original_matrix,
                    matrix = matrix_list, m = m_list, z = z_list)
simul$matrix_test <- ifelse(simul$original_matrix == simul$matrix, 1, 0)
rm(count, i, irany, j, m, m_list, matrix, matrix_list, original_matrix, p_value,
   relation_new, z, z_list, moran)
rm(elso_szomszed_3, kozut_elsodleges_km_3, kozut_elsodleges_ora_3, 
   kozut_masodlagos_km_3, kozut_masodlagos_ora_3, legkozelebbi_negy_3, 
   masod_szomszed_3, min_tav_3, regio_kozep_tav_3, NUTS_3)
save.image("d:/Users/Fanni/Desktop/Területi autokorreláció/simul_nuts_3.RData")

#------------------------------------------------------------------------------
load("d:/Users/Fanni/Desktop/Területi autokorreláció/data_shp.RData")
LoadLibrary()
rm(list = ls())
load("d:/Users/Fanni/Desktop/Területi autokorreláció/simul_nuts_3.RData")
simul_nem_szig <- simul %>% filter(p_value > 0.05)
simul_szig <- simul %>% filter(p_value < 0.05)

# hány % lett szignifikáns 5 %-n
nrow(simul_szig)
nrow(simul_szig) / nrow(simul) * 100

table(simul_szig$irany) # 1092 szignifikánsan negatív kimutatás van, ami 0.13 %
table(simul_szig$irany) / nrow(simul) * 100
proba <- simul %>% filter(p_value < 0.05 & irany == -1)
nrow(proba %>% filter(p_value < 0.01))
hist(proba$m, breaks = 5)
hist(proba$z, breaks = 100)
table(simul_szig$irany, simul_szig$matrix_test) # ebből 84 esetben a saját mátrix nem mutatta ki, ami weird

simul_szig <- simul_szig %>% filter(irany == 1)

table(simul_szig$irany, simul_szig$matrix_test)[1,1] / 144000*100 #hány százalékban mutatta ki egy másik mátrix
table(simul_szig$irany, simul_szig$matrix_test)[1,2] / 18000*100 # hány százalékban mutatta ki ugyanaz a mátrix

table(simul_szig$original_matrix) / 180 #hány százalékban mutatták ki az adott mátrix által generált autokorrt, na itt azért vannak eltérések
table(simul_szig$original_matrix, simul_szig$matrix) / 20 # 10% alatti értékeket kiemelni
table(simul_szig$original_matrix, simul_szig$m) / 36 # ennek növekednie kellene, de néhol ijesztően csökken
hist(simul_szig$m)
table(simul_szig$original_matrix, simul_szig$z) / 9 # ez nem rendellenes
table(simul_szig$z)/81 # ebben legalább növekszik
hist(simul_szig$z)

table(simul_szig$matrix) / 180 # mi a legjobb kimutató?

simul_3 <- simul
simul_3$original_matrix[which(simul_3$original_matrix == "elso_szomszed_3")] <- 1
simul_3$original_matrix[which(simul_3$original_matrix == "masod_szomszed_3")] <- 2
simul_3$original_matrix[which(simul_3$original_matrix == "regio_kozep_tav_3")] <- 3
simul_3$original_matrix[which(simul_3$original_matrix == "kozut_elsodleges_km_3")] <- 4
simul_3$original_matrix[which(simul_3$original_matrix == "kozut_elsodleges_ora_3")] <- 5
simul_3$original_matrix[which(simul_3$original_matrix == "kozut_masodlagos_km_3")] <- 6
simul_3$original_matrix[which(simul_3$original_matrix == "kozut_masodlagos_ora_3")] <- 7
simul_3$original_matrix[which(simul_3$original_matrix == "min_tav_3")] <- 8
simul_3$original_matrix[which(simul_3$original_matrix == "legkozelebbi_negy_3")] <- 9

simul_3$matrix[which(simul_3$matrix == "elso_szomszed_3")] <- 1
simul_3$matrix[which(simul_3$matrix == "masod_szomszed_3")] <- 2
simul_3$matrix[which(simul_3$matrix == "regio_kozep_tav_3")] <- 3
simul_3$matrix[which(simul_3$matrix == "kozut_elsodleges_km_3")] <- 4
simul_3$matrix[which(simul_3$matrix == "kozut_elsodleges_ora_3")] <- 5
simul_3$matrix[which(simul_3$matrix == "kozut_masodlagos_km_3")] <- 6
simul_3$matrix[which(simul_3$matrix == "kozut_masodlagos_ora_3")] <- 7
simul_3$matrix[which(simul_3$matrix == "min_tav_3")] <- 8
simul_3$matrix[which(simul_3$matrix == "legkozelebbi_negy_3")] <- 9

simul_3 %>% ggplot(aes(x = p_value)) + 
  geom_boxplot(outlier.colour="grey", outlier.shape=1,
               outlier.size=0.2) + 
  geom_vline(xintercept = 0.05, color = "red", linetype = "longdash", size = 0.4) +
  facet_grid(original_matrix ~ matrix, scales="free_y") +
  theme_minimal() +
  labs(title="Moran I tesztek p értékei a generáló és \na tesztelő mátrix bontásában",
       x="\nTesztelő mátrix", y = "Szimuláló mátrix\n")  + 
  theme(plot.title = element_text(color="black", size=18, hjust=0.5, face = "bold"),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_text(color="black", size=18, hjust=0.5, face = "bold"),
        axis.title.x = element_text(color="black", size=18, face = "bold"),
        text = element_text(family = "serif"),
        strip.text.x = element_text(size = 14, color = "black"),
        strip.text.y = element_text(size = 14, color = "black"))