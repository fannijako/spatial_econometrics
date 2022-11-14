load("d:/Users/Fanni/Desktop/Területi autokorreláció/data_shp.RData")
LoadLibrary()
rm(list = ls())
load("d:/Users/Fanni/Desktop/Területi autokorreláció/matrix_nuts_1.RData")
#-------------------------------------------------------------------------------
NUTS_1$proba <- rnorm(100, mean = 100, sd = 10)
relation_new <- kozut_masodlagos_km_1

for (i in 1:1000){
  NUTS_1$proba <- NUTS_1$proba + 0.4*(relation_new %*% NUTS_1$proba) 
  + rnorm(100, 10, 2)
}

NUTS_1 %>% ggplot() + ggtitle("Szimuláció bemutatása NUTS 1 szinten\n4-legközelebbi szomszéd mátrix") + 
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

elso_szomszed_1 <- elso_szomszed_1 / apply(elso_szomszed_1, 1, sum)
kozut_elsodleges_km_1 <- kozut_elsodleges_km_1 / apply(kozut_elsodleges_km_1, 1, sum)
kozut_elsodleges_ora_1 <- kozut_elsodleges_ora_1 / apply(kozut_elsodleges_ora_1, 1, sum)
kozut_masodlagos_km_1 <- kozut_masodlagos_km_1 / apply(kozut_masodlagos_km_1, 1, sum)
kozut_masodlagos_ora_1 <- kozut_masodlagos_ora_1 / apply(kozut_masodlagos_ora_1, 1, sum)
masod_szomszed_1 <- masod_szomszed_1 / apply(masod_szomszed_1, 1, sum)
legkozelebbi_negy_1 <- legkozelebbi_negy_1 / apply(legkozelebbi_negy_1, 1, sum)
min_tav_1 <- min_tav_1 / apply(min_tav_1, 1, sum)
regio_kozep_tav_1 <- regio_kozep_tav_1 / apply(regio_kozep_tav_1, 1, sum)

elso_szomszed_1[which(is.na(elso_szomszed_1))] <- 0
kozut_elsodleges_km_1[which(is.na(kozut_elsodleges_km_1))] <- 0
kozut_elsodleges_ora_1[which(is.na(kozut_elsodleges_ora_1))] <- 0
kozut_masodlagos_km_1[which(is.na(kozut_masodlagos_km_1))] <- 0
kozut_masodlagos_ora_1[which(is.na(kozut_masodlagos_ora_1))] <- 0
masod_szomszed_1[which(is.na(masod_szomszed_1))] <- 0
legkozelebbi_negy_1[which(is.na(legkozelebbi_negy_1))] <- 0
min_tav_1[which(is.na(min_tav_1))] <- 0
regio_kozep_tav_1[which(is.na(regio_kozep_tav_1))] <- 0

p_value <- vector()
irany <- vector()
original_matrix <- vector()
matrix_list <- vector()
m_list <- vector()
z_list <- vector()
count <- 0

for (m in 1:5){
  for (z in seq(0.01, 1, 0.05)){
    for (j in 1:100){
      for (relation_new in c("elso_szomszed_1", "kozut_elsodleges_km_1", 
                             "kozut_elsodleges_ora_1","kozut_masodlagos_km_1", 
                             "kozut_masodlagos_ora_1", "legkozelebbi_negy_1",
                             "masod_szomszed_1", "min_tav_1", "regio_kozep_tav_1")){
      
        NUTS_1$proba <- rnorm(100, mean = 100, sd = 10)
        for (i in 1:m){ NUTS_1$proba <- NUTS_1$proba + 
                                        z*(get(relation_new) %*% NUTS_1$proba) 
                                        + rnorm(100, 10, 2)}
      
        for (matrix in c("elso_szomszed_1", "kozut_elsodleges_km_1", 
                         "kozut_elsodleges_ora_1","kozut_masodlagos_km_1", 
                         "kozut_masodlagos_ora_1", "legkozelebbi_negy_1",
                         "masod_szomszed_1", "min_tav_1", "regio_kozep_tav_1")){
          
          count <- count + 1
          moran <- Moran.I(as.numeric(NUTS_1$proba), get(matrix))
          p_value[count] <- moran$p.value
          irany[count] <- ifelse(moran$observed > moran$expected, 1, -1)
          original_matrix[count] <- relation_new
          matrix_list[count] <- matrix
          m_list[count] <- m
          z_list[count] <- z
        
        if (count %% 10000 == 0)print(count)
}}}}}

simul <- data.frame(p_value = p_value, irany = irany, 
                    original_matrix = original_matrix,
                    matrix = matrix_list, m = m_list, z = z_list)
simul$matrix_test <- ifelse(simul$original_matrix == simul$matrix, 1, 0)
rm(count, i, irany, j, m, m_list, matrix, matrix_list, original_matrix, p_value,
   relation_new, z, z_list, moran)
rm(elso_szomszed_1, kozut_elsodleges_km_1, kozut_elsodleges_ora_1, 
   kozut_masodlagos_km_1, kozut_masodlagos_ora_1, legkozelebbi_negy_1, 
   masod_szomszed_1, min_tav_1, regio_kozep_tav_1, NUTS_1)
save.image("d:/Users/Fanni/Desktop/Területi autokorreláció/simul_nuts_1.RData")

#------------------------------------------------------------------------------
load("d:/Users/Fanni/Desktop/Területi autokorreláció/data_shp.RData")
LoadLibrary()
rm(list = ls())
load("d:/Users/Fanni/Desktop/Területi autokorreláció/simul_nuts_1.RData")
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

table(simul_szig$irany, simul_szig$matrix_test)[1,1] / 720000*100 #hány százalékban mutatta ki egy másik mátrix
table(simul_szig$irany, simul_szig$matrix_test)[1,2] / 90000*100 # hány százalékban mutatta ki ugyanaz a mátrix

table(simul_szig$original_matrix) / 900 #hány százalékban mutatták ki az adott mátrix által generált autokorrt, na itt azért vannak eltérések
table(simul_szig$original_matrix, simul_szig$matrix) / 100 # 10% alatti értékeket kiemelni
table(simul_szig$original_matrix, simul_szig$m) / 180 # ennek növekednie kellene, de néhol ijesztően csökken
hist(simul_szig$m)
table(simul_szig$original_matrix, simul_szig$z) / 45 # ez nem rendellenes
table(simul_szig$z)/405 # ebben legalább növekszik
hist(simul_szig$z)

table(simul_szig$matrix) / 900 # mi a legjobb kimutató?

simul %>% ggplot(aes(x = p_value)) + geom_boxplot() + facet_wrap(~original_matrix)

simul_2 <- simul
simul_2$original_matrix[which(simul_2$original_matrix == "elso_szomszed_1")] <- 1
simul_2$original_matrix[which(simul_2$original_matrix == "masod_szomszed_1")] <- 2
simul_2$original_matrix[which(simul_2$original_matrix == "regio_kozep_tav_1")] <- 3
simul_2$original_matrix[which(simul_2$original_matrix == "kozut_elsodleges_km_1")] <- 4
simul_2$original_matrix[which(simul_2$original_matrix == "kozut_elsodleges_ora_1")] <- 5
simul_2$original_matrix[which(simul_2$original_matrix == "kozut_masodlagos_km_1")] <- 6
simul_2$original_matrix[which(simul_2$original_matrix == "kozut_masodlagos_ora_1")] <- 7
simul_2$original_matrix[which(simul_2$original_matrix == "min_tav_1")] <- 8
simul_2$original_matrix[which(simul_2$original_matrix == "legkozelebbi_negy_1")] <- 9

simul_2$matrix[which(simul_2$matrix == "elso_szomszed_1")] <- 1
simul_2$matrix[which(simul_2$matrix == "masod_szomszed_1")] <- 2
simul_2$matrix[which(simul_2$matrix == "regio_kozep_tav_1")] <- 3
simul_2$matrix[which(simul_2$matrix == "kozut_elsodleges_km_1")] <- 4
simul_2$matrix[which(simul_2$matrix == "kozut_elsodleges_ora_1")] <- 5
simul_2$matrix[which(simul_2$matrix == "kozut_masodlagos_km_1")] <- 6
simul_2$matrix[which(simul_2$matrix == "kozut_masodlagos_ora_1")] <- 7
simul_2$matrix[which(simul_2$matrix == "min_tav_1")] <- 8
simul_2$matrix[which(simul_2$matrix == "legkozelebbi_negy_1")] <- 9

simul_2 %>% ggplot(aes(x = p_value)) + 
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