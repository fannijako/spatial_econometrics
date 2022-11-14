load("d:/Users/Fanni/Desktop/Területi autokorreláció/data_shp.RData")
LoadLibrary()

data <- rbind(region_neighbor_df_1, region_neighbor_df_2, region_neighbor_df_3)
data$neighbor_nr <- vector(length = length(data$NUTS_ID))
data$level <- c(rep(1, length(region_neighbor_df_1$NUTS_ID)),
                rep(2, length(region_neighbor_df_2$NUTS_ID)),
                rep(3, length(region_neighbor_df_3$NUTS_ID)))

for (i in 1:length(data$NUTS_ID)){
  data$neighbor_nr[i] <- length(strsplit(data$neighbor[i], split = "; ")[[1]])
}

ggplot(data, aes(x=neighbor_nr)) + 
  facet_wrap(~level)+
  geom_histogram(aes(y = stat(density)), binwidth=1, fill="#69b3a2", 
                 color="#e9ecef", alpha=0.9, bins = 15) +        
  ggtitle("Szomszédos területek aránya NUTS szinteken\n") +
  xlab("") + 
  ylab("") +
  theme_minimal()+
  theme(plot.title = element_text(color="black", size=25, hjust=0.5, face = "bold"),
        axis.text.y = element_text(color="black", size=16, hjust=0.5, face = "bold"),
        axis.text.x = element_text(color="black", size=16, face = "bold"),                
        axis.ticks.x=element_blank(),
        text = element_text(family = "serif"),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 12, family = "serif"),
        legend.background = element_rect( size=.5),
        strip.text.x = element_text(size = 14, face = "bold"))

ggplot(NUTS_3, aes(x = area)) + 
  geom_histogram(aes(y = stat(density)), fill="#69b3a2", 
                 color="#e9ecef", alpha=0.9) +        
  ggtitle("Szomszédos területek aránya NUTS szinteken\n") +
  xlab("") + 
  ylab("") +
  theme(plot.title = element_text(color="black", size=18, hjust=0.5, face = "bold"),
        axis.text.y = element_text(color="black", size=10, hjust=0.5, face = "bold"),
        axis.text.x = element_text(color="black", size=10, face = "bold"),                
        axis.ticks.x=element_blank(),
        text = element_text(family = "serif"),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 12, family = "serif"),
        legend.background = element_rect( size=.5))

index <- which(region_neighbor_df_3$NUTS_ID == "UKE22")
neighbor <- region_neighbor_df_3$neighbor[index]
neighbor <- strsplit(neighbor, split = "; ")[[1]]
neighbor <- c(neighbor, "UKE22")
data <- NUTS_3 %>%  filter(NUTS_ID %in% neighbor)

data %>% ggplot() + ggtitle("North Yorkshire CC\n") + 
  geom_sf(aes(fill = as.numeric(area), geometry = geometry))  +
  theme_minimal()+
  theme(plot.title = element_text(color="black", size=25, hjust=0.5, face = "bold"),
        axis.text.y = element_text(color="black", size=16, hjust=0.5, face = "bold"),
        axis.text.x = element_text(color="black", size=16, face = "bold"),                
        axis.ticks.x=element_blank(),
        text = element_text(family = "serif"),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 12, family = "serif"),
        legend.background = element_rect( size=.5),
        strip.text.x = element_text(size = 14, face = "bold")) +
  paletteer::scale_fill_paletteer_c("scico::vikO")

#-------------------------------------------------------------------------------

data <- rbind(region_neighbor_df_1_2, region_neighbor_df_2_2, region_neighbor_df_3_2)
data_2 <- rbind(region_neighbor_df_1, region_neighbor_df_2, region_neighbor_df_3)
data$nbr <- vector(length = length(data$NUTS_ID))

for (i in 1:length(data$NUTS_ID)){
  neighbor_2 <- data$neighbor[i]
  count <- 0
  if (neighbor_2 == "") next
  neighbor_2 <- strsplit(neighbor_2, split = "; ")[[1]]
  neighbor_1 <- data_2 %>% filter(NUTS_ID == data$NUTS_ID[i]) %>% select(neighbor)
  neighbor_1 <- strsplit(neighbor_1$neighbor[1], split = "; ")[[1]]
  
  for (j in 1:length(neighbor_2)){
    if (!neighbor[j] %in% neighbor_1) count <- count + 1
  }
  data$nbr[i] <- count
}

data$level <- c(rep(1, length(region_neighbor_df_1_2$NUTS_ID)),
                rep(2, length(region_neighbor_df_2_2$NUTS_ID)),
                rep(3, length(region_neighbor_df_3_2$NUTS_ID)))

ggplot(data, aes(x = nbr)) + 
  facet_wrap(~level)+
  geom_histogram(aes(y = stat(density)), binwidth=1, fill="#69b3a2", 
                 color="#e9ecef", alpha=0.9, bins = 15) +        
  ggtitle("Másodszomszédos területek aránya NUTS szinteken\n") +
  xlab("") + 
  ylab("") +
  theme_minimal()+
  theme(plot.title = element_text(color="black", size=25, hjust=0.5, face = "bold"),
        axis.text.y = element_text(color="black", size=16, hjust=0.5, face = "bold"),
        axis.text.x = element_text(color="black", size=16, face = "bold"),                
        axis.ticks.x=element_blank(),
        text = element_text(family = "serif"),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 12, family = "serif"),
        legend.background = element_rect( size=.5),
        strip.text.x = element_text(size = 14, face = "bold"))

index <- which(region_neighbor_df_3$NUTS_ID == "FRF11")
neighbor <- region_neighbor_df_3$neighbor[index]
neighbor <- strsplit(neighbor, split = "; ")[[1]]
neighbor <- c(neighbor, "FRF11")
neighbor_2 <- region_neighbor_df_3_2$neighbor[index]
neighbor_2 <- strsplit(neighbor_2, split = "; ")[[1]]
neighbor <- c(neighbor, neighbor_2)
data <- NUTS_3 %>%  filter(NUTS_ID %in% neighbor)

index <- which(data$NUTS_ID == "FRF11")
data$NUTS_NAME <- c(rep("", index - 1), "Alsó-Rajna megye\nFranciaország\n(Bas-Rhin \nregion)", rep("", length(data$NUTS_ID) - index ))

data %>% ggplot() + ggtitle("Bas-Rhin régió szomszédai és másodszomszédai\n") + 
  geom_sf(aes(fill = as.numeric(area), geometry = geometry))  +
  theme_minimal()+
  theme(plot.title = element_text(color="black", size=25, hjust=0.5, face = "bold"),
        axis.text.y = element_text(color="black", size=16, hjust=0.5, face = "bold"),
        axis.text.x = element_text(color="black", size=16, face = "bold"),                
        axis.ticks.x=element_blank(),
        text = element_text(family = "serif"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 12, family = "serif"),
        legend.background = element_rect( size=.5),
        strip.text.x = element_text(size = 14, face = "bold"))+
  paletteer::scale_fill_paletteer_c("scico::vikO") +
  geom_sf_text(aes(label = NUTS_NAME), color = "white")

#-------------------------------------------------------------------------------
region_cities_df_3$nbr <- vector(length = length(region_cities_df_3$NUTS_ID))

for (i in 1:length(region_cities_df_3$NUTS_ID)){
  cs <- strsplit(region_cities_df_3$cities[i], split = "; ")[[1]]
  region_cities_df_3$nbr[i] <- length(cs)
}

data <- region_cities_df_3 %>% filter(nbr == 0) %>% select(NUTS_ID)
data <- NUTS_3 %>% filter(NUTS_ID %in% data$NUTS_ID)

#------------------------------------------------------------------------------
data <- data.frame(mean_h = c(as.vector(distance_mean_hour_1),
                              as.vector(distance_mean_hour_2),
                              as.vector(distance_mean_hour_3)),
                   mean_k = c(as.vector(distance_mean_km_1),
                              as.vector(distance_mean_km_2),
                              as.vector(distance_mean_km_3)))
data$level <- as.factor(c(rep(1, length(as.vector(distance_mean_hour_1))),
                          rep(2, length(as.vector(distance_mean_hour_2))),
                          rep(3, length(as.vector(distance_mean_hour_3)))))

data <- data %>% filter(mean_h > 0)

data %>% ggplot(aes(x = mean_h, y = mean_k, shape=level, color=level)) + 
  geom_point()+
  scale_color_brewer(palette="Dark2") +
  labs(title = "Eltérés a közúti távolság alapú távolságértékekben\n",
       x = "\nh",
       y = "km\n",
       shape = "NUTS szint",
       color = "NUTS szint")+
  theme_minimal() + 
  theme(plot.title = element_text(color="black", size=25, hjust=0.5, face = "bold"),
        axis.text.y = element_text(color="black", size=16, hjust=0.5, face = "bold"),
        axis.text.x = element_text(color="black", size=16, face = "bold"),                
        
        axis.ticks.x=element_blank(),
        text = element_text(family = "serif"),
        axis.title.y = element_text(color="black", size=16, hjust=0.5, face = "bold"),
        axis.title.x = element_text(color="black", size=16, face = "bold"),                
        
        legend.position = "bottom",
        legend.title = element_text(size = 16, family = "serif"),
        legend.text = element_text(size = 16, family = "serif"),
        legend.background = element_rect( size=.5))

#------------------------------------------------------------------------------
index <- which(!intersection_1)
center_distance_1[index] <- 0
index <- which(!intersection_2)
center_distance_2[index] <- 0
index <- which(!intersection_3)
center_distance_3[index] <- 0

data <- data.frame(dist_min = c(as.vector(distance_mean_km_1),
                                as.vector(distance_mean_km_2),
                                as.vector(distance_mean_km_3)),
                   cent_dist = c(as.vector(center_distance_1),
                                 as.vector(center_distance_2),
                                 as.vector(center_distance_3)))
data$level <- as.factor(c(rep(1, length(as.vector(center_distance_1))),
                          rep(2, length(as.vector(center_distance_2))),
                          rep(3, length(as.vector(center_distance_3)))))

data <- data %>% filter(dist_min > 0)

data$cent_dist <- data$cent_dist / 1000

data %>% ggplot(aes(x = cent_dist, y = dist_min, shape=level, color=level)) + 
  geom_point()+
  geom_abline(intercept = 0, slope = 1, size = 1.3) +
  scale_color_brewer(palette="Dark2") +
  labs(title = "Eltérés a közúti távolság alapú távolságérték és a középpontok távolsága között\n",
       x = "\nközéppontok távolsága (km)",
       y = "városok átlagos távolsága közúton (km)\n\n",
       shape = "NUTS szint",
       color = "NUTS szint")+
  theme_minimal() + 
  theme(plot.title = element_text(color="black", size=25, hjust=0.5, face = "bold"),
        axis.text.y = element_text(color="black", size=16, hjust=0.5, face = "bold"),
        axis.text.x = element_text(color="black", size=16, face = "bold"),                
        
        axis.ticks.x=element_blank(),
        text = element_text(family = "serif"),
        axis.title.y = element_text(color="black", size=16, hjust=0.5, face = "bold"),
        axis.title.x = element_text(color="black", size=16, face = "bold"),                
        
        legend.position = "bottom",
        legend.title = element_text(size = 16, family = "serif"),
        legend.text = element_text(size = 16, family = "serif"),
        legend.background = element_rect( size=.5))

#-------------------------------------------------------------------------------
columnsum <- apply(relation_matrix, 2, sum)
length(which(columnsum == 0))
NUTS_1$NUTS_NAME[which(columnsum == 0)]
NUTS_1$NUTS_ID[which(columnsum == 0)]
max(columnsum)
hist(columnsum, breaks = max(columnsum))

columnsum <- apply(relation_matrix_2, 2, sum)
length(which(columnsum == 0))
NUTS_2$NUTS_NAME[which(columnsum == 0)]
NUTS_2$NUTS_ID[which(columnsum == 0)]
max(columnsum)
hist(columnsum, breaks = max(columnsum))

columnsum <- apply(relation_matrix_3, 2, sum)
length(which(columnsum == 0))
NUTS_3$NUTS_NAME[which(columnsum == 0)]
max(columnsum)
hist(columnsum, breaks = max(columnsum))