load("d:/Users/Fanni/Desktop/Területi autokorreláció/data_shp.RData")
LoadLibrary()
rm(list = ls())
load("d:/Users/Fanni/Desktop/Területi autokorreláció/matrix_nuts_3.RData")

test_function <- function(matrix){
  observed <- vector()
  expected <- vector()
  sd <- vector()
  p_value <- vector()
  
  for (i in 1:2500){
    NUTS_3$random <- rnorm(1355, mean = 100, sd = 2)
    moran <- Moran.I(NUTS_3$random, matrix)
    observed[i] <- moran$observed
    expected[i] <- moran$expected
    sd[i] <- moran$sd
    p_value[i] <- moran$p.value
  }
  
  data <- data.frame(observed = observed,
                     expected = expected,
                     sd = sd,
                     p_value = p_value, 
                     ref = rep(1, 2500))
  
  return(data)}

data_1 <- test_function(elso_szomszed_3)
data_2 <- test_function(masod_szomszed_3)
data_3 <- test_function(regio_kozep_tav_3)
data_4 <- test_function(kozut_elsodleges_km_3)
data_5 <- test_function(kozut_elsodleges_ora_3)
data_6 <- test_function(kozut_masodlagos_km_3)
data_7 <- test_function(kozut_masodlagos_ora_3)
data_8 <- test_function(min_tav_3)
data_9 <- test_function(legkozelebbi_negy_3)

data <- rbind(data_1, data_2, data_3, data_4, data_5, data_6, data_7, data_8, data_9)
data$level <- c(rep("elsődleges szomszédság",2500),
                rep("másodlagos szomszédság",2500),
                rep("régióközép távolság",2500),
                rep("közút - elsődleges - km",2500),
                rep("közút - elsődleges - h",2500),
                rep("közút - másodlagos - km",2500),
                rep("közút - másodlagos - h",2500),
                rep("legkisebb távolság",2500),
                rep("4-legközelebbi szomszéd",2500))

nrow(data %>% filter(level == "elsődleges szomszédság") %>% select(p_value) %>% filter(p_value < 0.1))/25
nrow(data %>% filter(level == "másodlagos szomszédság") %>% select(p_value) %>% filter(p_value < 0.1))/25
nrow(data %>% filter(level == "régióközép távolság") %>% select(p_value) %>% filter(p_value < 0.1))/25
nrow(data %>% filter(level == "közút - elsődleges - km") %>% select(p_value) %>% filter(p_value < 0.1))/25
nrow(data %>% filter(level == "közút - elsődleges - h") %>% select(p_value) %>% filter(p_value < 0.1))/25
nrow(data %>% filter(level == "közút - másodlagos - km") %>% select(p_value) %>% filter(p_value < 0.1))/25
nrow(data %>% filter(level == "közút - másodlagos - h") %>% select(p_value) %>% filter(p_value < 0.1))/25
nrow(data %>% filter(level == "legkisebb távolság") %>% select(p_value) %>% filter(p_value < 0.1))/25
nrow(data %>% filter(level == "4-legközelebbi szomszéd") %>% select(p_value) %>% filter(p_value < 0.1))/25

nrow(data %>% filter(level == "elsődleges szomszédság") %>% select(p_value) %>% filter(p_value < 0.05))/25
nrow(data %>% filter(level == "másodlagos szomszédság") %>% select(p_value) %>% filter(p_value < 0.05))/25
nrow(data %>% filter(level == "régióközép távolság") %>% select(p_value) %>% filter(p_value < 0.05))/25
nrow(data %>% filter(level == "közút - elsődleges - km") %>% select(p_value) %>% filter(p_value < 0.05))/25
nrow(data %>% filter(level == "közút - elsődleges - h") %>% select(p_value) %>% filter(p_value < 0.05))/25
nrow(data %>% filter(level == "közút - másodlagos - km") %>% select(p_value) %>% filter(p_value < 0.05))/25
nrow(data %>% filter(level == "közút - másodlagos - h") %>% select(p_value) %>% filter(p_value < 0.05))/25
nrow(data %>% filter(level == "legkisebb távolság") %>% select(p_value) %>% filter(p_value < 0.05))/25
nrow(data %>% filter(level == "4-legközelebbi szomszéd") %>% select(p_value) %>% filter(p_value < 0.05))/25

nrow(data %>% filter(level == "elsődleges szomszédság") %>% select(p_value) %>% filter(p_value < 0.01))/25
nrow(data %>% filter(level == "másodlagos szomszédság") %>% select(p_value) %>% filter(p_value < 0.01))/25
nrow(data %>% filter(level == "régióközép távolság") %>% select(p_value) %>% filter(p_value < 0.01))/25
nrow(data %>% filter(level == "közút - elsődleges - km") %>% select(p_value) %>% filter(p_value < 0.01))/25
nrow(data %>% filter(level == "közút - elsődleges - h") %>% select(p_value) %>% filter(p_value < 0.01))/25
nrow(data %>% filter(level == "közút - másodlagos - km") %>% select(p_value) %>% filter(p_value < 0.01))/25
nrow(data %>% filter(level == "közút - másodlagos - h") %>% select(p_value) %>% filter(p_value < 0.01))/25
nrow(data %>% filter(level == "legkisebb távolság") %>% select(p_value) %>% filter(p_value < 0.01))/25
nrow(data %>% filter(level == "4-legközelebbi szomszéd") %>% select(p_value) %>% filter(p_value < 0.01))/25

data %>% ggplot(aes(x = p_value)) +
  geom_histogram(aes(y=..density..), colour="black", fill="white", breaks = seq(0, 1, by = 0.01))+
  facet_wrap(~level) +
  ggtitle("Random értékekkel generált Moran I próbák által adott p-értékek eloszlása\nátlag = 100, szórás = 2 mellett\n") +
  labs(x = "\np-érték", y = "") + theme_minimal() +
  theme(plot.title = element_text(color="black", size=18, hjust=0.5, face = "bold"),
        axis.text.y = element_blank(),
        axis.text.x = element_text(color="black", size=10, face = "bold"),
        axis.title.x = element_text(color="black", size=10, face = "bold"),
        text = element_text(family = "serif"),
        strip.text.x = element_text(
          size = 14, face = "bold"))

save.image("d:/Users/Fanni/Desktop/Területi autokorreláció/random_3_2.RData")
