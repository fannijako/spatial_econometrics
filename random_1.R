load("d:/Users/Fanni/Desktop/Területi autokorreláció/data_shp.RData")
LoadLibrary()
rm(list = ls())
load("d:/Users/Fanni/Desktop/Területi autokorreláció/matrix_nuts_1.RData")

test_function <- function(matrix){
  observed <- vector()
  expected <- vector()
  sd <- vector()
  p_value <- vector()
  
  for (i in 1:10000){
    NUTS_1$random <- rnorm(100, mean = 100, sd = 2)
    moran <- Moran.I(NUTS_1$random, matrix)
    observed[i] <- moran$observed
    expected[i] <- moran$expected
    sd[i] <- moran$sd
    p_value[i] <- moran$p.value
  }
  
  data <- data.frame(observed = observed,
                     expected = expected,
                     sd = sd,
                     p_value = p_value, 
                     ref = rep(1, 10000))
  
  return(data)}

data_1 <- test_function(elso_szomszed_1)
data_2 <- test_function(masod_szomszed_1)
data_3 <- test_function(regio_kozep_tav_1)
data_4 <- test_function(kozut_elsodleges_km_1)
data_5 <- test_function(kozut_elsodleges_ora_1)
data_6 <- test_function(kozut_masodlagos_km_1)
data_7 <- test_function(kozut_masodlagos_ora_1)
data_8 <- test_function(min_tav_1)
data_9 <- test_function(legkozelebbi_negy_1)

data <- rbind(data_1, data_2, data_3, data_4, data_5, data_6, data_7, data_8, data_9)
data$level <- c(rep("elsődleges szomszédság",10000),
                rep("másodlagos szomszédság",10000),
                rep("régióközép távolság",10000),
                rep("közút - elsődleges - km",10000),
                rep("közút - elsődleges - h",10000),
                rep("közút - másodlagos - km",10000),
                rep("közút - másodlagos - h",10000),
                rep("legkisebb távolság",10000),
                rep("4-legközelebbi szomszéd",10000))

nrow(data %>% filter(level == "elsődleges szomszédság") %>% select(p_value) %>% filter(p_value < 0.1))/100
nrow(data %>% filter(level == "másodlagos szomszédság") %>% select(p_value) %>% filter(p_value < 0.1))/100
nrow(data %>% filter(level == "régióközép távolság") %>% select(p_value) %>% filter(p_value < 0.1))/100
nrow(data %>% filter(level == "közút - elsődleges - km") %>% select(p_value) %>% filter(p_value < 0.1))/100
nrow(data %>% filter(level == "közút - elsődleges - h") %>% select(p_value) %>% filter(p_value < 0.1))/100
nrow(data %>% filter(level == "közút - másodlagos - km") %>% select(p_value) %>% filter(p_value < 0.1))/100
nrow(data %>% filter(level == "közút - másodlagos - h") %>% select(p_value) %>% filter(p_value < 0.1))/100
nrow(data %>% filter(level == "legkisebb távolság") %>% select(p_value) %>% filter(p_value < 0.1))/100
nrow(data %>% filter(level == "4-legközelebbi szomszéd") %>% select(p_value) %>% filter(p_value < 0.1))/100

nrow(data %>% filter(level == "elsődleges szomszédság") %>% select(p_value) %>% filter(p_value < 0.05))/100
nrow(data %>% filter(level == "másodlagos szomszédság") %>% select(p_value) %>% filter(p_value < 0.05))/100
nrow(data %>% filter(level == "régióközép távolság") %>% select(p_value) %>% filter(p_value < 0.05))/100
nrow(data %>% filter(level == "közút - elsődleges - km") %>% select(p_value) %>% filter(p_value < 0.05))/100
nrow(data %>% filter(level == "közút - elsődleges - h") %>% select(p_value) %>% filter(p_value < 0.05))/100
nrow(data %>% filter(level == "közút - másodlagos - km") %>% select(p_value) %>% filter(p_value < 0.05))/100
nrow(data %>% filter(level == "közút - másodlagos - h") %>% select(p_value) %>% filter(p_value < 0.05))/100
nrow(data %>% filter(level == "legkisebb távolság") %>% select(p_value) %>% filter(p_value < 0.05))/100
nrow(data %>% filter(level == "4-legközelebbi szomszéd") %>% select(p_value) %>% filter(p_value < 0.05))/100

nrow(data %>% filter(level == "elsődleges szomszédság") %>% select(p_value) %>% filter(p_value < 0.01))/100
nrow(data %>% filter(level == "másodlagos szomszédság") %>% select(p_value) %>% filter(p_value < 0.01))/100
nrow(data %>% filter(level == "régióközép távolság") %>% select(p_value) %>% filter(p_value < 0.01))/100
nrow(data %>% filter(level == "közút - elsődleges - km") %>% select(p_value) %>% filter(p_value < 0.01))/100
nrow(data %>% filter(level == "közút - elsődleges - h") %>% select(p_value) %>% filter(p_value < 0.01))/100
nrow(data %>% filter(level == "közút - másodlagos - km") %>% select(p_value) %>% filter(p_value < 0.01))/100
nrow(data %>% filter(level == "közút - másodlagos - h") %>% select(p_value) %>% filter(p_value < 0.01))/100
nrow(data %>% filter(level == "legkisebb távolság") %>% select(p_value) %>% filter(p_value < 0.01))/100
nrow(data %>% filter(level == "4-legközelebbi szomszéd") %>% select(p_value) %>% filter(p_value < 0.01))/100

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

nrow(data %>% filter(p_value < 0.5) %>% filter(observed < expected) %>% filter(level == "elsődleges szomszédság")) -
nrow(data %>% filter(p_value < 0.5) %>% filter(observed > expected) %>% filter(level == "elsődleges szomszédság"))

nrow(data %>% filter(p_value < 0.5) %>% filter(observed < expected) %>% filter(level == "másodlagos szomszédság"))-
nrow(data %>% filter(p_value < 0.5) %>% filter(observed > expected) %>% filter(level == "másodlagos szomszédság"))

nrow(data %>% filter(p_value < 0.5) %>% filter(observed < expected) %>% filter(level == "régióközép távolság"))-
nrow(data %>% filter(p_value < 0.5) %>% filter(observed > expected) %>% filter(level == "régióközép távolság"))

nrow(data %>% filter(p_value < 0.5) %>% filter(observed < expected) %>% filter(level == "közút - elsődleges - km"))-
nrow(data %>% filter(p_value < 0.5) %>% filter(observed > expected) %>% filter(level == "közút - elsődleges - km"))

nrow(data %>% filter(p_value < 0.5) %>% filter(observed < expected) %>% filter(level == "közút - elsődleges - h"))-
nrow(data %>% filter(p_value < 0.5) %>% filter(observed > expected) %>% filter(level == "közút - elsődleges - h"))

nrow(data %>% filter(p_value < 0.5) %>% filter(observed < expected) %>% filter(level == "közút - másodlagos - km"))-
nrow(data %>% filter(p_value < 0.5) %>% filter(observed > expected) %>% filter(level == "közút - másodlagos - km"))

nrow(data %>% filter(p_value < 0.5) %>% filter(observed < expected) %>% filter(level == "közút - másodlagos - h"))-
nrow(data %>% filter(p_value < 0.5) %>% filter(observed > expected) %>% filter(level == "közút - másodlagos - h"))

nrow(data %>% filter(p_value < 0.5) %>% filter(observed < expected) %>% filter(level == "legkisebb távolság"))-
nrow(data %>% filter(p_value < 0.5) %>% filter(observed > expected) %>% filter(level == "legkisebb távolság"))

nrow(data %>% filter(p_value < 0.5) %>% filter(observed < expected) %>% filter(level == "4-legközelebbi szomszéd"))-
nrow(data %>% filter(p_value < 0.5) %>% filter(observed > expected) %>% filter(level == "4-legközelebbi szomszéd"))

#save.image("d:/Users/Fanni/Desktop/Területi autokorreláció/random_1_2.RData")

