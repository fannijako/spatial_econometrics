API_key <- "AIzaSyB_hP0TrJUvDff4ZH_xOvbhyJfyqGBhZvY"

setwd("d:/Users/Fanni/Documents/02. Corvinus/2. GPME/Szakdolgozat - mester/Területi autokorreláció/")

#install.packages(c("tidyverse", "tidyr", "dplyr", "sf", "spdep", 
#                   "maptools", "leaflet", "sp", "maps", "writexl", 
#                   "rnaturalearth", "rnaturalearthdata", "rgeos", "ggmap", 
#                   "rstudioapi", "extrafont", "ggplot2", "gridExtra", "grid", 
#                   "ape", "matlib", "units", "e1071", "readxl", "paletteer", 
#                   "lawstat", "ggpubr", "AICcmodavg", "plotly", "pgirmess", 
#                   "psych"))

LoadLibrary <- function(){
  library(tidyverse)
  library(tidyr)
  library(dplyr)
  library(sf)
  library(spdep)
  library(maptools)
  library(leaflet)
  require(RColorBrewer)
  library(sp)
  library(maps)
  library(readxl)
  library(writexl)
  library(rnaturalearth)
  library(rnaturalearthdata)
  library(rgeos)
  library(ggmap)
  library(rstudioapi)
  library(extrafont)
  library(ggplot2)
  library(gridExtra)
  library(grid) 
  library(ape)
  library(matlib)
  library(units)
  library(e1071)
  library(paletteer)
  library(lawstat)
  library(ggpubr)
  library(AICcmodavg)
  library(plotly)
  library(pgirmess)
  library(psych)
}
LoadLibrary()

#-------------------------------------------------------------------------------
# forrás : https://ec.europa.eu/eurostat/web/gisco/geodata/reference-data/administrative-units-statistical-units/nuts
# NUTS 2021, SHP, Polygons, 20M, EPSG:4326
shp <- read_sf('NUTS_RG_01M_2021_4326.shp')

#-------------------------------------------------------------------------------
# https://r-spatial.github.io/sf/reference/geos_unary.html
# https://r-spatial.github.io/sf/articles/sf3.html
# https://geocompr.robinlovelace.net/spatial-operations.html
shp$centroid <- st_centroid(shp$geometry, of_largest_polygon = TRUE)

#-------------------------------------------------------------------------------
NUTS_3 <- shp[shp$LEVL_CODE == 3, ]
NUTS_2 <- shp[shp$LEVL_CODE == 2, ]
NUTS_1 <- shp[shp$LEVL_CODE == 1, ]

# külterületek eltávolítása
NUTS_1_klt <- c("PT2","PT3","FRY","IS0","ES7","TR1","TR2","TR3","TR4","TR5",
                "TR6","TR7","TR8","TR9","TRA","TRB","TRC","CY0","RS1","RS2",
                "ME0","AL0","MK0","MT0","NO0")
NUTS_1 <- NUTS_1[which(!(NUTS_1$NUTS_ID %in% NUTS_1_klt)),]
NUTS_2_klt <- c("FRY1","FRY2","FRY3","FRY4","FRY5","IS00", "ES70","PT20",
                "PT30","NO0B", "TR10","TR21","TR22","TR31","TR32","TR33",
                "TR41","TR42","TR51","TR52","TR61","TR62","TR63","TR71",
                "TR72","TR81","TR82","TR83","TR90","TRA1","TRA2","TRB1",
                "TRB2","TRC1","TRC2","TRC3","NO02","NO06","NO07","NO08",
                "NO09","NO0A","NO0B","MT00","CY00","RS11","RS12","RS21",
                "RS22","ME00","AL01","AL02","AL03","MK00")
NUTS_2 <- NUTS_2[which(!(NUTS_2$NUTS_ID %in% NUTS_2_klt)),]
NUTS_3_klt <- c("ES703","ES704","ES705","ES706","ES707","ES708","ES709","FRY10",
                "FRY20","FRY30","FRY40","FRY50","IS001","IS002","PT200","PT300",
                "NO0B1","NO0B2","TR100","TR211","TR212","TR213","TR221","TR222",
                "TR310","TR321","TR322","TR323","TR331","TR332","TR333","TR334",
                "TR411","TR412","TR413","TR421","TR422","TR423","TR424","TR425",
                "TR510","TR521","TR522","TR611","TR612","TR613","TR621","TR622",
                "TR631","TR632","TR633","TR711","TR712","TR713","TR714","TR715",
                "TR721","TR722","TR723","TR811","TR812","TR813","TR821","TR822",
                "TR823","TR831","TR832","TR833","TR834","TR901","TR902","TR903",
                "TR904","TR905","TR906","TRA11","TRA12","TRA13","TRA21","TRA22",
                "TRA23","TRA24","TRB11","TRB12","TRB13","TRB14","TRB21","TRB22",
                "TRB23","TRB24","TRC11","TRC12","TRC13","TRC21","TRC22","TRC31",
                "TRC32","TRC33","TRC34","NO020","NO060","NO071","NO074","NO081",
                "NO082","NO091","NO092","NO0A1","NO0A2","NO0A3","NO0B1","NO0B2",
                "MT001","MT002","CY000","RS110","RS121","RS122","RS123","RS124",
                "RS125","RS126","RS127","RS211","RS212","RS213","RS214","RS215",
                "RS216","RS217","RS218","RS221","RS222","RS223","RS224","RS225",
                "RS226","RS227","RS228","RS229","ME000","AL011","AL012","AL013",
                "AL014","AL015","AL021","AL022","AL031","AL032","AL033","AL034",
                "AL035","MK001","MK002","MK003","MK004","MK005","MK006","MK007",
                "MK008")
NUTS_3 <- NUTS_3[which(!(NUTS_3$NUTS_ID %in% NUTS_3_klt)),]

# ha szomszédosak, akkor 0t ad!
# legkisebb távolság a két terület között
distance_1 <- st_distance(NUTS_1$geometry, NUTS_1$geometry)
distance_2 <- st_distance(NUTS_2$geometry, NUTS_2$geometry)

# középpontok távolsága
center_distance_1 <- st_distance(NUTS_1$centroid, NUTS_1$centroid)
center_distance_2 <- st_distance(NUTS_2$centroid, NUTS_2$centroid)

# ábrakészítés
extrafont::font_import("TT Times New Roman")
library(extrafont)
extrafont::loadfonts(device="win")

p1 <- NUTS_3 %>% ggplot() + ggtitle("Városi területek\n") + 
  geom_sf(aes(fill = factor(URBN_TYPE), geometry = geometry))  +
  theme_minimal()+
  theme(plot.title = element_text(color="black", size=25, hjust=0.5, face = "bold"),
        axis.text.y = element_text(color="black", size=16, hjust=0.5, face = "bold"),
        axis.text.x = element_text(color="black", size=16, face = "bold"),                
        axis.ticks.x=element_blank(),
        text = element_text(family = "serif"),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 14, family = "serif"),
        legend.background = element_rect( size=.5)) +
  paletteer::scale_fill_paletteer_d("colorBlindness::paletteMartin")

p2 <- NUTS_3 %>% ggplot() + ggtitle("Tengerparti területek\n") + 
  geom_sf(aes(fill = factor(COAST_TYPE), geometry = geometry))  +
  theme_minimal()+
  theme(plot.title = element_text(color="black", size=25, hjust=0.5, face = "bold"),
        axis.text.y = element_text(color="black", size=16, hjust=0.5, face = "bold"),
        axis.text.x = element_text(color="black", size=16, face = "bold"),                
        axis.ticks.x=element_blank(),
        text = element_text(family = "serif"),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 14, family = "serif"),
        legend.background = element_rect( size=.5)) +
  scale_fill_manual(
    values = c("darkblue", "cornflowerblue", "darkgrey"))

p3 <- NUTS_3 %>% ggplot() + ggtitle("Hegyvidéki területek\n") + 
  geom_sf(aes(fill = factor(MOUNT_TYPE), geometry = geometry)) +
  theme_minimal()+
  theme(plot.title = element_text(color="black", size=25, hjust=0.5, face = "bold"),
        axis.text.y = element_text(color="black", size=16, hjust=0.5, face = "bold"),
        axis.text.x = element_text(color="black", size=16, face = "bold"),                
        axis.ticks.x=element_blank(),
        text = element_text(family = "serif"),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 14, family = "serif"),
        legend.background = element_rect( size=.5)) +
  scale_fill_manual(
    values = c("tan4", "tan3", "yellowgreen", "forestgreen"))

png("NUTS_3_spatial.png", width = 1400, height = 700, units = "px")
grid.arrange(p1, p2, p3, nrow = 1, 
             top = textGrob("NUTS 3 szint területi osztályozása és tipológiái\n\n", 
                            vjust = 1, gp = gpar(fontface = "bold", cex = 3,
                                                 fontfamily= "Times", size = 45)))
dev.off()

# element [i,j] of this matrix has nine characters, referring to relationship 
# between x[i] and y[j], encoded as IxIy,IxBy,IxEy,BxIy,BxBy,BxEy,ExIy,ExBy,ExEy
# where I refers to interior, B to boundary, and E to exterior, and e.g. 
# BxIy the dimensionality of the intersection of the the boundary Bx of x[i] and
# the interior Iy of y[j], which is one of {0,1,2,F}, indicating zero-, one-, 
# two-dimension intersection, and (F) no intersection, respectively.

relation_1 <- st_relate(NUTS_1$geometry, NUTS_1$geometry)
unique(as.vector(relation_1))
# "FF2FF1212" - nincs érintkezés
# "2FFF1FFF2" - átló elemei
# "FF2F112F2" - közös határ
# "FF2F1F212" - közös határ
# "FF2F11212" - közös határ
# "FF2F01212" - határok egy pontban érintkeznek

relation_2 <- st_relate(NUTS_2$geometry, NUTS_2$geometry)

intersection_1 <- st_intersects(NUTS_1$geometry, NUTS_1$geometry, sparse = FALSE)
intersection_2 <- st_intersects(NUTS_2$geometry, NUTS_2$geometry, sparse = FALSE)

save.image("fakt_data_shp.RData")

#------------------------------------------------------------------------------
cities <- read_excel("urb_cpopstr_page_spreadsheet.xlsx",sheet = "Structure")
register_google(key = API_key)
code <- geocode(cities$whole[1])
distance <- data.frame(mapdist(as.character(cities$city[1]), 
                               as.character(cities$city[2]), 
                               override_limit = TRUE, mode = "transit"))

#------------------------------------------------------------------------------
rm(list = ls())
load("data_shp.RData")
load("matrix_nuts_1.RData")

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
data_3 <- test_function(regio_kozep_tav_1)
data_4 <- test_function(kozut_elsodleges_km_1)

data <- rbind(data_1,data_3, data_4)
data$level <- c(rep("elsődleges szomszédság",10000),
                rep("régióközép távolság",10000),
                rep("közút - elsődleges - km",10000))
nrow(data %>% filter(level == "régióközép távolság") %>% select(p_value) %>% filter(p_value < 0.1))/100
nrow(data %>% filter(level == "régióközép távolság") %>% select(p_value) %>% filter(p_value < 0.05))/100

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

#-------------------------------------------------------------------------------
load("eurostat_data_2.RData")
load("matrix_nuts_1.RData")
load("matrix_nuts_2.RData")
load("matrix_nuts_3.RData")
NUTS_1_eredeti <- colnames(NUTS_1)
NUTS_2_eredeti <- colnames(NUTS_2)
NUTS_3_eredeti <- colnames(NUTS_3)
rm(NUTS_1, NUTS_2, NUTS_3)

NUTS_1_n <- NUTS_1_n %>% select(-c(`demo_r_find3$_6_2016`, `demo_r_find3$_6_2017`, 
                                   `demo_r_find3$_6_2018`, `demo_r_find3$_6_2019`,
                                   `demo_r_find3$_3_2016`, `demo_r_find3$_3_2017`,
                                   `demo_r_find3$_3_2018`, `demo_r_find3$_3_2019`,
                                   `demo_r_find3$_1_2016`, `demo_r_find3$_1_2017`,
                                   `demo_r_find3$_1_2018`, `demo_r_find3$_1_2019`,
                                   `cens_11ms_r3$_1_2011`, `cens_11dwob_r3$_1_2011`,
                                   `cens_11ag_r3$_3_2011`, `cens_11ag_r3$_2_2011`,
                                   `cens_11ag_r3$_1_2011`
))

p_value <- vector()
observed <- vector()
expected <- vector()
sd <- vector()
matrix_l <- vector()
col_sd <- vector()
col_range <- vector()
col_skew <- vector()
col_kurt <- vector()
variable <- vector()
col_mean <- vector()
col_na <- vector()
count <- 0

for (i in 1:ncol(NUTS_1_n)){
  if (colnames(NUTS_1_n)[i] %in% NUTS_1_eredeti) next
  print(colnames(NUTS_1_n)[i])
  NUTS_1_n[which(NUTS_1_n[,i] == ":"),i] <- NA
  NUTS_1_n[,i] <- as.numeric(NUTS_1_n[,i])
  
  for (matrix in c("elso_szomszed_1", "kozut_elsodleges_km_1", "kozut_elsodleges_ora_1",
                   "kozut_masodlagos_km_1", "kozut_masodlagos_ora_1", "legkozelebbi_negy_1",
                   "masod_szomszed_1", "min_tav_1", "regio_kozep_tav_1")){
    count <- count + 1
    moran <- Moran.I(NUTS_1_n[,i], get(matrix), na.rm = TRUE)
    p_value[count] <- moran$p.value
    observed[count] <- moran$observed
    expected[count] <- moran$expected
    sd[count] <- moran$sd
    matrix_l[count] <- matrix
    col_sd[count] <- sd(NUTS_1_n[,i], na.rm = TRUE)
    range_v <- range(NUTS_1_n[,i], na.rm = TRUE)
    col_range[count] <- range_v[2] - range_v[1]
    col_skew[count] <- skewness(NUTS_1_n[,i], na.rm = TRUE)
    col_kurt[count] <- kurtosis(NUTS_1_n[,i], na.rm = TRUE)
    variable[count] <- colnames(NUTS_1_n)[i]
    col_mean[count] <- mean(NUTS_1_n[,i], na.rm = TRUE)
    col_na[count] <- length(which(is.na(NUTS_1_n[,i])))
  }
}

moran_1 <- data.frame(p_value = p_value, 
                      observed = observed, 
                      expected = expected,
                      sd = sd,
                      matrix = matrix_l, 
                      col_sd = col_sd,
                      col_range = col_range,
                      col_skew = col_skew,
                      col_kurt = col_kurt,
                      col_mean = col_mean,
                      col_na = col_na,
                      variable = variable)

moran_1$test <- ifelse(moran_1$p_value > 0.05, 0, ifelse(moran_1$observed > moran_1$expected, 1, -1))
moran_1 <- moran_1 %>% filter(col_na < 25)

proba <- moran_1 %>% filter(test == 1)
table(proba$matrix, proba$variable)

proba <- moran_1 %>% filter(test == -1)
table(proba$matrix, proba$variable)
