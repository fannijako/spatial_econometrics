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
}

LoadLibrary()
#-------------------------------------------------------------------------------
# forrás : https://ec.europa.eu/eurostat/web/gisco/geodata/reference-data/administrative-units-statistical-units/nuts
# NUTS 2021, SHP, Polygons, 20M, EPSG:4326
shp <- read_sf('d:/Users/Fanni/Desktop/Területi autokorreláció/NUTS_RG_01M_2021_4326.shp/NUTS_RG_01M_2021_4326.shp')

#------------------------------------------------------------------------------
# https://r-spatial.github.io/sf/reference/geos_unary.html

shp$centroid <- st_centroid(shp$geometry, of_largest_polygon = TRUE)

# multipolygon -> multilinestring
shp$boundary <- st_boundary(shp$geometry)

# adott sugarú kör
# inscribed_circle <- st_inscribed_circle(shp$geometry, dTolerance = 3000)
# shp$inscribed_circle <- inscribed_circle[1:2010]

# vonalakból poligont csinál
# shp$poligonize <- st_polygonize(shp$geometry)

# multilinestring vonalakat egyesít
# shp$line_merge <- st_line_merge(shp$boundary)

# random pont a poligon felszínén
# shp$point_on_surface <- st_point_on_surface(shp$geometry)

#------------------------------------------------------------------------------
# https://r-spatial.github.io/sf/articles/sf3.html
shp$area <- st_area(shp$geometry)
shp$border <- st_length(shp$boundary)

#-------------------------------------------------------------------------------
NUTS_3 <- shp[shp$LEVL_CODE == 3, ]
NUTS_2 <- shp[shp$LEVL_CODE == 2, ]
NUTS_1 <- shp[shp$LEVL_CODE == 1, ]

# külterületek eltávolítása
NUTS_1 <- NUTS_1[which(NUTS_1$NUTS_ID != "PT2" & NUTS_1$NUTS_ID != "PT3" & 
                         NUTS_1$NUTS_ID != "FRY" & NUTS_1$NUTS_ID != "IS0" & 
                         NUTS_1$NUTS_ID != "ES7"),]
NUTS_1 <- NUTS_1[which(NUTS_1$NUTS_ID != "TR1" & NUTS_1$NUTS_ID != "TR2" & 
                         NUTS_1$NUTS_ID != "TR3" & NUTS_1$NUTS_ID != "TR4" & 
                         NUTS_1$NUTS_ID != "TR5" & NUTS_1$NUTS_ID != "TR6" & 
                         NUTS_1$NUTS_ID != "TR7" & NUTS_1$NUTS_ID != "TR8" & 
                         NUTS_1$NUTS_ID != "TR9" & NUTS_1$NUTS_ID != "TRA" & 
                         NUTS_1$NUTS_ID != "TRB" & NUTS_1$NUTS_ID != "TRC" & 
                         NUTS_1$NUTS_ID != "CY0" & NUTS_1$NUTS_ID != "RS1" & 
                         NUTS_1$NUTS_ID != "RS2" & NUTS_1$NUTS_ID != "ME0" & 
                         NUTS_1$NUTS_ID != "AL0" & NUTS_1$NUTS_ID != "MK0" &
                         NUTS_1$NUTS_ID != "MT0" & NUTS_1$NUTS_ID != "NO0"),]
NUTS_2 <- NUTS_2[which(NUTS_2$NUTS_ID != "FRY1" & NUTS_2$NUTS_ID != "FRY2" & 
                         NUTS_2$NUTS_ID != "FRY3" & NUTS_2$NUTS_ID != "FRY4" & 
                         NUTS_2$NUTS_ID != "FRY5" & NUTS_2$NUTS_ID != "IS00" & 
                         NUTS_2$NUTS_ID != "ES70" & NUTS_2$NUTS_ID != "PT20" & 
                         NUTS_2$NUTS_ID != "PT30" & NUTS_2$NUTS_ID != "NO0B" ),]
NUTS_2 <- NUTS_2[which(NUTS_2$NUTS_ID != "TR10" & NUTS_2$NUTS_ID != "TR21" &
                       NUTS_2$NUTS_ID != "TR22" & NUTS_2$NUTS_ID !=  "TR31" & 
                       NUTS_2$NUTS_ID != "TR32" & NUTS_2$NUTS_ID != "TR33" & 
                       NUTS_2$NUTS_ID != "TR41" & NUTS_2$NUTS_ID !=  "TR42" & 
                       NUTS_2$NUTS_ID != "TR51" & NUTS_2$NUTS_ID !=  "TR52" & 
                       NUTS_2$NUTS_ID != "TR61" & NUTS_2$NUTS_ID !=  "TR62" & 
                       NUTS_2$NUTS_ID != "TR63" & NUTS_2$NUTS_ID !=  "TR71" & 
                       NUTS_2$NUTS_ID != "TR72" & NUTS_2$NUTS_ID !=  "TR81" & 
                       NUTS_2$NUTS_ID != "TR82" & NUTS_2$NUTS_ID !=  "TR83" & 
                       NUTS_2$NUTS_ID != "TR90" & NUTS_2$NUTS_ID !=  "TRA1" & 
                       NUTS_2$NUTS_ID != "TRA2" & NUTS_2$NUTS_ID !=  "TRB1" & 
                       NUTS_2$NUTS_ID != "TRB2" & NUTS_2$NUTS_ID !=  "TRC1" & 
                       NUTS_2$NUTS_ID != "TRC2" & NUTS_2$NUTS_ID !=  "TRC3" & 
                       NUTS_2$NUTS_ID != "NO02" & NUTS_2$NUTS_ID !=  "NO06" & 
                       NUTS_2$NUTS_ID != "NO07" & NUTS_2$NUTS_ID !=  "NO08" & 
                       NUTS_2$NUTS_ID != "NO09" & NUTS_2$NUTS_ID !=  "NO0A" & 
                       NUTS_2$NUTS_ID != "NO0B" & NUTS_2$NUTS_ID !=  "MT00" & 
                       NUTS_2$NUTS_ID != "CY00" & NUTS_2$NUTS_ID !=  "RS11" & 
                       NUTS_2$NUTS_ID != "RS12" & NUTS_2$NUTS_ID !=  "RS21" & 
                       NUTS_2$NUTS_ID != "RS22" & NUTS_2$NUTS_ID !=  "ME00" & 
                       NUTS_2$NUTS_ID != "AL01" & NUTS_2$NUTS_ID !=  "AL02" & 
                       NUTS_2$NUTS_ID != "AL03" & NUTS_2$NUTS_ID !=  "MK00"),]
NUTS_3 <- NUTS_3[which(NUTS_3$NUTS_ID != "ES703" & NUTS_3$NUTS_ID != "ES704" &
                         NUTS_3$NUTS_ID != "ES705" & NUTS_3$NUTS_ID != "ES706" &
                         NUTS_3$NUTS_ID != "ES707" & NUTS_3$NUTS_ID != "ES708" &
                         NUTS_3$NUTS_ID != "ES709" & NUTS_3$NUTS_ID != "FRY10" &
                         NUTS_3$NUTS_ID != "FRY20" & NUTS_3$NUTS_ID != "FRY30" &
                         NUTS_3$NUTS_ID != "FRY40" & NUTS_3$NUTS_ID != "FRY50" &
                         NUTS_3$NUTS_ID != "IS001" & NUTS_3$NUTS_ID != "IS002" &
                         NUTS_3$NUTS_ID != "PT200" & NUTS_3$NUTS_ID != "PT300" &
                         NUTS_3$NUTS_ID != "NO0B1" & NUTS_3$NUTS_ID != "NO0B2" ),]
NUTS_3 <- NUTS_3[which(NUTS_3$NUTS_ID != "TR100" & NUTS_3$NUTS_ID != "TR211" & 
                        NUTS_3$NUTS_ID != "TR212" & NUTS_3$NUTS_ID != "TR213" & 
                        NUTS_3$NUTS_ID != "TR221" & NUTS_3$NUTS_ID != "TR222" & 
                        NUTS_3$NUTS_ID != "TR310" & NUTS_3$NUTS_ID != "TR321" & 
                        NUTS_3$NUTS_ID != "TR322" & NUTS_3$NUTS_ID != "TR323" & 
                        NUTS_3$NUTS_ID != "TR331" & NUTS_3$NUTS_ID != "TR332" & 
                        NUTS_3$NUTS_ID != "TR333" & NUTS_3$NUTS_ID != "TR334" & 
                        NUTS_3$NUTS_ID != "TR411" & NUTS_3$NUTS_ID != "TR412" & 
                        NUTS_3$NUTS_ID != "TR413" & NUTS_3$NUTS_ID != "TR421" & 
                        NUTS_3$NUTS_ID != "TR422" & NUTS_3$NUTS_ID != "TR423" & 
                        NUTS_3$NUTS_ID != "TR424" & NUTS_3$NUTS_ID != "TR425" & 
                        NUTS_3$NUTS_ID != "TR510" & NUTS_3$NUTS_ID != "TR521" & 
                        NUTS_3$NUTS_ID != "TR522" & NUTS_3$NUTS_ID != "TR611" & 
                        NUTS_3$NUTS_ID != "TR612" & NUTS_3$NUTS_ID != "TR613" & 
                        NUTS_3$NUTS_ID != "TR621" & NUTS_3$NUTS_ID != "TR622" & 
                        NUTS_3$NUTS_ID != "TR631" & NUTS_3$NUTS_ID != "TR632" & 
                        NUTS_3$NUTS_ID != "TR633" & NUTS_3$NUTS_ID != "TR711" & 
                        NUTS_3$NUTS_ID != "TR712" & NUTS_3$NUTS_ID != "TR713" & 
                        NUTS_3$NUTS_ID != "TR714" & NUTS_3$NUTS_ID != "TR715" & 
                        NUTS_3$NUTS_ID != "TR721" & NUTS_3$NUTS_ID != "TR722" & 
                        NUTS_3$NUTS_ID != "TR723" & NUTS_3$NUTS_ID != "TR811" & 
                        NUTS_3$NUTS_ID != "TR812" & NUTS_3$NUTS_ID != "TR813" & 
                        NUTS_3$NUTS_ID != "TR821" & NUTS_3$NUTS_ID != "TR822" & 
                        NUTS_3$NUTS_ID != "TR823" & NUTS_3$NUTS_ID != "TR831" & 
                        NUTS_3$NUTS_ID != "TR832" & NUTS_3$NUTS_ID != "TR833" & 
                        NUTS_3$NUTS_ID != "TR834" & NUTS_3$NUTS_ID != "TR901" & 
                        NUTS_3$NUTS_ID != "TR902" & NUTS_3$NUTS_ID != "TR903" & 
                        NUTS_3$NUTS_ID != "TR904" & NUTS_3$NUTS_ID != "TR905" & 
                        NUTS_3$NUTS_ID != "TR906" & NUTS_3$NUTS_ID != "TRA11" & 
                        NUTS_3$NUTS_ID != "TRA12" & NUTS_3$NUTS_ID != "TRA13" & 
                        NUTS_3$NUTS_ID != "TRA21" & NUTS_3$NUTS_ID != "TRA22" & 
                        NUTS_3$NUTS_ID != "TRA23" & NUTS_3$NUTS_ID != "TRA24" & 
                        NUTS_3$NUTS_ID != "TRB11" & NUTS_3$NUTS_ID != "TRB12" & 
                        NUTS_3$NUTS_ID != "TRB13" & NUTS_3$NUTS_ID != "TRB14" & 
                        NUTS_3$NUTS_ID != "TRB21" & NUTS_3$NUTS_ID != "TRB22" & 
                        NUTS_3$NUTS_ID != "TRB23" & NUTS_3$NUTS_ID != "TRB24" & 
                        NUTS_3$NUTS_ID != "TRC11" & NUTS_3$NUTS_ID != "TRC12" & 
                        NUTS_3$NUTS_ID != "TRC13" & NUTS_3$NUTS_ID != "TRC21" & 
                        NUTS_3$NUTS_ID != "TRC22" & NUTS_3$NUTS_ID != "TRC31" & 
                        NUTS_3$NUTS_ID != "TRC32" & NUTS_3$NUTS_ID != "TRC33" & 
                        NUTS_3$NUTS_ID != "TRC34" & NUTS_3$NUTS_ID != "NO020" & 
                        NUTS_3$NUTS_ID != "NO060" & NUTS_3$NUTS_ID != "NO071" & 
                        NUTS_3$NUTS_ID != "NO074" & NUTS_3$NUTS_ID != "NO081" & 
                        NUTS_3$NUTS_ID != "NO082" & NUTS_3$NUTS_ID != "NO091" & 
                        NUTS_3$NUTS_ID != "NO092" & NUTS_3$NUTS_ID != "NO0A1" & 
                        NUTS_3$NUTS_ID != "NO0A2" & NUTS_3$NUTS_ID != "NO0A3" & 
                        NUTS_3$NUTS_ID != "NO0B1" & NUTS_3$NUTS_ID != "NO0B2" & 
                        NUTS_3$NUTS_ID != "MT001" & NUTS_3$NUTS_ID != "MT002" & 
                        NUTS_3$NUTS_ID != "CY000" & NUTS_3$NUTS_ID != "RS110" & 
                        NUTS_3$NUTS_ID != "RS121" & NUTS_3$NUTS_ID != "RS122" & 
                        NUTS_3$NUTS_ID != "RS123" & NUTS_3$NUTS_ID != "RS124" & 
                        NUTS_3$NUTS_ID != "RS125" & NUTS_3$NUTS_ID != "RS126" & 
                        NUTS_3$NUTS_ID != "RS127" & NUTS_3$NUTS_ID != "RS211" & 
                        NUTS_3$NUTS_ID != "RS212" & NUTS_3$NUTS_ID != "RS213" & 
                        NUTS_3$NUTS_ID != "RS214" & NUTS_3$NUTS_ID != "RS215" & 
                        NUTS_3$NUTS_ID != "RS216" & NUTS_3$NUTS_ID != "RS217" & 
                        NUTS_3$NUTS_ID != "RS218" & NUTS_3$NUTS_ID != "RS221" & 
                        NUTS_3$NUTS_ID != "RS222" & NUTS_3$NUTS_ID != "RS223" & 
                        NUTS_3$NUTS_ID != "RS224" & NUTS_3$NUTS_ID != "RS225" & 
                        NUTS_3$NUTS_ID != "RS226" & NUTS_3$NUTS_ID != "RS227" & 
                        NUTS_3$NUTS_ID != "RS228" & NUTS_3$NUTS_ID != "RS229" & 
                        NUTS_3$NUTS_ID != "ME000" & NUTS_3$NUTS_ID != "AL011" & 
                        NUTS_3$NUTS_ID != "AL012" & NUTS_3$NUTS_ID != "AL013" & 
                        NUTS_3$NUTS_ID != "AL014" & NUTS_3$NUTS_ID != "AL015" & 
                        NUTS_3$NUTS_ID != "AL021" & NUTS_3$NUTS_ID != "AL022" & 
                        NUTS_3$NUTS_ID != "AL031" & NUTS_3$NUTS_ID != "AL032" & 
                        NUTS_3$NUTS_ID != "AL033" & NUTS_3$NUTS_ID != "AL034" & 
                        NUTS_3$NUTS_ID != "AL035" & NUTS_3$NUTS_ID != "MK001" & 
                        NUTS_3$NUTS_ID != "MK002" & NUTS_3$NUTS_ID != "MK003" & 
                        NUTS_3$NUTS_ID != "MK004" & NUTS_3$NUTS_ID != "MK005" & 
                        NUTS_3$NUTS_ID != "MK006" & NUTS_3$NUTS_ID != "MK007" & 
                        NUTS_3$NUTS_ID != "MK008"),]
# https://r-spatial.github.io/sf/articles/sf3.html
# https://geocompr.robinlovelace.net/spatial-operations.html
# ha szomszédosak, akkor 0t ad!
# legkisebb távolság a két terület között

distance_1 <- st_distance(NUTS_1$geometry, NUTS_1$geometry)
distance_2 <- st_distance(NUTS_2$geometry, NUTS_2$geometry)
distance_3 <- st_distance(NUTS_3$geometry, NUTS_3$geometry)

# középpontok távolsága
center_distance_1 <- st_distance(NUTS_1$centroid, NUTS_1$centroid)
center_distance_2 <- st_distance(NUTS_2$centroid, NUTS_2$centroid)
center_distance_3 <- st_distance(NUTS_3$centroid, NUTS_3$centroid)

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
             top = textGrob("NUTS 3 szint területi osztályozása és tipológiái\n\n", vjust = 1, gp = gpar(fontface = "bold", cex = 3, fontfamily= "Times", size = 45)))
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
unique(as.vector(relation_2))

relation_3 <- st_relate(NUTS_3$geometry, NUTS_3$geometry)
unique(as.vector(relation_3))

intersection_1 <- st_intersects(NUTS_1$geometry, NUTS_1$geometry, sparse = FALSE)
intersection_2 <- st_intersects(NUTS_2$geometry, NUTS_2$geometry, sparse = FALSE)
intersection_3 <- st_intersects(NUTS_3$geometry, NUTS_3$geometry, sparse = FALSE)

save.image("d:/Users/Fanni/Desktop/Területi autokorreláció/data_shp.RData")