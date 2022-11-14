library("readxl")
library("psych")
library("car")

library( spdep )
library( maptools )
library( ape )
setwd("C:/Egyetem/GIDI/�ko")
munka <- read_xlsx( "pca.xlsx" )



munka_dist <- as.matrix( dist( cbind( munka$Longitude, munka$Latitude ) ) )
munka_dists_inv <- 1 / munka_dist
diag( munka_dists_inv ) <- 0
munka_dist[ 1:10, 1:10]

Moran.I( munka$jovedelem, munka_dists_inv )
Moran.I( munka$`B(1000)`, munka_dists_inv )
Moran.I( munka$`C(1000)`, munka_dists_inv )
Moran.I( munka$`D(1000)`, munka_dists_inv )
Moran.I( munka$`E(1000)`, munka_dists_inv )

# Másik módszer
Coords <- cbind( munka$Longitude, munka$Latitude )

moranIFunction <- function( data ){
  mI <- moransI( Coords, 20, data )
  moran.table <- matrix( data = NA, nrow = 1, ncol = 6 )
  col.names <- c( "Moran's I", "Expected I", "Z resampling", "P-value resampling",
                  "Z randomization", "P-value randomization" )
  colnames( moran.table ) <- col.names
  moran.table[ 1, 1 ] <- mI$Morans.I
  moran.table[ 1, 2 ] <- mI$Expected.I
  moran.table[ 1, 3 ] <- mI$z.resampling
  moran.table[ 1, 4 ] <- mI$p.value.resampling
  moran.table[ 1, 5 ] <- mI$z.randomization
  moran.table[ 1, 6 ] <- mI$p.value.randomization
  moran.table
}

moranIFunction( munka$`A(1000)` )
moranIFunction( munka$`B(1000)` )
moranIFunction( munka$`C(1000)` )
moranIFunction( munka$`D(1000)` )
moranIFunction( munka$`E(1000)` )


mI <- moransI( Coords, 20, munka$`A(1000)` )
moran.table <- matrix( data = NA, nrow = 1, ncol = 6 )
col.names <- c( "Moran's I", "Expected I", "Z resampling", "P-value resampling",
               "Z randomization", "P-value randomization" )
colnames( moran.table ) <- col.names
moran.table[ 1, 1 ] <- mI$Morans.I
moran.table[ 1, 2 ] <- mI$Expected.I
moran.table[ 1, 3 ] <- mI$z.resampling
moran.table[ 1, 4 ] <- mI$p.value.resampling
moran.table[ 1, 5 ] <- mI$z.randomization
moran.table[ 1, 6 ] <- mI$p.value.randomization
moran.table


#install.packages( "pgirmess" )
library( pgirmess )

moran_correlog  <- correlog( Coords, munka$`Regisztrált_Munkanélküliek(1000munkaképesre)`, method = "Moran" )
plot( moran_correlog )

geary_correlog <- correlog( Coords, munka$A, method = "Geary" )
plot( geary_correlog )
geary_correlog

neib <- dnearneigh( coordinates( Coords ), 0, 40, longlat = TRUE)
listw <- nb2listw( neib )
geary( munka$`A(1000)`, listw, 175, 174, Szero( listw ) )
geary( munka$`B(1000)`, listw, 175, 174, Szero( listw ) )
geary( munka$`C(1000)`, listw, 175, 174, Szero( listw ) )
geary( munka$`D(1000)`, listw, 175, 174, Szero( listw ) )
geary( munka$`E(1000)`, listw, 175, 174, Szero( listw ) )


# Global G

globalG.test( munka$`A(1000)`, listw, zero.policy = TRUE, alternative = "two.sided" )
globalG.test( munka$`B(1000)`, listw, zero.policy = TRUE, alternative = "two.sided" )
globalG.test( munka$`C(1000)`, listw, zero.policy = TRUE, alternative = "two.sided" )
globalG.test( munka$`D(1000)`, listw, zero.policy = TRUE, alternative = "two.sided" )
globalG.test( munka$`E(1000)`, listw, zero.policy = TRUE, alternative = "two.sided" )

# Moran I

moran( munka$`A(1000)`, listw, 175, 174, zero.policy = NULL, NAOK = FALSE )
moran( munka$`B(1000)`, listw, 175, 174, zero.policy = NULL, NAOK = FALSE )
moran( munka$`C(1000)`, listw, 175, 174, zero.policy = NULL, NAOK = FALSE )
moran( munka$`D(1000)`, listw, 175, 174, zero.policy = NULL, NAOK = FALSE )
moran( munka$`E(1000)`, listw, 175, 174, zero.policy = NULL, NAOK = FALSE )


#######
# SAR
#######

munka_SAR <-lagsarlm( log_munkanelkuli ~ terulet + jovedelem + 
                        onkormanyzati_beruhazasok + onkormanyzati_bevetelek + 
                        nepesseg_1 + nepesseg_2 + nepesseg_3 + nepesseg_4 + 
                        kultura_5 + kultura_6 + kultura_7 + kultura_8 + oktatas_1 + 
                        oktatas_2 + vallalatok_1 + vallalatok_2 + infrastuktura_1 + 
                        infrastuktura_2 + infrastuktura_3 + kultura_1 + kultura_2 + 
                        kultura_3 + kultura_4 + egeszsegugy_1 + egeszsegugy_2, 
                      data = munka, listw )
summary( munka_SAR )
