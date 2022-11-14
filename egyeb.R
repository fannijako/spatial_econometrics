load("d:/Users/Fanni/Desktop/Területi autokorreláció/data_shp.RData")
LoadLibrary()
#-------------------------------------------------------------------------
knearest_plot <- function(object, k_n, size = 2){
  
  coords_1 <- coordinates(as(object, "Spatial")) # koordináta dataframe
  col.knn_1_4 <- knearneigh(coords_1, k = k_n) #$nn: i. elemhez melyik 4 vagy a legközelebb
  nb <- nb2listw(knn2nb(col.knn_1_4))
  n <-  length(attributes(nb$neighbours)$region.id)
  
  DA <-  data.frame(
    from = rep(1:n,sapply(nb$neighbours,length)),
    to = unlist(nb$neighbours),
    weight = unlist(nb$weights)
  )
  DA <- cbind(DA,coords_1[DA$from,1:2],coords_1[DA$to,1:2])
  colnames(DA)[4:7] <- c("long","lat","long_to","lat_to")
  
  object %>% ggplot() +
    geom_sf(aes(geometry = geometry), color ='black') + 
    geom_segment(data = DA, aes(x = long, y = lat, xend = long_to,yend = lat_to),
                 size = 0.3, alpha = 0.5) +
    geom_point(data = DA, aes(x = long, y = lat), size = size)
}

knearest_plot(NUTS_1, 4, size = 1)

#--------------------------------------------------------------------------
nc.5nn.mat <- nb2mat(knn2nb(knearneigh(coordinates(as(NUTS_1, "Spatial")), k = 4)))

queen.R.nb <- poly2nb(NUTS_1, row.names = NUTS_1$NUTS_ID) # szomszédsági mátrix
summary(queen.R.nb)

coords<-coordinates(NUTS_1$geometry)
W_dist<-dnearneigh(coords,0,1,longlat = FALSE)

#-------------------------------------------------------------------------------
leaflet() %>%
  setView(lng = -1.47, lat = 52.3, zoom = 6) %>%
  addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
  addPolygons(data = NUTS_1$geometry,
              color = "blue",
              fillOpacity = 0,
              weight  = 1)

#-------------------------------------------------------------------------------