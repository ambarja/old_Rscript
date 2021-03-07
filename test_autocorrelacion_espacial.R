library(spdep)
library(sf)
library(tidyverse)

def <- st_read('area_deforestada.gpkg')
vecidad <- poly2nb(def,queen = TRUE)

plot(as(def[1],'Spatial'), border="lightgrey")
plot(vecidad, coordinates(as(def[1],'Spatial')),
     pch = 19, cex = 0.6, add = TRUE,
     col= "red", main="Queen Contiguity")

m_ps <- nb2listw(vecidad,style = 'W',zero.policy = TRUE)

# Test de Moran 
moran.test(x = def$Area_km2,listw = m_ps,zero.policy = TRUE)
moran.plot(x = def$Area_km2,listw = m_ps,
           xlab = "Areas deforestadas",
           ylab = "Areas deforestadas(spatial lag)",
           main = "Moran Scatterplot - Areas deforestadas en la amazonía peruana")

# Test de Geray 
geary.test(x = def$Area_km2,listw = m_ps,zero.policy = TRUE)

# Moran local 
local <- localmoran(x = def$Area_km2,listw = m_ps,zero.policy = TRUE)
summary(local)
names(def)

lisa_map <- cbind(def,local)

# Mapas de indices locales 

library(tmap)
lisa_map %>% 
  tm_shape() +
  tm_fill('Ii',
          style = 'quantile',
          palette = 'viridis',
          title = 'Local Moran I',n = 5) + 
  tm_borders(lwd = 0.2,col = 'white')
  
# tmap_save(tmap_last(),'Local_Mora.png')

# Potenciales cluster
lisa_map$cluster[lisa_map$Area_km2 > mean(lisa_map$Area_km2) & lisa_map$Ii >0 ] <- 1 # HH
lisa_map$cluster[lisa_map$Area_km2 < mean(lisa_map$Area_km2) & lisa_map$Ii >0 ] <- 2 # LL
lisa_map$cluster[lisa_map$Area_km2 < mean(lisa_map$Area_km2) & lisa_map$Ii <0 ] <- 3 # HL
lisa_map$cluster[lisa_map$Area_km2 > mean(lisa_map$Area_km2) & lisa_map$Ii <0 ] <- 4 # LH

# Confiabilidad del 90% 
lisa_map$cluster[lisa_map$Pr.z...0. >0.1] <- 0  # No significativo

dep <- st_read('departamento.gpkg')

tm_shape(dep) + 
  tm_borders(lwd = 0.5,col = 'black') + 
  tm_text(text = "NOMBDEP") + 
  tm_shape(lisa_map) +
  tm_fill('cluster',
          style = 'cat',
          palette = c('white','red','blue'),
          labels = c('No significant','High-High','Low-Low'),
          title = 'LISA Cluster',n = 5) + 
  tm_borders(lwd = 0.2,col = 'white') 


# Gi* statistic 

gi_map <- def %>% st_transform(32718)

# coordenas x - y 

x <- map_dbl(gi_map$geom,~st_centroid(.x)[[1]]) # este
y <- map_dbl(gi_map$geom,~st_centroid(.x)[[2]]) # norte

# juntando ambas coordenadas
xy <- cbind(x,y)
class(xy)
head(xy)

# Construyendo vecindad 
nbgi <- dnearneigh(xy,d1 = 0,d2 = 200000)
plot(vecidad, coordinates(as(gi_map[1],'Spatial')),
     pch = 19, cex = 0.6)


# Implementado el Gi* statistic 
gi_start <- localG(x = gi_map$Area_km2,
                   listw = nb2listw(include.self(nbgi),
                                    style = 'B'))

#  Uniendo los indices de gi * al df

gi_star_map <- cbind(gi_map,as.matrix(gi_start))
names(gi_star_map)[7] <- 'Gi'
write_sf(gi_star_map,'gi_map.gpkg')
