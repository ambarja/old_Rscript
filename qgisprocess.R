# install.packages("remotes")
remotes::install_github("paleolimbot/qgisprocess")
library(sf)
library(qgisprocess)
library(tidyverse)

malaria_regions <- function(bd_malaria, splayer){
  region <- bd_malaria 
  malaria_n <- region %>% 
    st_set_geometry(NULL) %>% 
    group_by(district,year,month) %>% 
    summarise(fal = sum(fal),
              viv = sum(viv),
              nrohab = sum(nrohab)) %>% 
    left_join(x = district,y = .,by = 'district') %>% 
    st_centroid()
  spatial_region <- splayer
  spdata <- spatial_region %>% 
    qgis_run_algorithm(algorithm = 'native:joinattributesbylocation',
                       INPUT = .,
                       JOIN = malaria_n,
                       PREDICATE = 1,
                       METHOD = 0,
                       OUTPUT = qgis_tmp_vector(),
                       .quiet = TRUE) %>% 
    qgis_output('OUTPUT') %>% 
    st_read() %>% drop_na() %>% 
    select(-fid_2)
  return(spdata)
}

data <- read_rds('malaria_basins.rds')
region <- st_read('district.gpkg')
newdata <- malaria_regions(bd_malaria = data,splayer = region)
saveRDS(object = newdata,'malaria_newdataset.rds')

