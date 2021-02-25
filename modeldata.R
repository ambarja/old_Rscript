library(tidyverse)
library(qgisprocess)
library(sf)
# Functions to remove geometry
remove_geom <- function(x){
  data <- x %>% st_set_geometry(NULL)
  return(data)
}

# 1. Reading of data  -----------------------------------------------------
esco <- read_rds('escorrentia.rds') %>% lapply(remove_geom)
etp  <- read_rds('etp.rds') %>% lapply(remove_geom)
evi <- read_rds('evi.rds') %>% lapply(remove_geom)
hsoil <- read_rds('hsoil.rds') %>% lapply(remove_geom)
ndvi <- read_rds('ndvi.rds') %>% lapply(remove_geom)
pp <- read_rds('pp.rds') %>% lapply(remove_geom)
savi <- read_rds('savi.rds') %>% lapply(remove_geom)
temp <- read_rds('temp.rds') %>% lapply(remove_geom)



# 2. Cooking data ---------------------------------------------------------
esco <- esco %>%
  bind_rows() %>% 
  gather(key = variable,valor,X2009Jan:X2019Jan) %>% 
  filter(valor != 'NA') %>% 
  mutate(year = extract_numeric(variable),
         month  = as.character(str_extract_all(variable, "[A-Z][a-z]+")),
         variable = 'esco') %>% 
  filter(year != 2019)

etp <- etp %>%
  bind_rows() %>% 
  gather(key = variable,valor,X2009Jan:X2019Jan) %>% 
  filter(valor != 'NA') %>% 
  mutate(year = extract_numeric(variable),
         month = as.character(str_extract_all(variable, "[A-Z][a-z]+")),
         variable = 'etp') %>% 
  filter(year != 2019)

evi <- evi %>%
  bind_rows() %>% 
  gather(key = variable,valor,X2009Jan:X2019Jan) %>% 
  filter(valor != 'NA') %>% 
  mutate(year = extract_numeric(variable),
         month  = as.character(str_extract_all(variable, "[A-Z][a-z]+")),
         variable = 'evi') %>% 
  filter(year != 2019)

hsoil <- hsoil %>%
  bind_rows() %>% 
  gather(key = variable,valor,X2009Jan:X2019Jan) %>% 
  filter(valor != 'NA') %>% 
  mutate(year = extract_numeric(variable),
         month  = as.character(str_extract_all(variable, "[A-Z][a-z]+")),
         variable = 'hsoil') %>% 
  filter(year != 2019)

ndvi <- ndvi %>%
  bind_rows() %>% 
  gather(key = variable,valor,X2009Jan:X2019Jan) %>% 
  filter(valor != 'NA') %>% 
  mutate(year = extract_numeric(variable),
         month  = as.character(str_extract_all(variable, "[A-Z][a-z]+")),
         variable = 'ndvi') %>% 
  filter(year != 2019)

pp <- pp %>%
  bind_rows() %>% 
  gather(key = variable,valor,X2009Jan:X2019Jan) %>% 
  filter(valor != 'NA') %>% 
  mutate(year = extract_numeric(variable),
         month = as.character(str_extract_all(variable, "[A-Z][a-z]+")),
         variable = 'pp') %>% 
  filter(year != 2019)

savi <- savi %>%
  bind_rows() %>% 
  gather(key = variable,valor,savi2009Jan:savi2018Dec) %>% 
  filter(valor != 'NA') %>% 
  mutate(year = extract_numeric(variable),
         month = as.character(str_extract_all(variable, "[A-Z][a-z]+")),
         variable = 'savi') %>% 
  filter(year != 2019)

temp <- temp %>%
  bind_rows() %>% 
  gather(key = variable,valor,X2009Jan:X2019Jan) %>% 
  filter(valor != 'NA') %>% 
  mutate(year = extract_numeric(variable),
         month = as.character(str_extract_all(variable, "[A-Z][a-z]+")),
         variable = 'temp') %>% 
  filter(year != 2019)

malaria <- read_rds('malaria_newdataset.rds') %>% 
  st_set_geometry(NULL) %>% 
  mutate(id = 1:n()) %>% 
  select(id, codigo,district,year,month,fal,viv,nrohab) %>% 
  gather(key = variable,valor,fal:viv:nrohab) %>% 
  filter(valor != 'NA') 

# 4. New dataset ----------------------------------------------------------
predictores <- rbind(esco,etp,evi,hsoil,ndvi,pp,savi,temp)
malaria <- malaria %>% select(id,codigo,district,variable,valor,year,month)
alldataset <- rbind(malaria,predictores)
saveRDS(object = alldataset,'alldataset.rds')
