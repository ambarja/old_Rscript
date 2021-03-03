library(tidyverse)
library(sf)
# Functions to remove geometry
remove_geom <- function(x){
  data <- x %>% st_set_geometry(NULL)
  return(data)
}

# 1. Reading of data  -----------------------------------------------------
esco <- read_rds('datos_mensuales/escorrentia.rds') %>% lapply(remove_geom)
etp  <- read_rds('datos_mensuales/etp.rds') %>% lapply(remove_geom)
evi <- read_rds('datos_mensuales/evi.rds') %>% lapply(remove_geom)
hsoil <- read_rds('datos_mensuales/hsoil.rds') %>% lapply(remove_geom)
ndvi <- read_rds('datos_mensuales/ndvi.rds') %>% lapply(remove_geom)
pp <- read_rds('datos_mensuales/pp.rds') %>% lapply(remove_geom)
savi <- read_rds('datos_mensuales/savi.rds') %>% lapply(remove_geom)
temp <- read_rds('datos_mensuales/temp.rds') %>% lapply(remove_geom)

# esco <- read_rds('escorrentia.rds')
# etp  <- read_rds('etp.rds')
# evi <- read_rds('evi.rds')
# hsoil <- read_rds('hsoil.rds')
# ndvi <- read_rds('ndvi.rds')
# pp <- read_rds('pp.rds')
# savi <- read_rds('savi.rds')
# temp <- read_rds('temp.rds')

# 2. Cooking data ---------------------------------------------------------
esco <- esco %>%
  bind_rows() %>% 
  gather(key = variable,valor,X2009Jan:X2019Jan) %>% 
  filter(valor != 'NA') %>% 
  mutate(year = extract_numeric(variable),
         month  = as.character(str_extract_all(variable, "[A-Z][a-z]+")),
         variable = 'esco') %>%
  filter(year != 2019) %>%
  mutate(month = factor(month,levels = month.abb,labels = 1:12))
  
etp <- etp %>%
  bind_rows() %>% 
  gather(key = variable,valor,X2009Jan:X2019Jan) %>% 
  filter(valor != 'NA') %>% 
  mutate(year = extract_numeric(variable),
         month = as.character(str_extract_all(variable, "[A-Z][a-z]+")),
         variable = 'etp') %>% 
  filter(year != 2019)%>%
  mutate(month = factor(month,levels = month.abb,labels = 1:12))


evi <- evi %>%
  bind_rows() %>% 
  gather(key = variable,valor,X2009Jan:X2019Jan) %>% 
  filter(valor != 'NA') %>% 
  mutate(year = extract_numeric(variable),
         month  = as.character(str_extract_all(variable, "[A-Z][a-z]+")),
         variable = 'evi') %>% 
  filter(year != 2019)%>%
  mutate(month = factor(month,levels = month.abb,labels = 1:12))


hsoil <- hsoil %>%
  bind_rows() %>% 
  gather(key = variable,valor,X2009Jan:X2019Jan) %>% 
  filter(valor != 'NA') %>% 
  mutate(year = extract_numeric(variable),
         month  = as.character(str_extract_all(variable, "[A-Z][a-z]+")),
         variable = 'hsoil') %>% 
  filter(year != 2019)%>%
  mutate(month = factor(month,levels = month.abb,labels = 1:12))


ndvi <- ndvi %>%
  bind_rows() %>% 
  gather(key = variable,valor,X2009Jan:X2019Jan) %>% 
  filter(valor != 'NA') %>% 
  mutate(year = extract_numeric(variable),
         month  = as.character(str_extract_all(variable, "[A-Z][a-z]+")),
         variable = 'ndvi') %>% 
  filter(year != 2019)%>%
  mutate(month = factor(month,levels = month.abb,labels = 1:12))


pp <- pp %>%
  bind_rows() %>% 
  gather(key = variable,valor,X2009Jan:X2019Jan) %>% 
  filter(valor != 'NA') %>% 
  mutate(year = extract_numeric(variable),
         month = as.character(str_extract_all(variable, "[A-Z][a-z]+")),
         variable = 'pp') %>% 
  filter(year != 2019)%>%
  mutate(month = factor(month,levels = month.abb,labels = 1:12))


savi <- savi %>%
  bind_rows() %>% 
  gather(key = variable,valor,savi2009Jan:savi2018Dec) %>% 
  filter(valor != 'NA') %>% 
  mutate(year = extract_numeric(variable),
         month = as.character(str_extract_all(variable, "[A-Z][a-z]+")),
         variable = 'savi') %>% 
  filter(year != 2019)%>%
  mutate(month = factor(month,levels = month.abb,labels = 1:12))


temp <- temp %>%
  bind_rows() %>% 
  gather(key = variable,valor,X2009Jan:X2019Jan) %>% 
  filter(valor != 'NA') %>% 
  mutate(year = extract_numeric(variable),
         month = as.character(str_extract_all(variable, "[A-Z][a-z]+")),
         variable = 'temp') %>% 
  filter(year != 2019)%>%
  mutate(month = factor(month,levels = month.abb,labels = 1:12))


malaria <- read_rds('spdb/malaria_newdataset.rds') %>% 
  mutate(id = 1:n()) %>% 
  select(id, codigo,district,year,month,fal,viv,nrohab) %>% 
  gather(key = variable,valor,fal:viv:nrohab) %>% 
  filter(valor != 'NA') %>% 
  remove_geom() %>% 
  mutate(month = factor(month,levels = 1:12))


# 4. New dataset ----------------------------------------------------------
predictores <- rbind(esco,etp,evi,hsoil,ndvi,pp,savi,temp)

malaria <- malaria %>%
  select(id,codigo,district,variable,valor,year,month)

alldataset <- rbind(malaria,predictores)
id_unico <- unique(alldataset$codigo)

alldataset <- alldataset %>% 
  arrange(district) %>% 
  mutate(new_id = factor(codigo,labels =1:51,levels = id_unico)) %>% 
  select(-id)

sp_data <- st_read('spdb/district.gpkg') %>%
  arrange(district) %>% 
  mutate(new_id = factor(codigo, labels = 1:n(),levels = id_unico)) %>% 
  select(new_id,codigo,district,geom)

alldataset %>% 
  filter(district == 'BARRANCA') %>%
  select(new_id) %>% 
  unique()

sp_data %>% 
  filter(district == 'BARRANCA') %>% 
  select(new_id) %>% 
  unique()

# 5. Exporting new dataset for models -------------------------------------

saveRDS(object = alldataset,'final_malaria-hydrobasin/dataset_malaria.rds')
saveRDS(object = sp_data,'final_malaria-hydrobasin/sf_malaria.rds')

