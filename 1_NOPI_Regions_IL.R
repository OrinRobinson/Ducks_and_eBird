library(ebirdst)
library(raster)
library(sf)
library(rnaturalearth)
library(ggplot2)
library(viridisLite)
library(dplyr)
library(tigris)
library(gridExtra)
library(tidyverse)
# handle namespace conflicts
extract <- raster::extract



### eBird S&T data

sp_path <- ebirdst_download(species = "norpin", force= TRUE)


# load trimmed median abundances
abd <- load_raster("abundance", path = sp_path)



lower <- load_raster("abundance_lower", path= sp_path)
upper <- load_raster("abundance_upper", path = sp_path)

# set an extent based on polygon
#Uncomment below for state or country cropping
# region <- ne_states(country = "United States of America", returnclass = "sf") %>% 
#   st_transform(crs = projection(abd)) %>% 
#   filter(name %in% c("California")) %>% 
#   st_union()




## Use Bounding Box to crop to Region 1


# define bbox with lngmin, latmin, lngmax, latmax 
bb_R1 <- st_bbox(c(xmin = -89.684037, ymin = 40.655011, xmax = -89.208337, ymax = 41.272832)) %>% 
  st_as_sfc() %>% 
  st_set_crs(4326) %>% 
  # transform to sinusoidal projection
  st_transform(crs = projection(abd)) %>% 
  st_as_sf()
e <- extent(bb_R1)
# crop to extent
abd_crop <- crop(abd, e)
low_crop<- crop(lower, e)
upp_crop <- crop(upper, e)
# mask to bounding box
abd_crop <- mask(abd_crop, bb_R1)
low_crop<- mask(low_crop, bb_R1)
upp_crop <- mask(upp_crop, bb_R1)
# could add additional masking here, e.g. if you had a county boundary
# mean cell value for each week within extent
NOPI_eBird_R1 <-data.frame(week = date_to_st_week(parse_raster_dates(abd_crop)),
           abd = cellStats(abd_crop, mean),
           lower = cellStats(low_crop, mean),
           upper = cellStats(upp_crop, mean),
           row.names = NULL)


saveRDS(NOPI_eBird_R1, "NOPI_ILL_1.rds")






# define bbox with lngmin, latmin, lngmax, latmax 
bb_R2 <- st_bbox(c(xmin = -90.65732, ymin = 39.83774, xmax = -89.68228, ymax = 40.57458)) %>% 
  st_as_sfc() %>% 
  st_set_crs(4326) %>% 
  # transform to sinusoidal projection
  st_transform(crs = projection(abd)) %>% 
  st_as_sf()
e <- extent(bb_R2)
# crop to extent
abd_crop <- crop(abd, e)
low_crop<- crop(lower, e)
upp_crop <- crop(upper, e)
# mask to bounding box
abd_crop <- mask(abd_crop, bb_R2)
low_crop<- mask(low_crop, bb_R2)
upp_crop <- mask(upp_crop, bb_R2)
# could add additional masking here, e.g. if you had a county boundary
# mean cell value for each week within extent
NOPI_eBird_R2 <-data.frame(week = date_to_st_week(parse_raster_dates(abd_crop)),
                           abd = cellStats(abd_crop, mean),
                           lower = cellStats(low_crop, mean),
                           upper = cellStats(upp_crop, mean),
                           row.names = NULL)


saveRDS(NOPI_eBird_R2, "NOPI_ILL_2.rds")






# define bbox with lngmin, latmin, lngmax, latmax 
bb_R3 <- st_bbox(c(xmin = -91.536937, ymin = 40.401114, xmax = -90.916078, ymax = 41.242343)) %>% 
  st_as_sfc() %>% 
  st_set_crs(4326) %>% 
  # transform to sinusoidal projection
  st_transform(crs = projection(abd)) %>% 
  st_as_sf()
e <- extent(bb_R3)
# crop to extent
abd_crop <- crop(abd, e)
low_crop<- crop(lower, e)
upp_crop <- crop(upper, e)
# mask to bounding box
abd_crop <- mask(abd_crop, bb_R3)
low_crop<- mask(low_crop, bb_R3)
upp_crop <- mask(upp_crop, bb_R3)
# could add additional masking here, e.g. if you had a county boundary
# mean cell value for each week within extent
NOPI_eBird_R3 <-data.frame(week = date_to_st_week(parse_raster_dates(abd_crop)),
                           abd = cellStats(abd_crop, mean),
                           lower = cellStats(low_crop, mean),
                           upper = cellStats(upp_crop, mean),
                           row.names = NULL)


saveRDS(NOPI_eBird_R3, "NOPI_ILL_3.rds")







# define bbox for each piece of region 4
bb_R4_a <- st_bbox(c(xmin = -91.649563, ymin = 38.819802, xmax = -91.007922, ymax = 40.400100)) %>% 
  st_as_sfc() %>% 
  st_set_crs(4326) %>% 
  # transform to sinusoidal projection
  st_transform(crs = projection(abd)) %>% 
  st_as_sf()
e <- extent(bb_R4_a)
# crop to extent
abd_crop <- crop(abd, e)
low_crop<- crop(lower, e)
upp_crop <- crop(upper, e)
# mask to bounding box
abd_crop <- mask(abd_crop, bb_R4_a)
low_crop<- mask(low_crop, bb_R4_a)
upp_crop <- mask(upp_crop, bb_R4_a)

NOPI_eBird_R4_a <-data.frame(week = date_to_st_week(parse_raster_dates(abd_crop)),
                             abd = cellStats(abd_crop, mean),
                             lower = cellStats(low_crop, mean),
                             upper = cellStats(upp_crop, mean),
                             row.names = NULL)


bb_R4_b <- st_bbox(c(xmin = -91.007922, ymin = 38.819802, xmax = -90.086517, ymax = 39.808032 )) %>% 
  st_as_sfc() %>% 
  st_set_crs(4326) %>% 
  # transform to sinusoidal projection
  st_transform(crs = projection(abd)) %>% 
  st_as_sf()
e <- extent(bb_R4_b)
# crop to extent
abd_crop <- crop(abd, e)
low_crop<- crop(lower, e)
upp_crop <- crop(upper, e)
# mask to bounding box
abd_crop <- mask(abd_crop, bb_R4_b)
low_crop<- mask(low_crop, bb_R4_b)
upp_crop <- mask(upp_crop, bb_R4_b)


NOPI_eBird_R4_b <-data.frame(week = date_to_st_week(parse_raster_dates(abd_crop)),
                             abd = cellStats(abd_crop, mean),
                             lower = cellStats(low_crop, mean),
                             upper = cellStats(upp_crop, mean),
                             row.names = NULL)



NOPI_eBird_R4 <- rbind(NOPI_eBird_R4_a, NOPI_eBird_R4_b) %>% 
  group_by(week) %>%
  summarise(abd = sum(abd),upper = sum(upper), lower = sum(lower))




saveRDS(NOPI_eBird_R4, "NOPI_ILL_4.rds")









