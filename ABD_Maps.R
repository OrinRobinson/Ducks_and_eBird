library(ebirdst)
library(rnaturalearth)
library(ggplot2)
library(viridisLite)
library(dplyr)
library(tidyverse)
library(raster)
library(sf)
library(readr)


#Select The species you want to plot here. In this example, I am using Green Winged Teal "gnwtea".

## For Mallard use "mallar3" and for Pintail use "norpin"

sp_path <- ebirdst_download(species = "gnwtea", force= TRUE)


# load trimmed median abundances
abunds <- load_raster("abundance", path = sp_path)


## Uncomment below for upper and lower percentiles (upper = 90th, lower = 10th)
#lower <- load_raster("abundance_lower", path= sp_path)
#upper <- load_raster("abundance_upper", path = sp_path)


date_vector <- parse_raster_dates(abunds)

# to convert the data to a simpler geographic format and access tabularly   
# reproject into geographic (decimal degrees) 
abund_stack_ll <- projectRaster(abunds[[4]], crs = "+init=epsg:4326", 
                                method = "ngb")

# Convert raster object into a matrix
p <- rasterToPoints(abund_stack_ll)
colnames(p) <- c("longitude", "latitude", "abundance_umean")

head(p)





# use parse_raster_dates() to get actual date objects for each layer

############################## REMEMBER ######################################
##  We are using the S&T weeks here, not the Duck Week from the other runs. ##
##  You will have to convert between the two.                               ##
##  Load "DuckWeek.csv" in the Data folder to see                           ##
##  the Duck Week for each S&T week                                         ##
##############################################################################


date_vector <- parse_raster_dates(abunds)
print(date_vector)


### Load Duck Week table for conversion.

DuckWeek <- read_csv("Data/DuckWeek.csv")

# define mollweide projection
mollweide <- "+proj=moll +lon_0=-90 +x_0=0 +y_0=0 +ellps=WGS84"


## Create area over which to plot data

us <- ne_countries(continent = "North America", returnclass = "sf") %>% 
  st_union() %>% 
  st_transform(crs = mollweide)
states <- ne_states(iso_a2 = "US", returnclass = "sf") %>% 
  filter(postal %in% c("IL", "MO", "IA")) %>% 
  st_transform(crs = mollweide) %>% 
  st_geometry()


## Select S&T week to plot. Here we choose week 16, which is the week of 04/19/2018, 
## and Duck Week 33. Change the "16" in the first line below to the S&T week you want to plot. 

abd <- projectRaster(abunds[[16]], crs = mollweide, method = "ngb")
abd_mask <- mask(crop(abd, as_Spatial(states)), as_Spatial(states))
bins <- calc_bins(abd_mask)
pal <- abundance_palette(length(bins$bins) - 1, season = "weekly")

par(mar = c(0, 0, 0, 0))

plot(states, col = NA, border = NA)
plot(us, col = "grey90", border = NA, add = TRUE)
plot(states, col = "grey80", border = NA, add = TRUE)
plot(abd_mask, 
     breaks = bins$bins,
     col = pal, 
     axes = FALSE, box = FALSE, legend = FALSE,
     maxpixels = ncell(abd),
     add = TRUE)
plot(states, col = NA, border = "white", lwd = 0.5, add = TRUE)


# create a thinner set of labels
bin_labels <- format(round(bins$bins, 2), nsmall = 2)
bin_labels[!(bin_labels %in% c(bin_labels[1],
                               bin_labels[round((length(bin_labels) / 2)) + 1],
                               bin_labels[length(bin_labels)]))] <- ""

# plot legend
plot(abd_mask^bins$power, legend.only = TRUE, 
     col = abundance_palette(length(bins$bins) - 1, season = "weekly"), 
     breaks = bins$bins^bins$power, lab.breaks = bin_labels,
     legend.shrink = 0.97, legend.width = 2,  
     axis.args = list(cex.axis = 0.9, lwd.ticks = 0, col = NA, line = -0.8))

title("AGWT Relative Abundance Week of 04/19/2018", 
      line = -1, cex.main = 1)

