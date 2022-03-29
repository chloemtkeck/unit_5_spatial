library(tidyverse)
install.packages("raster")
library(raster)
install.packages("mapdata")
library(mapdata)
install.packages("marmap")
library(marmap)

chl_raster = raster('data/A20021822017212.L3m_MC_CHL_chlor_a_9km.nc')
class(chl_raster)
chl_raster
names(chl_raster) = "chl_a"
chl_raster


chl_pts = rasterToPoints(chl_raster, spatial = T)
class(chl_pts)
chl_df = data.frame(chl_pts)
head(chl_df)

hist(log10(chl_df$chl_a))
cols = rainbow(7, rev = T)[-1] # making nasa color pallette

# Image our chloro 

global_chl_map = ggplot() +
  geom_raster(data = chl_df, aes(x = x, y = y, fill = log10(chl_a))) + 
  scale_fill_gradientn(colors = cols, limits = c(-1.5, 0.75), name = "log_10(chl_a)")
ggsave(global_chl_map, filename = "figures/global_chl_map.pdf", height = 5, width = 9)  


# crop out to gulf of maine 



lon_bounds = c(-72, -62)
lat_bounds = c(39, 47)

chl_GOM_raster = raster::crop(chl_raster, extent(c(lon_bounds, lat_bounds)))

chl_GOM_df = data.frame(rasterToPoints(chl_GOM_raster, spatial = T))
head(chl_GOM_df)


chl_GOM_df = chl_GOM_df %>%
  dplyr::select(-optional)

world_map = map_data("worldHires")


GOM_chl_map = ggplot() + 
  geom_raster(data = chl_GOM_df, aes(x = x, y = y, fill = log10(chl_a))) +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "darkgrey") +
  coord_fixed(1.3, xlim = lon_bounds, ylim = lat_bounds, expand = F) +
  scale_fill_gradientn(colors = cols, limits = c(-1, 1.75)) +
  theme_bw()
ggsave(GOM_chl_map, filename = "figures/GOM_chl_map.pdf", height = 5, width = 9)  
