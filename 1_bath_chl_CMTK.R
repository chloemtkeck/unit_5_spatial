install.packages("rgdal")

install.packages("raster")

library(rgdal)
library(raster)
library(tidyverse)
library(mapdata)
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


bath_m_raw = marmap::getNOAA.bathy(lon1=lon_bounds[1], 
                                   lon2=lon_bounds[2], 
                                   lat1=lat_bounds[1], 
                                   lat2=lat_bounds[2], 
                                   resolution=4)

class(bath_m_raw)
bath_m_df = marmap::fortify.bathy(bath_m_raw)
head(bath_m_df)

bath_m = bath_m_df %>%
  mutate(depth_m = ifelse(z>20, NA, z)) %>%
  dplyr::select(-z)

summary(bath_m)

GOM_bath_map = ggplot() + 
  geom_contour(data = bath_m, aes(x = x, y = y, z = depth_m), breaks = c(-100), size = 0.25, color = "white") +
  geom_contour(data = bath_m, aes(x = x, y = y, z = depth_m), breaks = c(-200), size = 0.5, color = "white") +
  geom_contour(data = bath_m, aes(x = x, y = y, z = depth_m), breaks = c(-500), size = 0.75, color = "white") +
  geom_raster(data = bath_m, aes(x = x, y = y, fill = depth_m)) +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "darkgrey", color = NA) +
  coord_fixed(1.3, xlim = lon_bounds, ylim = lat_bounds, expand = F) + 
  scale_fill_gradientn(colors = c("black", "darkgreen", "lightgreen"), 
                       values = scales::rescale(c(-6000, -300, 0)))

# combine chl and bath rasters 


bath_m_raster = marmap::as.raster(bath_m_raw)

names(bath_m_raster) = "bath_m"

bath_layer_chl_dims = raster::resample(bath_m_raster, chl_GOM_raster)

raster_stack = stack(chl_GOM_raster, bath_layer_chl_dims)
plot(raster_stack)


stack_df = data.frame(raster::rasterToPoints(raster_stack))
head(stack_df)


# O'Reilly 2019 

oligo_chl_a = 0.1
eutro_chl_a = 1.67 #mg/m^3

stack_df = stack_df %>%
  mutate(trophic_index = case_when(chl_a < oligo_chl_a ~ "oligotrophic", 
                                   chl_a >= oligo_chl_a & chl_a <= eutro_chl_a ~ "mesotrophic", 
                                   chl_a > eutro_chl_a ~ "eutrophic")) %>%
  mutate(trophic_index = as.factor(trophic_index))

head(stack_df)
tail(stack_df)
summary(stack_df)

ggplot() +
  geom_histogram(aes(x = bath_m), data = stack_df) + 
  facet_wrap(~trophic_index)


# map the trophic index 

trophic_map = ggplot() + 
  geom_raster(data = stack_df, aes(x = x, y = y, fill = trophic_index)) + 
  geom_polygon(data = world_map, aes(x = long, y = lat, ))