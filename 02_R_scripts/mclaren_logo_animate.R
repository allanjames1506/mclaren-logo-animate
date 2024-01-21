#inspiration : https://alistaire.rbind.io/blog/fireworks/

# https://stackoverflow.com/questions/71704465/spatial-operations-how-to-buffer-a-polygon-by-area-not-distance-from-edge

library(tidyverse)
library(sf)
library(gganimate)
theme_set(hrbrthemes::theme_ipsum_ps())


# grid_plot <- expand.grid(lon = seq(-180, 180, 10), 
#                          lat = seq(-90, 90, 10))
# 
# grid_mclaren <- read.csv('./00_raw_data/mclaren_logo_dots.csv') %>% 
#   select(2:3) %>% 
#   rename(lon = x,
#          lat = y) %>% 
#   pmap(~st_point(c(...))) %>%
#   st_sfc(crs = 4326) %>%    
#   st_sf() 
# 
# #%>%    # convert to sf data frame
#   ggplot() + 
#   geom_sf()
# 
# glimpse(grid_mclaren)
# class(grid_mclaren)
# 
# plot(st_buffer(grid_mclaren, dist = 1, endCapStyle="ROUND"), reset = FALSE, main = "endCapStyle: ROUND")
# 
# plot(grid_mclaren,col='blue')
# plot(grid_mclaren, col ='red')
# 
# plot(grid_mclaren_buf)
# polygon(grid_mclaren)
# 
# p = st_polygon(list(grid_mclaren))
# > pbuf = st_buffer(p, .4)
# > plot(pbuf)
# > plot(p,add=TRUE,col="red")
#   
# grid_mclaren_polar <- read.csv('./00_raw_data/mclaren_logo_dots.csv') %>% 
#   select(2:3)
# 
# p_polar <- ggplot(grid_mclaren_polar, aes(x, y)) +
#   geom_point(fill=NA,col="black")
# #+
#   geom_shape(fill=NA,col="red",expand=unit(-0.95,"cm"))+
#   theme_minimal()
# 
# p_polar  
#   # ggplot() + 
#   # geom_sf()
# 
# grid_mclaren_alt 
# 
# grid_mclaren + coord_sf(crs = "+proj=laea +lat_0=-90 +ellps=WGS84 +no_defs")
# 
# 
# theme_set(theme_void() + theme(
#   panel.background = element_rect(fill = 'black')))
# 
# x <- grid_mclaren$x
# y <- grid_mclaren$y
# t <- 1:10
# 
# y = .5^(seq(t))
# y
# test <- crossing(x, nesting(t=1:5), y = y*0.5^(seq(t)))
# 
# test <- crossing(grid_mclaren, nesting(x, y))
# 
# 
# p <- crossing(x = 1:30, nesting(t = 1:10, y = .5^(seq(t)))) %>% 
#   ggplot(aes(x, y)) + 
#   geom_point(color = 'white') + 
#   coord_polar()
# 
# p

grid_mclaren_simple <- read.csv('./00_raw_data/mclaren_logo_dots.csv') %>% 
  select(2:3) %>% 
  rename(lon = x, lat = y)

#class(grid_mclaren_simple) <- 'data.frame'

glimpse(grid_mclaren_simple)
class(grid_mclaren_simple)

p_mclaren <- grid_mclaren_simple %>% 
  ggplot(aes(lon, lat)) + 
  geom_point(color = 'white') 

p_mclaren

p_mclaren_polygon <- grid_mclaren_simple %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")

plot(p_mclaren_polygon)
plot(p_mclaren_polygon_buf1, add=TRUE)
plot(p_mclaren_polygon_buf2, add=TRUE)
plot(p_mclaren_polygon_buf3, add=TRUE)
plot(p_mclaren_polygon_buf4, add=TRUE)
plot(p_mclaren_polygon_buf5, add=TRUE)
plot(p_mclaren_polygon_buf6, add=TRUE)
plot(p_mclaren_polygon_buf7, add=TRUE)
plot(p_mclaren_polygon_buf8, add=TRUE)

class(p_mclaren_polygon)

sf_use_s2(FALSE)

p_mclaren_polygon_buf1 = st_buffer(p_mclaren_polygon, -2.5)

p_mclaren_polygon_buf2 = st_buffer(p_mclaren_polygon, -5)

p_mclaren_polygon_buf3 = st_buffer(p_mclaren_polygon, -10)

p_mclaren_polygon_buf4 = st_buffer(p_mclaren_polygon, -15)

p_mclaren_polygon_buf5 = st_buffer(p_mclaren_polygon, -20)

p_mclaren_polygon_buf6 = st_buffer(p_mclaren_polygon, -25)

p_mclaren_polygon_buf7 = st_buffer(p_mclaren_polygon, -27.5)

p_mclaren_polygon_buf8 = st_buffer(p_mclaren_polygon, -30)

original_coords <- p_mclaren_polygon %>%
  st_coordinates()

buf1_coords <- p_mclaren_polygon_buf1 %>%
  st_coordinates() %>% 
  as_tibble() %>% 
  select(1:2)

buf2_coords <- p_mclaren_polygon_buf2 %>%
  st_coordinates()


