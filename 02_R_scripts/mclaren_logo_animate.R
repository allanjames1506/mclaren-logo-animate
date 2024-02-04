#inspiration : https://alistaire.rbind.io/blog/fireworks/

# https://stackoverflow.com/questions/71704465/spatial-operations-how-to-buffer-a-polygon-by-area-not-distance-from-edge

library(tidyverse)
library(sf)
library(gganimate)
library(readr)
library(ggfx)
library(showtext)
library(vctrs)
theme_set(hrbrthemes::theme_ipsum_ps())

# 2 Set fonts----
font_add_google("Lato", "lato")
font_add_google(name = "Leckerli One", family = "Leckerli")
font_add_google(name = "Pacifico", family = "Pacifico")
font_add_google(name = "Zen Dots", family = "Zen")
font_add_google(name = "Goldman", family = "Goldman")

showtext_auto()
showtext_opts(dpi = 300)

# ANIMATE: McLaren speedmark buffered polygons----
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
p <- crossing(x = 1:30, nesting(t = 1:10, y = .5^(seq(t)))) 
#%>%
  ggplot(aes(x, y)) +
  geom_point(color = 'white') +
  coord_polar()

p

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

st_centroid(p_mclaren_polygon)

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
  st_coordinates() %>% 
  as_tibble() %>% 
  select(1:2) %>% 
  write_csv('./01_tidy_data/original_coords.csv')

buf1_coords <- p_mclaren_polygon_buf1 %>%
  st_coordinates() %>% 
  as_tibble() %>% 
  select(1:2) %>% 
  write_csv('./01_tidy_data/buf1_coords.csv')

buf2_coords <- p_mclaren_polygon_buf2 %>%
  st_coordinates() %>% 
  as_tibble() %>% 
  select(1:2) %>% 
  write_csv('./01_tidy_data/buf2_coords.csv')

buf3_coords <- p_mclaren_polygon_buf3 %>%
  st_coordinates() %>% 
  as_tibble() %>% 
  select(1:2) %>% 
  write_csv('./01_tidy_data/buf3_coords.csv')

buf4_coords <- p_mclaren_polygon_buf4 %>%
  st_coordinates() %>% 
  as_tibble() %>% 
  select(1:2) %>% 
  write_csv('./01_tidy_data/buf4_coords.csv')

buf5_coords <- p_mclaren_polygon_buf5 %>%
  st_coordinates() %>% 
  as_tibble() %>% 
  select(1:2) %>% 
  write_csv('./01_tidy_data/buf5_coords.csv')

buf6_coords <- p_mclaren_polygon_buf6 %>%
  st_coordinates() %>% 
  as_tibble() %>% 
  select(1:2) %>% 
  write_csv('./01_tidy_data/buf6_coords.csv')

buf7_coords <- p_mclaren_polygon_buf7 %>%
  st_coordinates() %>% 
  as_tibble() %>% 
  select(1:2) %>% 
  write_csv('./01_tidy_data/buf7_coords.csv')

buf8_coords <- p_mclaren_polygon_buf8 %>%
  st_coordinates() %>% 
  as_tibble() %>% 
  select(1:2) %>% 
  write_csv('./01_tidy_data/buf8_coords.csv')

original_coords <- original_coords %>% 
  mutate(t = 1)

buf1_coords <- buf1_coords %>% 
  mutate(t = 2)

buf2_coords <- buf2_coords %>% 
  mutate(t = 3)

buf3_coords <- buf3_coords %>% 
  mutate(t = 4)

buf4_coords <- buf4_coords %>% 
  mutate(t = 5)

buf5_coords <- buf5_coords %>% 
  mutate(t = 6)

buf6_coords <- buf6_coords %>% 
  mutate(t = 7)

buf7_coords <- buf7_coords %>% 
  mutate(t = 8)

buf8_coords <- buf8_coords %>% 
  mutate(t = 9)

scatter_plot_points <- original_coords %>% 
  bind_rows(buf1_coords,
            buf2_coords,
            buf3_coords,
            buf4_coords,
            buf5_coords,
            buf6_coords,
            buf7_coords,
            buf8_coords)

scatter_plot <- scatter_plot_points %>%
  filter(t == c(1, 2, 3)) %>% 
  ggplot(aes(X, Y, color = type)) + 
  geom_point() +
  scale_color_manual(values=c('white','blue', 'red'))

scatter_plot

scatter_plot_animate <- scatter_plot_points %>% 
  ggplot(aes(X, Y)) + 
  geom_point(color = '#E27231', size =2) +
  #with_outer_glow(geom_point(color = '#E27231', size =2), colour='white', sigma = 1, expand = 1) +
  transition_time(t) + 
  shadow_wake(0.5) +
  annotate("text", y = 70, x = 170, label = "@allanjames1506", lineheight = 0.75, family = 'Zen',  size = 5, color = "gray40", vjust = 0.5)

animate(scatter_plot_animate, fps = 30)

anim_save("./04_animate_gifs/third_saved_animation_logo_animate.gif", height = 372, width = 538, units = "px")

# ANIMATE: firework Lando initials----
p_alt <- map_dfr(1:10, ~crossing(x = 1:30, nesting(y = seq(1, .x, length.out = 10)^0.5, t = 1:10)))

p_alt_mclaren <- map_dfr(1:10, ~crossing(grid_mclaren_simple$lon, nesting(y = seq(1, .x, length.out = 10)^0.5, t = 1:10)))

grid_mclaren_simple$lat

p_straight <- map_dfr(1:10, ~crossing(x = 1:30, nesting(
  y = seq(1, .x, length.out = 10)^0.5, 
  t = 1:10))) %>% 
  ggplot(aes(x, y)) + 
  geom_point(color = '#E27231') + 
  #with_outer_glow(geom_point(color = '#CEF14E'), colour='#E27231', sigma = 5, expand = 2.5) +
  coord_polar() + 
  transition_time(t) + 
  shadow_wake(0.3) +
  annotate("text", y = 1, x = 1, label = "LN4", lineheight = 0.75, family = 'Zen',  size = 48, color = "#CEF14E", vjust = 0.5) 

animate(p_straight, fps = 30)

anim_save("./04_animate_gifs/first_saved_animation_LN4_firework_animate.gif", height = 372, width = 538, units = "px")

y_try <- seq(109.28, 109.28, length.out = 10)^0.5

x_lon <- grid_mclaren_simple$lon
x_test <- vec_rep_each(x_lon, 10)
x_test2 <- vec_rep_each(x_test, 10)
t_var <- vec_rep(1:10, 840)

df <- tibble(x_test2, t_var) 

x_test

?vec_rep_each

grid_mclaren_slice <- grid_mclaren_simple %>% 
  slice(1:83)

grid_mclaren_slice %>% expand(nesting(lon, lat))

# ANIMATE: expand outwards from centroid to perimeter---- 
#https://stackoverflow.com/questions/74408586/how-to-use-map-dfc-and-map-dfr-purrr-r-package-it-appears-they-are-doing-the

tibble(site = rep(c(LETTERS[1:3]), each = 6),
       name = rep(c(letters[10:15]), 3),
       size = runif(18)) %>%
  arrange(site, name) -> d_tibble

test_fct <- function(a, i) {
  a ^ i
}

test_fct_aj <- function(a, i){
  
  seq(from = a, to = i, length.out = 10)
  
}

# create sequence of i's
i_seq <- seq(1, 5, by = 1)
i_seq_aj <- 0.5

d_tibble |>
  group_split(site, name) |>
  map_dfr(~tibble(site = .x$site, 
                  name = .x$name,
                  i = i_seq,
                  val = test_fct(.x$size, i_seq)))

grid_mclaren_slice |>
  group_split(lon) |>
  map_dfr(1:10, ~expand(.x$lat, test_fct(.x$lon, i_seq_aj))) -> mc_test_aj 


p_straight <- map_dfr(1:10, ~crossing(x = 1:30, nesting(
  y = seq(1, .x, length.out = 10)^0.5, 
  t = 1:10)))

centroid_longitude <- 134.246
centroid_latitude <- 124.0314

seq(from = 134.246, to = 26.78571, length.out = 10)

grid_mclaren_slice |>
  group_split(lon, lat) %>% 
  map_dfr(~tibble(t= 1:10,
                  lon = .x$lon, 
                  lat = .x$lat,
                  lon_vals = test_fct_aj(centroid_longitude, .x$lon),
                  lat_vals = test_fct_aj(centroid_latitude, .x$lat))) -> mc_test_aj

glimpse(grid_mclaren_simple)

scatter_plot_animate2 <- mc_test_aj %>% 
  ggplot(aes(lon_vals, lat_vals)) + 
  geom_point(color = '#E27231', size =2) +
  #with_outer_glow(geom_point(color = '#E27231', size =2), colour='gold', sigma = 5, expand = 5) +
  transition_time(t) + 
  shadow_wake(0.5) +
  annotate("text", y = 70, x = 170, label = "@allanjames1506", lineheight = 0.75, family = 'Zen',  size = 5, color = "gray40", vjust = 0.5)

animate(scatter_plot_animate2, fps = 30)

anim_save("./04_animate_gifs/first_saved_animation_logo_animate.gif", height = 372, width = 538, units = "px")

# ANIMATE: expand inwards from perimeter to centroid---- 
grid_mclaren_slice |>
  group_split(lon, lat) %>% 
  map_dfr(~tibble(t= 10:1,
                  lon = .x$lon, 
                  lat = .x$lat,
                  lon_vals = test_fct_aj(centroid_longitude, .x$lon),
                  lat_vals = test_fct_aj(centroid_latitude, .x$lat))) -> mc_test_aj2

glimpse(grid_mclaren_simple)

scatter_plot_animate3 <- mc_test_aj2 %>% 
  ggplot(aes(lon_vals, lat_vals)) + 
  geom_point(color = '#E27231', size = 2) +
  #with_outer_glow(geom_point(color = '#E27231', size =2), colour='gold', sigma = 5, expand = 5) +
  transition_time(t) + 
  shadow_wake(0.5)

animate(scatter_plot_animate3, fps = 30)

anim_save("./04_animate_gifs/second_saved_animation_logo_animate.gif", height = 372, width = 538, units = "px")

p_straight <- map_dfr(1:10, ~crossing(x = 1:30, nesting(
  y = seq(1, .x, length.out = 10)^0.5, 
  t = 1:10)))

test_fct_aj2 <- function(a, i){
  
  crossing(a, nesting(
    y = seq(i, a, length.out = 10)^0.5))
  
}

test_fct_aj3 <- function(a, i){
  
  sample(a:i, 10, replace = TRUE)
  
}

grid_mclaren_slice |>
  group_split(lon, lat) %>% 
  map_dfr(~tibble(t= 1:10,
                  lon = .x$lon, 
                  lat = .x$lat,
                  lon_vals = test_fct_aj3(centroid_longitude, max(.x$lon)),
                  lat_vals = test_fct_aj3(centroid_latitude, max(.x$lat)))) -> mc_test_aj3

scatter_plot_animate4 <- mc_test_aj3 %>% 
  ggplot(aes(lon_vals, lat_vals)) + 
  geom_point(color = '#E27231', size = 2) +
  #with_outer_glow(geom_point(color = '#E27231', size =2), colour='gold', sigma = 5, expand = 5) +
  transition_time(t) + 
  shadow_wake(0.5)

animate(scatter_plot_animate4, fps = 30)


test_30 <- grid_mclaren_slice %>% 
  select(lon) %>% 
  slice(1:30)

test_10 <- grid_mclaren_slice %>% 
  select(lat) %>% 
  slice(1:10)

test_cross_aj <- crossing(x = test_30, nesting(t = test_10, y = 0.5^(seq(t))))

test_cross <- crossing(x = 1:30, nesting(t = 1:10, y = .5^(seq(t))))

mc_test_aj |> mutate(lon_vals = runif(n())) -> mc_test_aj4

grid_mclaren_slice |>
  group_split(lon, lat) %>% 
  map_dfr(~tibble(t= 11:20,
                  lon = .x$lon, 
                  lat = .x$lat,
                  lon_vals = test_fct_aj(centroid_longitude, .x$lon),
                  lat_vals = test_fct_aj(centroid_latitude, .x$lat))) -> mc_test_aj5

mc_test_aj6 <- mc_test_aj3 %>% 
  bind_rows(mc_test_aj5)

scatter_plot_animate5 <- mc_test_aj6 %>% 
  ggplot(aes(lon_vals, lat_vals)) + 
  geom_point(color = '#E27231', size = 2) +
  #with_outer_glow(geom_point(color = '#E27231', size =2), colour='gold', sigma = 5, expand = 5) +
  transition_time(t) + 
  shadow_wake(0.5)

animate(scatter_plot_animate5, fps = 30)



