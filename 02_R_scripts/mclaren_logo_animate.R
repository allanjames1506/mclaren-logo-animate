#inspiration : https://alistaire.rbind.io/blog/fireworks/

# https://stackoverflow.com/questions/71704465/spatial-operations-how-to-buffer-a-polygon-by-area-not-distance-from-edge

# 1 Libraries----

library(tidyverse)
library(sf)
library(gganimate)
library(readr)
library(ggfx)
library(showtext)
library(vctrs)
library(sf)
theme_set(hrbrthemes::theme_ipsum_ps())

# 2 Fonts----
font_add_google("Lato", "lato")
font_add_google(name = "Leckerli One", family = "Leckerli")
font_add_google(name = "Pacifico", family = "Pacifico")
font_add_google(name = "Zen Dots", family = "Zen")
font_add_google(name = "Goldman", family = "Goldman")

showtext_auto()
showtext_opts(dpi = 300)

# 3 Theme----

theme_set(theme_void() + theme(
  panel.background = element_rect(fill = 'black')))

# 4 Animate----
# *4.1 McLaren speedmark buffered polygons----

# example from https://alistaire.rbind.io/blog/fireworks/

p <- crossing(x = 1:30, nesting(t = 1:10, y = .5^(seq(t)))) %>% 
  ggplot(aes(x, y)) +
  geom_point(color = 'white') +
  coord_polar()

p

grid_mclaren_simple <- read.csv('./00_raw_data/mclaren_logo_dots.csv') %>% 
  select(2:3) %>% 
  rename(lon = x, lat = y)

p_mclaren <- grid_mclaren_simple %>% 
  ggplot(aes(lon, lat)) + 
  geom_point(color = 'white') 

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
  transition_time(t) + 
  shadow_wake(0.5) +
  annotate("text", y = 70, x = 170, label = "@allanjames1506", lineheight = 0.75, family = 'Zen',  size = 5, color = "gray40", vjust = 0.5)

animate(scatter_plot_animate, fps = 30)

anim_save("./04_animate_gifs/third_saved_animation_logo_animate.gif", height = 372, width = 538, units = "px")

# *4.2 Firework Lando initials----

p_straight <- map_dfr(1:10, ~crossing(x = 1:30, nesting(
  y = seq(1, .x, length.out = 10)^0.5, 
  t = 1:10))) %>% 
  ggplot(aes(x, y)) + 
  geom_point(color = '#E27231') + 
  coord_polar() + 
  transition_time(t) + 
  shadow_wake(0.3) +
  annotate("text", y = 1, x = 1, label = "LN4", lineheight = 0.75, family = 'Zen',  size = 48, color = "#CEF14E", vjust = 0.5) 

animate(p_straight, fps = 30)

anim_save("./04_animate_gifs/first_saved_animation_LN4_firework_animate.gif", height = 372, width = 538, units = "px")

# *4.3 Expand outwards from centroid to perimeter---- 
#https://stackoverflow.com/questions/74408586/how-to-use-map-dfc-and-map-dfr-purrr-r-package-it-appears-they-are-doing-the

test_fct_aj <- function(a, i){
  
  seq(from = a, to = i, length.out = 10)
  
}

# *4.4 Expand from centroid----

centroid_longitude <- 134.246
centroid_latitude <- 124.0314

grid_mclaren_slice <- grid_mclaren_simple %>% 
  slice(1:83)

grid_mclaren_slice |>
  group_split(lon, lat) %>% 
  map_dfr(~tibble(t= 1:10,
                  lon = .x$lon, 
                  lat = .x$lat,
                  lon_vals = test_fct_aj(centroid_longitude, .x$lon),
                  lat_vals = test_fct_aj(centroid_latitude, .x$lat))) -> mc_test_aj

scatter_plot_animate2 <- mc_test_aj %>% 
  ggplot(aes(lon_vals, lat_vals)) + 
  geom_point(color = '#E27231', size =2) +
  #with_outer_glow(geom_point(color = '#E27231', size =2), colour='gold', sigma = 5, expand = 5) +
  transition_time(t) + 
  shadow_wake(0.5) +
  annotate("text", y = 70, x = 170, label = "@allanjames1506", lineheight = 0.75, family = 'Zen',  size = 5, color = "gray40", vjust = 0.5)

animate(scatter_plot_animate2, fps = 30)

anim_save("./04_animate_gifs/first_saved_animation_logo_animate.gif", height = 372, width = 538, units = "px")

# *4.5 expand inwards from perimeter to centroid---- 
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

# *4.6 Dynamic travel bottom left to top right----

make_seq <- function(x) {
  seq <- seq(x, 2*x, length.out = 10)
}

out_lon <- vector("list", length(grid_mclaren_simple$lon))
for (i in seq_along(grid_mclaren_simple$lon)) {
  out_lon[[i]] <- make_seq(grid_mclaren_simple$lon[[i]])
}

lons_unlisted <- purrr::map_df(out_lon, tibble::as_tibble) %>% 
  rename(lon = value) %>% 
  mutate(t = rep(1:10, length.out = nrow(.)))

out_lat <- vector("list", length(grid_mclaren_simple$lat))
for (i in seq_along(grid_mclaren_simple$lat)) {
  out_lat[[i]] <- make_seq(grid_mclaren_simple$lat[[i]])
}

lats_unlisted <- purrr::map_df(out_lat, tibble::as_tibble) %>% 
  rename(lat = value) %>% 
  mutate(t = rep(1:10, length.out = nrow(.)))

lon_lat <- lons_unlisted %>%
  select(-t) %>% 
  bind_cols(lats_unlisted)

scatter_plot_animate_lon_lat <- lon_lat %>% 
  ggplot(aes(lon, lat)) + 
  geom_point(color = '#E27231', size = 2) +
  #with_outer_glow(geom_point(color = '#E27231', size =2), colour='gold', sigma = 5, expand = 5) +
  transition_time(t) + 
  shadow_wake(0.5) 

animate(scatter_plot_animate_lon_lat, fps = 30)

anim_save("./04_animate_gifs/fourth_saved_animation_logo_animate.gif", height = 372, width = 538, units = "px")

# *4.7 Straight outwards attempt : too many points----

make_seq2 <- function(value) {
  seq <- map_dfr(1:3, ~crossing(x = 1:84, nesting(
    y = seq(value, .x, length.out = 3)^0.5, 
    t = 1:3
  )))
}

out_lon2 <- vector("list", length(grid_mclaren_simple$lon))
for (i in seq_along(grid_mclaren_simple$lon)) {
  out_lon2[[i]] <- make_seq2(grid_mclaren_simple$lon[[i]])
}

lons2_unlisted <- purrr::map_df(out_lon2, tibble::as_tibble)  

scatter_plot_animate_random <- lons2_unlisted %>% 
  ggplot(aes(x, y)) + 
  geom_point(color = '#E27231', size = 2) +
  #with_outer_glow(geom_point(color = '#E27231', size =2), colour='gold', sigma = 5, expand = 5) +
  transition_time(t) + 
  shadow_wake(0.5) 

animate(scatter_plot_animate_random, fps = 30)

# *4.8 Expand in and out from bottom left----

make_seq3 <- function(value) {
  seq <- crossing(x = value, nesting(t = 1:10, y = 0.75^(seq(t))))
}

out_lon3 <- vector("list", length(grid_mclaren_simple$lon))
for (i in seq_along(grid_mclaren_simple$lon)) {
  out_lon3[[i]] <- make_seq3(grid_mclaren_simple$lon[[i]])
}

lons3_unlisted <- purrr::map_df(out_lon3, tibble::as_tibble) %>% 
  mutate(xy = x*y) %>% 
  rename(lon = xy) %>% 
  select(t, lon)

out_lat3 <- vector("list", length(grid_mclaren_simple$lat))
for (i in seq_along(grid_mclaren_simple$lat)) {
  out_lat3[[i]] <- make_seq3(grid_mclaren_simple$lat[[i]])
}

lats3_unlisted <- purrr::map_df(out_lat3, tibble::as_tibble) %>% 
  mutate(xy = x*y) %>% 
  rename(lat = xy) %>% 
  select(lat)

lons_lats3 <- lons3_unlisted %>% 
  bind_cols(lats3_unlisted)

make_seq4 <- function(value) {
  seq <- crossing(x = value, nesting(t = 20:11, y = 0.75^(seq(t))))
}

out_lon4 <- vector("list", length(grid_mclaren_simple$lon))
for (i in seq_along(grid_mclaren_simple$lon)) {
  out_lon4[[i]] <- make_seq4(grid_mclaren_simple$lon[[i]])
}

lons4_unlisted <- purrr::map_df(out_lon4, tibble::as_tibble) %>% 
  mutate(xy = x*y) %>% 
  rename(lon = xy) %>% 
  select(t, lon)

out_lat4 <- vector("list", length(grid_mclaren_simple$lat))
for (i in seq_along(grid_mclaren_simple$lat)) {
  out_lat4[[i]] <- make_seq4(grid_mclaren_simple$lat[[i]])
}

lats4_unlisted <- purrr::map_df(out_lat4, tibble::as_tibble) %>% 
  mutate(xy = x*y) %>% 
  rename(lat = xy) %>% 
  select(lat)

lons_lats4 <- lons4_unlisted %>% 
  bind_cols(lats4_unlisted)

lon_lats_in_out <- lons_lats3 %>% 
  bind_rows(lons_lats4)

scatter_plot_animate_hole <- lon_lats_in_out %>% 
  ggplot(aes(lon, lat)) + 
  #geom_point(color = '#E27231', size = 2) +
  with_outer_glow(geom_point(color = '#E27231', size =2), colour='#47c7fc', sigma = 2, expand = 2) +
  transition_time(t) + 
  shadow_wake(0.5) 

animate(scatter_plot_animate_hole, nframes = 400, fps=30)

anim_save("./04_animate_gifs/fifth_saved_animation_logo_animate2.gif", height = 372, width = 538, units = "px")

# *4.9 Dynamic travel top right to bottom left----

make_seq5 <- function(value) {
  seq <- map_dfr(1:10, ~crossing(x = value, nesting(
    y = seq(value, .x, length.out = 10)^0.5, 
    t = 1:10
  )))
}

out_lon5 <- vector("list", length(grid_mclaren_simple$lon))
for (i in seq_along(grid_mclaren_simple$lon)) {
  out_lon5[[i]] <- make_seq5(grid_mclaren_simple$lon[[i]])
}

lons5_unlisted <- purrr::map_df(out_lon5, tibble::as_tibble) %>% 
  mutate(xy = x*y) %>% 
  rename(lon = xy) %>% 
  select(t, lon)

out_lat5 <- vector("list", length(grid_mclaren_simple$lat))
for (i in seq_along(grid_mclaren_simple$lat)) {
  out_lat5[[i]] <- make_seq5(grid_mclaren_simple$lat[[i]])
}

lats5_unlisted <- purrr::map_df(out_lat5, tibble::as_tibble) %>% 
  mutate(xy = x*y) %>% 
  rename(lat = xy) %>% 
  select(lat)

lons_lats5 <- lons5_unlisted %>% 
  bind_cols(lats5_unlisted)

scatter_plot_animate_straight <- lons_lats5 %>% 
  ggplot(aes(lon, lat)) + 
  geom_point(color = '#E27231', size = 2) +
  #with_outer_glow(geom_point(color = '#E27231', size =2), colour='#47c7fc', sigma = 2, expand = 2) +
  transition_time(t) + 
  shadow_wake(0.5) 

animate(scatter_plot_animate_straight, fps = 30)

# *4.10 Particles and gnats----

make_seq6 <- function(value) {
  seq <- map_dfr(1:4, ~data_frame(y = seq(value, .x, length.out = 4), t = 4:1)) %>% 
    mutate(x = runif(n()))
}

out_lon6 <- vector("list", length(grid_mclaren_simple$lon))
for (i in seq_along(grid_mclaren_simple$lon)) {
  out_lon6[[i]] <- make_seq6(grid_mclaren_simple$lon[[i]])
}

lons6_unlisted <- purrr::map_df(out_lon6, tibble::as_tibble) %>% 
  mutate(xy = x*y) %>% 
  rename(lon = xy) %>% 
  select(t, lon)

out_lat6 <- vector("list", length(grid_mclaren_simple$lat))
for (i in seq_along(grid_mclaren_simple$lat)) {
  out_lat6[[i]] <- make_seq6(grid_mclaren_simple$lat[[i]])
}

lats6_unlisted <- purrr::map_df(out_lat6, tibble::as_tibble) %>% 
  mutate(xy = x*y) %>% 
  rename(lat = xy) %>% 
  select(lat)

lons_lats6 <- lons6_unlisted %>% 
  bind_cols(lats6_unlisted)

scatter_plot_animate_particles <- lons_lats6 %>% 
  ggplot(aes(lon, lat)) + 
  #geom_point(color = '#E27231', size = 2) +
  with_outer_glow(geom_point(color = '#E27231', size =2), colour='#47c7fc', sigma = 2, expand = 2) +
  transition_time(t) + 
  shadow_wake(0.5) 

animate(scatter_plot_animate_particles, nframes = 30)

# *4.11 Explosion----

make_seq7 <- function(value) {
  seq <- map_dfr(1:10, ~crossing(
    x = runif(10), 
    nesting(
      y = seq(value, .x, length.out = 10)^0.5, 
      t = 1:10)
  )
  )
}

out_lon7 <- vector("list", length(grid_mclaren_simple$lon))
for (i in seq_along(grid_mclaren_simple$lon)) {
  out_lon7[[i]] <- make_seq7(grid_mclaren_simple$lon[[i]])
}

lons7_unlisted <- purrr::map_df(out_lon7, tibble::as_tibble) %>% 
  mutate(xy = x*y) %>% 
  rename(lon = xy) %>% 
  select(t, lon)

out_lat7 <- vector("list", length(grid_mclaren_simple$lat))
for (i in seq_along(grid_mclaren_simple$lat)) {
  out_lat7[[i]] <- make_seq7(grid_mclaren_simple$lat[[i]])
}

lats7_unlisted <- purrr::map_df(out_lat7, tibble::as_tibble) %>% 
  mutate(xy = x*y) %>% 
  rename(lat = xy) %>% 
  select(lat)

lons_lats7 <- lons7_unlisted %>% 
  bind_cols(lats7_unlisted)

scatter_plot_animate_explosion <- lons_lats7 %>% 
  ggplot(aes(lon, lat)) + 
  geom_point(color = '#E27231', size = 2) +
  #with_outer_glow(geom_point(color = '#E27231', size =2), colour='#47c7fc', sigma = 2, expand = 2) +
  transition_time(t) + 
  shadow_wake(0.5) 

animate(scatter_plot_animate_explosion, fps = 30)

# *4.12 Particle and gnats centroid to max (lon, lat)----

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

animate(scatter_plot_animate4, nframes = 30)

mc_test_aj |> mutate(lon_vals = runif(n())) -> mc_test_aj4

# *4.13 Particles transitioning to logo expand outwards----
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

# **4.14 Spirals----

make_seq8 <- function(value) {
  seq <- map_dfr(1:3, ~crossing(
    x = {
      x = seq(3) + 0.3*.x; 
      ifelse(x > 2, x - 2, x)
    }, 
    nesting(
      y = seq(value, .x, length.out = 3)^0.5, 
      t = 3:1)
  )
  )
}

out_lon8 <- vector("list", length(grid_mclaren_simple$lon))
for (i in seq_along(grid_mclaren_simple$lon)) {
  out_lon8[[i]] <- make_seq8(grid_mclaren_simple$lon[[i]])
}

lons8_unlisted <- purrr::map_df(out_lon8, tibble::as_tibble) %>% 
  mutate(xy = x*y) %>% 
  rename(lon = xy) %>% 
  select(t, lon)

out_lat8 <- vector("list", length(grid_mclaren_simple$lat))
for (i in seq_along(grid_mclaren_simple$lat)) {
  out_lat8[[i]] <- make_seq8(grid_mclaren_simple$lat[[i]])
}

lats8_unlisted <- purrr::map_df(out_lat8, tibble::as_tibble) %>% 
  mutate(xy = x*y) %>% 
  rename(lat = xy) %>% 
  select(lat)

lons_lats8 <- lons8_unlisted %>% 
  bind_cols(lats8_unlisted)

scatter_plot_animate_spirals <- lons_lats8 %>% 
  ggplot(aes(lon, lat)) + 
  geom_point(color = '#E27231', size = 0.5) +
  #with_outer_glow(geom_point(color = '#E27231', size =2), colour='#47c7fc', sigma = 2, expand = 2) +
  transition_time(t) + 
  shadow_wake(0.5) 

animate(scatter_plot_animate_spirals, fps = 30)


# **4.15 Firework----

p_firework <- map_dfr(1:10, ~crossing(
  x = {
    x = seq(30) + 0.6*.x; 
    ifelse(x > 30, x - 30, x)
  }, 
  nesting(
    y = seq(1, .x, length.out = 10)^0.5, 
    t = 1:10)
)
)

make_seq9 <- function(value) {
  seq <- map_dfr(1:3, ~crossing(
    x = {
      x = seq(3) + 0.6*.x; 
      ifelse(x > 2, x - 2, x)
    }, 
    nesting(
      y = seq(value, .x, length.out = 3)^0.5, 
      t = 3:1)
  )
  )
}

out_lon9 <- vector("list", length(grid_mclaren_simple$lon))
for (i in seq_along(grid_mclaren_simple$lon)) {
  out_lon9[[i]] <- make_seq9(grid_mclaren_simple$lon[[i]])
}

lons9_unlisted <- purrr::map_df(out_lon9, tibble::as_tibble) %>% 
  mutate(xy = x*y) %>% 
  rename(lon = xy) %>% 
  select(t, lon)

out_lat9 <- vector("list", length(grid_mclaren_simple$lat))
for (i in seq_along(grid_mclaren_simple$lat)) {
  out_lat9[[i]] <- make_seq9(grid_mclaren_simple$lat[[i]])
}

lats9_unlisted <- purrr::map_df(out_lat9, tibble::as_tibble) %>% 
  mutate(xy = x*y) %>% 
  rename(lat = xy) %>% 
  select(lat)

lons_lats9 <- lons9_unlisted %>% 
  bind_cols(lats9_unlisted)

scatter_plot_animate_firework <- lons_lats9 %>% 
  ggplot(aes(lon, lat)) + 
  geom_point(color = '#E27231', size = 1) +
  #with_outer_glow(geom_point(color = '#E27231', size =2), colour='#47c7fc', sigma = 2, expand = 2) +
  transition_time(t) + 
  shadow_wake(0.3) 

animate(scatter_plot_animate_firework, fps=10)

anim_save("./04_animate_gifs/sixth_saved_animation_logo_animate2.gif", height = 372, width = 538, units = "px")


# *4.14 Hearts to McLaren logo----
# **4.14.1 McLaren heart third phase----

grid_mclaren_heart1 <- read.csv('./00_raw_data/mclaren_logo_heart1_dots_horizontal_flip.csv') %>% 
  select(2:3) %>% 
  rename(lon = x, lat = y)

p_mclaren_heart1 <- grid_mclaren_heart1 %>% 
  ggplot(aes(lon, lat)) + 
  geom_point(color = 'white') 

p_mclaren_heart1

p_mclaren_heart1_polygon <- grid_mclaren_heart1 %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")

st_centroid(p_mclaren_heart1_polygon)

centroid_longitude_heart1 <- 143.7585
centroid_latitude_heart1 <- 119.9406

grid_mclaren_heart1 |>
  group_split(lon, lat) %>% 
  map_dfr(~tibble(t= 1:10,
                  lon = .x$lon, 
                  lat = .x$lat,
                  lon_vals = test_fct_aj(centroid_longitude_heart1, .x$lon),
                  lat_vals = test_fct_aj(centroid_latitude_heart1, .x$lat))) -> mc_test_aj_heart1

# find mid-colours btween deeppink and papaya
# https://meyerweb.com/eric/tools/color-blend/#FF1493:E27231:2:hex

scatter_plot_animate_heart1 <- mc_test_aj_heart1 %>% 
  ggplot(aes(lon_vals, lat_vals)) + 
  geom_point(color = '#EC5352', size =2) +
  #with_outer_glow(geom_point(color = '#E27231', size =2), colour='gold', sigma = 5, expand = 5) +
  transition_time(t) + 
  shadow_wake(0.5) +
  annotate("text", y = 70, x = 170, label = "@allanjames1506", lineheight = 0.75, family = 'Zen',  size = 5, color = "gray40", vjust = 0.5)

animate(scatter_plot_animate_heart1, fps = 30)

# **4.14.2 McLaren heart second phase----

grid_mclaren_heart2 <- read.csv('./00_raw_data/mclaren_logo_heart2_dots_horizontal_flip.csv') %>% 
  select(2:3) %>% 
  rename(lon = x, lat = y)

p_mclaren_heart2 <- grid_mclaren_heart2 %>% 
  ggplot(aes(lon, lat)) + 
  geom_point(color = 'white') 

p_mclaren_heart2

p_mclaren_heart2_polygon <- grid_mclaren_heart2 %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")

st_centroid(p_mclaren_heart2_polygon)

centroid_longitude_heart2 <- 145.2814
centroid_latitude_heart2 <- 121.1838

grid_mclaren_heart2 |>
  group_split(lon, lat) %>% 
  map_dfr(~tibble(t= 1:10,
                  lon = .x$lon, 
                  lat = .x$lat,
                  lon_vals = test_fct_aj(centroid_longitude_heart2, .x$lon),
                  lat_vals = test_fct_aj(centroid_latitude_heart2, .x$lat))) -> mc_test_aj_heart2

# find mid-colours btween deeppink and papaya
# https://meyerweb.com/eric/tools/color-blend/#FF1493:E27231:2:hex

scatter_plot_animate_heart2 <- mc_test_aj_heart2 %>% 
  ggplot(aes(lon_vals, lat_vals)) + 
  geom_point(color = '#F53372', size =2) +
  #with_outer_glow(geom_point(color = '#E27231', size =2), colour='gold', sigma = 5, expand = 5) +
  transition_time(t) + 
  shadow_wake(0.5) +
  annotate("text", y = 70, x = 170, label = "@allanjames1506", lineheight = 0.75, family = 'Zen',  size = 5, color = "gray40", vjust = 0.5)

animate(scatter_plot_animate_heart2, fps = 30)

# **4.14.3 McLaren heart central phase----

grid_mclaren_heart3 <- read.csv('./00_raw_data/mclaren_logo_heart3_dots_horizontal_flip.csv') %>% 
  select(2:3) %>% 
  rename(lon = x, lat = y)

p_mclaren_heart3 <- grid_mclaren_heart3 %>% 
  ggplot(aes(lon, lat)) + 
  geom_point(color = 'white') 

p_mclaren_heart3

p_mclaren_heart3_polygon <- grid_mclaren_heart3 %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")

st_centroid(p_mclaren_heart3_polygon)

centroid_longitude_heart3 <- 140.4518
centroid_latitude_heart3 <- 118.0075

grid_mclaren_heart3 |>
  group_split(lon, lat) %>% 
  map_dfr(~tibble(t= 1:10,
                  lon = .x$lon, 
                  lat = .x$lat,
                  lon_vals = test_fct_aj(centroid_longitude_heart3, .x$lon),
                  lat_vals = test_fct_aj(centroid_latitude_heart3, .x$lat))) -> mc_test_aj_heart3

# find mid-colours btween deeppink and papaya
# https://meyerweb.com/eric/tools/color-blend/#FF1493:E27231:2:hex

scatter_plot_animate_heart3 <- mc_test_aj_heart3 %>% 
  ggplot(aes(lon_vals, lat_vals)) + 
  geom_point(color = '#FF1493', size =2) +
  #with_outer_glow(geom_point(color = '#E27231', size =2), colour='gold', sigma = 5, expand = 5) +
  transition_time(t) + 
  shadow_wake(0.5) +
  annotate("text", y = 70, x = 170, label = "@allanjames1506", lineheight = 0.75, family = 'Zen',  size = 5, color = "gray40", vjust = 0.5)

animate(scatter_plot_animate_heart3, fps = 30)

# **4.14.4 4 phased heart to McLaren speedmark--------
#grid_mclaren_heart3 == core heart --> core (1:10) --> split 1:5
#grid_mclaren_heart2 == core heart to mclaren phase 1 --> phase1 (1:10)  --> split 6:10
#grid_mclaren_heart1 == core heart to mclaren phase 2 --> phase2 (11:20) --> split 11:15
#grid_mclaren_slice == mclaren speedmark --> speedmark (11:20) --> split 16:20

grid_mclaren_heart3 |>
  group_split(lon, lat) %>% 
  map_dfr(~tibble(t= 1:10,
                  lon = .x$lon, 
                  lat = .x$lat,
                  lon_vals = test_fct_aj(centroid_longitude_heart3, .x$lon),
                  lat_vals = test_fct_aj(centroid_latitude_heart3, .x$lat))) -> core

core <- core %>% 
  mutate(colour = '#FF1493') %>% 
  filter(t == 1 | t == 2 | t == 3 | t == 4 | t == 5)

grid_mclaren_heart2 |>
  group_split(lon, lat) %>% 
  map_dfr(~tibble(t= 1:10,
                  lon = .x$lon, 
                  lat = .x$lat,
                  lon_vals = test_fct_aj(centroid_longitude_heart3, .x$lon),
                  lat_vals = test_fct_aj(centroid_latitude_heart3, .x$lat))) -> phase1

phase1 <- phase1 %>% 
  mutate(colour = '#F53372') %>% 
  filter(t == 6 | t == 7 | t == 8 | t == 9 | t == 10)

grid_mclaren_heart1 |>
  group_split(lon, lat) %>% 
  map_dfr(~tibble(t= 3:12,
                  lon = .x$lon, 
                  lat = .x$lat,
                  lon_vals = test_fct_aj(centroid_longitude_heart3, .x$lon),
                  lat_vals = test_fct_aj(centroid_latitude_heart3, .x$lat))) -> phase2

phase2 <- phase2 %>%
  mutate(colour = '#EC5352') %>% 
  filter(t == 11 | t == 12)
  
grid_mclaren_slice |>
  group_split(lon, lat) %>% 
  map_dfr(~tibble(t= 11:20,
                  lon = .x$lon, 
                  lat = .x$lat,
                  lon_vals = test_fct_aj(centroid_longitude_heart3, .x$lon),
                  lat_vals = test_fct_aj(centroid_latitude_heart3, .x$lat))) -> speedmark

speedmark <- speedmark %>%
  mutate(colour = '#E27231') 
#%>% 
  filter(t == 16 | t == 17 | t == 18 | t == 19 | t == 20)
  
heart_to_speedmark <- bind_rows(core, phase1, speedmark)

scatter_plot_animate_heart_to_speedmark <- heart_to_speedmark %>% 
  ggplot(aes(lon_vals, lat_vals)) + 
  geom_point(colour = heart_to_speedmark$colour, size =2) +
  #with_outer_glow(geom_point(color = '#E27231', size =2), colour='gold', sigma = 5, expand = 5) +
  transition_time(t) + 
  shadow_wake(0.5) +
  annotate("text", y = 70, x = 170, label = "@allanjames1506", lineheight = 0.75, family = 'Zen',  size = 5, color = "gray40", vjust = 0.5)

animate(scatter_plot_animate_heart_to_speedmark, fps = 30, duration = 10)

anim_save("./04_animate_gifs/first_saved_animation_heart_to_speedmark_logo_animate.gif", height = 372, width = 538, units = "px")

# 5 No.4----

mclaren_no4 <- read.csv('./00_raw_data/lando_no4_points_horizontal_flip.csv') %>% 
  select(2:3) %>% 
  rename(lon = x, lat = y)

mclaren_no4_polygon <- mclaren_no4 %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")

ggplot(mclaren_no4_polygon, aes())+
  geom_sf()+
  labs(fill="Member of a sport association")+
  guides(
    fill=guide_legend(
      nrow=1,
      title.position="top",
      label.position="bottom"
    ))

grd_mclaren_no4 <- st_make_grid(
  mclaren_no4_polygon, # map name 
  n = c(20,20) # number of cells per longitude/latitude
)%>%
  # convert back to sf object
  st_sf()%>%
  # add a unique id to each cell 
  # (will be useful later to get back centroids data)
  mutate(id=row_number())

# Extract mclaren no4 centroids----
cent_grd_mclaren_no4 <- grd_mclaren_no4 %>%
  st_centroid()

# Take a look at the results
ggplot()+
  geom_sf(grd_mclaren_no4, mapping = aes(geometry=geometry))+
  geom_sf(cent_grd_mclaren_no4, mapping = aes(geometry=geometry), pch=21, size=0.5)+
  theme_void()

# Intersect centroids with basemap
cent_grd_mclaren_no4_clean <- cent_grd_mclaren_no4 %>%
  st_intersection(cent_grd_mclaren_no4)

# Make a centroid without geom
# (convert from sf object to tibble)
cent_grd_mclaren_no4_no_geom <- cent_grd_mclaren_no4_clean %>%
  st_drop_geometry()

# Join with grid thanks to id column
grd_mclaren_no4_clean <- grd_mclaren_no4 %>%
  #filter(id%in%sel)%>%
  left_join(cent_grd_mclaren_no4_no_geom)

# *6.1 nhs board area----
plot_no4 <- ggplot() +
  geom_sf(
    # drop_na() is one way to suppress the cells outside the country
    grd_mclaren_no4_clean %>% tidyr::drop_na(), 
    mapping = aes(geometry = geometry)
  ) +
  geom_sf(cent_grd_mclaren_no4_clean, mapping = aes(geometry = geometry), fill=NA, pch=21, size=0.5) +
  guides(
    color = 'none',
    fill = guide_legend(
      keywidth = 4, keyheight = 1, nrow = 1,
      title.position = "top", label.position = "bottom"
    )
  ) +
  theme_void()

plot_no4







