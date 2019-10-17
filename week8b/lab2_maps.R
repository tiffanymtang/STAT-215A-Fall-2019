library(maps)
library(tidyverse)
library(irlba)
library(viridis)
set.seed(123)

# load the data
ling_data <- read.table('data/lingData.txt', header = T)
ling_location <- read.table('data/lingLocation.txt', header = T)

# ggplot map themes
state_df <- map_data("state")
my_map_theme <- theme_void()

# do pca on unnormalized aggregated data
X_binned <- ling_location %>%
  select(-Number.of.people.in.cell, -Latitude, -Longitude)
X_binned_svd <- irlba(as.matrix(X_binned), nv = 2, nu = 2)  # compute fast svd
X_binned_scores <- X_binned_svd$u %*% diag(X_binned_svd$d) # compute pc scores
plt_df <- cbind(X_binned_scores, ling_location[, c("Latitude", "Longitude")]) %>%
  setNames(c("PC1", "PC2", "Latitude", "Longitude")) %>%
  filter(Longitude > -125)  # remove HI and AK

# pc plot: unnormalized aggregated data
ggplot(plt_df) +
  aes(x = PC1, y = PC2) +
  geom_point()

# pca map: unnormalized aggregated data
ggplot(plt_df) +
  aes(x = Longitude, y = Latitude, color = PC1) +
  geom_point(shape = 15, size = 2) +
  geom_polygon(data = state_df,
               aes(x = long, y = lat, group = group),
               colour = "black", fill = NA) +
  scale_color_viridis(direction = -1, begin = 0.05, end = 0.97) +
  my_map_theme


# do pca on unaggregated data
load("lingBinary.Rdata")  # from lab3 folder
X <- lingBinary %>%
  select(starts_with("Q")) %>%
  scale(., center = T, scale = F)
X_svd <- irlba(as.matrix(X), nv = 2, nu = 2)  # compute fast svd
X_scores <- X_svd$u %*% diag(X_svd$d)  # compute pc scores
plt_df <- cbind(X_scores, lingBinary[, c("lat", "long")]) %>%
  setNames(c("PC1", "PC2", "Latitude", "Longitude")) %>%
  filter(Longitude > -125)  # remove HI and AK

# pc plot: unaggregated data (without color)
ggplot(plt_df) +
  aes(x = PC1, y = PC2) +
  geom_point(size = .1, alpha = .35)

# pc plot: unaggregated data (with color)
ggplot(plt_df) +
  aes(x = PC1, y = PC2, color = Longitude) +
  geom_point(size = .1, alpha = .5) +
  scale_color_viridis()

# pca map: unnormalized aggregated data
ggplot(plt_df) +
  aes(x = Longitude, y = Latitude, color = PC1) +
  geom_point(size = .25) +
  geom_polygon(data = state_df,
               aes(x = long, y = lat, group = group),
               colour = "black", fill = NA) +
  scale_color_viridis() +
  my_map_theme

