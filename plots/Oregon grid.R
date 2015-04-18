library(ggmap)
library(ggplot2)
library(SpatialEpi)
library(geoR)
library(dplyr)


well_logs <- read.csv("well_logs.csv", header = TRUE)

well_logs <- filter(well_logs, !is.na(latitude)) %>%
  filter(!is.na(longitude)) %>%
  filter(!is.na(depth_first_water)) %>%
  filter(!is.na(max_yield)) %>%
  filter(!is.na(start_date)) %>%
  filter(longitude >= -124.5, longitude <= -116.5) %>%
  filter(latitude >= 42, latitude <= 46.5)

well_logs$numeric_date <- as.numeric(gsub("/", "", well_logs$start_date))
well_logs$numeric_date <- (well_logs$numeric_date %% 100)

well_logs <- filter(well_logs, numeric_date < 16) %>%
  filter(numeric_date > 5)

Map <- get_map(location = c(long = -121.9113903,
                            lat = 43.7701953),
               color = "color", # or bw
               source = "google",
               maptype = "roadmap",
               zoom = 6)


ggmap(Map,
      extent = "panel",
      ylab = "Latitude",
      xlab = "Longitude",
      legend = "right") +
  geom_point(data = well_logs, 
             #color = "red",
             aes(x = longitude, y = latitude, color = well_logs$depth_first_water))


# create dataframe with grid coordinates
coordinates <- cbind(well_logs$longitude, well_logs$latitude)
grid <- latlong2grid(coordinates)
View(grid)
plot(grid)

# trim coordinates to oregon
grid <- filter(grid, x < -10300) %>%
  filter(y > 4600) %>%
  filter(x > -11000) %>%
  filter(y < 5200)

# remove duplicate coordinates
grid <- unique(grid)

grid <- mutate(grid, depth = well_logs$depth_first_water)




# summary of data
plot(grid$x, grid$y)
dim(grid)

dists <- dist(grid[,1:2])
summary(dists)

# set up data for variogram
grid$coords <- grid[,1:2]
grid$data <- grid$depth

geogrid <- as.geodata(grid)
geogrid$kappa <- 0.5
geogrid$lambda <- 1
geogrid$cov.model <- "matern"

# run variogram, alter binning options to include 30 bins
breaks <- seq(from = 0, to = 847, by = 847/29)
variogram <- variog(geogrid, breaks = breaks, option = "bin")
plot(variogram, main = "Variogram")