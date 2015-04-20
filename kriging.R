library(ggmap)
library(ggplot2)
library(SpatialEpi)
library(geoR)
library(dplyr)


latlong <- read.csv("latlong.csv")
grid <- read.csv("grid.csv")

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
  geom_point(data = latlong, 
             #color = "red",
             aes(x = x, y = y, color = depth))

# summary of data
plot(grid$x, grid$y)
dim(grid)

# set up data for variogram
geogrid <- as.geodata(grid)
geogrid$kappa <- 0.5
geogrid$lambda <- 1
geogrid$cov.model <- "cubic"

breaks <- seq(from = 0, to = 755, by = 755/29)

# make variogram
variogram <- variog(geogrid, breaks = breaks, option = "bin")
plot(variogram, main = "Variogram")

# fit model
fit <- variofit(variogram, 
                ini.cov.pars=c(40000,225), 
                cov.model="powered.exponential", 
                fix.nugget=FALSE, 
                max.dist=700)
summary(fit)
lines(fit)

# choose prediction regions based on coordinate mins/maxes
loc <- expand.grid(seq(-10830, -10170, by=20), seq(4668, 5138, by=20))

# kriging step, provide sigmasq/phi vector from fitted variogram
kc <- krige.conv(geogrid, loc=loc, krige=krige.control(cov.pars=c(40448.6829, 229.4953)))

# create prediction map
image(kc)

# and variance map
image(kc, val=sqrt(kc$krige.var))


results <- data.frame(
  x = loc$Var1,
  y = loc$Var2,
  predict = kc$predict,
  var = kc$krige.var
)