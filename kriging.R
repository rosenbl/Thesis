library(ggmap)
library(ggplot2)
library(SpatialEpi)
library(geoR)
library(dplyr)


latlong <- read.csv("latlong2.csv")
grid <- read.csv("grid2.csv")

Map <- get_map(location = "Oregon",
               color = "color", # or bw
               source = "google",
               maptype = "roadmap",
               zoom = 6)

gm <- ggmap(Map,
      extent = "panel",
      ylab = "Latitude",
      xlab = "Longitude",
      legend = "right") 

gm + geom_point(data = latlong, 
             #color = "red",
             aes(x = x, y = y, color = depth)) + scale_colour_gradient(low="black", high="red")

# summary of data
plot(grid$x, grid$y)
dim(grid)

dists <- dist(grid[,1:2])
summary(dists)

# set up data for variogram
geogrid <- as.geodata(grid)
geogrid$kappa <- 0.5
geogrid$lambda <- 1
geogrid$cov.model <- "cubic"

breaks <- seq(from = 0, to = 300, by = 300/15)

# make variogram
variogram <- variog(geogrid, breaks = breaks, option = "bin")
plot(variogram, main = "Variogram")

variogram

# fit model
fit <- variofit(variogram, 
                ini.cov.pars=c(40000,225), 
                cov.model="spherical", 
                fix.nugget=FALSE, 
                max.dist=300)
summary(fit)
lines(fit)


# choose prediction regions based on coordinate mins/maxes
summary(grid)

loc <- expand.grid(seq(-10785, -10170, by=10), seq(4668, 5125, by=10))

# kriging step, provide sigmasq/phi vector from fitted variogram
kc <- krige.conv(geogrid, loc=loc, krige=krige.control(cov.pars=c(28048.6894, 254.8803)))

# create prediction map
image(kc)

# and variance map
image(kc, val=sqrt(kc$krige.var))


resultsxy <- data.frame(
  x = loc$Var1,
  y = loc$Var2
  )


resultslatlong <- grid2latlong(resultsxy)

results <- data.frame(
  x = resultslatlong$x,
  y = resultslatlong$y,
  predict = kc$predict,
  var = kc$krige.var
)

# build the maps

# prediction points only
gm + geom_point(data=results, aes(x,y, alpha=0.5))

# prediction points with predicted values
gm + geom_point(data=results, aes(x, y, alpha=0.7, color=predict))

# tiled prediction map
tile <- gm + geom_tile(data=results, aes(x, y, alpha=0.7, fill=predict))
tile

# with contour plot
contour <- tile + geom_contour(data=results, aes(x,y, z=predict))
contour

# polygon plot
poly <- gm + stat_contour(data=results, 
                          geom="polygon", 
                          bins=4, 
                          aes(x,y, z=predict, fill=..level.., alpha = ..level..))
poly + guides(alpha="none")

# variance map
gm + geom_tile(data=results, aes(x,y, alpha=0.7, fill=var)) + scale_fill_gradient(low="yellow", high="red")
