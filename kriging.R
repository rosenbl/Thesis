library(ggmap)
library(ggplot2)
library(SpatialEpi)
library(geoR)
library(dplyr)
library(knitr)


latlong <- read.csv("latlong.csv")
grid <- read.csv("grid2.csv")
oregonborder <- read.csv("oregonborder3.csv")

Map <- get_map(location = "Oregon",
               color = "color", # or bw
               source = "google",
               maptype = "roadmap",
               zoom = 6)

Portland <- get_map(location = "Portland, Oregon",
                    color = "color",
                    source = "google",
                    maptype = "roadmap",
                    zoom = 7)

gm <- ggmap(Map,
      extent = "panel",
      ylab = "Latitude",
      xlab = "Longitude",
      legend = "right") 

pgm <- ggmap(Portland,
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
geogrid$cov.model <- "spherical"

breaks <- seq(from = 0, to = 300, by = 300/15)

# make variogram
variogram <- variog(geogrid, breaks = breaks, option = "bin")

plot(variogram, main = "Variogram")


v.summary <- cbind(cbind(breaks[2:16]), variogram$v, variogram$n)
colnames(v.summary) <- c("lag", "semi-variance", "# of pairs")

v.summary
plot(v.summary[,1], v.summary[,3], main="Lag distances", xlab="lag", ylab="# of pairs")

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

loc <- expand.grid(seq(-10885, -10070, by=10), seq(4668, 5200, by=10))

border <- latlong2grid(oregonborder)

ins <- locations.inside(loc, border)

# kriging step, provide sigmasq/phi vector from fitted variogram
krige <- krige.conv(geogrid, loc=ins, krige=krige.control(cov.pars=c(28048.6894, 254.8803)))


resultsxy <- data.frame(
  x = ins$Var1,
  y = ins$Var2
  )


resultslatlong <- grid2latlong(resultsxy)

results <- data.frame(
  x = resultslatlong$x,
  y = resultslatlong$y,
  predict = krige$predict,
  var = krige$krige.var
)

# build the maps

# prediction points only
gm + geom_point(data=results, alpha=0.5, aes(x,y))
pgm + geom_point(data=results, aes(x,y))

# prediction points with predicted values
gm + geom_point(data=results, alpha=0.7, aes(x, y, color=predict))

# tiled prediction map
tile <- gm + geom_tile(data=results, alpha=0.5, aes(x, y, fill=predict))
tile

tile_zoom <- pgm + geom_tile(data=results, alpha=0.7, aes(x, y, fill=predict))
tile_zoom

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
gm + geom_tile(data=results, alpha=0.7,
               aes(x,y, fill=var)) + scale_fill_gradient(low="yellow", high="red")

# standard deviation map
sd <- sqrt(results$var)

gm + geom_tile(data=results, alpha=0.7, aes(x,y, fill=sd)) + scale_fill_gradient(low="yellow", high="red")

# cropped variance map
crop <- filter(results, x > -123.8) %>% filter(x < -117.3) %>% filter(y > 42.2) %>% filter(y < 45.5)

gm + geom_tile(data=crop, alpha=0.7,
               aes(x,y, fill=var)) + scale_fill_gradient(low="yellow", high="red")

# cropped sd map
sd <- sqrt(crop$var)
gm + geom_tile(data=crop, alpha=0.7, aes(x,y, fill=sd)) + scale_fill_gradient(low="yellow", high="red")

# some more data visualization
hist(latlong$depth, main="Depths", xlab="depth", ylab="frequency")

# condensed variogram plots

fit.sph <- variofit(variogram, 
                ini.cov.pars=c(40000,225), 
                cov.model="spherical", 
                fix.nugget=FALSE, 
                max.dist=300)

fit.exp <- variofit(variogram, 
                ini.cov.pars=c(40000,225), 
                cov.model="exponential", 
                fix.nugget=FALSE, 
                max.dist=300)

fit.mat <- variofit(variogram, 
                ini.cov.pars=c(40000,225), 
                cov.model="matern", 
                fix.nugget=FALSE, 
                max.dist=300)

fit.cube <- variofit(variogram, 
                ini.cov.pars=c(40000,225), 
                cov.model="cubic", 
                fix.nugget=FALSE, 
                max.dist=300)

lines(fit.sph, col="red")
lines(fit.mat, col="blue")
lines(fit.cube, col="green")


