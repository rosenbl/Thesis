library(ggmap)
library(ggplot2)
library(SpatialEpi)
library(geoR)
library(dplyr)


latlong <- read.csv("latlong.csv")
grid <- read.csv("grid2.csv")
oregonborder <- read.csv("oregonborder3.csv")

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
gm + geom_tile(data=results, 
               aes(x,y, alpha=0.7, fill=var)) + scale_fill_gradient(low="yellow", high="red")

# some more data visualization
hist(latlong$depth, main="Depths", xlab="depth", ylab="frequency")
