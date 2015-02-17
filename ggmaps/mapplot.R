data <- read.csv("~/Desktop/GitThesis/gasLocations_withLatLong.csv", header = TRUE)

library(ggmap)
library(ggplot2)

mapImageData <- get_map(location = c(long = mean(data$long),
                                     lat = mean(data$lat)),
                        color = "color", # or bw
                        source = "google",
                        maptype = "roadmap",
                        zoom = 9)


data$NOX <- as.numeric(gsub(",", "", data$NOX))

ggmap(mapImageData,
      extent = "panel",
      ylab = "Latitude",
      xlab = "Longitude",
      legend = "right") +
  geom_point(data = data, 
             #color = "blue",
             aes(x = long, y = lat, color=NOX)) 


