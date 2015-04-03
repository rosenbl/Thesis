well_logs <- read.csv("~/Desktop/GitThesis/well_logs.csv", header = TRUE)

well_logs <- filter(well_logs, !is.na(latitude))
well_logs <- filter(well_logs, !is.na(longitude))
well_logs <- filter(well_logs, !is.na(depth_first_water))
well_logs <- filter(well_logs, !is.na(max_yield))

well_logs <- filter(well_logs, longitude >= -124.5, longitude <= -116.5)
well_logs <- filter(well_logs, latitude >= 42, latitude <= 46.5)


library(ggmap)
library(ggplot2)

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
             color = "red",
             aes(x = longitude, y = latitude))
