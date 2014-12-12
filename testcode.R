thickness <- read.csv("./datasets/icesat_icethk_ma07_filled_cleaned.csv", header=TRUE, stringsAsFactors=FALSE)
thickness$SSMI.x.location..km. <- as.numeric(gsub(",", "", thickness$SSMI.x.location..km.))

set.seed(76)
n <- 300
subset <- sample(1:nrow(thickness), n)

# http://cran.r-project.org/web/packages/gstat/vignettes/gstat.pdf
# http://www.asdar-book.org/

