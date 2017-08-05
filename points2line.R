library(sp)    
library(dplyr)

if ('order' %in% colnames(pointsdata)) pointsdata <- arrange(pointsdata, group, order)
coordinates(pointsdata) <- ~lng+lat

## list of Lines per id, each with one Line in a list
x.data <- lapply(split(pointsdata, pointsdata$group), function(x) Lines(list(Line(coordinates(x))), x$group[1L]))

# the corrected part goes here:
lines <- SpatialLines(x.data)
y.data <- data.frame(id = unique(pointsdata$group))
rownames(y.data) <- y.data$id
line.data <- SpatialLinesDataFrame(lines, y.data)

# write as shp
writeOGR(line.data, dsn=".",layer = "Line", driver="ESRI Shapefile", overwrite_layer = TRUE)