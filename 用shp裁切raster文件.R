# load the raster, sp, and rgdal packages
library(raster)
library(rgdal)
library(spdep)

library(sp)
library(ggplot2)
library(ggthemes)
library(data.table)

library(RColorBrewer)
library(classInt)
library(viridis) 

# load raster in an R object called 'DEM'
DEM <- raster("/Users/souseki/Documents/R/temp/Economic_Light/灯光数据/F182013.v4c.avg_lights_x_pct/F182013.v4c.stable_lights.avg_vis.tif")
ja <- readOGR('/Users/souseki/Downloads/temp/Shanghai.shp')
crs(ja) <- crs(DEM)

ja_crop <- crop(x = DEM, y = ja)
ja_crop2 <- mask(ja_crop, ja) 

plot(ja_crop2)

# plot with ggplot2
test_sp <- as(ja_crop2, "SpatialPixelsDataFrame")
test <- as.data.table(test_sp)
OR <- fortify(ja)
ggplot() +  
  geom_tile(data=test, aes(x=x, y=y, fill=F182013.v4c.stable_lights.avg_vis), alpha=0.8) + 
  geom_polygon(data=OR, aes(x=long, y=lat, group=group), 
               fill=NA, color="grey50", size=0.25) +
  scale_fill_viridis() +
  coord_equal() +
  theme_bw()

# 提取数据
# set df=TRUE to return a data.frame rather than a list of values
tree_height <- extract(x = DEM, 
                       y = ja, 
                       df=TRUE)

av_tree_height <- extract(x = DEM, 
                       y = ja, 
                       fun = mean,
                       df=TRUE)

# av_tree_height_tower <- extract(x = chm_HARV, 
#                                 y = point_HARV, 
#                                 buffer=20,
#                                 fun=mean, 
#                                 df=TRUE)

## 栅格空间自回归
## raster to shp
ja_sp <- as(ja_crop2, "SpatialPolygonsDataFrame")
weight <- poly2nb(ja_sp, queen = T)
weight_mat <- nb2listw(weight, style="W", zero.policy=TRUE)
moran.lm <- localmoran(ja_sp@data$F182013.v4c.stable_lights.avg_vis, listw = weight_mat, zero.policy = T, na.action=na.pass)
ja_sp@data$lm <- moran.lm[,4]
ja_sp@data$lm[!(ja_sp@data$F182013.v4c.stable_lights.avg_vis > mean(ja_sp@data$F182013.v4c.stable_lights.avg_vis) & ja_sp@data$lm >0)] <- 0

# lm.palette <- colorRampPalette(c("white","orange", "red"), space ="rgb")
# spplot(ja_sp, zcol=c("lm"), col.regions=lm.palette(5), main="高值聚集区域", pretty=T)

colors <- brewer.pal(6, "YlOrRd") #set breaks for the 9 colors 
brks<-classIntervals(ja_sp$lm, n=6, style="quantile")
brks<- brks$brks
plot(ja_sp, col = colors[findInterval(ja_sp$lm, brks, all.inside=TRUE)])
#writeOGR(ja_sp,dsn=".",layer = "/Users/souseki/Downloads/temp/Shanghai_hot.shp", driver="ESRI Shapefile", overwrite_layer = TRUE)

