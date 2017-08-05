library(rgeos)
library(spdep)
library(maptools)
china <- readShapePoly('/Users/souseki/Documents/CBNweekly/map_data/province/province.shp')

## 虚拟数据
china@data$runif <- runif(34, min = 1, max = 5)
## 空间邻接矩阵
#aj.mat  <- gIntersects(china, byid = TRUE)
weight <- poly2nb(china, queen = T)
weight[[5]] <- as.integer(26)
weight[[21]] <- as.integer(13)
weight_mat <- nb2listw(weight, style="W", zero.policy=TRUE)

## 全局空间自相关检验: Global Moran's I
moran.test(china@data$runif, listw = weight_mat, zero.policy = T, na.action=na.pass)

## 局部自相关性检验: Local Moran's I
moran.lm <- localmoran(china@data$runif, listw = weight_mat, zero.policy = T, na.action=na.pass)

## 找出高值-高值聚集区
china@data$lm <- moran.lm[,4]

# 将高于平均值且z得分大于0的区域z得分保留，
# 其他区域赋0值
china@data$lm[!(china@data$runif >mean(china@data$runif) & china@data$lm >0)] <- 0
# 设置颜色梯度
lm.palette <- colorRampPalette(c("white","orange", "red"), space ="rgb")
## 绘图显示高值聚集区域
spplot(china, zcol=c("lm"), col.regions=lm.palette(5), main="高值聚集区域", pretty=T)


