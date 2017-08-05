
source('/Users/souseki/Documents/CBNweekly/大学榜单/data/University/prepare.R')


adcode.data <- data.table(read.xlsx('/Users/souseki/Documents/CBNweekly/Library_theRisingMap/amap_getCounty/高德地图API 城市编码对照表.xlsx', sheet = 2))
adcode.data$adcode %<>% as.character() %>% as.numeric()
adcode.data <- adcode.data[grep('^022', adcode.data$citycode), ]
setwd('/Users/souseki/Downloads/temp/')
filename <- 'Shanghai'
source('/Users/souseki/Documents/CBNweekly/Project_Myselt/algorithm/algorithmProject/getCountySHP.R')


setwd('/Users/souseki/Downloads/temp/')
pointsdata <- data.frame(lng=runif(7), lat=runif(7), group = c(rep("a", 3), rep("b", 4)), info = sample(LETTERS, 7))
source('/Users/souseki/Documents/CBNweekly/Project_Myselt/algorithm/algorithmProject/points2line.R')


