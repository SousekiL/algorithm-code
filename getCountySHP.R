library(risingMap)
library(rgdal)
library(sp)

points2polygons <- function(df,data) {
  get.grpPoly <- function(group,ID,df) {
    Polygon(coordinates(df[df$adcode==ID & df$group==group,]))
  }
  get.spPoly  <- function(ID,df) {
    Polygons(lapply(unique(df[df$adcode==ID,]$group), get.grpPoly, ID, df),ID)
  }
  spPolygons  <- SpatialPolygons(lapply(unique(df$adcode), get.spPoly, df))
  SpatialPolygonsDataFrame(spPolygons, match.ID=T, data=data)
}

poly.data.info <- data.table()
for (i in 1:dim(adcode.data)[1]) {
  
  adcode = adcode.data$adcode[i]
  
  url <- paste0("http://restapi.amap.com/v3/config/district?",
                "key=29fff7960c355da1865995277d494bb9", 
                "&keywords=", 
                adcode, 
                "&level=city&subdistrict=3&extensions=all")
  
  msg.load <- tryCatch({
    json.file <- getURL(url)
    msg.load <- "true"
  }, error = function(e) {
    "error"
  })
  retry <- 1
  
  while (retry <= 5 & msg.load == 'error'){
    print(paste0('error in url:', url,'; ','retry:',retry ))
    Sys.sleep(runif(1,3,5))
    msg.load <- tryCatch({
      json.file <- getURL(url)
      msg.load <- "true"
    }, error = function(e) {
      "error"
    })
    retry = retry + 1
  }
  
  list.file <- fromJSON(json.file)
  if (length(list.file$districts) == 0) next
  line.file <- list.file$districts[[1]]$polyline
  line.file <- str_split(line.file, "\\|")
  line.file <- lapply(line.file, function(x) str_split(x, ';'))[[1]]
  line.data <- Map(function(x,y) cbind(x,y), line.file,  seq_along(line.file))
  line.data <- lapply(line.data, function(x) cbind(x, seq(1:dim(x)[1])))
  line.data <- do.call(rbind, line.data)
  location <- str_split(line.data[,1], ',')
  lng <- lapply(location, function(x) x[1])
  lat <- lapply(location, function(x) x[2])
  
  centre.location <- unlist(str_split(list.file$districts[[1]]$center, ','))
  centre.lng <- centre.location[1]
  centre.lat <- centre.location[2]
  poly.data <- data.table(lng = lng, lat = lat, 
                          group = line.data[,2], order = line.data[,3],
                          centre.lng = centre.lng,
                          centre.lat = centre.lat)
  
  poly.data$lng %<>% as.character() %>% as.numeric()
  poly.data$lat %<>% as.character() %>% as.numeric()
  poly.data$group %<>% as.character() %>% as.numeric()
  poly.data$order %<>% as.character() %>% as.numeric()
  poly.data$centre.lng %<>% as.character() %>% as.numeric()
  poly.data$centre.lat %<>% as.character() %>% as.numeric()
  
  poly.data.info.temp <- data.table(poly.data,
                                    name = adcode.data$名称[i],
                                    adcode = adcode.data$adcode[i],
                                    citycode = adcode.data$citycode[i])
  
  poly.data.info <- data.table(rbind(poly.data.info, poly.data.info.temp))
  
  cat(paste0(i, '/', dim(adcode.data)[1], ':', adcode.data$名称[i]), collapse = '\n')
  
}

# GCJ to WGS
location <- gcj2wgs(lng = poly.data.info$lng, lat = poly.data.info$lat)
poly.data.info$trans_lng <- location$trans_lng
poly.data.info$trans_lat <- location$trans_lat

# save as shp
poly.data <- poly.data.info
poly.data.info$trans_lng  <- as.numeric(poly.data.info$trans_lng)
poly.data.info$trans_lat  <- as.numeric(poly.data.info$trans_lat)
coordinates(poly.data.info) <- c('trans_lng', 'trans_lat')

index.data <- unique(poly.data[, .(name, citycode, adcode)])
rownames(index.data) <- index.data$adcode
spDF <- points2polygons(poly.data.info, index.data)

#Write as geojson
writeOGR(spDF, paste0(filename, '.geojson'), filename, driver='GeoJSON')

# write as shp
writeOGR(spDF,dsn=".",layer = filename, driver="ESRI Shapefile", overwrite_layer = TRUE)
