#############################################
##### Load packages
#############################################
packages <- c("rgdal","caret","raster","foreign", "kernlab", "colorRamps", "landsat", "tiff", "sf", "rgeos", "profvis")
for(i in 1:NROW(packages)){
  if (!require(packages[i], character.only= TRUE)) {
    install.packages(packages[i])
    library(packages[i], character.only= TRUE)
  }
}
# turn off factors
options(stringsAsFactors = FALSE)

#############################################
##### Set directories
#############################################
if(Sys.info()['sysname'] == "Windows"){
  # paths Win
  wd <- "D:/Dateien/Studium_KIT/Master_GOEK/7_FS_Geooekologie/Methoden_der_Umweltforschung_2/Projekt"
  dir_Landsat_data <- paste(wd, "/Landsat", sep = "")
  dir_shapefile <- paste(wd, "/Landsat/shapefile/", fire_name, sep = "")
  db <- "C:/Users/Manuel/Dropbox"
}else if(Sys.info()['sysname'] == "Linux"){
  # paths Lin
  wd <- "/media/manuel/MANU/Studium_KIT/Master_GOEK/Methoden_der_Umweltforschung_2/Projekt"
  dir_Landsat_data <- paste(wd, "/Landsat", sep = "")
  dir_shapefile <- paste(wd, "/Landsat/shapefile/", fire_name, sep = "")
  db <- "/home/manuel/Dropbox"
}else{
  print("Error: OS not identified.")
}
VegMapDate <- 2010
if(VegMapDate == 2001){
  NOAACCAP <- paste(wd, "/QGIS/NOAA/2001/", sep = "")
}else if(VegMapDate == 2006){
  NOAACCAP <- paste(wd, "/QGIS/NOAA/2006/", sep = "")
}else if(VegMapDate == 2010){
  NOAACCAP <- paste(wd, "/QGIS/NOAA/2010/", sep = "")
}else{
  cat(paste("There is no data available for the date ", VegMapDate,
            ".\nTry 2001, 2006 or 2010.", sep = ""))
}

dir.create(paste(wd, "/output", sep = ""))
dir.create(paste(wd, "/output/", fire_name, sep = ""))
out_dir <- paste(wd, "/output/", fire_name, sep = "")
dir.create(paste(wd, "/r-script", sep = ""))
dir.create(paste(wd, "/paper", sep = ""))

# backup copies
file.copy(paste(db, "/MDU_2_r-script_new.R", sep = ""),
          paste(wd, "/r-script/MDU_2_r-script_new.R", sep = ""),
          overwrite = TRUE
          )
file.copy(paste(db, "/MDU2_Proj.R", sep = ""),
          paste(wd, "/r-script/MDU2_Proj.R", sep = ""),
          overwrite = TRUE
)

#############################################
##### Load, stack and clip Landsat data
#############################################
#U.S. Landsat ARD products are generated in the Albers Equal Area (AEA) Conic map projection,
#processed directly from Landsat Level-1 AEA scenes through Landsat Level-2 data products using the WGS84 datum
dirs_Landsat_data003012 <- list.dirs(path = paste(
  dir_Landsat_data, LS_pic, sep = ""
  ), full.names = TRUE, recursive = FALSE)
dirs_LS05_data <- dirs_Landsat_data003012[grep("LT05", dirs_Landsat_data003012)]
dirs_LS08_data <- dirs_Landsat_data003012[grep("LC08", dirs_Landsat_data003012)]

dates_LS05_data <- substr(dirs_LS05_data, nchar(dir_Landsat_data)+24, nchar(dir_Landsat_data)+31)
dates_LS08_data <- substr(dirs_LS08_data, nchar(dir_Landsat_data)+24, nchar(dir_Landsat_data)+31)

if(file.exists(paste(dir_shapefile, "/shape.shp", sep = ""))){
ext <- readOGR(paste(dir_shapefile, "/shape.shp", sep = ""))
ext <- spTransform(ext, crs(stack(
  list.files(dirs_LS05_data[1], pattern="\\.tif$", full.names = T, ignore.case = TRUE)[
    grep("SRB", list.files(dirs_LS05_data[1], pattern="\\.tif$", full.names = T, ignore.case = TRUE))
    ])))
}else{
ext <- NULL
}
raster_stacks <- vector(mode = "list", length = NROW(dirs_LS05_data))

for(i in 1:NROW(dirs_LS05_data)){
  bandnames <- list.files(dirs_LS05_data[i], pattern="\\.tif$", full.names = T, ignore.case = TRUE)
  #info <- list.files(dirs_LS05_data[i], pattern="\\GCP.txt$", full.names = T, ignore.case = TRUE)
  #tmp <- read.table(info, sep = "\t")
  date <- dates_LS05_data[i]
  if(is.null(ext)){
    raster_stacks[[i]] <- stack(bandnames[grep("SRB", bandnames)])
  }else{
    raster_stacks[[i]] <- crop(stack(bandnames[grep("SRB", bandnames)]), ext)
  }
  names(raster_stacks)[i] <- date
  rm(bandnames)
  #rm(info)
  #rm(tmp)
  rm(date)
}

#remove negative values
#"Landsat atmospheric correction and surface reflectance retrieval algorithms are not ideal for water bodies due to the inherently low surface reflectance of water.
#Similarly, surface reflectance values greater than 1.0 can be encountered over bright targets such as snow and playas.
#These are known computational artifacts in the Landsat surface reflectance products"
for(i in 1:NROW(dirs_LS05_data)){
  raster_stacks[[i]][raster_stacks[[i]] < 0] <- NA
}

#############################################
##### Calculate burned area
#############################################
# select pictures
pre <- 1
post <- 1
if(TRUE){ # calculate burned area based on NBR
  threshold <- 0.1
  if(pre == 1){
    preImage <- raster_stacks[[which(names(raster_stacks)==pre_fire_pic1)]]
  }else{
    preImage <- raster_stacks[[which(names(raster_stacks)==pre_fire_pic2)]]
  }
  if(post == 1){
    postImage <- raster_stacks[[which(names(raster_stacks)==post_fire_pic1)]]
  }else{
    postImage <- raster_stacks[[which(names(raster_stacks)==post_fire_pic2)]]
  }
  preNBR <- raster::overlay(preImage[[4]], preImage[[6]],
                            fun = function(x,y){return((x-y)/(x+y))})
  postNBR <- raster::overlay(preImage[[4]], postImage[[6]],
                             fun = function(x,y){return((x-y)/(x+y))})
  burned_area <- raster::overlay(x = preNBR,
                                y = postNBR, fun = function(x, y){
                                x-y >= threshold
                                               })
}else{ # calculate burned area based on NDVI
  LS05_NDVI <- vector(mode = "list", length = NROW(dirs_LS05_data))
  
  for(i in 1:NROW(dirs_LS05_data)){
    LS05_NDVI[[i]] <- raster::overlay(raster_stacks[[i]][[4]], raster_stacks[[i]][[3]],
                                      fun = function(x,y){return((x-y)/(x+y))})
  }
  names(LS05_NDVI) <- names(raster_stacks)
  threshold <- 0.5
  burned_area_rast_1 <- raster::overlay(x = LS05_NDVI[[which(names(LS05_NDVI)==post_fire_pic1)]],
                                        y = LS05_NDVI[[which(names(LS05_NDVI)==pre_fire_pic1)]], fun = function(x, y){
                                          x-y < threshold
                                        })
  
  burned_area_rast_2 <- raster::overlay(x = LS05_NDVI[[which(names(LS05_NDVI)==post_fire_pic2)]],
                                        y = LS05_NDVI[[which(names(LS05_NDVI)==pre_fire_pic2)]], fun = function(x, y){
                                          x-y < threshold
                                        })
  burned_area <- raster::overlay(x = burned_area_rast_1,
                                 y = burned_area_rast_2, fun = function(x, y){
                                   x+y == 2
                                 })
}
#plot(burned_area)

# transform to polygon
overwrite_burned_areaSHP <- TRUE
if(!file.exists(paste(out_dir, "/burned_areas/burned.shp", sep = ""))&&overwrite_burned_areaSHP){
burned_poly <- rasterToPolygons(burned_area, fun = function(x){x >= threshold}, n = 4, na.rm = TRUE, digits = 12, dissolve = TRUE)
bp_sf <- st_as_sf(burned_poly)
pause(0.5)
bps_sf <- st_cast(bp_sf, "POLYGON")
pause(0.5)
BpSf <- bps_sf[as.numeric(st_area(bps_sf))>=50000,]
pause(0.5)

BpSt <- as(BpSf, 'Spatial')
pause(0.5)

# export burned area as polygon
dir.create(paste(out_dir, "/burned_areas", sep = ""))
writeOGR(BpSt,
         paste(out_dir, "/burned_areas", sep = ""),
         driver = "ESRI Shapefile", layer = "burned", overwrite_layer = TRUE
)
rm(burned_poly)
rm(bp_sf)
rm(bps_sf)
rm(BpSf)
} # end of condition: Does the burned area shapefile already exist
if(!exists("BpSt")){
  BpSt <- shapefile(path.expand(paste(out_dir, "/burned_areas/burned", sep = "")))
}

# get burned area as a raster layer
burnedArea <- mask(burned_area, mask = BpSt)
# export raster
writeRaster(burnedArea, paste(out_dir, "/burned_areas/burned_area.tif", sep = ""), format = "GTiff", overwrite = TRUE)

#############################################
##### Get landcover information from NOAA C-CAP data
#############################################
rm(Vega)
rm(Vegb)
if(tryCatch(!is.null(crop(raster(list.files(NOAACCAP, pattern="\\.tif$", full.names = T, ignore.case = TRUE)[1]), BpSt)),
            error=function(e) return(FALSE))){
  Vega <- crop(raster(list.files(NOAACCAP, pattern="\\.tif$", full.names = T, ignore.case = TRUE)[1]), BpSt)
  NAvalue(Vega) <- 0
}
if(tryCatch(!is.null(crop(raster(list.files(NOAACCAP, pattern="\\.tif$", full.names = T, ignore.case = TRUE)[2]), BpSt)),
            error=function(e) return(FALSE))){
  Vegb <- crop(raster(list.files(NOAACCAP, pattern="\\.tif$", full.names = T, ignore.case = TRUE)[1]), BpSt)
  NAvalue(Vegb) <- 0
}
if(exists("Vega")&&exists("Vegb")){
  VegCover <- merge(Vega, Vegb)
}else if(exists("Vega")){
  VegCover <- Vega
}else if(exists("Vegb")){
  VegCover <- Vegb
}

vals <- as.factor(VegCover@data@values)
VegClasses <- levels(vals)
VegCover@data@values <- as.factor(VegCover@data@values)
#VegCover <- projectRaster(VegCover, burned_area)
#plot(VegCover)

#############################################
##### Vegetation classes
#############################################
# list of NOAA C-CAM vegetation classes
classNames <- data.frame(Nb = 0:25, Class = c("NA", "Unclassified", "High_intensity_Developed",
                                              "Medium_intensity_Developed", "Low_Intensity_Developed",
                                              "Developed_Open_Space", "Cultivated", "PastureHay",
                                              "Grassland", "Deciduous_Forest", "Evergreen_Forest",
                                              "Mixed_Forest", "ScrubShrub", "Palustrine_Forested_Wetland",
                                              "Palustrine_Shrub_Wetland", "Palustrine_Emergent_Wetland",
                                              "Estuarine_Forested_Wetland", "Estuarine_Scrub_Wetland",
                                              "Estuarine_Emergent_Wetland", "Unconsolidated_Shore",
                                              "Bare_Land", "Water", "Palustrine_Aquatic_Bed",
                                              "Estuarine_Aquatic_Bed", "Tundra", "SnowIce"))

#############################################
##### Create point layer
#############################################
RasterTemplate <- raster(crop(raster_stacks[[1]], VegCover))
values(RasterTemplate) <- seq(1, ncell(RasterTemplate))
pointsTemplate <- rasterToPoints(RasterTemplate, spatial = TRUE)

#############################################
##### Assign vegetation classes to point layer
#############################################
points <- raster::extract(VegCover, pointsTemplate, sp = TRUE)
names(points) <- c("id", "CCAP_2010")
#points@data$CCAP_2010 <- as.factor(points@data$CCAP_2010)

#############################################
##### Assign burned/ not burned to point layer
#############################################
temp <- raster::extract(burnedArea$layer, pointsTemplate, sp = TRUE)
names(temp) <- c("id", "layer")
points$burned <- as.logical(temp$layer)

#############################################
##### Crop Landsat05 tiles
#############################################
for(i in 1:NROW(dirs_LS05_data)){
raster_stacks[[i]] <- crop(raster_stacks[[i]], VegCover)
}

#############################################
##### Calculate NDVI LS05
#############################################
LS05_NDVI <- vector(mode = "list", length = NROW(dirs_LS05_data))

for(i in 1:NROW(dirs_LS05_data)){
  LS05_NDVI[[i]] <- raster::overlay(raster_stacks[[i]][[4]], raster_stacks[[i]][[3]],
                                    fun = function(x,y){return((x-y)/(x+y))})
}
names(LS05_NDVI) <- names(raster_stacks)

#############################################
##### Calculate DVI LS05
#############################################
#In Landsat 4-7, EVI = 2.5 * ((Band 4 - Band 3) / (Band 4 + 6 * Band 3 - 7.5 * Band 1 + 1))
#source: https://www.usgs.gov/land-resources/nli/landsat/landsat-enhanced-vegetation-index?qt-science_support_page_related_con=0#qt-science_support_page_related_con
#LS05_DVI <- vector(mode = "list", length = NROW(dirs_LS05_data))

#for(i in 1:NROW(dirs_LS05_data)){
#  LS05_DVI[[i]] <- 2.5*((raster_stacks[[i]][[4]]-raster_stacks[[i]][[3]])/(raster_stacks[[i]][[4]]+6*raster_stacks[[i]][[3]]-7.5*raster_stacks[[i]][[1]]+1))
#}
#for(i in 1:NROW(dirs_LS05_data)){
#  LS05_DVI[[i]] <- raster::overlay(raster_stacks[[i]][[4]], raster_stacks[[i]][[3]], raster_stacks[[i]][[1]],
#                                   fun = function(x,y,z){return(
#                                     2.5*((x-y)/(x+6*y-7.5*z+1))
#                                   )})
#}
#names(LS05_DVI) <- names(raster_stacks)

rm(raster_stacks)
#############################################

#############################################
##### Assign LS05 NDVI values to points layer
#############################################
rm(temp)
for(n in 1:NROW(dirs_LS05_data)){
  temp <- raster::extract(LS05_NDVI[[n]]$layer, pointsTemplate, sp = TRUE)
  points[[n+3]] <- temp[[2]]
  names(points)[n+3] <- paste("d", names(LS05_NDVI)[n], sep = "")
}
rows_points <- NROW(names(points))

#############################################
### LS08 data
#############################################

#############################################
##### Load, stack and clip Landsat data
#############################################
#U.S. Landsat ARD products are generated in the Albers Equal Area (AEA) Conic map projection,
#processed directly from Landsat Level-1 AEA scenes through Landsat Level-2 data products using the WGS84 datum
raster_stacks08 <- vector(mode = "list", length = NROW(dirs_LS08_data))

#ext <- spTransform(ext,
#                   crs(stack(list.files(dirs_LS08_data[1], pattern="\\.tif$", full.names = T, ignore.case = TRUE)[
#                     grep("SRB", list.files(dirs_LS08_data[1], pattern="\\.tif$", full.names = T, ignore.case = TRUE))])))

for(i in 1:NROW(dirs_LS08_data)){
  bandnames <- list.files(dirs_LS08_data[i], pattern="\\.tif$", full.names = T, ignore.case = TRUE)
  #info <- list.files(dirs_LS08_data[i], pattern="\\GCP.txt$", full.names = T, ignore.case = TRUE)
  #tmp <- read.table(info, sep = "\t")
  date <- dates_LS08_data[i]
  if(is.null(ext)){
    raster_stacks[[i]] <- stack(bandnames[grep("SRB", bandnames)])
  }else{
    raster_stacks08[[i]] <- crop(stack(bandnames[grep("SRB", bandnames)]), ext)
  }
  names(raster_stacks08)[i] <- date
  rm(bandnames)
  #rm(info)
  #rm(tmp)
  rm(date)
}

#remove negative values
#"Landsat atmospheric correction and surface reflectance retrieval algorithms are not ideal for water bodies due to the inherently low surface reflectance of water.
#Similarly, surface reflectance values greater than 1.0 can be encountered over bright targets such as snow and playas.
#These are known computational artifacts in the Landsat surface reflectance products"
for(i in 1:NROW(dirs_LS08_data)){
  raster_stacks08[[i]][raster_stacks08[[i]] < 0] <- NA
}
#plotRGB(raster_stacks08[[10]], r=3, g=2, b=1)

#############################################
##### Crop Landsat08 tiles
#############################################
for(i in 1:NROW(dirs_LS08_data)){
  raster_stacks08[[i]] <- crop(raster_stacks08[[i]], VegCover)
}

#############################################
##### Calculate NDVI LS08
#############################################
LS08_NDVI <- vector(mode = "list", length = NROW(dirs_LS08_data))

for(i in 1:NROW(dirs_LS08_data)){
  LS08_NDVI[[i]] <- raster::overlay(raster_stacks08[[i]][[5]], raster_stacks08[[i]][[4]], fun = function(x,y){return((x-y)/(x+y))})
}
names(LS08_NDVI) <- names(raster_stacks08)

#############################################
##### Calculate DVI LS08
#############################################
#In Landsat 8, DVI = 2.5 * ((Band 5 - Band 4) / (Band 5 + 6 * Band 4 - 7.5 * Band 2 + 1))
#source: https://www.usgs.gov/land-resources/nli/landsat/landsat-enhanced-vegetation-index?qt-science_support_page_related_con=0#qt-science_support_page_related_con
#LS08_DVI <- vector(mode = "list", length = NROW(dirs_LS08_data))

#for(i in 1:NROW(dirs_LS08_data)){
#  LS08_DVI[[i]] <- raster::overlay(raster_stacks08[[i]][[5]], raster_stacks08[[i]][[4]], raster_stacks08[[i]][[2]],
#                                   fun = function(x,y,z){return(
#                                     2.5*((x-y)/(x+6*y-7.5*z+1))
#                                   )})
#}
#names(LS08_DVI) <- names(raster_stacks08)

#############################################
##### Assign LS08 NDVI values to points layer
#############################################
rm(temp)
for(n in 1:NROW(dirs_LS08_data)){
  temp <- raster::extract(LS08_NDVI[[n]]$layer, pointsTemplate, sp = TRUE)
  points[[n+rows_points]] <- temp[[2]]
  names(points)[n+rows_points] <- paste("d", names(LS08_NDVI)[n], sep = "")
}

#############################################
##### Export points shapefile
#############################################
dir.create(paste(out_dir, "/points", sep = ""))
writeOGR(
  points,
  paste(out_dir, "/points", sep = ""),
  driver = "ESRI Shapefile",
  layer = "Points",
  overwrite_layer = TRUE
)
write.csv2(
  points@data,
  paste(out_dir, "/", "/points/Points.csv", sep = "")
)