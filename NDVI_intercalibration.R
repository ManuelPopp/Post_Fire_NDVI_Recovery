packages <- c("rgdal", "raster", "colorRamps", "landsat", "tiff", "sf", "rgeos", "robustbase")
for(i in 1:NROW(packages)){
  if (!require(packages[i], character.only= TRUE)) {
    install.packages(packages[i])
    library(packages[i], character.only= TRUE)
  }
}
wd <- "/media/manuel/MANU/Studium_KIT/Master_GOEK/7_FS_Geooekologie/Methoden_der_Umweltforschung_2/Projekt/NDVI_intercalibration/US_Landsat_4-8_ARD"

dirs_LS <- list.dirs(wd)
dirs_LS05_data <- dirs_LS[grep("LT05", dirs_LS)]
dirs_LS07_data <- dirs_LS[grep("LE07", dirs_LS)]
dirs_LS08_data <- dirs_LS[grep("LC08", dirs_LS)]

dates_LS05_data <- substr(dirs_LS05_data, nchar(dirs_LS[1])+17, nchar(dirs_LS[1])+24)
dates_LS07_data <- substr(dirs_LS07_data, nchar(dirs_LS[1])+17, nchar(dirs_LS[1])+24)
dates_LS08_data <- substr(dirs_LS08_data, nchar(dirs_LS[1])+17, nchar(dirs_LS[1])+24)

setwd(wd)
setwd("..")
# LS05
dir.create(paste(getwd(), "/LS05/", sep = ""))
for(i in 1:NROW(dirs_LS05_data)){
  bandnames <- list.files(dirs_LS05_data[i], pattern="\\.tif$", full.names = T, ignore.case = TRUE)
  img <- stack(bandnames[grep("SRB", bandnames)])
  writeRaster(img, filename = paste(getwd(), "/LS05/LS5COL", dates_LS05_data[i], ".tif", sep = ""), overwrite = TRUE)
  ndvi <- raster::overlay(img[[4]], img[[3]],
                          fun = function(x,y){return((x-y)/(x+y))})
  writeRaster(ndvi, filename = paste(getwd(), "/LS05/LS5", dates_LS05_data[i], ".tif", sep = ""), overwrite = TRUE)
}
rm(i)

# LS07
dir.create(paste(getwd(), "/LS07/", sep = ""))
for(i in 1:NROW(dirs_LS07_data)){
  if(!file.exists(paste(getwd(), "/LS07/LS7COL", dates_LS07_data[i], ".tif", sep = ""))){
    bandnames <- list.files(dirs_LS07_data[i], pattern="\\.tif$", full.names = T, ignore.case = TRUE)
    img <- stack(bandnames[grep("SRB", bandnames)])
    writeRaster(img, filename = paste(getwd(), "/LS07/LS7COL", dates_LS07_data[i], ".tif", sep = ""), overwrite = TRUE)
    ndvi <- raster::overlay(img[[4]], img[[3]],
                            fun = function(x,y){return((x-y)/(x+y))})
    writeRaster(ndvi, filename = paste(getwd(), "/LS07/LS7", dates_LS07_data[i], ".tif", sep = ""), overwrite = TRUE)
  }else{
    print("File exists.\n")
  }
}
rm(i)

# LS08
dir.create(paste(getwd(), "/LS08/", sep = ""))
for(i in 1:NROW(dirs_LS08_data)){
  if(!file.exists(paste(getwd(), "/LS08/LS8COL", dates_LS08_data[i], ".tif", sep = ""))){
    bandnames <- list.files(dirs_LS08_data[i], pattern="\\.tif$", full.names = T, ignore.case = TRUE)
    img <- stack(bandnames[grep("SRB", bandnames)])
    writeRaster(img, filename = paste(getwd(), "/LS08/LS8COL", dates_LS08_data[i], ".tif", sep = ""), overwrite = TRUE)
    ndvi <- raster::overlay(img[[5]], img[[4]],
                            fun = function(x,y){return((x-y)/(x+y))})
    writeRaster(ndvi, filename = paste(getwd(), "/LS08/LS8", dates_LS08_data[i], ".tif", sep = ""), overwrite = TRUE)
  }else{
    print("File exists.\n")
  }
}
plotRGB(img, r=3, g=2, b=1,  stretch = "lin")

# correction 5-7
L5 <- c("LS520111107", "LS520111031", "LS520110929", "LS520110920", "LS520110718", "LS520110702")
L7 <- c("LS720111108", "LS720111030", "LS720110928", "LS720110921", "LS720110719", "LS720110703")
Pol <- c("S201111", "S201110", "S201109", "201109a", "201107", "2011x")
dats <- data.frame(LS5 = L5, LS7 = L7, pol = Pol)
NDVI5 <- vector()
NDVI7 <- vector()
for(n in 1:nrow(dats)){
  pol <- readOGR(dsn = paste(getwd(), "/", dats[n, 3], ".shp", sep = ""))
  for(i in 1:nrow(pol)){
    NDVI5[i+(n-1)*nrow(pol)] <- cellStats(x = mask(raster(paste(getwd(), "/LS05/", dats[n, 1], ".tif", sep = "")), pol[i, ]), stat = "mean")
    NDVI7[i+(n-1)*nrow(pol)] <- cellStats(x = mask(raster(paste(getwd(), "/LS07/", dats[n, 2], ".tif", sep = "")), pol[i, ]), stat = "mean")
  }
}
rm(i)
rm(n)

dir.create(paste(getwd(), "/output", sep = ""))
mod5to7 <- lm(NDVI7 ~ NDVI5)
ltsmod5to7 <- ltsReg(NDVI7 ~ NDVI5)
sink(file = paste(getwd(), "/output/NDVI5to7.txt", sep = ""))
summary(mod5to7)
summary(ltsmod5to7)
sink()
pdf(file = paste(getwd(), "/output/NDVI5to7.pdf", sep = ""))
plot(NDVI7 ~ NDVI5)
abline(a = summary(mod5to7)$coefficients[1], b = summary(mod5to7)$coefficients[2], col = "red")
dev.off()

# correction 7-8
L7a <- c("LS720150707", "LS720150723", "LS720140517", "LS720130910", "LS720130724", "LS720130816"#
)
L8 <- c("LS820150706", "LS820150724", "LS820140516", "LS820130911", "LS820130725", "LS820130614"#
)
Pola <- c("S201377", "201577", "201405", "S201309", "S201307", "201308"#
)
datt <- data.frame(LS7a = L7a, LS8 = L8, pol = Pola)
NDVI7a <- vector()
NDVI8 <- vector()
for(n in 1:nrow(datt)){
  pol <- readOGR(dsn = paste(getwd(), "/", datt[n, 3], ".shp", sep = ""))
  for(i in 1:nrow(pol)){
    NDVI7a[i+(n-1)*nrow(pol)] <- cellStats(x = mask(raster(paste(getwd(), "/LS07/", datt[n, 1], ".tif", sep = "")), pol[i, ]), stat = "mean")
    NDVI8[i+(n-1)*nrow(pol)] <- cellStats(x = mask(raster(paste(getwd(), "/LS08/", datt[n, 2], ".tif", sep = "")), pol[i, ]), stat = "mean")
  }
}

cbind(NDVI8, NDVI7a)
mod7to8 <- lm(NDVI8 ~ NDVI7a)
ltsmod7to8 <- ltsReg(NDVI8 ~ NDVI7a)
sink(file = paste(getwd(), "/output/NDVI7to8.txt", sep = ""))
summary(mod7to8)
summary(ltsmod7to8)
sink()
pdf(file = paste(getwd(), "/output/NDVI7to8.pdf", sep = ""))
plot(NDVI8 ~ NDVI7a)
abline(a = summary(mod7to8)$coefficients[1], b = summary(mod7to8)$coefficients[2], col = "red")
dev.off()
