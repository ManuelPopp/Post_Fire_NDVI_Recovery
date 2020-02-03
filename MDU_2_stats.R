require("ggplot2")
require("lubridate")
require("rgdal")
theme_set(theme_classic())

#############################################
##### Set directories
#############################################
if(Sys.info()['sysname'] == "Windows"){
  # paths Win
  wd <- "D:/Dateien/Studium_KIT/Master_GOEK/7_FS_Geooekologie/Methoden_der_Umweltforschung_2/Projekt"
  db <- "C:/Users/Manuel/Dropbox"
}else if(Sys.info()['sysname'] == "Linux"){
  # paths Lin
  wd <- "/media/manuel/MANU/Studium_KIT/Master_GOEK/Methoden_der_Umweltforschung_2/Projekt"
  db <- "/home/manuel/Dropbox"
}else{
  print("Error: OS not identified.")
}
out_dirx <- paste(wd, "/output", sep = "")

# backup copies
file.copy(paste(db, "/MDU_2_r-script_new.R", sep = ""),
          paste(wd, "/r-script/MDU_2_r-script_new.R", sep = ""),
          overwrite = TRUE
)
file.copy(paste(db, "/MDU2_Proj.R", sep = ""),
          paste(wd, "/r-script/MDU2_Proj.R", sep = ""),
          overwrite = TRUE
)
file.copy(paste(db, "/MDU_2_stats.R", sep = ""),
          paste(wd, "/r-script/MDU_2_stats.R", sep = ""),
          overwrite = TRUE
)

#############################################
##### Load data
#############################################
if(FALSE){
  points <- readOGR(
    paste(out_dirx, "/", fire_name, "/points", sep = "")
  )
}

pointData <- read.csv2(
  paste(out_dirx, "/", fire_name, "/points/Points.csv", sep = ""), sep = ";"
)

#############################################
##### Visualization
#############################################


#############################################
##### Old Script
#############################################
dirx <- list.dirs(path = out_dirx, full.names = TRUE, recursive = FALSE)
namex <- sapply(strsplit(dirx, "output/"), "[", 2)
for(i in 1:NROW(dirx)){
  do.call("<-", list(namex[i], read.csv2(paste(dirx[i], "/Vegetation_indices.csv", sep = ""))))
}

months <- as.Date(c("1900-06-01", "1900-07-01", "1900-08-01", "1900-09-01"))
for(i in 1:NROW(dirx)){
  dax <- as.data.frame(eval(parse(text = namex[i])))
  names(dax)[4] <- "fire"
  dax[, 2] <- as.Date(x = as.character(dax[, 2]))
  dax[, 4] <- as.character(dax[, 4])
  dax[which(dax[, 4] == "pre"), 4] <- 1
  dax[which(dax[, 4] == "during"), 4] <- 2
  dax[which(dax[, 4] == "post"), 4] <- 3
  dax[, 4] <- ordered(dax[, 4], labels = c("pre", "during", "post")[
    c("pre", "during", "post") %in% levels(eval(parse(text = namex[i]))[, 4])])
  daX <- dax[which(month(as.POSIXct(dax[, 2], format = "%Y-%m-%d")) %in% month(as.POSIXct(months, format = "%Y-%m-%d"))),]
  plot <- ggplot(data = daX, aes(x = year, y = mean_NDVI, shape = fire)) +
    geom_point(aes(colour = fire)) +
    geom_smooth(data = daX[daX[, 4] == "post",], fullrange = FALSE, level = 0.95, method = 'loess', formula = 'y ~ x') +
    scale_color_manual(values = c("forestgreen", "red3", "chartreuse3")[
      c("pre", "during", "post") %in% levels(daX[, 4])]) +
    scale_shape_manual(values = c(15, 17, 19)[
      c("pre", "during", "post") %in% levels(dax[, 4])]) + theme(panel.border = element_rect(linetype = "solid", fill = NA),
                                                                 legend.position = c(1, 0), legend.justification = c(1, 0),
                                                                 legend.title = element_blank(), legend.background =
                                                                   element_blank())
  print(plot)
}

out_station <- paste(wd, "/output/station_fire/", sep = "")
files_station <- list.files(out_station, pattern="\\.csv$", full.names = F, ignore.case = TRUE)
for(i in 1:NROW(files_station)){
  dat <- read.csv2(paste(out_station, files_station[i], sep = ""))
  names(dat)[4] <- "fire"
  plot_curr <- ggplot(data = dat, aes(x = date, y = mean_NDVI, shape = fire)) +
    geom_point(aes(colour = fire)) +
    geom_smooth(data = dat[dat[, 4] == "post",], fullrange = FALSE, level = 0.95, method = 'loess', formula = 'y ~ x') +
    scale_color_manual(values = c("forestgreen", "red3", "chartreuse3")[
      c("pre", "during", "post") %in% levels(dat[, 4])]) +
    scale_shape_manual(values = c(15, 17, 19)[
      c("pre", "during", "post") %in% levels(dat[, 4])]) + theme(panel.border = element_rect(linetype = "solid", fill = NA),
                                                                 legend.position = c(1, 0), legend.justification = c(1, 0),
                                                                 legend.title = element_blank(), legend.background =
                                                                   element_blank())
  #assign(sub(".csv", "", files_station[i]), plot_curr)
  print(plot_curr)
}

for(i in 1:50){
  if(pointData[i,4]==1){
    x <- data.frame(NDVI= as.numeric(t(pointData[i,-c(1,2,3,4)])), year= seq(5, ncol(pointData)))
    names(x) <- c("NDVI", "year")
    plot(NDVI ~year, data= x)  
  }
}