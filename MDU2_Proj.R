#############################################
##### Set directories
#############################################
if(Sys.info()['sysname'] == "Windows"){
  # paths Win
  r_script <- "C:/Users/Manuel/Dropbox/MDU_2_r-script_new.R"
  stats_script <- "C:/Users/Manuel/Dropbox/MDU_2_stats.R"
  wd <- "D:/Dateien/Studium_KIT/Master_GOEK/7_FS_Geooekologie/Methoden_der_Umweltforschung_2/Projekt"
  dir_Landsat_data <- paste(wd, "/Landsat", sep = "")
  db <- "C:/Users/Manuel/Dropbox"
}else if(Sys.info()['sysname'] == "Linux"){
  # paths Lin
  r_script <- "/home/manuel/Dropbox/MDU_2_r-script_new.R"
  stats_script <- "/home/manuel/Dropbox/MDU_2_stats.R"
  wd <- "/media/manuel/MANU/Studium_KIT/Master_GOEK/7_FS_Geooekologie/Methoden_der_Umweltforschung_2/Projekt"
  dir_Landsat_data <- paste(wd, "/Landsat", sep = "")
  db <- "/home/manuel/Dropbox"
}else{
  print("Error: OS not identified.")
}
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

##########################################################
### Station fire
##########################################################
fire_name <- "station_fire"
LS_pic <- "/003012"

pre_fire_pic1 <- "20090806"
post_fire_pic1 <- "20091025"
pre_fire_pic2 <- "20090721"
post_fire_pic2 <- "20091110"
VegMapDate <- 2010
prior <- 2006

fire_start <- "2009-08-26"
fire_end <- "2009-10-16"
# calculate new and overwrite?
overwrite_NOAA <- TRUE
overwrite_burned_areaSHP <- FALSE

source(file = r_script)
source(file = stats_script)

##########################################################
### Painted Cave fire
##########################################################
fire_name <- "painted_cave_fire"
LS_pic <- "/002011"

pre_fire_pic1 <- "19900606"
post_fire_pic1 <- "19900910"
pre_fire_pic2 <- "19900622"
post_fire_pic2 <- "19901012"

fire_start <- "1990-06-27"
fire_end <- "1990-07-02"
# calculate new and overwrite?
overwrite_NOAA <- TRUE
overwrite_burned_areaSHP <- TRUE

source(file = r_script)
source(file = stats_script)

##########################################################
### Thomas, Rye and Creek fire
##########################################################
fire_name <- "thomas-rye-creek_fire"
sentinel_pic <- "/003012"

pre_fire_pic1 <- "20171130"
post_fire_pic1 <- "20180203"
pre_fire_pic2 <- "20171130"
post_fire_pic2 <- "20180218"

fire_start <- "2017-12-04"
fire_end <- "2018-01-12"
# calculate new and overwrite?
overwrite_NOAA <- TRUE
overwrite_burned_areaSHP <- TRUE

source(file = "C:/Users/Manuel/Dropbox/MDU_2_r-script.R")
