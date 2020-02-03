if(Sys.info()['sysname'] == "Windows"){
  # paths Win
  r_script <- "C:/Users/Manuel/Dropbox/MDU_2_r-script_new.R"
}else if(Sys.info()['sysname'] == "Linux"){
  # paths Lin
  r_script <- "/home/manuel/Dropbox/MDU_2_r-script_new.R"
}else{
  print("Error: OS not identified.")
}

##########################################################
### Station fire
##########################################################
fire_name <- "station_fire"
LS_pic <- "/003012"

pre_fire_pic1 <- "20090806"
post_fire_pic1 <- "20091025"
pre_fire_pic2 <- "20090721"
post_fire_pic2 <- "20091110"

fire_start <- "2009-08-26"
fire_end <- "2009-10-16"
source(file = r_script)

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
source(file = r_script)

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
source(file = "C:/Users/Manuel/Dropbox/MDU_2_r-script.R")