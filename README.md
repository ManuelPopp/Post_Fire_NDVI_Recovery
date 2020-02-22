# Post_Fire_NDVI_Recovery
This repository contains three R scripts:
1) MDU2_Proj.R where input information specific to different wildfires is provided and from which the other scripts can be ran automatically.
2) MDU_2_r-script_new.R which is the main R script for the raster calculations. The data is processed to a point shapefile and exported as a .csv table
3) MDU_2_stats.R which serves for the statistical calculations and visualization of the data.
4) NDVI_intercalibration.R which was used for the intercalibration of Landsat 8 and Landsat 5 derived NDVI.

In order to run the scripts, the following data has to be provided in a specific file structure:
wd = Working directory
wd/Landsat -> Directory housing the Landsat data. Names of folders have to be provided in MDU2_Proj.R in the LS_pic variable, naming the folder that contains the tile required for processing data for the respective wildfire.
/Landsat/*fire name*/shapefile/ -> A shapefile of roughly the area of the fire. *fire name* is the name of the fire e.g. station_fire as given in the MDU2_Proj.R script.
The directories
wd/QGIS/NOAA/2001/
wd/QGIS/NOAA/2006/
wd/QGIS/NOAA/2010/
contain the NOAA C-CAP landcover data for the respective dates for the complete U.S. west coast downloadable on https://coast.noaa.gov/dataviewer/#/landcover/search/

Note that the processing of the data might demand a considerable amount of time. Calculations could take more than 24hrs on an average personal computer.
If your system cannot provide more than 30GB RAM, the scripts should not be run from the main script directly (via source()) but the scripts should be run manually. In between, various save points are provided, where data is saved on the hard drive and can be loaded later on without re-running the calculations entirely.
