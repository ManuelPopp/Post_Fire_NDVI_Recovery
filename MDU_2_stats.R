packages <- c("ggplot2", "lubridate", "rgdal", "svMisc", "stringr", "nlstools", "nls2", "robustbase")
for(i in 1:NROW(packages)){
  if (!require(packages[i], character.only= TRUE)) {
    install.packages(packages[i])
    library(packages[i], character.only= TRUE)
  }
}
theme_set(theme_classic())
### SETTINGS
# calculate w/ new data and overwrite existing pointData csv table
calculate_new <- TRUE
# export plots as pdf files
export_pdf <- TRUE
# remove most likely unburned pixels
rmUNBURNED <- TRUE
# set observation time (spring = May, June, July; summer = August, September)
obstime <- "summer"

#############################################
##### Set directories
#############################################
out_dirx <- paste(wd, "/output", sep = "")
dir.create(paste(out_dirx, "/", fire_name, sep = ""))
dir.create(paste(out_dirx, "/", fire_name, "/figs/", sep = ""))

#############################################
##### Load data
#############################################
if(exists("rm(already_ran)")){rm(already_ran)}
if(FALSE){
  points <- readOGR(
    dsn = paste(out_dirx, "/", fire_name, "/points/Points.shp", sep = ""), layer = "Points")
}

if(!file.exists(paste(out_dirx, "/", fire_name, "/points/Points_extract.csv", sep = ""))|calculate_new){
pointData <- read.csv2(
  paste(out_dirx, "/", fire_name, "/points/Points.csv", sep = ""), sep = ";"
)
if(names(pointData)[1] == "X"){
  pointData <- pointData[,-1]
}
pointData$CCAP_2010 <- as.character(pointData$CCAP_2010)
pointData$CCAP_2010 <- as.factor(pointData$CCAP_2010)
# remove NA values, areas w/ uncertain fire damage, areas w/out classified vegetation, water pixels
pointData <- pointData[!is.na(pointData$burned),]
pointData <- pointData[pointData$burned != 0,]
#pointData <- pointData[pointData$CCAP_2010 != 0,]
#pointData <- pointData[pointData$CCAP_2010 != 21,]
#pointData[which(is.na(pointData[,3])),3] <- FALSE
pointData <- pointData[!is.na(pointData$CCAP_2010),]
pointData$burned[which(pointData$burned==-1)] <- 0
pointData$burned <- as.logical(pointData$burned)
# remove factor levels which no longer occur
pointData$CCAP_2010 <- as.character(pointData$CCAP_2010)
pointData$CCAP_2010 <- as.factor((pointData$CCAP_2010))
# remove unnatural landcover classes
NatPoint <- pointData[-which(as.character(pointData$CCAP_2010) %in% c("0", "1", "2", "3", "4", "5", "6", "NA")),]
#NatPoint <- NatPoint[-which(as.numeric(NatPoint$CCAP_2010) > 18),]
NatPoint$CCAP_2010 <- as.character(NatPoint$CCAP_2010)
NatPoint$CCAP_2010 <- as.factor(NatPoint$CCAP_2010)
pointData <- NatPoint
rm(NatPoint)
# write data as csv2
write.csv2(pointData,
  paste(out_dirx, "/", fire_name, "/points/Points_extract.csv", sep = ""), row.names = FALSE)
}else{
  pointData <- read.csv2(paste(out_dirx, "/", fire_name, "/points/Points_extract.csv", sep = ""))
  pointData$CCAP_2010 <- as.character(pointData$CCAP_2010)
  pointData$CCAP_2010 <- as.factor(pointData$CCAP_2010)
}

dates <- as.Date(sub("([[:digit:]]{2,2})$", "-\\1",
             sub("([[:digit:]]{2,4})$", "-\\1",
                 gsub("d", "", colnames(pointData)[
                   grepl("^[[:digit:]]", substr(colnames(pointData), 2, 2))
                   ])))
)

# remove points that were most likely not affected by the respective fire
if(rmUNBURNED){
StartDate <- dates[which(dates > fire_start)[1] + which(substr(names(pointData), 1, 1) == "d")[1]-1]
postfire_years <- as.numeric(which(dates > StartDate))+as.numeric(which(substr(names(pointData), 1, 1) == "d")[1]-1)
prefire_years <- which(dates < StartDate)+which(substr(names(pointData), 1, 1) == "d")[1]-1
dat <- pointData[which(pointData$burned == TRUE),]
dat$postmean <- apply(dat[, postfire_years[c(1:5)]], 1, FUN = median, na.rm = TRUE)
dat$premean <- apply(dat[, prefire_years[NROW(prefire_years)-5:NROW(prefire_years)]], 1, FUN = median, na.rm = TRUE)
falseIDs <- dat[which(dat$premean <= dat$postmean+0.1), 1]
pointData <- pointData[-which(pointData$id %in% falseIDs),]
rm(dat)
rmUNBURNED <- FALSE
}

# calculate models
pointData$A <- rep(NA, nrow(pointData))
pointData$phi <- rep(NA, nrow(pointData))
pointData$C <- rep(NA, nrow(pointData))

Nday <- as.numeric(dates-ymd(paste(year(dates), "-01-01", sep = "")))
cols <- which(substr(names(pointData), 1, 1) == "d")
mean <- apply(pointData[,cols], 1, FUN = mean)
pointData <- pointData[-which(mean < 0.15),]
Start <- data.frame(A = c(0, 0.9), phi = c(20, 340), C = c(0.01, 0.95))

Sys.time()
for(i in 1:nrow(pointData)){
  series <- data.frame(NDVI = t(pointData[i, cols]), nday = Nday)
  names(series) <- c("NDVI", "nday")
  na_rows <- which(is.na(series$NDVI))
  if(length(na_rows)>0){
    series <- series[-na_rows,]
  }
  mod <- nls2(NDVI ~ A*sin((2*pi)/365.25*(nday+phi))+C, data = series, start = Start, algorithm = "random-search")
  pointData$A[i] <- coef(mod)[1]
  pointData$phi[i] <- coef(mod)[2]
  pointData$C[i] <- coef(mod)[3]
  progress(i, progress.bar = TRUE, max.value = nrow(pointData))
}
rm(i)

# select months to observe
if(obstime == "spring"){
  months <- c(
    5, 6, 7
  )
}else if(obstime == "summer"){
  months <- c(
    8, 9
  )
}

Dates <- dates[month(dates) %in% month(as.Date(paste("1900-0", months, "-01", sep = "")))]
# export dates to list them in the appendix
Texport <- TRUE
if(Texport){
library(xtable)
  hhh <- as.factor(substr(LS_pic, 2, 4))
  vvv <- as.factor(substr(LS_pic, 5, 7))
  TGR <- "CONUS"
  collect <- "U.S. Landsat ARD"
tiles <- data.frame(
  Date = as.character(Dates), Horizontal = rep(hhh, NROW(Dates)), Vertical = rep(vvv, NROW(Dates)),
                    TileGridRegion = rep(TGR, NROW(Dates)), Collection = rep(collect, NROW(Dates))
  )
tiles$Date <- as.Date(tiles$Date)
names(tiles)[4] <- "Tile Grid Region"
for(i in 1:nrow(tiles)){
  if(tiles[i, 1] < as.Date("2013-02-11")){
    tiles[i, 6] <- "LS05"
    tiles[i, 7] <- "TM"
  }else if(tiles$Date[i] > as.Date("1984-03-05")){
    tiles[i, 6] <- "LS08"
    tiles[i, 7] <- "OLI"
  }else{
    tiles[i, 6] <- "LS04"
    tiles[i, 7] <- "TM"
  }
}
names(tiles)[6] <- "Platform"
names(tiles)[7] <- "Sensor"
fncap <- function(x){
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}
tiles$Date <- as.character(tiles$Date)
print(xtable(tiles, type = "latex", caption =
               paste("Landsat data used for the calculation of the \\ac{NDVI} in the region affected by the ",
                     sub("_", " ", fncap(fire_name)), ". Landsat \\ac{ARD} tiles are referenced by their horizontal and vertical tile coordinates (as opposed to the path and row system); for further information see \\citet{dwyerAnalysisReadyData2018a}.", sep = ""),
             label = paste("tab:Data", fire_name, sep = "")), file = paste(db, "/MDU2_paper/LStiles", obstime, ".tex", sep = ""), include.rownames = FALSE,
      booktabs = TRUE, tabular.environment = "longtable", floating = FALSE, caption.placement = "top")
}

PointData <- pointData[,
                       c(rep(TRUE, which(substr(names(pointData), 1, 1) == "d")[1]-1), #sum(!grepl("^[[:digit:]]", substr(colnames(pointData), 2, 2)), na.rm = TRUE)
                          month(dates) %in% month(as.Date(paste("1900-0", months, "-01", sep = ""))),
                          rep(TRUE, 1+ncol(pointData)-which(substr(names(pointData), 1, 1) != "d")[which(substr(names(pointData), 1, 1) == "d")[1]])
                         )
                       ]
PointData$CCAP_2010 <- as.factor(PointData$CCAP_2010)
rm(pointData)

write.csv2(PointData, file = paste(
  out_dirx, "/", fire_name, "/Points_SinMod", obstime, ".csv", sep = ""
), row.names = FALSE)
if(FALSE){
  PointData <- read.csv2(paste(out_dirx, "/", fire_name, "/Points_SinMod.csv", sep = ""))
  PointData$CCAP_2010 <- as.character(PointData$CCAP_2010)
  PointData$CCAP_2010 <- as.factor(PointData$CCAP_2010)
}

# Translate from LS8 NDVI to LS5 NDVI
LS8cols <- which(suppressWarnings(as.numeric(str_sub(names(PointData), -6, -5)))>=13)
# transformation factors: NDVI LS5=((LS8-a)/b-c)d
a <- 0.037771
b <- 1.039615
c <- -0.030966
d <- 1.067218
PointData[, LS8cols] <- (PointData[, LS8cols]/(b*d))-(a+b*c)/(b*d)
write.csv2(PointData, file = paste(wd, "/output/", fire_name, "/points/Points_corrected.csv", sep = ""), col.names = FALSE)
if(FALSE){
  PointData <- read.csv2(paste(wd, "/output/", fire_name, "/points/Points_corrected.csv", sep = ""))
  PointData$CCAP_2010 <- as.character(PointData$CCAP_2010)
  PointData$CCAP_2010 <- as.factor(PointData$CCAP_2010)
}

# Limit the number of observations to 10000 randomly selected pixels for each class in both burned and non burned areas
if(!exists("already_ran")){
  already_ran <- TRUE
  dir.create(paste(out_dirx, "/", fire_name, "/text/", sep = ""))
  sink(file = paste(out_dirx, "/", fire_name, "/text/SampleSize_info_", obstime, ".txt", sep = ""))
  for(j in 1:length(levels(PointData$CCAP_2010))){
    if(NROW(which(PointData$CCAP_2010 == levels(PointData$CCAP_2010)[j] & PointData$burned == TRUE)) >= 10000){
      PointData <- PointData[-sample(which(PointData$CCAP_2010 == levels(PointData$CCAP_2010)[j] & PointData$burned == TRUE),
                                     size = (NROW(which(PointData$CCAP_2010 == levels(PointData$CCAP_2010)[j] & PointData$burned == TRUE))-10000), # remove all but 10000 samples for each class
                                     replace = FALSE, prob = NULL),]
      cat(paste("Burned sample size for class ", levels(PointData$CCAP_2010)[j], " is 10000.\n", sep = ""))
    }else{
      cat(paste("Burned sample size for class ", levels(PointData$CCAP_2010)[j], " is ",
                NROW(which(PointData$CCAP_2010 == levels(PointData$CCAP_2010)[j] & PointData$burned == TRUE)), " <10000.\n", sep = ""))
    }
    if(NROW(which(PointData$CCAP_2010 == levels(PointData$CCAP_2010)[j] & PointData$burned == FALSE)) >= 10000){
      PointData <- PointData[-sample(which(PointData$CCAP_2010 == levels(PointData$CCAP_2010)[j] & PointData$burned == FALSE),
                                     size = (NROW(which(PointData$CCAP_2010 == levels(PointData$CCAP_2010)[j] & PointData$burned == FALSE))-10000), # remove all but as much samples as there are for the respective burned area
                                     replace = FALSE, prob = NULL),]
    }else{
      cat(paste("Nonburned sample size for class ", levels(PointData$CCAP_2010)[j], " is ",
                NROW(which(PointData$CCAP_2010 == levels(PointData$CCAP_2010)[j] & PointData$burned == FALSE)), " <10000.\n", sep = ""))
    }
  }
  sink()
  dir.create(paste(out_dirx, "/", fire_name, "/points/", obstime, "/", sep = ""))
  write.csv2(PointData, file = paste(out_dirx, "/", fire_name, "/points/", obstime, "/PointExtr.csv", sep = ""), row.names = FALSE)
}else{
  PointData <- read.csv2(paste(out_dirx, "/", fire_name, "/points/", obstime, "/PointExtr.csv", sep = ""))
  PointData$CCAP_2010 <- as.character(PointData$CCAP_2010)
  PointData$CCAP_2010 <- as.factor(PointData$CCAP_2010)
}

few_data <- levels(PointData$CCAP_2010)[which(summary(PointData$CCAP_2010[PointData$burned == TRUE]) < 60)]
PointData <- PointData[-which(as.numeric(as.character(PointData$CCAP_2010)) %in% as.numeric(few_data)),]
PointData$CCAP_2010 <- as.character(PointData$CCAP_2010)
PointData$CCAP_2010 <- as.factor(PointData$CCAP_2010)

# Fit NDVI values to get approximations for June, 15th/ August, 31st
if(obstime == "spring"){
  approxMonth <- 166.25
}else if(obstime == "summer"){
  approxMonth <- 243.25
}
pData <- PointData[, which(substr(names(PointData), 1, 1) != "d")]
years <- unique(year(Dates))
skip <- ncol(pData)
for(yr in 1:NROW(years)){
  for(n in 1:nrow(pData)){
    ndvi <- PointData[n, which(substr(names(PointData), 1, 1) == "d")][which(year(Dates) == years[yr])]
    n_day <- as.numeric(Dates[which(year(Dates) == years[yr])]-ymd(paste(years[yr], "-01-01", sep = "")))
    Data <- data.frame(x = t(ndvi), y = n_day)
    names(Data) <- c("NDVI", "Nday")
    if(length(which(is.na(Data$NDVI))) >= 1){
      Data <- Data[-which(is.na(Data$NDVI)),]
    }
    if(nrow(Data) > 1){
      a <- PointData$A[n]
      b <- PointData$phi[n]
      start <- data.frame(Val = c(min(Data$NDVI), max(Data$NDVI)))
      curr <- nls2(NDVI ~ a*sin((2*pi)/365.25*(n_day+b))+Val, data = Data,
                   start = start, control = list(maxiter = 500), algorithm = "random-search")
      v <- coef(curr)
      pData[n, yr+skip] <- PointData$A[n]*sin((2*pi)/365.25*(approxMonth+PointData$phi[n]))+v
      names(pData)[yr+skip] <- paste("d", years[yr], sep = "")
    }else if(nrow(Data) == 1){
      #ndvi <- PointData[n, which(substr(names(PointData), 1, 1) == "d")][which(year(Dates) == years[yr])]
      #n_day <- as.numeric(Dates[which(year(Dates) == years[yr])]-ymd(paste(years[yr], "-01-01", sep = "")))
      #Data <- data.frame(x = t(ndvi), y = n_day)
      #names(Data) <- c("NDVI", "Nday")
      atCurr <- PointData$A[n]*sin((2*pi)/365.25*(n_day+PointData$phi[n]))+PointData$C[n]
      pData[n, yr+skip] <- PointData$A[n]*sin((2*pi)/365.25*(approxMonth+PointData$phi[n]))+PointData$C[n]+Data$NDVI-atCurr
    }
  }
  progress(yr, max.value = NROW(years), progress.bar = TRUE)
}
names(pData)[-seq(1, skip)] <- paste("d", years, sep = "")
write.csv2(pData, file = paste(out_dirx, "/", fire_name, "/points/", obstime, "/pData.csv", sep = ""), row.names = FALSE)
PointData <- pData
rm(pData)

# NDVI as percentage of max NDVI
if(TRUE){
  cols <- c(which(substr(colnames(PointData), 1, 1)=="d")#, which(names(PointData) == "A"), which(names(PointData) == "C")
            )
  for(i in 1:nrow(PointData)){
    PointData[i,cols] <- PointData[i,cols]*100/max(
      PointData[i,cols])
    progress(i, progress.bar = TRUE, max.value = nrow(PointData))
  }
  ylab <- "NDVI as percentage of max NDVI"
  write.csv2(PointData, file = paste(out_dirx, "/", fire_name, "/points/", obstime, "/Points_percent.csv", sep = ""), row.names = FALSE)
}else{
  PointData <- read.csv2(paste(out_dirx, "/", fire_name, "/points/", obstime, "/Points_percent.csv", sep = ""))
  PointData$CCAP_2010 <- as.character(PointData$CCAP_2010)
  PointData$CCAP_2010 <- as.factor(PointData$CCAP_2010)
}

#for(j in which(substr(colnames(PointData), 1, 1)=="d")){
#PointData[which(PointData[, j] < 0), j] #<- NA
#}

#############################################
##### Visualization
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

# Get data in a data.frame called Data with the columns: NDVI, nday
#preburn <- pData[,c(which(substr(names(pData), 1, 1) != "d"), which(years >= year(fire_start))+which(
#  substr(names(pData), 1, 1) != "d")[length(which(substr(names(pData), 1, 1) != "d"))]
#  )]

pData <- PointData
dates <- substr(names(pData)[which(substr(names(pData), 1, 1) == "d")], 2, 5)
id <- base::rep(pData[, which(names(pData) == "id")],
                each = NROW(dates))
d <- base::rep(dates, nrow(pData))
b <- base::rep(pData[, which(names(pData) == "burned")],
               each = NROW(dates))
c <- base::rep(pData[, which(names(pData) == "CCAP_2010")],
               each = NROW(dates))
ndvi <- as.vector(
  t(pData[, which(substr(colnames(pData), 1, 1) == "d")])
)
Data <- data.frame(id = id, year = d, burned = b, class = c, NDVI = ndvi)
if(length(which(is.na(Data$NDVI)))>=1){
  Data <- Data[-which(is.na(Data$NDVI)),]
}
if(obstime == "spring"){
  Data$date <- as.Date(paste(Data$year, "-06-15", sep = ""))
}else if(obstime == "summer"){
  Data$date <- as.Date(paste(Data$year, "-08-31", sep = ""))
}
write.csv2(Data, file = paste(out_dirx, "/", fire_name, "/points/", obstime, "/DatFinal.csv", sep = ""), row.names = FALSE)

B <- TRUE
for(i in 1:NROW(levels(as.factor(pData$CCAP_2010)))){
cl <- classNames[as.numeric(as.character(levels(PointData$CCAP_2010)))+1, 2][i]
dat <- Data[which(as.numeric(Data$class) == i),]
dat <- dat[which(dat$burned == B),]
n <- length(unique(dat$id))

# create ggplot
if(B){
# create model
  pre <- data.frame(NDVI = Data$NDVI[which(Data$date < fire_start)], date = Data$date[which(Data$date < fire_start)])
#  post <- data.frame(NDVI = Data$NDVI[which(Data$date >= fire_end)], date = Data$date[which(Data$date >= fire_end)])
#  for(i in 1:NROW(levels(as.factor(pre$date)))){
#    outl_min <- boxplot.stats(pre$NDVI[which(pre$date == as.Date(levels(as.factor(pre$date))[i]))])$stats[1]
#    outl_max <- boxplot.stats(pre$NDVI[which(pre$date == as.Date(levels(as.factor(pre$date))[i]))])$stats[5]
#    pre <- pre[-which(pre$date == as.Date(levels(as.factor(pre$date))[i]) & pre$NDVI < outl_min),]
#  }
premean <- mean(pre$NDVI)
#for(i in 1:NROW(levels(as.factor(post$date)))){
#  outl_min <- boxplot.stats(post$NDVI[which(post$date == as.Date(levels(as.factor(post$date))[i]))])$stats[1]
#  outl_max <- boxplot.stats(post$NDVI[which(post$date == as.Date(levels(as.factor(post$date))[i]))])$stats[5]
#  post <- post[-which(post$date == as.Date(levels(as.factor(post$date))[i]) & post$NDVI < outl_min),]
#}
#mod <- ltsReg(post$NDVI ~ post$date)
# plot
gg <- ggplot(data = dat, aes(x = date, y = NDVI, fill = date>fire_start)) +
  geom_hline(yintercept = premean, colour = "lightgray", linetype = "dashed", size = 0.75) +
#  geom_abline(slope = mod$coefficients[2], intercept = mod$coefficients[1], colour = "darkgray", linetype = "dashed", size = 0.75) +
  stat_boxplot(aes(group = year), geom ='errorbar') +
  geom_boxplot(aes(group = year), outlier.color = "darkgray", outlier.size = 0.5, outlier.alpha = 0.3, show.legend = FALSE) +
  ylim(0, 100) +
  ylab("Percent of max NDVI") +
  xlab(element_blank()) +
  annotate(geom = "text", label = paste(" n=", n, sep = ""), x = min(Data$date), y = 1, hjust = 0, size = 5) +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1),
        axis.text = element_text(size = 12), axis.title = element_text(size = 14)) +
  scale_fill_manual(values = c("lightgray", "darkgray"))
dir.create(paste(out_dirx, "/", fire_name, "/text/", obstime, "/", sep = ""))
sink(file = paste(out_dirx, "/", fire_name, "/text/", obstime, "/", cl, "_", B, "_info.txt", sep = ""))
cat("Mean NDVI in the years before the fire: ", premean, ".\n", "Model parameters for NDVI development:", "\n", sep = "")
#mod
sink()
}else if(!B){
  gg <- ggplot(data = dat, aes(x = date, y = NDVI)) +
    stat_boxplot(aes(group = year), geom ='errorbar') +
    geom_boxplot(aes(group = year), outlier.color = "darkgray", outlier.size = 0.5, outlier.alpha = 0.3, show.legend = FALSE,
                 fill = "lightgray") +
    ylim(0, 100) +
    ylab("Percent of max NDVI") +
    xlab(element_blank()) +
    annotate(geom = "text", label = paste(" n=", n, sep = ""), x = min(Data$date), y = 1, hjust = 0, size = 5) +
    theme(panel.border = element_rect(colour = "black", fill = NA, size = 1),
          axis.text = element_text(size = 12), axis.title = element_text(size = 14))
}

dir.create(paste(out_dirx, "/", fire_name, "/figs/", obstime, "/", sep = ""))
if(export_pdf){
  pdf(file = paste(
    paste(out_dirx, "/", fire_name, "/figs/", obstime, "/", cl, B, ".pdf", sep = "")
  ), width = 6, height = 4)
print(
  gg
)
dev.off()
}
}
