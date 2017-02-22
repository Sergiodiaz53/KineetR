# Calibrato.R
# Author: Sergio Diaz-del-Pino sergiodiazdp(at)gmail.com
# Description: R script to rotate a skeleton points Kinect file (A) to the coordinate space of (B) using the Kabsch Algorithm
# front_file: Kinect skeleton data from reference camera
# side_file: Kinect skeleton data from side camera

# Set path to files and working directory
front_file <- scan("/Users/Sergio/Dropbox/Trabajo/Deep Learning/R/Ficheros/90_oscar_60_B.csv", skip=1, sep="\n", what = character())
side_file <- scan("/Users/Sergio/Dropbox/Trabajo/Deep Learning/R/Ficheros/90_oscar_60_A.csv", skip=1, sep="\n", what = character())
setwd("/Users/Sergio/Dropbox/Trabajo/Deep Learning/R/Ficheros/")

library(scatterplot3d)
library(pracma)
if(!exists("foo", mode="function")) source("Mast.R")

# Read file from front view (front_matrix)
front_file_rows <- length(front_file)
front_file_cols <- length(strsplit(front_file[[1]], split = ";")[[1]])
front_matrix <- array(NA, dim = c(front_file_rows, front_file_cols))

for(i in 1:front_file_rows){
  each_field <- strsplit(front_file[[i]], split = ";")[[1]]
  for(j in 2:front_file_cols){
    front_matrix[i,j] <- as.numeric(each_field[[j]][1])
  }
}

# Read file from side view (side_matrix)
side_file_rows <- length(side_file)
side_file_cols <- length(strsplit(side_file[[1]], split = ";")[[1]])
side_matrix <- array(NA, dim = c(side_file_rows, side_file_cols))

for(i in 1:side_file_rows){
  each_field <- strsplit(side_file[[i]], split = ";")[[1]]
  for(j in 2:side_file_cols){
    side_matrix[i,j] <- as.numeric(each_field[[j]][1])
  }
}

# Preparing output file
final_file <- front_matrix
colnames(final_file) <- c("tiempo","esq0Inf","esq0X","esq0Y","esq0Z","esq1Inf","esq1X","esq1Y","esq1Z","esq2Inf","esq2X","esq2Y","esq2Z","esq3Inf","esq3X","esq3Y","esq3Z", "esq4Inf","esq4X","esq4Y","esq4Z","esq5Inf","esq5X","esq5Y","esq5Z","esq6Inf","esq6X","esq6Y","esq6Z","esq7Inf","esq7X","esq7Y","esq7Z","esq8Inf","esq8X","esq8Y","esq8Z","esq9Inf","esq9X","esq9Y","esq9Z","esq10Inf","esq10X","esq10Y","esq10Z","esq11Inf","esq11X","esq11Y","esq11Z","esq12Inf","esq12X","esq12Y","esq12Z","esq13Inf","esq13X","esq13Y","esq13Z","esq14Inf","esq14X","esq14Y","esq14Z","esq15Inf","esq15X","esq15Y","esq15Z","esq16Inf","esq16X","esq16Y","esq16Z","esq17Inf","esq17X","esq17Y","esq17Z","esq18Inf","esq18X","esq18Y","esq18Z","esq19Inf","esq19X","esq19Y","esq19Z","esq20Inf","esq20X","esq20Y","esq20Z","esq21Inf","esq21X","esq21Y","esq21Z","esq22Inf","esq22X","esq22Y","esq22Z","esq23Inf","esq23X","esq23Y","esq23Z","esq24Inf", "esq24X","esq24Y","esq24Z","esqRefInf","esqRefX","esqRefY","esqRefZ")

# Rotating A to B using the Kabsch Algorithm
for(j in 0:24){
  body_pointX <- j*4+3
  body_pointZ <- body_pointX+2
  
  points_cameraA <- array(c(side_matrix[1:front_file_rows,body_pointX:body_pointZ]), dim = c(front_file_rows,3))
  points_cameraB <- array(c(front_matrix[1:front_file_rows,body_pointX:body_pointZ]), dim = c(front_file_rows,3))
  
  k_result <-procrustes(points_cameraB, points_cameraA)
  final_file[,body_pointX:body_pointZ] <- k_result$P
}

# Writing outputfile in working directory
write.table(final_file, file = "WalkingOscarRotatedBA.csv",row.names=FALSE, na="",col.names=TRUE, sep=";")

