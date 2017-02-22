# Summarize.R
# Author: Sergio Diaz-del-Pino sergiodiazdp(at)gmail.com
# Description: R Script for trim, align and generate files for later combine them. It also counts inferred, replacable, nframes... of a 
# front_file: Kinect skeleton data from reference camera
# side_file: Kinect skeleton data from side camera

#Parameters
body_point <- 19

# Set path to files and working directory
front_file <- scan("/Users/Sergio/Dropbox/Trabajo/Deep Learning/R/Ficheros/90_oscar_60_B.csv", skip=1, sep="\n", what = character())
side_file <- scan("/Users/Sergio/Dropbox/Trabajo/Deep Learning/R/Ficheros/WalkingOscarRotatedBA.csv", skip=1, sep="\n", what = character())
setwd("/Users/Sergio/Documents/BitlabProjects/KineetR")

if(!exists("foo", mode="function")) source("Mast.R")

dataset <- array(list(),dim=c(2,3))
rownames(dataset) <- c("front", "side")
colnames(dataset) <- c("frames","inferred","replaceable")

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

# Setting up body point columns
body_point_X <- body_point*4+3
body_point_Y <- body_point_X+1
body_point_Z <- body_point_Y+1

#Trimming side_matrix
side_matrix <- side_matrix[20:(side_file_rows-20),]

#Aligning both files
side_view <- sqrt(side_matrix[, body_point_X]^2 + side_matrix[, body_point_Y]^2 + side_matrix[, body_point_Z]^2)
front_view <- sqrt(front_matrix[, body_point_X]^2 + front_matrix[, body_point_Y]^2 + front_matrix[, body_point_Z]^2)
align_position <- alignFiles(front_view, side_view)

#Trimming front_matrix. Now both have the same lenght.
front_matrix <- front_matrix[(align_position):(length(side_view)+align_position-1),]

#Preparing Dataset
dataset[["front", "inferred"]] <- 0
dataset[["side", "inferred"]] <- 0
dataset[["front", "replaceable"]] <- 0
dataset[["side", "replaceable"]] <- 0
both_inferred <- 0
nFrames <- 0
positions_front <- c()
positions_side <- c()

for(i in 1:nrow(front_matrix)){
  for(j in seq(from=2, to=ncol(front_matrix), by=4)){
    #Front
    if(front_matrix[i,j] == 1){
      dataset[["front", "inferred"]]  <- dataset[["front", "inferred"]]  + 1
      positions_front <- append(positions_front, i)
    }
    
    #Side
    if(side_matrix[i,j] == 1){
      dataset[["side", "inferred"]] <- dataset[["side", "inferred"]] + 1
      positions_side <- append(positions_side, i)
    }
    
    if((side_matrix[i,j] == 1) && (front_matrix[i,j] == 0)){
      dataset[["side", "replaceable"]] <- dataset[["side", "replaceable"]] + 1
    }
    
    if((side_matrix[i,j] == 0) && (front_matrix[i,j] == 1)){
      dataset[["front", "replaceable"]] <- dataset[["front", "replaceable"]] + 1
    }
    
    if((side_matrix[i,j] == 1) && (front_matrix[i,j] == 1)){
      both_inferred <- both_inferred + 1
    }
    
    nFrames <- nFrames + 1;
  }
}

dataset[["front", "frames"]]  <- nFrames
dataset[["side", "frames"]]  <- nFrames

colnames(side_matrix) <- c("tiempo","esq0Inf","esq0X","esq0Y","esq0Z","esq1Inf","esq1X","esq1Y","esq1Z","esq2Inf","esq2X","esq2Y","esq2Z","esq3Inf","esq3X","esq3Y","esq3Z", "esq4Inf","esq4X","esq4Y","esq4Z","esq5Inf","esq5X","esq5Y","esq5Z","esq6Inf","esq6X","esq6Y","esq6Z","esq7Inf","esq7X","esq7Y","esq7Z","esq8Inf","esq8X","esq8Y","esq8Z","esq9Inf","esq9X","esq9Y","esq9Z","esq10Inf","esq10X","esq10Y","esq10Z","esq11Inf","esq11X","esq11Y","esq11Z","esq12Inf","esq12X","esq12Y","esq12Z","esq13Inf","esq13X","esq13Y","esq13Z","esq14Inf","esq14X","esq14Y","esq14Z","esq15Inf","esq15X","esq15Y","esq15Z","esq16Inf","esq16X","esq16Y","esq16Z","esq17Inf","esq17X","esq17Y","esq17Z","esq18Inf","esq18X","esq18Y","esq18Z","esq19Inf","esq19X","esq19Y","esq19Z","esq20Inf","esq20X","esq20Y","esq20Z","esq21Inf","esq21X","esq21Y","esq21Z","esq22Inf","esq22X","esq22Y","esq22Z","esq23Inf","esq23X","esq23Y","esq23Z","esq24Inf", "esq24X","esq24Y","esq24Z","esqRefInf","esqRefX","esqRefY","esqRefZ")
colnames(front_matrix) <- c("tiempo","esq0Inf","esq0X","esq0Y","esq0Z","esq1Inf","esq1X","esq1Y","esq1Z","esq2Inf","esq2X","esq2Y","esq2Z","esq3Inf","esq3X","esq3Y","esq3Z", "esq4Inf","esq4X","esq4Y","esq4Z","esq5Inf","esq5X","esq5Y","esq5Z","esq6Inf","esq6X","esq6Y","esq6Z","esq7Inf","esq7X","esq7Y","esq7Z","esq8Inf","esq8X","esq8Y","esq8Z","esq9Inf","esq9X","esq9Y","esq9Z","esq10Inf","esq10X","esq10Y","esq10Z","esq11Inf","esq11X","esq11Y","esq11Z","esq12Inf","esq12X","esq12Y","esq12Z","esq13Inf","esq13X","esq13Y","esq13Z","esq14Inf","esq14X","esq14Y","esq14Z","esq15Inf","esq15X","esq15Y","esq15Z","esq16Inf","esq16X","esq16Y","esq16Z","esq17Inf","esq17X","esq17Y","esq17Z","esq18Inf","esq18X","esq18Y","esq18Z","esq19Inf","esq19X","esq19Y","esq19Z","esq20Inf","esq20X","esq20Y","esq20Z","esq21Inf","esq21X","esq21Y","esq21Z","esq22Inf","esq22X","esq22Y","esq22Z","esq23Inf","esq23X","esq23Y","esq23Z","esq24Inf", "esq24X","esq24Y","esq24Z","esqRefInf","esqRefX","esqRefY","esqRefZ")

#Writting output files
write.table(front_matrix, file = "FrontFile.csv",row.names=FALSE, na="",col.names=TRUE, sep=";")
write.table(side_matrix, file = "RotatedAlignedSidefile.csv",row.names=FALSE, na="",col.names=TRUE, sep=";")

