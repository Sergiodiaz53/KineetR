# PlotDistance.R
# Author: Sergio Diaz-del-Pino sergiodiazdp(at)gmail.com
# Description: R script to align two Kinect skeleton points files and paint the distance between points
# front_file: Kinect skeleton data from reference camera
# side_file: Kinect skeleton data from side camera

library(Metrics)
if(!exists("foo", mode="function")) source("Mast.R")

# Set path to files and working directory
front_file <- scan("/Users/Sergio/Dropbox/Trabajo/Deep Learning/R/Ficheros/90_oscar_60_B.csv", skip=1, sep="\n", what = character())
side_file <- scan("/Users/Sergio/Dropbox/Trabajo/Deep Learning/R/Ficheros/WalkingOscarRotatedBA.csv", skip=1, sep="\n", what = character())
setwd("/Users/Sergio/Documents/BitlabProjects/")

# Parameters #
# @Body_point: Body point to do the alignment
# @smoothing: If TRUE the final graph is smoothed
# @type: "original" paints the original data, "zscore" paint the zscored data
# @window_size: Only if @smoothing: TRUE. It is the size of the window to do the smooth
# @max_std: max standard deviation
# @align: TRUE if the files should be aligned
# @outliers_lines: If TRUE it paints the mean as a green line and other two lines with mean + 3SD and mean - 3SD.
# @side: TRUE if you want that both files are represented
body_point <- 19; smoothing <- FALSE; type <- "original"; window_size <- 3; max_std <- 100; align <- TRUE; outliers_lines <- TRUE; side <- TRUE

#Vector with points names
POINT_NAMES <- c("Spine mid", "Neck", "Head", "LShouder", "LElbow", "LWrist", "LHand", "RShoulder", "RElbow", "RWrist", "RHand", "LHip", 
                  "LKnee", "LAnkle", "LFoot", "RHip", "RKnee", "RAnkle", "RFoot", "Spine Shoulder", "LHand tip", "LThumb", "RHand tip", "RThumb")

# Setting up body point columns
body_point_X <- body_point*4+3
body_point_Y <- body_point_X+1
body_point_Z <- body_point_Y+1

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

# Adjust number of frames
n_frames_front = front_file_rows
n_frames_side = side_file_rows

#Preparing the full dataset
dataset <- array(list(),dim=c(3,2))
rownames(dataset) <- c("original","zscore","smoothed")
colnames(dataset) <- c("front", "side")

## Original & zscores calculation##
#  Front
dataset[["original", "front"]] <- sqrt(front_matrix[, body_point_X]^2 + front_matrix[, body_point_Y]^2 + front_matrix[, body_point_Z]^2)
for(i in 1:front_file_rows){
  dataset[["zscore", "front"]][i] <- zscore(dataset[["original", "front"]][i], dataset[["original", "front"]])
}

#  Side
dataset[["original", "side"]] <- sqrt(side_matrix[20:(side_file_rows-20), body_point_X]^2 + side_matrix[20:(side_file_rows-20), body_point_Y]^2 + side_matrix[20:(side_file_rows-20), body_point_Z]^2)
for(i in 1:length(dataset[["original","side"]])){
  dataset[["zscore", "side"]][i] <- zscore(dataset[["original", "side"]][i], dataset[["original", "side"]])
}
###########

## Smoothing ##
if(smoothing) {
  #Front
  for(i in 1:front_file_rows){
    dataset[["smoothed", "front"]][i] <- mean(dataset[[type, "front"]][max(1, i-window_size):min(front_file_rows, i+window_size)], na.rm = TRUE)
  }
  #Side
  for(i in 1:length(dataset[[type,"side"]])){
    dataset[["smoothed", "side"]][i] <- mean(dataset[[type, "side"]][max(1, i-window_size):min(front_file_rows, i+window_size)], na.rm = TRUE)
  }
}
###########


## Alignment ##
if(align){
  align_position <- alignFiles(dataset[[type, "front"]], dataset[[type, "side"]])
} else {
  align_position <- 0
}
###########

## Plot them
# Get limits
limit_min <- min(dataset[[type, "front"]], dataset[[type, "side"]])
limit_max <- max(dataset[[type, "front"]], dataset[[type, "side"]])

#Chart and axis names
if(side) side_name <- "Blue:side, Red:Front)" else side_name <- "Red:Front)"
if(smoothing) smoothChart <- "S:True" else smoothChart <- "S:False"
y_name <- paste("Spine Distance (", side_name, sep='')
chart_name <- paste(POINT_NAMES[body_point], "movement", type, smoothChart, sep=' ')

# Init plot
plot(1, type="n", xlim = c(1, n_frames_front), ylim = c(limit_min, limit_max), xlab = "Frames", ylab = y_name, main = chart_name)

# Plot front and side views
last_v_plotted <- NaN
last_p_plotted <- NaN
if(smoothing) type <- "smoothed"
for(i in 1:front_file_rows){
  if(!is.na(dataset[[type, "front"]][i]) && abs(dataset[["zscore", "front"]][i]) < max_std){
    if(!is.na(last_v_plotted)){
      lines(c(last_p_plotted,i), c(last_v_plotted, dataset[[type, "front"]][i]), col = "red")  
    }
    last_v_plotted = dataset[[type, "front"]][i]
    last_p_plotted = i
  }
}

last_v_plotted <- NaN
last_p_plotted <- NaN
for(i in 1:side_file_rows){
  if(!is.na(dataset[[type, "side"]][i]) && abs(dataset[["zscore", "side"]][i]) < max_std){
    if(!is.na(last_v_plotted)){
      lines(c(last_p_plotted,i+align_position), c(last_v_plotted, dataset[[type, "side"]][i]), col = "blue")
    }
    last_v_plotted = dataset[[type, "side"]][i]
    last_p_plotted = i+align_position
  }
}

# Paint outliers lines #
if(outliers_lines){
  mean_front = mean(dataset[[type, "front"]])
  SD_front = sd(dataset[[type, "front"]])
  lines(c(0,front_file_rows),c(mean_front,mean_front), col="darkgreen")
  uplimit <- mean_front + 3*SD_front
  downlimit <- mean_front - 3*SD_front
  lines(c(0,front_file_rows),c(uplimit, uplimit), col="darkgreen")
  lines(c(0,front_file_rows),c(downlimit, downlimit), col="darkgreen")
}


