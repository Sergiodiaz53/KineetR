# Mast.R
# Author: Sergio Diaz-del-Pino sergiodiazdp(at)gmail.com
# Description: Set of R functions from proccess Kinect files

library(Metrics)

# zscore
# Description: Calculate zScores given a value and a column
zscore <- function(v_value, v_col){
  the_mean <- mean(v_col, na.rm = TRUE)
  the_std <- sd(v_col, na.rm = TRUE)
  return ((v_value - the_mean)/the_std)
}
#

#AlignFiles
#Description: Align two camera files (front_view and side_view)
alignFiles <- function(front_view, side_view){
  min_MSE <- 0
  align_position <- 0
  for(i in 1:(length(front_view)-length(side_view))){
    currentFrontPart <- front_view[i:(i+length(side_view))]
    currentMSE <- mse(currentFrontPart,side_view)
    if(i==1) {
      min_MSE <- currentMSE
      align_position <- i
    } else if (currentMSE < min_MSE) {
      min_MSE <- currentMSE
      align_position <- i
    }
  }
  return(align_position)
}

#Rep.row // rep.col
#Description: Repeat rows and columns
rep.row<-function(x,n){
  matrix(rep(x,each=n),nrow=n)
}

rep.col<-function(x,n){
  matrix(rep(x,each=n), ncol=n, byrow=TRUE)
}
