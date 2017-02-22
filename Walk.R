# Walk.R
# Author: Eugenia Ulzurrun euv(at)uma.es
# Description: R Script for paint each frame of a Kinect file as a cloud point representation in a scatterplot 3D.

## !/usr/bin/env Rscript      
# args = commandArgs(trailingOnly=TRUE) 
# 
# if(length(args) < 7){
#   stop("USE: Rscript.exe walker_f_consola.R <pac_cam.csv> <traslation 1/0> <n_frames (use -1 for all)> <point_esq_1> <point_esq_2 > <dist_to_spinebase_or_time 1/0> <h (use -1 for all)>")
# }
# 
# setwd("C:/Users/Admin/Desktop/FINALES_walker/walker_f_consola")

# pac_cam <- scan(args[1], skip=1, sep="\n", what = character())
# traslation <-  as.numeric(args[2])
# n_frames <- as.numeric(args[3])
# point_esq_1 <- as.numeric(args[4]) + 1
# point_esq_2 <- as.numeric(args[5]) + 1
# dist_to_spinebase_or_time <- as.numeric(args[6])
# h <- as.numeric(args[7])

#Load file to paint
pac_cam <- scan("/Users/Sergio/Dropbox/Trabajo/Deep Learning/R/Ficheros/90_oscar_60_B.csv", skip=1, sep="\n", what = character())
setwd("/Users/Sergio/Desktop/KinectImgs")

traslation <- 1
n_frames <- -1
point_esq_1 <- 15
point_esq_2 <- 17
dist_to_spinebase_or_time <- 1
h <- 90


m_filas <- length(pac_cam)
m_cols <- length(strsplit(pac_cam[[1]], split = ";")[[1]])
mi_matriz <- array(NA, dim = c(m_filas, m_cols))

## Convert all table to numeric
for(i in 1:m_filas){
  cada_campo <- strsplit(pac_cam[[i]], split = ";")[[1]]
  for(j in 2:m_cols){
    mi_matriz[i,j] <- as.numeric(cada_campo[[j]][1])
  }
}

l_distances_1 <- c()
l_distances_2 <- c()

# Map limb points
mapeo <- array(-1, dim=c(26,3))
mapeo[1,1] = 13
mapeo[1,2] = 2
mapeo[1,3] = 17
mapeo[2,2] = 21
mapeo[2,2] = 1
mapeo[3,1] = 21
mapeo[3,2] = 4
mapeo[4,1] = 3
mapeo[5,1] = 6
mapeo[5,2] = 21
mapeo[6,1] = 7
mapeo[6,2] = 5
mapeo[7,1] = 8
mapeo[7,2] = 6
mapeo[8,1] = 22
mapeo[8,2] = 23
mapeo[8,3] = 7
mapeo[9,1] = 10
mapeo[9,2] = 21
mapeo[10,1] = 11
mapeo[10,2] = 9
mapeo[11,1] = 12
mapeo[12,1] = 11
mapeo[12,2] = 25
mapeo[12,3] = 24
mapeo[13,1] = 14
mapeo[13,2] = 1
mapeo[14,1] = 13
mapeo[14,2] = 15
mapeo[15,1] = 14
mapeo[15,2] = 16
mapeo[16,1] = 15
mapeo[17,1] = 1
mapeo[17,2] = 18
mapeo[18,1] = 17
mapeo[18,2] = 19
mapeo[19,1] = 18
mapeo[19,2] = 20
mapeo[20,1] = 19
mapeo[21,1] = 3
mapeo[21,2] = 2
mapeo[21,3] = 5
mapeo[22,1] = 8
mapeo[23,1] = 8
mapeo[24,1] = 12
mapeo[25,1] = 12

library(scatterplot3d)

start <- 1
l.col <- m_filas
l.fila <- m_cols

if(n_frames == -1){
  n_frames <- m_filas
}else{
  l.col <- n_frames
  
}

if(h == -1){
  h <- l.col
  
}

time <- 1

siono_maxmin <- 0
max_x <- 1
max_y <- 1
max_z <- 1  

min_x <- 1
min_y <- 1
min_z <- 1 

for(i in start:l.col){ 
  j <- 3
  
  if(is.na(mi_matriz[i,j])){ 
    next
    
  }else{
    while(j < (l.fila-8)){
      
      if(traslation == 1){
        
        if(siono_maxmin == 0){
          max_x <- mi_matriz[i,j]+mi_matriz[i,103] 
          max_z <- mi_matriz[i,j+1]+mi_matriz[i,104]
          max_y <- mi_matriz[i,j+2]+mi_matriz[i,105]
          
          min_x <- mi_matriz[i,j]+mi_matriz[i,103] 
          min_z <- mi_matriz[i,j+1]+mi_matriz[i,104]
          min_y <- mi_matriz[i,j+2]+mi_matriz[i,105]
          
          siono_maxmin <- 1
          
        }else{
          if(mi_matriz[i,j]+mi_matriz[i,103]  > max_x){
            max_x <- mi_matriz[i,j]+mi_matriz[i,103]
          }
          if(mi_matriz[i,j+1]+mi_matriz[i,104]  > max_z){
            max_z <- mi_matriz[i,j+1]+mi_matriz[i,104]
          }
          if(mi_matriz[i,j+2]+mi_matriz[i,105]  > max_y){
            max_y <- mi_matriz[i,j+2]+mi_matriz[i,105]
          }
          if(mi_matriz[i,j]+mi_matriz[i,103]   < min_x){
            min_x <- mi_matriz[i,j]+mi_matriz[i,103]
          }
          if(mi_matriz[i,j+1]+mi_matriz[i,104]  < min_z){
            min_z <- mi_matriz[i,j+1]+mi_matriz[i,104]
          }
          if(mi_matriz[i,j+2]+mi_matriz[i,105]  < min_y){
            min_y <- mi_matriz[i,j+2]+mi_matriz[i,105]
          }
        }
        
      }else{
        if(siono_maxmin == 0){
          
          max_x <- mi_matriz[i,j]
          max_z <- mi_matriz[i,j+1]
          max_y <- mi_matriz[i,j+2]
          
          min_x <- mi_matriz[i,j]
          min_z <- mi_matriz[i,j+1]
          min_y <- mi_matriz[i,j+2]
          
          siono_maxmin <- 1
          
        }else{
          if(mi_matriz[i,j]  > max_x){
            max_x <- mi_matriz[i,j]
          }
          if(mi_matriz[i,j+1]  > max_z){
            max_z <- mi_matriz[i,j+1]
          }
          if(mi_matriz[i,j+2]  > max_y){
            max_y <- mi_matriz[i,j+2]
          }
          if(mi_matriz[i,j]   < min_x){
            min_x <- mi_matriz[i,j]
          }
          if(mi_matriz[i,j+1]  < min_z){
            min_z <- mi_matriz[i,j+1]
          }
          if(mi_matriz[i,j+2]  < min_y){
            min_y <- mi_matriz[i,j+2]
          }
        }
      }
      j <- j + 4
      
    }
  }
  
}

for(i in start:l.col){  
  
  j <- 3
  equis <- c()
  zeta <- c()
  igriega <- c()
  
  k <- 1
  
  if(is.na(mi_matriz[i,j])){ 
    next
  }else{
    
    while(j < (l.fila-8)){
      
      if(traslation == 1){
        x <-  mi_matriz[i,j]+mi_matriz[i,103]
        z <-  mi_matriz[i,j+1]+mi_matriz[i,104]
        y <-  mi_matriz[i,j+2]+mi_matriz[i,105]
      }else{
        x <-  mi_matriz[i,j]
        z <-  mi_matriz[i,j+1]
        y <-  mi_matriz[i,j+2]
      }
      
      if(j==3){ 
        
        mar.default <- c(5,4,4,2) + 0.1
        par(mfrow=c(1,2), mar = mar.default + c(0, 4, 0, 0) , oma = c(0,1,0,0))
    
        #Creates scatterplot  
        move_pac_cam <- scatterplot3d(x,y,z,xlim=c(min_x,max_x),ylim=c(min_y,max_y),zlim=c(min_z,max_z), main="3D Spatial Movement")  
        
      }else{
        
        # Paint points. Red if are inferred, green if are replaced.
        
        if(mi_matriz[i,j-1] == 1) {
          move_pac_cam$points3d(x,y,z, col="red", type="o", pch=19) 
        } else if(mi_matriz[i,j-1] == 2){
          move_pac_cam$points3d(x,y,z, col="darkgreen", type="o", pch=19) 
        } else {
          move_pac_cam$points3d(x,y,z)
        }
      }
      
      if(point_esq_1==k){
        col_point_esq_1 <- j
      }
      
      if(point_esq_2==k){
        col_point_esq_2 <- j
      }
      
      j <- j + 4
      
      equis[k] <-  x
      zeta[k] <- z
      igriega[k] <- y
      
      k<-k+1
      
    }
    
    for(puntos in 1:26){
      for(union in 1:3){
        if(mapeo[puntos,union] != -1){
          move_pac_cam$points3d(c(equis[puntos],equis[mapeo[puntos,union]]),c(igriega[puntos],igriega[mapeo[puntos,union]]),c(zeta[puntos],zeta[mapeo[puntos,union]]),type="l",col="blue",lwd=2) 
        }
      }
    }
    
    
    if(dist_to_spinebase_or_time == 1){
      
      myx <- equis[point_esq_1] + mi_matriz[i,103]
      myy <- igriega[point_esq_1] + mi_matriz[i,105]
      myz <- zeta[point_esq_1] + mi_matriz[i,104]
      
      myx2 <- equis[point_esq_2] + mi_matriz[i,103]
      myy2 <- igriega[point_esq_2] + mi_matriz[i,105]
      myz2 <- zeta[point_esq_2] + mi_matriz[i,104]
      
      value <- sqrt((myx)^2 + (myy)^2 + (myz)^2)
      value2 <- sqrt((myx2)^2 + (myy2)^2 + (myz2)^2) 
      
    }else{
      myx <- equis[point_esq_1] 
      myy <- igriega[point_esq_1]
      myz <- zeta[point_esq_1] 
      
      myx_n <- mi_matriz[i+1,col_point_esq_1]+mi_matriz[i+1,103]
      myx_n <- mi_matriz[i+1,col_point_esq_1+2]+mi_matriz[i+1,105]
      mzx_n <- mi_matriz[i+1,col_point_esq_1+1]+mi_matriz[i+1,104]
      
      myx2 <- equis[point_esq_2] 
      myy2 <- igriega[point_esq_2] 
      myz2 <- zeta[point_esq_2] 
      
      myx2_n <- mi_matriz[i+1,col_point_esq_2]+mi_matriz[i+1,103]
      myx2_n <- mi_matriz[i+1,col_point_esq_2+2]+mi_matriz[i+1,105]
      mzx2_n <- mi_matriz[i+1,col_point_esq_2+1]+mi_matriz[i+1,104]
      
      
      value <- sqrt((myx-myx_n)^2 + (myy-myy_n)^2 + (myz-myz_n)^2)
      value2 <- sqrt((myx2-myx2_n)^2 + (myy2-myy2_n)^2 + (myz2-myz2_n)^2)
      
    }
    
    if(!is.na(value)){
      
      l_distances_1 <- c(l_distances_1, value)
      
    }else{
      l_distances_1 <- c(l_distances_1, 0)
      
    }
    
    if(!is.na(value2)){ 
      
      l_distances_2 <- c(l_distances_2, value2)
      
    }else{
      l_distances_2 <- c(l_distances_2, 0)
      
    }
    
    if(time > 1){ 
      
      if(dist_to_spinebase_or_time == 1){
        
        if(h+1 > time){
          
          plot(c(1:time), l_distances_1, xlim=c(1, time), ylim=c(min(l_distances_1,l_distances_2), max(l_distances_1,l_distances_2)), type="l", main="Distance from Point to Spine Base", xlab="Frames", ylab="")
          par(new=TRUE)
          plot(c(1:time), l_distances_2, xlim=c(1, time), ylim=c(min(l_distances_1,l_distances_2), max(l_distances_1,l_distances_2)), type="l", col="red",main="Distance from Point to Spine Base", xlab="Frames", ylab="")
          
          mtext(text="Point Displacement to Spine Base", side=2, line=2, las=0)   
          
        }else{
          
          plot(c((time-h):time), c(l_distances_1[(length(l_distances_1)-h):(length(l_distances_1))]), xlim=c((time-h),time), ylim=c(min(c(l_distances_1[(length(l_distances_1)-h):(length(l_distances_1))]),c(l_distances_2[(length(l_distances_2)-h):(length(l_distances_2))])), max(c(l_distances_1[(length(l_distances_1)-h):(length(l_distances_1))]),c(l_distances_2[(length(l_distances_2)-h):(length(l_distances_2))]))), type="l", main="Distance from point to spine base", xlab="Frames", ylab="")
          par(new=TRUE)
          plot(c((time-h):time), c(l_distances_2[(length(l_distances_2)-h):(length(l_distances_2))]), xlim=c((time-h),time), ylim=c(min(c(l_distances_1[(length(l_distances_1)-h):(length(l_distances_1))]),c(l_distances_2[(length(l_distances_2)-h):(length(l_distances_2))])), max(c(l_distances_1[(length(l_distances_1)-h):(length(l_distances_1))]),c(l_distances_2[(length(l_distances_2)-h):(length(l_distances_2))]))), type="l", col="red",main="Distance from point to spine base", xlab="Frames", ylab="")
          
          mtext(text="Point Displacement to Spine Base", side=2, line=2, las=0)   
        }
      } 
      
      if(dist_to_spinebase_or_time == 0){
        
        if(h+1 > time){
          
          plot(c(1:time), l_distances_1, xlim=c(1, time), ylim=c(min(l_distances_1,l_distances_2), max(l_distances_1,l_distances_2)), type="l", main="Gait Cycle", xlab="Frames", ylab="")
          par(new=TRUE)
          plot(c(1:time), l_distances_2, xlim=c(1, time), ylim=c(min(l_distances_1,l_distances_2), max(l_distances_1,l_distances_2)), type="l", col="red",main="Gait Cycle", xlab="Frames", ylab="")
          
          mtext(text="Stride Length", side=2, line=2, las=0)   
          
        }else{
          
          plot(c((time-h):time), c(l_distances_1[(length(l_distances_1)-h):(length(l_distances_1))]), xlim=c((time-h),time), ylim=c(min(c(l_distances_1[(length(l_distances_1)-h):(length(l_distances_1))]),c(l_distances_2[(length(l_distances_2)-h):(length(l_distances_2))])), max(c(l_distances_1[(length(l_distances_1)-h):(length(l_distances_1))]),c(l_distances_2[(length(l_distances_2)-h):(length(l_distances_2))]))), type="l", main="Gait Cycle", xlab="Frames", ylab="")
          par(new=TRUE)
          plot(c((time-h):time), c(l_distances_2[(length(l_distances_2)-h):(length(l_distances_2))]), xlim=c((time-h),time), ylim=c(min(c(l_distances_1[(length(l_distances_1)-h):(length(l_distances_1))]),c(l_distances_2[(length(l_distances_2)-h):(length(l_distances_2))])), max(c(l_distances_1[(length(l_distances_1)-h):(length(l_distances_1))]),c(l_distances_2[(length(l_distances_2)-h):(length(l_distances_2))]))), type="l", col="red",main="Gait Cycle", xlab="Frames", ylab="")
          
          mtext(text="Stride Length", side=2, line=2, las=0)   
          
        }
        
      }   
    }else{
      
      if(dist_to_spinebase_or_time == 1){
        plot(1,value, type="n", main="Distance from Point to Spine Base", xlab="Frames", ylab="")
        mtext(text="Point Displacement to Spine Base", side=2, line=2, las=0)
      }else{
        plot(1,value, type="n", main="Gait Cycle", xlab="Frames", ylab="")
        mtext(text="Stride Length", side=2, line=2, las=0)
      }
    } 
    time <- time + 1
    
    #png(filename=paste(paste("image",i,sep=""),".png",sep=""),width = 600, height = 300)
    dev.copy(png, paste(paste("image",i,sep=""),".png",sep=""),width = 600, height = 300)
    dev.off()
  }
}





