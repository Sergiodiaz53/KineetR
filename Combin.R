# Combin.R
# Author: Eugenia Ulzurrun euv(at)uma.es
# Description: R Script for combine two Kinects files in order to improve its quality replacing inferred points from one with not-inferred points from other.
  
  setwd("/Users/Sergio/Documents/BitlabProjects/KineetR")
  
  r_excelfrontal <- read.csv("/Users/Sergio/Dropbox/Trabajo/Deep Learning/R/Ficheros/FrontFile.csv", header=TRUE, sep=";")
  nf_excelfrontal <- length(r_excelfrontal[,1]) # Aunque diferencie el numero de filas(nf) de ambos archivos este n?mero es igual en ambos casos
  r_excellateral <- read.csv("RotatedAlignedSidefile.csv", header=TRUE, sep=";")
  nf_excellateral <- length(r_excellateral[,2])
  
  # Segun lo anterior este if es para practicar
  
  if(nf_excelfrontal > nf_excellateral){
    nf_mayor <- nf_excelfrontal 
  }else{
    nf_mayor <- nf_excellateral
  }
  #print(nf_mayor)
  
  nc_excelfyc <- length(r_excelfrontal[1,]) # Este numero es igual en ambos archivos
  
  # Crear un excel con ambos archivos, frontal y lateral evitando puntos inferidos. Si el punto es inferido en ambos entonces se calcula la media (indicado en la casilla con el numero 2).
  
  cabecera <- paste("tiempo","esq0Inf","esq0X","esq0Y","esq0Z","esq1Inf","esq1X","esq1Y","esq1Z","esq2Inf","esq2X","esq2Y","esq2Z","esq3Inf","esq3X","esq3Y","esq3Z", "esq4Inf","esq4X","esq4Y","esq4Z","esq5Inf","esq5X","esq5Y","esq5Z","esq6Inf","esq6X","esq6Y","esq6Z","esq7Inf","esq7X","esq7Y","esq7Z","esq8Inf","esq8X","esq8Y","esq8Z","esq9Inf","esq9X","esq9Y","esq9Z","esq10Inf","esq10X","esq10Y","esq10Z","esq11Inf","esq11X","esq11Y","esq11Z","esq12Inf","esq12X","esq12Y","esq12Z","esq13Inf","esq13X","esq13Y","esq13Z","esq14Inf","esq14X","esq14Y","esq14Z","esq15Inf","esq15X","esq15Y","esq15Z","esq16Inf","esq16X","esq16Y","esq16Z","esq17Inf","esq17X","esq17Y","esq17Z","esq18Inf","esq18X","esq18Y","esq18Z","esq19Inf","esq19X","esq19Y","esq19Z","esq20Inf","esq20X","esq20Y","esq20Z","esq21Inf","esq21X","esq21Y","esq21Z","esq22Inf","esq22X","esq22Y","esq22Z","esq23Inf","esq23X","esq23Y","esq23Z","esq24Inf", "esq24X","esq24Y","esq24Z","esqRefInf","esqRefX","esqRefY","esqRefZ",sep=";")
  
  write(cabecera, file = "excelfrontalylateral.csv", append = TRUE )
  
  count2 <- 0
  count1 <- 0
  
  for(i in 1:nf_mayor){
    j <- 2
    linea <- paste(r_excelfrontal[i,1], sep=";")
    
    while(j <= nc_excelfyc){
      
      if((r_excelfrontal[i,j]==1) && (r_excellateral[i,j]==0)){
        
        count2 <- count2 + 1
        
        r_excelfrontal[i,j] <- c(2)
        r_excelfrontal[i,j+1] <- r_excellateral[i,j+1]
        r_excelfrontal[i,j+2] <- r_excellateral[i,j+2]
        r_excelfrontal[i,j+3] <- r_excellateral[i,j+3] 
        
      }
      
      if((r_excelfrontal[i,j]==1) && (r_excellateral[i,j]==1)){
        
        count1 <- count1 + 1

        r_excelfrontal[i,j] <- c(1)
        r_excelfrontal[i,j+1] <- ((r_excelfrontal[i-1,j+1]) + (r_excelfrontal[i+1,j+1]))/2
        r_excelfrontal[i,j+2] <- ((r_excelfrontal[i-1,j+2]) + (r_excelfrontal[i+1,j+2]))/2
        r_excelfrontal[i,j+3] <- ((r_excelfrontal[i-1,j+3]) + (r_excelfrontal[i+1,j+3]))/2

      }
      linea <- paste(linea, r_excelfrontal[i,j], r_excelfrontal[i,j+1], r_excelfrontal[i,j+2], r_excelfrontal[i,j+3],sep=";")
      j <- j + 4
      
    }
    
    write(linea, file="excelfrontalylateral.csv", append=TRUE)
  }
  
  
  
