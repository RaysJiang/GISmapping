LoadData <- function(file,row.names=TRUE){
     read.table(file, 
                row.names = 1,  
                header = TRUE, 
                sep = "\t",            
                na.strings = "NA", 
                strip.white = TRUE, 
                blank.lines.skip = TRUE,
     ) -> mylist
     return(mylist)
}

write <- function(x,filename="/Users/rjiang/RaysLocal/RaysR/Routput.txt"){
     write.table(x,qmethod='double',sep = "\t", col.names = NA,file = filename)    
}

DeleteNARowMatrix <- function(M,ColNum){
     M.del <- c()
     C <- M[,ColNum]
     n = 0
     for(i in 1:nrow(M)){
          n = n + 1
          j = C[n]
          if(!is.na(j)){
               M.del <- rbind(M.del,M[i,])
               
          }
     }
     #print(M.del)
     return(M.del)
}

###############################################

WorldContourPlot <- function(x,y,z,colours,gridX=18,gridY=18){
     library(maps)
     require(reshape) 
     require(mapdata)
     require(mapproj)
     
     
     gridX = gridX
     gridY = gridY
     X = seq(-180,180,length.out=gridX)
     Y = seq(-90,90,length.out=gridY)
     
     myMatrix = matrix(0, nrow=gridX, ncol=gridY)
     for (n in 1:length(z)){
          i = x[n]
          j = y[n]
          k = z[n]
          for(nx in 1:gridX){
               Xa =X[nx]
               Xb =X[nx+1]
               if(i>=Xa & i<Xb){
                    for(ny in 1:gridY){
                         Ya = Y[ny]
                         Yb = Y[ny+1]
                         if(j>=Ya & j<Yb){
                              myMatrix[nx,ny] -> k.tmp
                              myMatrix[nx,ny] <- k.tmp +k
                              #print(c(Xa,Xb,i,j,z))
                         }
                    }}}}
     
     
     #mm<-array(rep.int(0,gridX*gridY),dim=c(gridX,gridY))
     mm <- myMatrix
     
     filled.contour(x=X,y=Y,mm,
                    asp = 1,
                    color.palette = colours,
                    xlab = "Longitude (°)", ylab = "Latitude (°)",
                    plot.axes = {axis(1); axis(2);      
                                 map('world',
                                     xlim = c(-180, 180), 
                                     ylim = c(-90, 90), 
                                     fill = F,
                                     boundary = F,
                                     add = T, col = NA);
                    }
     )->p.world
     
     
     library(png)
     library(raster)
     img <- readPNG('/Users/rjiang/RaysLocal/RaysR/GIS/oceanmask.png')
     # mask is made for canvas size 698 X544 !!!
     lim <- par()
     rasterImage(img, lim$usr[1]-13, lim$usr[3]-2, lim$usr[2]-85, lim$usr[4]-2)
}


###############################################
###############################################

# mask is made for canvas size 698 X544 !!!
#quartz(title='', width=6.98, height=5.44)
# for Rstudio output 675X526

data = LoadData("/Users/rjiang/RaysLocal/RaysR/GIS/InputALFRED_ALL.txt")
print(colnames(data))



gene = 4
data <- DeleteNARowMatrix(data,gene)
data <- data[order(data[gene]),]
print(colnames(data)[gene])

x = data$meanlon
y = data$meanlat
z = data[,gene]
#z = 1-z
z = z*20+1

colours <- colorRampPalette(c("white","yellow","orange","red"))
gridX <- 18 ; gridY <-18

pdf( "mygraph.pdf", width = 6.98, height = 5.44 )
WorldContourPlot(x,y,z,colours,gridX,gridY)
dev.off()

WorldContourPlot(x,y,z,colours,gridX,gridY)




# source('~/RaysLocal/RaysR/GIS/ContourMap.R')

