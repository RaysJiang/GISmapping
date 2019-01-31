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

write <- function(x,filename="Routput.txt"){
     write.table(x,qmethod='double',sep = "\t", col.names = NA,file = filename)    
}

###############################################


library(sp)


myPath <- paste(getwd(),'/GIS/',sep='')
myFile = paste(myPath,'LBR_adm1.RData',sep='')
con <- file(myFile)
load(con)
close(con)



regions = c()
name.1 = as.character(gadm$NAME_1)
regions = cbind(regions,name.1)
print(regions)
write(regions)



library(ggplot2)
library(gridExtra)


colfuncRed <- colorRampPalette(c("white","red", "darkred"),bias = 1)
colfuncBlue <- colorRampPalette(c("white","light blue", "darkblue"),bias = 1)


data <- LoadData(paste(myPath,'/Input_GIS.txt',sep=''))
gadm$population <- data$population
gadm$sickle <- data$sickle
n = length(gadm$NAME_1)
colRed = colfuncRed(n)
colBlue = colfuncBlue(n)

p1 <- spplot(gadm, "sickle", col.regions=colRed, main="Sickle Cell Incidences")
p2 <- spplot(gadm, "population", col.regions=colBlue, main="Population")
grid.arrange(p1, p2, ncol=2)

