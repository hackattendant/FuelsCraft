
require("VoxR")
require(rgl)
require(moments)
library(stats)
library(VoxR)
library(moments)
library(misc3d)
library(Rvcg)
library(rgl)
library(MASS)

# ptm = proc.time()

inData = read.csv('/Users/hackattendant/Desktop/FuelsCraft/R_Meshing_Playground/tipOfTree.txt')
# file name to write to 
fileName <- "/Users/hackattendant/Desktop/subsets/subset"

#Data should fit the format of "X,Y,Z,I" or "X,Y,Z"
colnames(inData)<- c("X","Y","Z")
inData = inData[,1:3]

inData$Z = inData$Z-min(inData$Z)
cellSize=0.5
VoxSize = 0.1
VoxSize2 = 0.01
options(digits=9)
 
# find the nearest .5 that covers all X values
minX = floor(min(inData[,1])*2) / 2
maxX = ceiling(max(inData[,1]*2)) / 2
 
minY = floor(min(inData[,2]*2)) / 2
maxY = ceiling(max(inData[,2]*2)) / 2
  
b1=ceiling((maxX - minX)/cellSize) #calculates x dimensions
b2=ceiling((maxY - minY)/cellSize) #calculates y dimensions
  
#Divides the lidar data into cells for which voxels will be analyzed ; set in meters 0.5 = 0.5m 1=1m
  
#specify breaks so everything aligns
xManualBreaks = seq(minX, maxX, cellSize)

yManualBreaks = seq(minY, maxY, cellSize)

numCells = b1*b2
l=1
V.Store <- data.frame(id = integer(numCells),
                      x=numeric(numCells),
                      y=numeric(numCells),
                      npts=integer(numCells),
                      MaxHt=numeric(numCells),
                      MeanHt=numeric(numCells),
                      StdevHt=numeric(numCells),
                      kurt=numeric(numCells),
                      skew=numeric(numCells),
                      v10=numeric(numCells),
                      v20=numeric(numCells),
                      v30=numeric(numCells),
                      v40=numeric(numCells),
                      v50=numeric(numCells),
                      v60=numeric(numCells),
                      v70=numeric(numCells),
                      v80=numeric(numCells),
                      v90=numeric(numCells),
                      v100=numeric(numCells),
                      vTtl=numeric(numCells),
                      x1=numeric(numCells),
                      y2=numeric(numCells),
                      cellX=integer(numCells),
                      cellY=integer(numCells)
)
v.store2 <- data.frame(id = integer(1),
                       VoxID = integer(1),
                       x=numeric(1),
                       y=numeric(1),
                       z=numeric(1),
                       npts=integer(1),
                       MaxHt=numeric(1),
                       MeanHt=numeric(1),
                       BD=numeric(1),
                       SA=numeric(1),
                       Vol=numeric(1),
                       SAV=numeric(1),
                       pca1a=numeric(1),
                       pca1b=numeric(1),
                       pca1c=numeric(1),
                       pca2a=numeric(1),
                       pca2b=numeric(1),
                       pca2c=numeric(1),
                       pca3a=numeric(1),
                       pca3b=numeric(1),
                       pca3c=numeric(1),
                       PAD=numeric(1),
                       PADR=numeric(1),
                       PADV=numeric(1)
)
v.temp2=v.store2
v.temp3=v.temp2
nameIndex <- 0
for(i in 1:length(xManualBreaks)) 
{
    for( s in 1:length(yManualBreaks)) 
    {
      TLSSub = subset(inData, inData$X >= xManualBreaks[i] & inData$X < xManualBreaks[i+1] & 
                        inData$Y >= yManualBreaks[s] & inData$Y < yManualBreaks[s+1])
      # write out here?
      
      if(length(TLSSub$X) >= 10)
      {
        TLSSub =  subset(TLSSub, TLSSub$Z > 0)
        TLSSub$Z= TLSSub$Z-min(TLSSub$Z)
         
        V.Store[l,1] = l
        V.Store[l,2] = mean(TLSSub$X, digits=4)
        V.Store[l,3] = mean(TLSSub$Y, digits=4)
        V.Store[l,4] = length(TLSSub$X)
        V.Store[l,5] = max(TLSSub$Z)
        V.Store[l,6] = mean(TLSSub$Z)
        V.Store[l,7] = sd(TLSSub$Z)
        V.Store[l,8] = kurtosis(TLSSub$Z)
        V.Store[l,9] = skewness(TLSSub$Z)
        V.Store[l,21] =((max(TLSSub$X)-min(TLSSub$X))/2)+min(TLSSub$X)
        V.Store[l,22] =((max(TLSSub$Y)-min(TLSSub$Y))/2)+min(TLSSub$Y)
        V.Store[l,23] = xManualBreaks[i]
        V.Store[l,24] = yManualBreaks[s]
        TLSSubS <- TLSSub[order(TLSSub$Z),]
        
        v.vol<- vox(TLSSubS, res=VoxSize)
        colnames(v.vol)<- c("X","Y","Z","NBPTS")
        
        # write out into 10cm x 10cm voxels
        # already subsetting into voxels
        # each loop it does subgrid, write it out as a text file for subsets of 10cm
        for (n in 1: length(v.vol$X)){
          SubGrid = subset(TLSSubS, (TLSSubS$X > v.vol[n,1]-0.05) & (TLSSubS$X < v.vol[n,1]+0.05) & (TLSSubS$Y > v.vol[n,2]-0.05) &
                             (TLSSubS$Y < v.vol[n,2]+0.05) & (TLSSubS$Z > v.vol[n,3]-0.05) & (TLSSubS$Z < v.vol[n,3]+0.05))
          

          # index for changing file name of subset
          fileIncr <- toString(nameIndex)
          # increment number for saving csv file
          nameIndex <- (nameIndex + 1)
          # File Name to add with fileIncr
          fileForSubset <- paste(fileName, paste(fileIncr, ".xyz", sep=""), sep="")
          print(fileForSubset)
          
          # save subset
          write.csv(SubGrid, fileForSubset, row.names=FALSE)

   
            if (length(SubGrid$X) > 0){
              v.temp2$id = l
              v.temp2$VoxID = n
            }
            if (length(SubGrid$X) > 0){
              ps = SubGrid[,1:3]
              v.vol2 <- vox(SubGrid, res=VoxSize2)
              v.vol2n =v.vol2
              v.vol2n[,1] = v.vol2[,1]-min(v.vol2[,1])      #this will shift matrix values to left
              v.vol2n[,2] = v.vol2[,2]-min(v.vol2[,2])      # this will shift matrix values down
              v.vol2n[,3] = v.vol2[,3]-min(v.vol2[,3])
              colnames(v.vol2n)<- c("X","Y","Z","NBPTS")
              PAD = matrix(data=0,nrow=10,ncol=10)
              PADV = matrix(data=0,nrow=10,ncol=10)
              
              # fill in matrices by looking at each voxel 
              # then directly index the matrix
              #for(e in 1:length(v.vol2n$X)){
             # xIndex = min(floor(100*v.vol2n$X[e])+1, 10)
             # yIndex = min(floor(100*v.vol2n$Y[e])+1, 10)
             # zIndex = min(floor(100*v.vol2n$Z[e])+1, 10)
                
              #  PADfast[xIndex, yIndex] = 1
              #  PADfastV[xIndex, zIndex] = 1
              #}
              
              v.temp2$id = l
              v.temp2$VoxID = n
              v.temp2$npts = length(ps[,1])
              v.temp2$x = v.vol$X[n]+0.05
              v.temp2$y = v.vol$Y[n]+0.05
              v.temp2$z = v.vol$Z[n]+0.05
              v.temp2$MaxHt= max(SubGrid$Z)
              v.temp2$MeanHt=mean(SubGrid$Z)
              v.temp2$BD <- length(v.vol2$npts)/(VoxSize*100)^3
              colnames(v.vol2)<- c("X","Y","Z","NBPTS")
              ps = SubGrid[,1:3]
              ps[,1]  <-  ps[,1]-min(ps[,1])
              ps[,2]  <-  ps[,2]-min(ps[,2])
              v.temp2$Vol= length(v.vol2$X)*2
#              if(v.vol$NBPTS[n] > 50 ) {
              if(v.vol$NBPTS[n] > 5 ) {
                SG.dens <- kde3d(ps[,1],ps[,2],ps[,3], n=50)
#                tempdf<-data.frame(l, n, v.vol$NBPTS[n], bandwidth.nrd(ps$X), bandwidth.nrd(ps$Y), bandwidth.nrd(ps$Z), max(SG.dens$d))
#                names(tempdf)<-c("cell", "voxel", "points", "Xbw", "Ybw", "Zbw", "maxKDE")
#                debug.df <- rbind(debug.df, tempdf)
                
                # the number 10000 will affect speed of script dramatically
                # 10000 vs 50000 => 7.5 vs 13.5 seconds
                # if(max(SG.dens$d) < 50000 & min(SG.dens$d) != "NaN" ) { 
                if(max(SG.dens$d) < 10000 & min(SG.dens$d) != "NaN" ) { 
                  storage.mode(SG.dens$d) <- "integer"
                  #iso surface mesh based off desnity id
                  #vcgIsosurface current meshing                  
                  SG.mesh <- vcgIsosurface(SG.dens$d, threshold=400)
                  #wire3d(SG.mesh)
                  # use vcgArea and see change of surface area as you change threshold
                  v.temp2$SA = (vcgArea(SG.mesh)/2)
                  v.temp2$SAV = v.temp2$SA/v.temp2$Vol
                }
                if(sd(ps$X)& sd(ps$X) > 0) {
                  v.temp2$PAD = sum(PAD)
                  v.temp2$PADR = sum(PAD)/100
                  v.temp2$PADV = sum(PADV)
                }
              }
              v.store2 <- rbind(v.store2, v.temp2)
              v.temp2=v.temp3
            }
            v.temp = subset(v.vol, v.vol[,3] < 0.1)
            V.Store[l,10] =length(v.temp[,1])
            v.temp = subset(v.vol, v.vol[,3] >= 0.1 & v.vol[,3] < 0.2)
            V.Store[l,11] =length(v.temp[,1])
            v.temp = subset(v.vol, v.vol[,3] >= 0.2 & v.vol[,3] < 0.3)
            V.Store[l,12] =length(v.temp[,1])
            v.temp = subset(v.vol, v.vol[,3] >= 0.3 & v.vol[,3] < 0.4)
            V.Store[l,13] =length(v.temp[,1])
            v.temp = subset(v.vol, v.vol[,3] >= 0.4 & v.vol[,3] < 0.5)
            V.Store[l,14] =length(v.temp[,1])
            v.temp = subset(v.vol, v.vol[,3] >= 0.5 & v.vol[,3] < 0.6)
            V.Store[l,15] =length(v.temp[,1])
            v.temp = subset(v.vol, v.vol[,3] >= 0.6 & v.vol[,3] < 0.7)
            V.Store[l,16] =length(v.temp[,1])
            v.temp = subset(v.vol, v.vol[,3] >= 0.7 & v.vol[,3] < 0.8)
            V.Store[l,17] =length(v.temp[,1])
            v.temp = subset(v.vol, v.vol[,3] >= 0.8 & v.vol[,3] < 0.9)
            V.Store[l,18] =length(v.temp[,1])
            v.temp = subset(v.vol, v.vol[,3] >= 0.9 & v.vol[,3] < 1.0)
            V.Store[l,19] =length(v.temp[,1])
            
            if(length(v.temp[,1]) > 0 ) {if( length(v.temp[,4])>0){V.Store[l,20] = sum(V.Store[l,10:19])}}
            
          }
        }
        
        l=l+1
        l
      }
  }
  
#   V.Export <- subset(V.Store, V.Store[,4] > 1)
#   write.csv(V.Export,file=outD[j])
#   V.Export <- subset(v.store2, v.store2$x > 0)
  
#   write.csv(V.Export,file=outD2[j])
#  # write.csv(debug.df, file="debugR.csv")
 

# outD2

proc.time() - ptm
