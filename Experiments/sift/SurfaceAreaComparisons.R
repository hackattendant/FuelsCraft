# imports
# gg plot nice plotting R
library(stats)
library(VoxR)
library(moments)
library(misc3d)
library(Rvcg)
library(rgl)
library(MASS)

rgl.clear()

# set working directory
setwd("~/Desktop/Root/Projects/FuelsCraft/R_Meshing_Playground")


totalSurfaceArea <- 0

# build loop that reads through files
# 0 to 45 because of subsets in directory
cat("\nBeginning Surface Area Calculations\n",
    "_______________________________________________________________________")
for (n in 0:74) {
  # build file path
  file_to_read <- "~/Desktop/Root/Projects/FuelsCraft/R_Meshing_Playground/freshSubsets/freshSubSets"
  file_to_read <- (paste(file_to_read, toString(n)))
  file_to_read <- (paste(file_to_read, ".xyz"))
  #print(file_to_read)
  
  # read in file
  data <- read.csv(file_to_read)
  
  # subset format is label, x, y, z
  # strip out xyz
  data <- data[,2:4]
  
  # maybe TODO
  # if data doesn't contain enough points
  if (length(data[,1]) < 6) {
    cat("\n\n~~~~~~~~~~~~~~~~~~~~~~~~~~~NEXT~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
    cat("FILE PROBLEM not enough points in :\n")
    cat(paste("subset",toString(n)))
    cat("\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\n")
    next
  }
  
  # todo: might need to change threshold as you move out
  # resolution change
  # increase threshold proportionally to scale of size of data you are meshing

  # normalize x and y channels
  data[,1] <- (data[,1]-min(data[,1]))
  data[,2] <- (data[,2]-min(data[,2]))
  
  # run kde
  dens <- kde3d(data[,1], data[,2], data[,3], n=100)
  
  # perform meshing
  storage.mode(dens$d) <- "integer"
  # 0 through 10
  # 0 means everything is kept
  # go towards 10 if smaller
  # TODO make correction factor
  mesh <- vcgIsosurface(dens$d, threshold=100)
  
  # calculate surface area of current mesh
  surfArea = (vcgArea(mesh)/2)
  # add surface area to total
  totalSurfaceArea <- (totalSurfaceArea + surfArea)
  # report surface area in console
  cat("File ")
  cat(toString(n))
  saMessage = (paste(", Surface Area: ", toString(surfArea)))
  cat(saMessage)
  cat("\n")

  # plot in shade3d
  rgl.open()
  shade3d(mesh)
}
# display total surface area calculated
cat("\nTotal Surface Area Using subsets: ", totalSurfaceArea)
cat("\n\n")



# _______________ calculate surface area of entire tree tip
data = read.csv("~/Desktop/Root/Projects/FuelsCraft/R_Meshing_Playground/tipOfTreeCleaned.xyz")
# strip out xyz
data <- data[,1:3]
# normalize x and y channels
data[,1] <- (data[,1]-min(data[,1]))
data[,2] <- (data[,2]-min(data[,2]))
data[,3] <- (data[,3]-min(data[,3]))

# run kde
# n is number of grid points in each dimension
# defining limits could be useful
# can we plot density output from kde?
# TODO: here
dens <- kde3d(data[,1], data[,2], data[,3], n=100)
# get dimensionality for entire tip and change thresh until surface area calcs are about same
# for 10cm subsets vs full 
# density at 50 at 10cm vs entire top of tree get range from all dimensions lowest to highest
# multiply n by 10 and compare surface of entire tip to total surface of subset combinations
# 500 for full tree tip
# 20 is default
# increments of 10 for n
# plot out surface areas per iteration with different values of n, is the change in surface area linear
# rank surface area from low to high
# plot , independ value name of file, x axis.  depen value surface area. is change overresolution linear change
# different colors for each value of n
# legend colors and iterations and groups of thresholds
# perform meshing
storage.mode(dens$d) <- "integer"
mesh <- vcgIsosurface(dens$d, threshold=50)
surfAreaWhole = (vcgArea(mesh)/2)

cat("\n Surface area of entire point cloud: ", surfAreaWhole)
cat("\n\n")
