library(Rvcg)
library(misc3d)
library(rgl)
# read in data
data <- read.csv("~/Desktop/FuelsCraft/DEV/FuelsCraft/Data/cylinder_r.25_h10_normals_r.ply", sep="", header=FALSE)

# strip out xyz
data <- data[,1:3]
# normalize x and y channels
#data[,1] <- (data[,1]-min(data[,1]))
#data[,2] <- (data[,2]-min(data[,2]))
#data[,3] <- (data[,3]-min(data[,3]))

# kde3d need to vary n 
dens <- kde3d(data[,1], data[,2], data[,3], n=100)

# convert storage mode to integer
storage.mode(dens$d) <- "integer"

# generate triangular mesh 
mesh <- vcgIsosurface(dens$d, 0)

# calculate surfae area
surf_area = vcgArea(mesh)/2

shade3d(mesh, col="green")



# run kde
# n is number of grid points in each dimension
# defining limits could be useful
# can we plot density output from kde?
# TODO: here
dens <- kde3d(data[,1], data[,2], data[,3], n=100)
# get dimensionality for entire tip and change th0resh until surface area calcs are about same
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
mesh <- vcgIsosurface(dens$d, threshold=100)
surfAreaWhole = (vcgArea(mesh)/2)

cat("\n Surface area of entire point cloud: ", surfAreaWhole)
cat("\n\n")

rgl.open()
shade3d(mesh, col="blue")

