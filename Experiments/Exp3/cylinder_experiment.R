# plot a cylinder
library(rgl)
library(Rvcg)
library(stats)
library(VoxR)
library(moments)
library(misc3d)
library(Rvcg)
library(rgl)
library(MASS)
library(plotrix)
open3d()
coords <- seq(from=1, to=10, by=0.01)
mesh <- cylinder3d(
  center = cbind(coords, coords, coords),
  radius = 0.5,
  sides=360,
  closed = FALSE,
  keepVars=TRUE)

# pull out x, y and z values
x = mesh$vb[1,]
y = mesh$vb[2,]
z = mesh$vb[3,]
scalar = mesh$vb[4,]

#triangle_mesh_cylinder = as.mesh3d((x, y, z, triangles=TRUE, smooth=TRUE)
#area_of_triangle_mesh <- vcgArea(triangle_mesh_cylinder)/2

shade3d(mesh, col="red")
# 10 to 1 and 2 to 4 tomorrow
# plot subsets
plot3d(x, y, z)
t = shade3d(as.mesh3d(x, y, z))
plot3d(t)
# divide by two to only calculate surface area of 1 side
# surface_area = (vcgArea(mesh2)/2)
# cat(surface_area)

# don't run all run each between full line separators
#_______________________________________________________________________________

# Here is some stuff playing with as.mesh3d using triangles
xyz <- matrix(c(-1, -1, -1,
                -1,  1, -1,
                 1,  1, -1,
                 1, -1, -1,
                -1,  1, -1,
                -1,  1,  1,
                 1,  1,  1,
                 1,  1, -1,
                 1, -1, -1,
                 1,  1, -1,
                 1,  1,  1,
                 1, -1,  1), byrow = TRUE, ncol = 3)
mesh <- as.mesh3d(xyz, triangles = TRUE, col = "red")
mesh$vb
mesh$ib
open3d()
shade3d(mesh)

# Stop vertices 2 and 5 from being merged
notEQ <- matrix(FALSE, 12, 12)
notEQ[2, 5] <- TRUE
mesh <- as.mesh3d(xyz, triangles = FALSE, notEqual = notEQ)
mesh$vb
mesh$ib

# CYLINDER CODE BELOW BEST WORKING SO FAR
# ______________________________________________________________________________
# plot circle
library(plotrix)
library(rgl)
library(misc3d)
library(Rvcg)

# test draw circle function
#plot(-5:5,seq(-5,5),type="n",xlab="",ylab="",main="Test draw.circle")
#draw.circle(0,0,c(1,0.66,0.33))
plot.new() 


x_y <- draw.circle(x=0,y=0,radius=0.25, nv = 200)
z <- seq(from=0, to=10, by=0.2)
# build out list 
xlist <- c()
ylist <- c()
zlist <- c()
for (step in z) {
  xlist <- c(xlist, x_y$x)
  ylist <- c(ylist, x_y$y)
  zlist <- c(zlist, z)
}
zlist = zlist[1:length(xlist)]
plot3d(xlist, ylist, zlist, col = "red")

# TODO: vary n range and see differences in 10, 20, 30, 40, 50
# TODO: make plot of resulting meshes and their surface areas
# TODO: linear relationship in changing n?
# TODO: plot surface area against threshold (n=10 and 5 different thresholds)
# TODO: y surface area x- threshold
# scatter plot ^
# TODO: 5 different graphs, how different is the surface area and is the change a linear relationship?
# TODO: figure out limits of kde3d threshold
# lims=c(xlimit, ylimit, zlimit)
# TODO: try -2 to 2 for x,y; z 0 to 20
dens <- kde3d(xlist, ylist, zlist, n=10)

dens$d <- dens$d * 100

# convert to int
storage.mode(dens$d) <- "integer"

# TODO: we are working in square mill meters for surface
# TODO:run from 1 to 20 in thresholds for vcgIsosurface and see how surface areas are changing
mesh <- vcgIsosurface(dens$d, threshold=50)

# TODO: removed divide by 2
surfArea_cylinder <- vcgArea(mesh)
rgl.open()
shade3d(mesh, color="blue")
#plot3d(dens$x, dens$y, dens$z)
cat(surfArea_cylinder)


dens[which(dens$d < 10)]$d <- 0
