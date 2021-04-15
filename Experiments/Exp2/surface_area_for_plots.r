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


# CYLINDER CODE BELOW BEST WORKING SO FAR
# ______________________________________________________________________________
# plot circle
library(plotrix)
library(rgl)
library(misc3d)
library(Rvcg)
library(plotly)
plot(-5:5,seq(-5,5),type="n",xlab="",ylab="",main="Test draw.circle")
#draw.circle(0,0,c(1,0.66,0.33))


x_y <- draw.circle(x=0,y=0,radius=0.5, nv = 30)
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
plot3d(xlist, ylist, zlist)

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

dens$d <- dens$d * 1000

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






# __________
plot_ly(x=xlist, y=ylist, z=zlist, type="mesh3d")




# TODO: write out x list y and z list to text files to be read in by python
# tis is the point cloud that is the cylinder
