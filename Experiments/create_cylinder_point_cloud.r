# GetCylinder() Arguments:
# x: x coordinate for cylinder base center
# y: y coordinate for cylinder base center
#   the cylinder will be built over this x,y coordinate by stacking in z dim.
# radius: the radius for the cylinder base
# num_aroud: the number of vertices for the circle that builds the cylinder
# num_up: the number of circles we stack to make the cylinder
# height: the height of the cylinder
# lids: boolean for if the cylinder should have end caps (lids).
#   this effectively creates a cylinder instead of a pipe
# lid_step: The step size for converging towards the center point for end lids
# ______________________________________________________________________________


GetCylinder <- function(x=0, y=0, radius=1, num_around=25, num_up=5, height=10,
                        lids=FALSE, lid_step=-0.01) {
  
  # ____________________________________ argument validation ___________________
  # must have a radius
  if (radius <= 0) {
    err_message <- "radius MUST be greater than 0."
    stop(err_message)
  }
  # must have at least 3 points to create surfae
  if (num_around < 3) {
    err_message <- "num_around MUST be at least 3."
  }
  # num_up must be at least 2
  if (num_up < 2) {
    err_message <- "num_up MUST be at least 2."
    stop(err_message)
  }
  # must have a height
  if (height <= 0) {
    err_message <- "height MUST be greater than 0."
  }
  
  # ____________________________________ build cylinder ________________________
  # get angle step for chopping circle into number of circle_points
  angle_step <- 2*pi / num_around
  # get sequence of angles
  angles <- seq(0, 2*pi - angle_step, by<-angle_step)
  
  # get x, y coordinates of circle base for cylinder
  # we will use same circle for stacking cylinder
  circle_x <- cos(angles)*radius + x
  circle_y <- sin(angles)*radius + y
  
  # define the ground to be at the 0 level
  ground <- 0
  
  # chop up z dimension so we have height steps from the ground to 
  # the top of our cylinder, effectively we are stacking them
  z_steps <- seq(from<-ground, to<-height, by<-((height-ground)/(num_up - 1)))
  
  # initialize empty data frame for holding our cylinder of stacked circles
  cylinder <- c()
  
  # build cylinder by stacking circles
  for (z in z_steps) {
    
    # if it's our base circle or top circle, and lids is TRUE,
    # we want to fill it in creating a full cylinder, not just a pipe
    if ((lids == TRUE && z == z_steps[1]) || (lids == TRUE && z == height)) {
      
      # make sure lid step is negative so we converge on center
      if (lid_step > 0)
        lid_step <- lid_step * -1
      
      # get sequence of converging radius for building cylinder top and bottom
      radii <- seq(radius, 0, lid_step)
      
      # get points for caps
      cap_x <- (cos(angles)*radii + x)
      cap_y <- (sin(angles)*radii + y)
      cap_z <- rep(z, length(cap_x))
      
      # append caps to our cylinder data frame point cloud
      cylinder$x <- c(cylinder$x, cap_x)
      cylinder$y <- c(cylinder$y, cap_y)
      cylinder$z <- c(cylinder$z, cap_z)
      
      # if no lids, we just build a pipe
    } else {
      
      # append circle base to cylinder
      # this is the current circle being stacked, but has no height value
      cylinder$x <- c(cylinder$x, circle_x)
      cylinder$y <- c(cylinder$y, circle_y)
      # build out z dimension points for the current circle
      # here we are giving our circle a height value and stacking it
      circle_z <- rep(z, num_around)
      # add z dimension to current circle in cylinder
      cylinder$z <- c(cylinder$z, circle_z)
      
    }
  }
  
  # return xyz data frame representing our cylinder
  return(cylinder)
}

# ______________________________________ cylinder with default args ____________
cylinder_default_args <- GetCylinder()
library(rgl)
plot3d(cylinder_default_args)


# ______________________________________ dense cylinder for meshing ____________
x <- 0
y <- 0
radius <- 0.25
height <- 10
num_around <- 360
num_stacked <- 500
lids = TRUE
lid_step = 0.00001

cylinder_for_mesh <- GetCylinder(x, y, radius, num_around, num_stacked, height,
                                 lids, lid_step)
library(rgl)
plot3d(cylinder_for_mesh)


# ______________________________________ save full cylinder for meshing ________
write.csv(cylinder_for_mesh,
          file="~/Desktop/cylinder_rad0.25_height10_withlids.xyz",
          row.names = FALSE)

