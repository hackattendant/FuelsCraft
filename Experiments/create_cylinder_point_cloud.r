# GetCylinder() Arguments:
  # x: x coordinate for cylinder base center
  # y: y coordinate for cylinder base center
  # radius: the radius for the cylinder base
  # num_aroud: the number of vertices for the circle that builds the cylinder
  # num_up: the number of circles we stack to make the cylinder
  # height: the height of the cylinder
# ______________________________________________________________________________


GetCylinder <- function(x=0, y=0, radius=1, num_around=25,
                        num_up=5, height=10) {
  # argument validation
  # ____________________________________________________________________________
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
    err_message <- "height MUST be greater than 0"
  }

  
  # build cylinder
  # ____________________________________________________________________________
  # get angle step for chopping circle into number of circle_points
  angle_step <- 2*pi / num_around
  # get sequence of angles
  angles <- seq(0, 2*pi - angle_step, by<-angle_step)
  
  # get x, y coordinates of circle base for cylinder
  circle_x <- cos(angles)*radius + x
  circle_y <- sin(angles)*radius + y
  
  # define the ground to be at the origin 0
  ground <- 0
  # chop up z dimension so we have height steps from the ground to 
  # the top of our cylinder, effectively we are stacking them
  z_steps <- seq(from<-ground, to<-height, by<-((height-ground)/(num_up - 1)))

  # initialize empty data frame for holding our cylinder of stacked circles
  cylinder <- c()
  # build cylinder by stacking circles
  for (z in z_steps) {
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

  # return xyz data frame representing our cylinder
  return(cylinder)
}


# Example Usage:
# ______________________________________________________________________________
# specify arguments
center_x <- 0
center_y <- 0
cyl_radius <- 0.25
num_points_around <- 360
num_circles_stacked <-100
cylinder_height <- 10

# build cylinder 
cylinder <- GetCylinder(center_x, center_y, cyl_radius, num_points_around,
                        num_circles_stacked, cylinder_height)

# visualize cylinder
plot3d(cylinder, col="red")

# save out point cloud
write.csv(cylinder, file="~/Desktop/cylinder_radius:0.25_height:10.xyz")
