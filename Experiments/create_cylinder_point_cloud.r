# ~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~ GetCylinder() Function ~_~
GetCylinder <- function(x=0, y=0, radius=1, height=10, num_around=25,
						num_up=10, lids=TRUE, lid_step=-0.2) {

	# __ GetCylinder() Arguments _______________________________________________
	# Creates cylinder point cloud with various options.
	#	x (int): ........... x coordinate for cylinder base center
	#	y (int): ........... y coordinate for cylinder base center
	#	radius (double): ... radius for the cylinder base
	#	num_aroud (int): ... number of vertices in circle to stack
	#	num_up (int): ...... number of circles we stack to make the cylinder
	#	height (double): ... height of the cylinder
	#	lids (boolean): .... when TRUE we create cylinder, FALSE creates pipe
	#	lid_step: .......... step size for changing the radius to generate lids

  
	# ___________________________________________________ argument validation __
  # must have a radius
	if (radius <= 0)
		stop("radius MUST be greater than 0.")
  # must have at least 3 points to create surfae
	if (num_around < 3)
		stop("num_around MUST be at least 3.")
  # num_up must be at least 2
	if (num_up < 2)
		stop("num_up MUST be at least 2.")
  # must have a height
	if (height <= 0)
		stop("height MUST be greater than 0.")
  

	# ________________________________________________________ build cylinder __
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
      
			# loop through radii and create lids for cylinder
			for (radius_i in radii)
			{
				# get points for caps (lids) on cylinder
				cap_x <- (cos(angles)*radius_i + x)
				cap_y <- (sin(angles)*radius_i + y)
      cap_z <- rep(z, length(cap_x))
      
      # append caps to our cylinder data frame point cloud
      cylinder$x <- c(cylinder$x, cap_x)
      cylinder$y <- c(cylinder$y, cap_y)
      cylinder$z <- c(cylinder$z, cap_z)
			}
      
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


# ________________________________________________ cylinder with default args __
# generate cylinder
cylinder_default_args <- GetCylinder()

# plot cylinder
library(rgl)
plot3d(cylinder_default_args)

# ________________________________________________ dense cylinder for meshing __
# We want a cylinder with 0.25 radius and 10 height
x <- 0                    # center x coord
y <- 0                    # center y coord
radius <- 0.25            # cylinder radius
height <- 10              # cylinder height
num_around <- 360         # number of points around circle for cylinder pipe
num_stacked <- 1000       # number of circles stacked to create cylinder pipe
lids = TRUE               # We want full cylinder with lids (top, bottom)
lid_step = 0.00001        # step size for lids converging on center

# create cylinder using parameters above
cylinder_for_mesh <- GetCylinder(x, y, radius, num_around, num_stacked,
									height, lids, lid_step)

cylinder_for_mesh <- GetCylinder(x, y, radius, num_around, num_stacked, height,
                                 lids, lid_step)
library(rgl)
plot3d(cylinder_for_mesh)


# ______________________________________ save full cylinder for meshing ________
write.csv(cylinder_for_mesh,
          file="~/Desktop/cylinder_rad0.25_height10_withlids.xyz",
          row.names = FALSE)

