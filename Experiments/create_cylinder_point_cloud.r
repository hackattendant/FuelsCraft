validate_get_cylinder_args <- function(radius, height, num_around, num_up,
                                       lid_step) {
    #   Checks to make sure inputs to get_cylinder are logical

    #   Args:
    #       radius (double): ............ cylinder radius
    #       height (double): ............ cylinder height
    #       num_around (int): ........... number of points around circle
    #       num_up (int): ............... number of circles to be stacked
    #
    #   Returns:
    #       The program is stopped if an input is found to be invalid.

    # must have a radius
    if (radius <= 0)
        stop("radius MUST be greater than 0.")
    # must have a height
    if (height <= 0)
        stop("height MUST be greater than 0.")
    # must have at least 3 points to create surfae
    if (num_around < 3)
        stop("num_around MUST be at least 3.")
    # num_up must be at least 2
    if (num_up < 2)
        stop("num_up MUST be at least 2.")
    # lid step must be negative to converge on center point
    if (lid_step > 0)
        stop("lid_step MUST be negative step size.")
}


get_circle <- function(radius, num_around) {
    #   Gets x, y coordinates for a circle with radius and number of points.

    #   Args:
    #       radius (double):  ..... cylinder radius
    #       num_around (int): ..... number of points to discretize circle into
    #
    #   Returns:
    #       The coordinates of the circle are returned as a data frame with x, y

    # get angle step for chopping circle into number of circle points
    angle_step <- 2 * pi / num_around
    # get sequence of angles for points around circle
    angles <- seq(0, 2 * pi - angle_step, by = angle_step)

    # empty circle dataframe to hold points
    circle <- c()
    # get coordinates for circle with radius, num_points, centered at (0,0)
    circle$x <- cos(angles) * radius
    circle$y <- sin(angles) * radius

    return(circle)
}


get_cylinder <- function(radius=1, height=10, num_around=25, num_up=10,
                         lids=TRUE, lid_step=-0.2) {
    #   Creates cylinder point cloud with various options.

    #   Args:
    #       radius (double): ... radius for the cylinder base
    #	    num_aroud (int): ... number of vertices in circle to stack
    #	    num_up (int): ...... number of circles we stack to make the cylinder
    #	    height (double): ... height of the cylinder
    #	    lids (boolean): .... closed cylinder if TRUE, open pipe if FALSE
    #	    lid_step: .......... step size for lid radius change
    #
    #   Returns:
    #       The point cloud cylinder is returned as a data frame x, y, z.

    # validate arguments given before building cylinder
    validate_get_cylinder_args(radius, height, num_around, num_up, lid_step)

    # ________________________________________________________ build cylinder __

    # get circle for stacking in cylinder
    circle_base <- get_circle(radius, num_around)

    # set the ground as 0
    ground <- 0
    # chop up z dimension so we have height steps from the ground (0) to
    # the top of our cylinder, effectively we are stacking the circles
    z_steps <- seq(from <- ground, to <- height,
                    by <- ((height - ground) / (num_up - 1)))

    # initialize empty data frame for holding our cylinder of stacked circles
    cylinder <- c()
    # build cylinder by stacking circles
    for (z in z_steps) {
        # if it's our bottom circle or top circle, and lids is TRUE,
        # we have to fill it in creating a full cylinder, not just a pipe
        if ((lids == TRUE && z == z_steps[1]) ||
            (lids == TRUE && z == height)) {

            # get sequence of converting radius so we can create the lids
            radii <- seq(radius, 0, lid_step)
            # loop through radii and create lids for cylinder
            for (radius_i in radii) {
                # get points for caps (lids) on cylinder
                circle_lid <- get_circle(radius_i, num_around)
                # append caps to our cylinder data frame point cloud
                cylinder$x <- c(cylinder$x, circle_lid$x)
                cylinder$y <- c(cylinder$y, circle_lid$y)
                cylinder$z <- c(cylinder$z, rep(z, length(circle_lid$x)))
            }

        # if no lids, we just build a pipe
        } else {
            # append circle base to cylinder
            # this is the current circle being stacked, but has no height value

            cylinder$x <- c(cylinder$x, circle_base$x)
            cylinder$y <- c(cylinder$y, circle_base$y)
            # build out z dimension points for the current circle
            # here we are giving our circle a height value and stacking it
            circle_z <- rep(z, num_around)
            # add z dimension to current circle in cylinder
            cylinder$z <- c(cylinder$z, circle_z)
        }
    }

    return(cylinder)
}


main <- function() {
    # rgl for plot3d
    require(rgl)

    # get cylinder with default arguments
    cylinder <- get_cylinder()
    # plot
    rgl::open3d()
    rgl::plot3d(cylinder, col = "blue")

    # set values for custom cylinder
    # We want a cylinder with radius=0.25 and height = 10
    radius <- 0.25          # cylinder radius
    height <- 10            # cylinder height
    num_around <- 360       # number of points around circle for cylinder pipe
    num_stacked <- 720      # number of circles stacked to create cylinder pipe
    lids <- TRUE            # We want full cylinder with lids (top, bottom)
    lid_step <- -0.01       # step size for lids converging on center

    # create cylinder using parameters above
    cylinder_for_mesh <- get_cylinder(radius, height, num_around,
                                        num_stacked, lids, lid_step)
    # plot
    rgl::open3d()
    rgl::plot3d(cylinder_for_mesh, col = "red")

    # save cylinder as csv for meshing
    write.csv(cylinder_for_mesh,
            file = "~/Desktop/cylinder_rad0.25_height10_withlids.xyz",
            row.names = FALSE)
}


# ______________________________________________________________________________
main()
