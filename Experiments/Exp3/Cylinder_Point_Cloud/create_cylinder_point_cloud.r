validate_get_cylinder_args <- function(radius, height, num_around, num_stacks,
                                       lid_step) {
    #   Checks to make sure inputs to get_cylinder are logical.
    #   helper function to get_cylinder()

    #   Args:
    #       radius (double): ............ cylinder radius
    #       height (double): ............ cylinder height
    #       num_around (int): ........... number of points around circle
    #       num_stacks (int): ........... number of circles to be stacked
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
    # num_stacks must be at least 2
    if (num_stacks < 2)
        stop("num_stacks MUST be at least 2.")
    # lid step must be small and positive
    if (lid_step < 0 || lid_step >= 1)
        stop("lid_step MUST be small positive step size.")
}


get_circle <- function(radius, num_around) {
    #   Gets x, y coordinates for a circle with radius and number of points.
    #   helper function to get_cylinder()

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


get_cylinder <- function(radius=1, height=10, num_around=25, num_stacks=10,
                         lids=TRUE, lid_step=0.2) {
    #   Creates cylinder point cloud with various options.

    #   Args:
    #       radius (double): ... radius for the cylinder base
    #	    num_aroud (int): ... number of vertices in circle to stack
    #	    num_stacks (int): .. number of circles we stack to make the cylinder
    #	    height (double): ... height of the cylinder
    #	    lids (boolean): .... closed cylinder if TRUE, open pipe if FALSE
    #	    lid_step: .......... step size for lid radius change
    #
    #   Returns:
    #       The point cloud cylinder is returned as a data frame x, y, z.

    # validate arguments given before building cylinder
    validate_get_cylinder_args(radius, height, num_around, num_stacks, lid_step)

    # get circle for specified radius and discretization number
    circle_base <- get_circle(radius, num_around)

    # set the ground as 0
    ground <- 0

    # discretize z dimension from ground 0 to top of cylinder
    z_steps <- seq(from <- ground, to <- height,
                    by <- ((height - ground) / (num_stacks - 1)))

    # initialize empty data frame for holding our cylinder of stacked circles
    cylinder <- c()

    # build cylinder by stacking circles
    for (z in z_steps) {
        # if it's our bottom circle or top circle, and lids is TRUE,
        # we have to fill it in creating a circular plane, not a hollow circle
        if ((lids == TRUE && z == ground) ||
            (lids == TRUE && z == height)) {

            # get sequence of diverging radius so we can create the lids
            radii <- seq(radius, 0, -lid_step)

            # loop through radius in seq and create top or bottom cylinder
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


plot_cylinder <- function(cylinder, color) {
    #   Uses rgl to plot cylinder
    #
    #   Args:
    #       cylinder(3d point cloud): ........ the cylinder from get_cylinder()
    #       color (string): .................. color for points
    #   Returns:
    #       Rgl opens a plot window and displays the point cloud.

    # rgl for plot3d
    require(rgl)

    # plot
    rgl::open3d()
    rgl::plot3d(cylinder, col = color)
}


save_cylinder <- function(cylinder, file_name) {
    write.csv(cylinder,
            file = file_name,
            row.names = FALSE)
}


main <- function() {
    # build cylinder using default arguments
    print("Building cylinder with defaults")
    cylinder_default <- get_cylinder()
    # build cylinder with surface area of 16.1
    radius <- 0.25
    height <- 10
    num_around <- 360
    num_stacks <- 1000
    print("Building cylinder with radius 0.25 and height = 10")
    cylinder_for_mesh <- get_cylinder(radius, height, num_around, num_stacks, 
                                      lid_step = 0.02)

    print("Plotting cylinders")
    # plot cylinder with default arguments
    plot_cylinder(cylinder_default, "blue")
    # plot cylinder with surface area of 16.1
    plot_cylinder(cylinder_for_mesh, "red")


    # save cylinder with surface area of 16.1 radius=0.25 height=10
    print("Saving cylinder point cloud")
    save_path <- paste("Experiments/Data/",
                       "cylinder.xyz", sep = "")

    save_cylinder(cylinder_for_mesh, save_path)
}


# ______________________________________________________________________________
main()
