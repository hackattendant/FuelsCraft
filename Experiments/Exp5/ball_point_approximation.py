# TODO: add shebangg
import numpy as np
import open3d as o3d


if __name__ == "__main__":
    # set cylinder surface area for height = 10, radius = 0.25
    CYLINDER_SURFACE_AREA = 16.1

    # read in point cloud for cylinder
    cylinder_file = ("Experiments/Data/cylinder_r.25_h10_normals_" +
                     "point_cloud.ply")

    # create point cloud object from cylinder file
    pc = o3d.io.read_point_cloud(cylinder_file)

    # # visualize
    # o3d.visualization.draw_geometries([pc], point_show_normal=True)
#TODO: add cpu time output
    # set radii for various ball sizes
    radii = [0.005, 0.01, 0.02, 0.04]

    with o3d.utility.VerbosityContextManager(
        o3d.utility.VerbosityLevel.Debug) as cm:
        # perform ball pivoting using bpa algorithm
        bpa_mesh = o3d.geometry.TriangleMesh \
                    .create_from_point_cloud_ball_pivoting(
                        pc, o3d.utility.DoubleVector(radii))

    # surface area of ball pivoting method
    bpa_surface_area = bpa_mesh.get_surface_area()
    print("Ball Pivoting surface area: {bpa_surf_area}")
