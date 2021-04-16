# TODO: add shebang

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

    # visualize
    o3d.visualization.draw_geometries([pc], point_show_normal=True)

    # perform poisson surface reconstruction
    poisson_mesh, dens = o3d.geometry.TriangleMesh \
                                     .create_from_point_cloud_poisson(pc)

    # visualize mesh
    o3d.visualization.draw_geometries([poisson_mesh])

    # grab surface area of triangulated mesh
    surface_area_poisson = o3d.geometry.TriangleMesh \
                                       .get_surface_area(poisson_mesh)

    print(f"Surface Area of Mesh: {surface_area_poisson}")
