# TODO: add shebangg
import numpy as np
import open3d as o3d


if __name__ == "__main__":
    # set cylinder surface area for height = 10, radius = 0.25
    CYLINDER_SURFACE_AREA = 16.1

    # read in point cloud for cylinder
    file = ("Experiments/Data/cylinder_r.25_h10_normals_" +
                     "point_cloud.ply")
    # file = ("/Users/hackattendant/Desktop/FuelsCraft/R_Meshing_Playground/tip_ply.ply")

    # create point cloud object from cylinder file
    pcd = o3d.io.read_point_cloud(file)

    # # visualize
    # o3d.visualization.draw_geometries([pc], point_show_normal=True)

    #TODO: add cpu time output
    # mesh point cloud cylinder using alpha shape approach
    alpha = 0.5
    print(f"alpha={alpha:.3f}")
    # with o3d.utility.VerbosityContextManager(
    #     o3d.utility.VerbosityLevel.Debug) as cm:
    #     alpha_mesh = o3d.geometry.TriangleMesh \
    #                             .create_from_point_cloud_alpha_shape(pc, 
    #                                                                 alpha)

    mesh = o3d.geometry.TriangleMesh.create_from_point_cloud_alpha_shape(pcd, alpha)

    

    # view alpha shape mesh
    o3d.visualization.draw_geometries([alpha_mesh])

    # get surface area of alpha triangle mesh
    alpha_surface_area = o3d.geometry.TriangleMesh.get_surface_area(alpha_mesh)

    print("Alpha shape surface area: {alpha_surface_area}")