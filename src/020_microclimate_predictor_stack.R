#
# Name: 20_microclimate_predictor_stack.R
# Type: control script 
# Description:  creates a predictor stack derive raster/tree segment based indices
#               for microclimate modelling
#               canopy height model, digital elevation models and standard statistics
#         - irridiance (r.sun.hourly)
#         - topographic information (based on dem): elevation, slope, aspect, TPI
#         - forest structure (based on lidar): chm, ipground, metrics, 
#                            tree clusters based on LAD and metrics
#                            gridmetrics
#         (- Optional: Distance to water bodies and forest edges (from land use classification))
#         (- Optional: Sentinel Time series from NDVI, LAI and Albedo etc.)
# Input: las_file (optional Sentinel)
# Output: - las catalog "ctg.rds"
#         - single layers as mentioned
#         - prediction stack with all layers "pred_forest_structure.tif"
# Copyright: GPL (>= 3),Lena Perzlmaier, Chris Reudenbach, creuden@gmail.com 
# Date: 2022-01-24
#

# ----project setup----
require(envimaR)
# MANDANTORY: defining the root folder DO NOT change this line
root_folder = "~/edu/courses/misc/MicroclimateMOF"
# work around for a subproject folder
prefix = "MOF_lidar_2018"

# define  additional packages comment if not needed
appendpackagesToLoad = c("lidR","future","lwgeom","tmap")

# define additional subfolders comment if not needed
tmpPath = paste0("data/lidar/",prefix,"/") 
appendProjectDirList =  c(paste0(tmpPath,"gmetrics"),paste0(tmpPath,"tmetrics"),
                          paste0(tmpPath,"dem"),paste0(tmpPath,"dsm"),
                          paste0(tmpPath,"chm"),tmpPath)


# MANDANTORY: calling the setup script also DO NOT change this line
source(file.path(envimaR::alternativeEnvi(root_folder = root_folder),"src/000_setup.R"),local = knitr::knit_global())


# ---additional variables----
# logical switch 
# if TRUE the calculation is performed 
# if FALSE the results are loaded into memory
calculate = TRUE
# GRASS irridiance calculation
sol = FALSE
# read only forest structure stack 
fs_only = TRUE

#- define current projection ETRS89 / UTM zone 32N
proj4 = "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"

epsg_number = 25832

#- tmap options
tmap_options(check.and.fix = TRUE) 

# catalog number of cores 
cores = 16

# resolution for catalog operations
res = 1.0

#- setup parallelisation 
future::plan(multisession, workers = cores)
set_lidr_threads(cores)

#
# ----processing----
#
if (calculate){
  #--- Load data
  ctg = lidR::readLAScatalog(envrmt$path_lidar_org)
  
  # define options
  lidR::opt_chunk_size(ctg) = 500
  lidR::opt_chunk_buffer(ctg) = 50
  lidR::opt_output_files(ctg) = file.path(envrmt$path_MOF_lidar_2018, "/ctg","mof_{ID}")
  projection(ctg) = epsg_number
  lidR::opt_progress(ctg) = TRUE
  lidR::opt_laz_compression(ctg) = TRUE
  ctg@output_options$drivers$Raster$param$overwrite = TRUE
  ctg@output_options$drivers$Vector$param$overwrite = TRUE
  plot(ctg)  
  
  ## ----1. calculate dem------- 
  # source: https://github.com/Jean-Romain/lidR/wiki/Rasterizing-perfect-canopy-height-models
  lidR::opt_output_files(ctg) = paste0(envrmt$path_dem,"/{ID}","_dem")
  dem = grid_terrain(ctg, res = res,lidR::knnidw(k = 6L, p = 2))
  
  ## calculate further parameters based on dsm
  # ----2. slope----
  slope <- terrain(dem, opt = "slope")
  
  # ---- 3. aspect----
  aspect <- terrain(dem, opt = "aspect", unit = "degrees")
  
  # ----4. topographic position index----
  TPI <- terrain(dem, opt = "TPI")
  
  # ----5. topographic position index simple----
  TPI_simple <- cut(TPI, breaks = c(TPI@data@min,0,TPI@data@max))    # simplify TPI because here it is only relevant whether it is positive or negative
  #plot(TPI_simple)
  
  # stack topographic information
  topo <- raster::stack(dem, slope, aspect, TPI_simple)
  names(topo) <- c("dtm", "slope", "aspect", "TPI")
  
  writeRaster(topo,  file.path(envrmt$path_dem,"/pred_topo.tif"), overwrite = TRUE)
  saveRDS(topo, file.path(envrmt$path_dem,"/pred_topo.rds"))
  topo <- readRDS(file.path(envrmt$path_dem,"/pred_topo.rds"))
  
  
  # ----7. calculate dsm----
  lidR::opt_output_files(ctg) <- paste0(envrmt$path_dsm,"/{ID}","_dsm") # add output filname template
  dsm = lidR::grid_canopy(ctg, res = res, algorithm = pitfree())
  
  #- normalize height (point cloud)
  lidR::opt_output_files(ctg) = paste0(envrmt$path_MOF_lidar_2018,"/norm/{ID}","_norm_height")
  ctg = lidR::normalize_height(ctg,lidR::knnidw(k = 6L, p = 2))
  
  #- ----8. calculate chm----
  lidR::opt_output_files(ctg) = paste0(envrmt$path_chm,"/{ID}","_chm")
  chm = grid_canopy(ctg, res = res, lidR::pitfree())
  
  # ----9. tree segmentation based on the point cloud using catalog----
  # source: https://gis.stackexchange.com/questions/364546/individual-tree-segmentation-workflow-with-lidr-package
  ttops_chm = find_trees(chm, lmf(8))
  algo1 = dalponte2016(chm, ttops_chm,th_seed = 0.55,th_tree = 0.65,max_cr = 8)
  algo2 = silva2016(chm, ttops_chm,exclusion = 0.5)
  algo3 = li2012(hmin = 5, R = 5)
  
  #- catalog modifications
  opt_output_files(ctg) = paste0(envrmt$path_MOF_lidar_2018,"/seglas/HULL_{XCENTER}_{YCENTER}")
  opt_chunk_size(ctg) = 0
  opt_filter(ctg) = "filter_noise(ctg, sensitivity = 1.2, res = 5)"
  future::plan(multisession, workers = 6L)
  set_lidr_threads(6L)
  #- tree segmentation using the 99 percentile filter
  ctg = lidR::segment_trees(ctg,  algo1 , uniqueness = "bitmerge")
  #laz = do.call(rbind, lapply(list.files(paste0(envrmt$path_level2,"/",prefix), pattern = "*.laz", full.names = TRUE),readLAS))
  saveRDS(ctg,paste0(envrmt$path_MOF_lidar_2018,"/ctg.rds"))
  
  
  #- tree filtering and calculate hull 
  opt_filter(ctg) = "!is.na(treeID)"
  ctg@output_options$drivers$Spatial$extension = ".shp"
  opt_output_files(ctg) = paste0(envrmt$path_MOF_lidar_2018,"/seg/HULL_{XCENTER}_{YCENTER}")
  hulls = catalog_apply(ctg = ctg, FUN = tree_fn)
  
  # ----10. tree segements----
  #hulls = list.files(paste0(envrmt$path_level2,"/",prefix), pattern = "*.shp", full.names = TRUE)
  seg = do.call(rbind, lapply(hulls, sf::read_sf))
  sf::st_write(seg,paste0(envrmt$path_MOF_lidar_2018,"/seg/segmentation.shp"),append=F)
  trees = st_read(paste0(envrmt$path_MOF_lidar_2018,"/seg/segmentation.shp"))
  
  #- tmap plot
  #tmap_mode("view")
  #tm_shape(seg) + tm_fill(col = "zq95") 
  #mapview(seg,zcol="zq95", fgb = FALSE)
  
  # ----11. gridmetrics----
  # NOTE: the metrics is performed on the manipulated (see above) ctg! 
  # So the results will differ from the raw lasfile/ctg
  lidR::opt_output_files(ctg) = paste0(envrmt$path_gmetrics,"/{ID}","_metrics")
  metrics = grid_metrics(ctg, .stdmetrics, res)
  # saveRDS(names(metrics),paste0(envrmt$path_gmetrics,"/names_grid_metrics.rds"))
  # "zmax"         "zmean"        "zsd"          "zskew"        "zkurt"       
  # [6] "zentropy"     "pzabovezmean" "pzabove2"     "zq5"          "zq10"        
  # [11] "zq15"         "zq20"         "zq25"         "zq30"         "zq35"        
  # [16] "zq40"         "zq45"         "zq50"         "zq55"         "zq60"        
  # [21] "zq65"         "zq70"         "zq75"         "zq80"         "zq85"        
  # [26] "zq90"         "zq95"         "zpcum1"       "zpcum2"       "zpcum3"      
  # [31] "zpcum4"       "zpcum5"       "zpcum6"       "zpcum7"       "zpcum8"      
  # [36] "zpcum9"       "itot"         "imax"         "imean"        "isd"         
  # [41] "iskew"        "ikurt"        "ipground"     "ipcumzq10"    "ipcumzq30"   
  # [46] "ipcumzq50"    "ipcumzq70"    "ipcumzq90"    "p1th"         "p2th"        
  # [51] "p3th"         "p4th"         "p5th"         "pground"      "n"           
  # [56] "area" 
  saveRDS(metrics,paste0(envrmt$path_gmetrics,"/grid_metrics.rds"))
  metrics = readRDS(paste0(envrmt$path_gmetrics,"/grid_metrics.rds"))
  
  #--- carry out a cluster analysis on the spatial units of the tree hulls. 
  # basically using the metrics of lidar plus the LAD profile
  # this is more stable than on grid metrics. 
  # the cluster class is then used as a prediction layer
  # 
  # ----12. LAD metrics for the derived trees----
  # review vertical LAI ditribution http://dx.doi.org/10.3390/rs12203457
  lidR::opt_output_files(ctg) = paste0(envrmt$path_tmetrics,"/{ID}","_lad")
  lad_tree = tree_metrics(ctg, func = ~LAD(Z))
  lad_tree = list.files(paste0(envrmt$path_tmetrics), pattern = "*_lad.shp", full.names = TRUE)
  lad = do.call(rbind, lapply(lad_tree, sf::read_sf))
  sf::st_write(lad,paste0(envrmt$path_tmetrics,"/lad.shp"),append=F)
  lad = sf::st_read(paste0(envrmt$path_tmetrics,"/lad.shp"))
  # GAP and transmittance metrics (optional)
  # https://www.isprs.org/proceedings/xxxvi/3-W52/final_papers/Hopkinson_2007.pdf
  # lidR::opt_output_files(ctg) = paste0(envrmt$path_level2,"/",prefix,"/{ID}","_gap")
  # gap_tree = tree_metrics(ctg, func = ~lidR::gap_fraction_profile(Z))
  # gap_tree$gf = gap_tree$gf * 0.8
  # gap_tree = list.files(paste0(envrmt$path_level2,"/",prefix), pattern = "*_gap.shp", full.names = TRUE)
  # gap = do.call(rbind, lapply(gap_tree, read_sf))
  # st_write(gap,paste0(envrmt$path_level2,"/",prefix,"/gap.shp"),append=F)
  
  # join trees and lad
  tmp = sf::st_drop_geometry(as(lad,"sf"))
  lad_trees = inner_join(tmp,trees,by = c("treeID"="treeID"))
  # duplicate bitmerge ID
  lad_trees$tree_ID = lad_trees$treeID
  
  # transpose lad takes quite a while
  trees_lad = lad_trees %>%
    group_by(treeID,z) %>%
    mutate(treeID = 1:n()) %>%
    spread(z,lad,fill = 0,sep = "_")
  # drop geometry
  trees_lad = trees_lad[,  !(names(trees_lad) %in% "geometry")]
  ##--- remove rows with NA
  trees_lad = trees_lad[complete.cases(trees_lad) ,]
  #treeID = trees_lad$tree_ID
  saveRDS(trees_lad,paste0(envrmt$path_MOF_lidar_2018,"/trees_lad_raw.rds"))
  trees_lad = readRDS(paste0(envrmt$path_MOF_lidar_2018,"/trees_lad_raw.rds"))
  tmpID=trees_lad$tree_ID
  
  # clean df  
  ##--- check based on a small partion near zero values
  ids = createDataPartition(trees_lad$treeID,list = FALSE,p = 0.5)
  test =  trees_lad[ids,]
  nzv = nearZeroVar(test)
  if (length(nzv) > 0) 
    trees_lad = trees_lad[, -nzv]
  
  # drop empty cols
  trees_lad=trees_lad[ , colSums(is.na(trees_lad)) == 0]
  
  ##--- filter correlations that are > cor_cutoff
  filt = findCorrelation(cor(trees_lad), cutoff = 0.9)
  trees_lad = trees_lad[,-filt]
  
  ##- re-read IDs
  trees_lad$tree_ID = tmpID
  saveRDS(trees_lad,paste0(envrmt$path_MOF_lidar_2018,"/clean_trees_lad_raw.rds"))
  trees_lad = readRDS(paste0(envrmt$path_MOF_lidar_2018,"/clean_trees_lad_raw.rds"))
  
  # create header to drop  fantasy trees
  header_height=paste0("z_",seq(44.5,100.5,1))
  # define df for clustering without the following attributes
  data_clust = trees_lad[ ,!(names(trees_lad) %in% c("tree_ID","treeID","XTOP","YTOP","n","area","x","y",header_height))]
  
  # ----13. tree cluster----
  # brute force data sampling 
  data = sample_n(data_clust,floor(nrow(trees_lad)* 0.05))
  
  # We will cluster with the PCA results this is much faster and yields the same results
  pca_dat = stats::princomp(data)$scores[, 1:10]
  
  # highspeed all clusternumbers below threshold 0.85 are ok but prefer stable results
  opt = ClusterR::Optimal_Clusters_KMeans(pca_dat, max_clusters = 25, plot_clusters = T,num_init = 50,
                                          criterion = 'distortion_fK',
                                          initializer = 'kmeans++',
                                          seed = 123)
  saveRDS(opt,paste0(envrmt$path_MOF_lidar_2018,"/opt.rds"))
  
  # kmeans clustering with the number of clusters as derived by opt
  # this is difficult to automize due to the effect that I prefer the cutoff level
  # after the first stable grow of the threshold value the basically the best value 
  # is two due to build up and non build up areas than we see in all runs a stable 
  # grow up to roughly < 15 clusters with a values > .85  clusters
  km_arma = ClusterR::KMeans_arma(data_clust, clusters = 10, n_iter = 500,
                                  seed_mode = "random_subset",
                                  verbose = T, CENTROIDS = NULL)
  
  # prediction on all data
  trees_lad$cluster = as.integer(ClusterR::predict_KMeans(data_clust, km_arma))
  class(trees_lad$cluster)="integer"
  
  # join the results to sf object
  t_cluster = inner_join(trees_lad[,names(trees_lad) %in% c("tree_ID","cluster"),],trees,by = c("tree_ID"="treeID"))
  t_cluster = t_cluster[,  !(names(t_cluster) %in% "geometry")]
  t_cluster = t_cluster[complete.cases(t_cluster) ,]
  tree_clust_sf = st_as_sf(t_cluster,coords = c("x", "y"), crs = epsg_number)
  sf::st_write(tree_clust_sf,file.path(paste0(envrmt$path_gmetrics,"/tree_cluster.gpkg")), append= FALSE)
  saveRDS(t_cluster,file.path(paste0(envrmt$path_gmetrics,"/tree_cluster.rds")))
  
  ## ----14. voronoi tesselation of the tree stands----
  t_clust <- vect(tree_clust_sf)
  # calculate voronoi
  t_voronoi <- terra::voronoi(t_clust)
  # reconvert it
  sf_voronoi=st_as_sf(t_voronoi)
  # mapview(sf_voronoi,zcol="cluster")
  sf::st_write(sf_voronoi,file.path(paste0(envrmt$path_tmetrics,"/sf_voronoi.gpkg")), append= FALSE)
  saveRDS(sf_voronoi,file.path(paste0(envrmt$path_tmetrics,"/sf_voronoi.rds")))
  
  ## ----15. final voronoi tree clusters----
  # depreceated use tree clusters from envimetR/src/90_tree_cluser_analysis
  # tree_clust_sf <- st_read(file.path(envrmt$path_level2,"sapflow_tree_all_cluster_sf.gpkg"))
  
  tree_clust_rast <- gdalUtils::gdal_rasterize(src_datasource = paste0(envrmt$path_tmetrics,"/sf_voronoi.gpkg"),
                                               dst_filename = file.path(envrmt$path_tmetrics,"/all_seg.tif"),
                                               a = c("cluster"),
                                               l = "sf_voronoi",
                                               a_nodata = 0,
                                               a_srs = proj4,
                                               tr = c(res*1,res*1),
                                               te = c(chm@extent[1]  , 
                                                      chm@extent[3],
                                                      chm@extent[2],
                                                      chm@extent[4]),
                                               output_Raster = TRUE
  )
  
  tree_clust_rast <- raster::stack(file.path(envrmt$path_tmetrics, "/all_seg.tif"))
  # plot(tree_clust_rast)
  
  ## ----16. ipground----
  # I am interested in  "ipground" , which is the percentage of intensity returned by points classified as "ground"
  # plot "ipground"
  # function for calculation only ipground (copied from lidR sourcecode) 
  # metrics <- c(metrics, list(ipground = sum(i[class == 2])/itot*100))
  # class == 2 are ground points, class == 1 are above ground points
  lidR::opt_output_files(ctg) = paste0(envrmt$path_tmetrics,"/{ID}","_ip_ground")  
  ip_ground <- grid_metrics(ctg, ~sum(Intensity[Classification == 2])/sum(Intensity)*100, res = res)
  saveRDS(ip_ground, file.path(envrmt$path_tmetrics,"/ip_ground.rds"))
  ip_ground=readRDS(file.path(envrmt$path_tmetrics,"/ip_ground.rds"))
  
  
  
  # ----18. stack forest structure----
  forest_structure <- raster::stack(chm, ip_ground)
  forest_structure <- raster::crop(forest_structure, tree_clust_rast)
  forest_structure <- raster::stack(forest_structure, tree_clust_rast)
  names(forest_structure) <- c("CHM", "ip_ground", "cluster")
  names(metrics) = names_metrics
  forest_structure = stack(forest_structure, metrics)
  
  saveRDS(forest_structure,file.path(envrmt$path_MOF_lidar_2018, "/pred_forest_structure.rds"))
  raster::writeRaster(forest_structure, file.path(envrmt$path_MOF_lidar_2018, "/pred_forest_structure.tif"), overwrite = TRUE,progress="text")
  forest_structure <- raster::stack(file.path(envrmt$path_MOF_lidar_2018, "/pred_forest_structure.tif"))
  
  ## ----19 calculate irradiance----
  if (sol)
  {
    # irradiance is calculated with the GRASS GIS function r.sun.hourly
    # if (sol)
    {# Better the DSM
      # in order to be able to access GRASS from R, Rstudio needs to be opened  from osgeo4w command shell with
      # "C:\Program Files\RStudio\bin\rstudio.exe"
      
      #Sys.getenv("PROJ_USER_WRITABLE_DIRECTORY")
      #Sys.setenv("PROJ_SUER_WRTIABLE_DIRECTORY" = "C:\\OSGEO4W64\\share\\proj")
      
      
      link2GI::linkGRASS7(dem, gisdbase = root_folder, location = "MOF2")
      # rgrass7::initGRASS(gisBase = "C:/OSGEO4W64/apps/grass/grass78",
      #                    gisDbase = "D:/Uni_Marburg/Arbeit/data/GRASS",
      #                    location = "MOF",
      #                    mapset = "PERMANENT",
      #                    override = TRUE)
      
      # rgrass7::execGRASS('g.proj',
      #                    flags = "p")
      # 
      # import dtm
      rgrass7::execGRASS("r.in.gdal", 
                         flags = "o",
                         parameters = list(
                           input = file.path(envrmt$path_level2, "dem.tif"),
                           output = "dtm"
                         ))
      
      rgrass7::execGRASS("r.info", 
                         parameters = list(map = "dtm"))
      
      # import topo pred
      rgrass7::execGRASS("r.in.gdal", 
                         flags = "o",
                         parameters = list(
                           input = file.path(envrmt$path_level2, "pred_topo.tif"),
                           output = "pred_topo"
                         ))
      
      rgrass7::execGRASS("r.info", 
                         parameters = list(map = "pred_topo.1"))
      
      # set extent of the region to the extent of the dtm
      rgrass7::execGRASS("g.region", 
                         parameters = list(raster = "dtm"))
      
      # install r.sun.hourly
      rgrass7::execGRASS("g.extension",
                         parameters = list(extension = "r.sun.hourly"))
      
      
      # calculate hourly irradiance for the whole study period
      for (i in 151:274) {
        
        output <- paste("tot_rad_", i, sep = "")
        rgrass7::execGRASS("r.sun.hourly", flags=c("overwrite", "quiet"), elevation="pred_topo.1", aspect="pred_topo.3", slope="pred_topo.2", mode="mode1",
                           start_time=0, end_time=23, day=i, year=1900, glob_rad_basename=output)
      }
      
      # the resulting files are not stored as rasters but in a GRASS format
      # for further analysis the radiation files will be loaded indivdually as rasters when needed
    }
  }
  
} else {
  if (fs_only) 
  {
    forest_structure = readRDS(file.path(envrmt$path_MOF_lidar_2018, "/pred_forest_structure.rds"))
  } else {
    forest_structure = readRDS(file.path(envrmt$path_MOF_lidar_2018, "/pred_forest_structure.rds"))    
    trees_cluster = sf::st_read(file.path(paste0(envrmt$path_MOF_lidar_2018,"/tree_cluster.gpkg")))
    dem = raster::raster(paste0(envrmt$path_dem,"/grid_terrain.vrt"))
    dsm = raster::raster(paste0(envrmt$path_dsm,"/grid_terrain.vrt"))
    chm = raster::raster(paste0(envrmt$path_chm,"/grid_canopy.vrt"))
    ctg = readRDS(paste0(envrmt$path_MOF_lidar_2018,"/ctg.rds"))
    trees = st_read(paste0(envrmt$path_MOF_lidar_2018,"/seg/segmentation.shp"))
    metrics = raster::stack(paste0(envrmt$path_gmetrics,"/grid_metrics.vrt"))}
}