#------------------------------------------------------------------------------
# Type: control script
# Name: 30_calculate_addon_predictors.R
# Author: Chris Reudenbach, creuden@gmail.com
# Description:  derives LAD , albedo, transmissivity
# Data: point cloud dem chm sentinel 2 bands 2,3,4,8
# Output: predictor stack
# Copyright: GPL (>= 3)
#------------------------------------------------------------------------------
### 

# typical setup here

#####

# 2 - define variables
#---------------------
fn = "5-25_MOF_rgb"
## ETRS89 / UTM zone 32N
epsg = 25832

# we need a segmented pointcloud if working on tree level
# trees=lidR::readLAS(file.path(envrmt$path_sapflow,"sapflow_tree_segments_multichm_dalponte2016.las"))

hulls_sf=st_read(file.path(envrmt$path_l_raster,"segmentation_small3.shp"))



# calculationg and extracting some values  per tree
species_hulls = species_ex %>% group_by(treeID) %>%
  dplyr::summarize(species_median = median(value, na.rm=TRUE),
                   species_mode = modeest::mlv(value, method='mfv'))
species_hulls = inner_join(hulls_sf,species_hulls)
species_sf=species_hulls[,c("treeID","ZTOP","zmax","zmean","zsd","zskew","species_mode","area")]
st_write(species_hulls,file.path(envrmt$path_level1,"sapflow_tree_segments_multichm_dalponte2016_species.gpkg"),append=FALSE)
#species_hulls = st_read(file.path(envrmt$path_sapflow ,"sapflow_tree_segments_multichm_dalponte2016_species.gpkg"))


# GAP and transmittance metrics
# https://www.isprs.org/proceedings/xxxvi/3-W52/final_papers/Hopkinson_2007.pdf
# https://link.springer.com/content/pdf/10.1007%2F978-94-017-8663-8_20.pdf
#  http://doi.org/10.1016/j.rse.2014.10.004
gap_tree = tree_metrics(trees, func = ~lidR::gap_fraction_profile(Z))
gap_tree$gf = gap_tree$gf * 0.8 # 0.8 transmittance coefficient
plot(gap_tree)

# calculate the LAD metrics for the derived trees
# review vertical LAI ditribution http://dx.doi.org/10.3390/rs12203457
lad_tree = tree_metrics(trees, func = ~LAD(Z))
lt=st_drop_geometry(as(lad_tree,"sf"))
lad_trees = inner_join(lt,species_sf)
tmp = lt %>%
  group_by(treeID,z) %>%
  spread(z,lad,fill = 0,sep = "_")
lad_trees =  inner_join(tmp,lad_trees)

# read sentinel data
lai = raster(paste0(envrmt$path_sapflow,"2021-06-13-00:00_2021-06-13-23:59_Sentinel-2_L1C_Custom_script.tiff"))
albedo = raster(paste0(envrmt$path_sapflow,"S2B2A_20210613_108_sapflow_BOA_10_albedo.tif"))


## extract the values
lai_ex = exactextractr::exact_extract(lai, hulls_sf,  force_df = TRUE,
                                      include_cols = "treeID")
lai_ex = dplyr::bind_rows(lai_ex)
lai_ex$coverage_fraction=NULL
names(lai_ex)=c("treeID","lai")
alb_ex = exactextractr::exact_extract(albedo, hulls_sf,  force_df = TRUE,
                                      include_cols = "treeID",)
alb_ex = dplyr::bind_rows(alb_ex)
alb_ex$coverage_fraction=NULL
names(alb_ex)=c("treeID","albedo")
l_trees =  inner_join(lai_ex,lad_trees)
a_trees = inner_join(alb_ex,l_trees)

# optional and  pretty trickky this part
# make mean of all unique treeIds and provide vertical LAD
tree = a_trees %>% group_by(treeID) %>%
  mutate_all(.funs = mean) %>%
  distinct(.keep_all = TRUE)
tree$geom =NULL
t_lad=tree
tree_clust = tree[,c("treeID","albedo","lai","zmax","zmean","zskew","species_mode")]

tree_tmp = hulls_sf[ , names(hulls_sf) %in% c("treeID","geom")]
t=inner_join(tree_clust,tree_tmp)
tree_clust_sf=st_as_sf(t)
mapview(tree_clust_sf,zcol="albedo")
#t=st_drop_geometry(trees_sf)
header_height=seq(2.5,44.5,1)
lad= tree %>% distinct(across(contains(".5")))
norm1 = (lad[][2:length(lad)] - min(lad[][2:length(lad)] ))/ (max(lad[][2:length(lad)])-min(lad[][2:length(lad)]))
lad_spl = by(norm1, seq_len(nrow(norm1)), function(row) {try(smooth.spline(header_height, row,spar = 0.1))
})

xx = unique(sort(c(seq(4, 40, by = 4))))
lad_pred = lapply(lad_spl, function(x){predict(x, xx)[[2]]})
lad_pred = abs(as.data.frame(do.call(rbind, lad_pred)))
names(lad_pred)= paste0("lev_",xx)
lad_pred$treeID = lad$treeID

trees_all = inner_join(lad_pred,tree_clust)
trees_all =inner_join(trees_all,tree_tmp )
trees_all_sf=st_as_sf(trees_all)

st_write(trees_all_sf,file.path(envrmt$path_sapflow,"sapflow_tree_all_sf.gpkg"),append = FALSE)