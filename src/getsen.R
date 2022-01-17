library(sen2r)
library(sf)
aoi = st_read("~/edu/courses_src/geoinfo/data/data_lev0/harz.geojson")
time = as.Date(c("2018-04-10", "2018-10-30"))
s2_list = s2_list(spatial_extent = aoi,
                  time_interval = time,
                  max_cloud = 10)

print(s2_list)
as.vector(sort(sapply(names(s2_list), function(x) {
  strftime(safe_getMetadata(x,"nameinfo")$sensing_datetime)
})))

s2_download(s2_list[c(1,2,4)], outdir = "d:/07_Paper3")
sen2r::
  
dir.create("sen2r_out_1") # output folder
dir.create("sen2r_safe")
out_dir_1="sen2r_out_1/"
safe_dir ="sen2r_safe/"

myextent_1 <- system.file("extdata/vector/barbellino.geojson", package = "sen2r") 

library(sen2r)
out_paths_1 <- sen2r(
  gui = FALSE,
  step_atmcorr = "l2a",
  extent = myextent_1,
  extent_name = "Barbellino",
  timewindow = c(as.Date("2020-11-13"), as.Date("2020-11-25")),
  list_prods = c("BOA","SCL"),
  list_indices = c("NDVI","MSAVI2"),
  list_rgb = c("RGB432B"),
  mask_type = "cloud_and_shadow",
  max_mask = 10, 
  path_l2a = safe_dir,
  path_out = out_dir_1
)
getwd()
