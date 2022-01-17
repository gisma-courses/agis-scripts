get_vrt_img = function(name,path,tag,rasterStack = TRUE) {
  fList = list.files(path, 
                     pattern = glob2rx(paste0("*",tag,".tif")),
                     full.names = TRUE)
  
  res = invisible(gdalUtils::mosaic_rasters(gdalfile = fList,
                            output_Raster = TRUE,
                            dst_dataset = paste0(path,name,".tif"),verbose = FALSE))
  
  if (rasterStack) return(raster::stack(paste0(path,name,".tif")))
  
}
