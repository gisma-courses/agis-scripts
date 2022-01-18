tree_fn <- function(chunk)
{
  attempt_max=5
  for(i in 1:attempt_max){
    las_chunk = try(readLAS(chunk))
    if(!class(las_chunk) == "try-error") i = attempt_max
    else Sys.sleep(5)
  }
  if (is.empty(las_chunk)) return(NULL)
  
  
  # #ht_ws_chunk <- lidR::segment_trees(chunk, li2012(R = 3, speed_up = 5), uniqueness = "incremental")
  # trs_chunk <- lidR::filter_poi(las_chunk, !is.na(treeID))
   hulls_chunk <- lidR::delineate_crowns(chunk, type = "concave", concavity = 3, func = .stdmetrics)
  
   hulls_chunk@data[,c("x","y")] = sp::coordinates(hulls_chunk)
   #remove trees with crowns outside extent
   dat_trs = hulls_chunk@data
   coordinates(dat_trs) = ~x+y
   tile0_ext = as(raster::extent(chunk),"SpatialPolygons")
   in_tile = rgeos::gIntersects(dat_trs, tile0_ext,byid=T)
   hulls_chunk1 = subset(hulls_chunk,subset=as.vector(in_tile))
  return(hulls_chunk)
}


filter_noise = function(chunk, sensitivity, res)
{
  attempt_max=5
  for(i in 1:attempt_max){
    las_chunk = try(readLAS(chunk))
    if(!class(las_chunk) == "try-error") i = attempt_max
    else Sys.sleep(5)
  }
  if (is.empty(las_chunk)) return(NULL)
  
  p99 = grid_metrics(las_chunk, ~quantile(Z, probs = 0.99), res)
  las = merge_spatial(las_chunk, p99, "p99")
  las = filter_poi(las, Z < p99*sensitivity)
  las$p99 <- NULL
  return(las)
}
