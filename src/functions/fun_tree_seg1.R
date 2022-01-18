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
  # 
  # Removing the buffer is tricky on this one and
  # this is suboptimal. When used standalone with a
  # catalog delineate_crowns() does the job better than that
  hulls_chunk <- raster::crop(hulls_chunk, raster::extent(chunk))
  
  
  return(hulls_chunk)
}


filter_noise = function(chunk, sensitivity)
{
  attempt_max=5
  for(i in 1:attempt_max){
    las_chunk = try(readLAS(chunk))
    if(!class(las_chunk) == "try-error") i = attempt_max
    else Sys.sleep(5)
  }
  if (is.empty(las_chunk)) return(NULL)
  
  p99 = grid_metrics(las_chunk, ~quantile(Z, probs = 0.99), 10)
  las = merge_spatial(las_chunk, p99, "p99")
  las = filter_poi(las, Z < p99*sensitivity)
  las$p99 <- NULL
  return(las)
}
