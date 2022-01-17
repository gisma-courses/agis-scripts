tree_fn <- function(chunk, vrt_dsm, vrt_dtm, th)
{
  las_chunk = readLAS(chunk)
  if (is.empty(las_chunk)) return(NULL)
  
  
  ht_chunk <- lidR::normalize_height(las_chunk, vrt_dtm)
  algo_all <- lidR::watershed(vrt_dsm, th = th)
  ht_ws_chunk <- lidR::segment_trees(ht_chunk, algo_all, uniqueness = "incremental")
  trs_chunk <- lidR::filter_poi(ht_ws_chunk, !is.na(treeID))
  hulls_chunk <- lidR::delineate_crowns(trs_chunk, type = "concave", concavity = 2, func = .stdmetrics)
  
  # Removing the buffer is tricky on this one and
  # this is suboptimal. When used standalone with a
  # catalog delineate_crowns() does the job better than that
  hulls_chunk <- raster::crop(hulls_chunk, raster::extent(chunk))
  return(hulls_chunk)
}