model_gain <- function(my_raster, my_sites){
  
  my_site_classes <- table(factor(raster::extract(my_raster,my_sites),levels = levels(my_raster)[[1]]$ID))
  my_site_classes_n <- rev(cumsum(rev(my_site_classes)))
  
  my_raster_classes <- table(values(my_raster))
  my_raster_classes_n <- rev(cumsum(rev(my_raster_classes)))
  
  my_pa <- my_raster_classes_n/sum(my_raster_classes)
  my_ps <- my_site_classes_n/sum(my_site_classes)
  my_g <- 1 - my_pa/my_ps
  return(list(g = my_g, pa=my_pa, ps = my_ps))
}
