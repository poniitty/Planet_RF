library(terra)
library(sf)
library(tidyverse)
library(fasterize)
library(foreach, quiet = TRUE)
library(doMPI, quiet = TRUE)

setwd("/projappl/project_2003061/repos/Planet_RF")

paths <- c("/scratch/project_2003061/Planet/Kilpisjarvi/mosaiced",
           "/scratch/project_2003061/Planet/Varrio/mosaiced",
           "/scratch/project_2003061/Planet/Pallas/mosaiced",
           "/scratch/project_2003061/tiilikka/mosaiced")

shps <- list.files(paths, pattern = "_trainpoints.shp$", full.names = T)

cl<-startMPIcluster()
registerDoMPI(cl)

foreach(i = shps, .packages = c("terra","tidyverse","sf","fasterize","tidyr")) %dopar% {
  # i <- "/scratch/project_2003061/tiilikka/mosaiced/planet_2020-9-26_09-10-00_0f4e.tif_trainpoints.shp"
  p <- st_read(i)
  
  if(nrow(p) > 2){
    
    p <- p %>% slice(-1) %>% 
      mutate(X1 = as.numeric(substr(X1, 1, 1)))
    
    bind_cols(p,
              fill(st_drop_geometry(p), X1, .direction = "down") %>% rename(X2 = X1),
              fill(st_drop_geometry(p), X1, .direction = "up") %>% rename(X3 = X1)) %>% 
      filter(X2 == X3) %>%
      select(-X1, -X3) %>% 
      rename(X1 = X2) -> p
    
    if(grepl(".tif_", i)){
      r <- rast(gsub("_trainpoints.shp","",i))
    } else {
      r <- rast(gsub("_trainpoints.shp",".tif",i))
    }
    names(r) <- c("blue","green","red","infrared")

    chm <- rast(paste0(strsplit(i, "mosaiced")[[1]][1], "predictors/chm.tif"))
    chm <- project(chm, r)

    chm_conif <- rast(paste0(strsplit(i, "mosaiced")[[1]][1], "predictors/chm_conif.tif"))
    chm_conif <- project(chm_conif, r)

    slope <- rast(paste0(strsplit(i, "mosaiced")[[1]][1], "predictors/slope.tif"))
    slope <- project(slope, r)

    pisr <- rast(paste0(strsplit(i, "mosaiced")[[1]][1], "predictors/pisr/",
                        if(grepl(".tif_", i)){
                          gsub("_trainpoints.shp","",tail(strsplit(i, "/")[[1]],1))
                        } else {
                          gsub("_trainpoints.shp",".tif",tail(strsplit(i, "/")[[1]],1))
                        }))
    pisr <- project(pisr, r)

    r[["blue_min"]] <- focal(r[[1]], w = 5, fun = "min", expand = T)
    r[["infrared_min"]] <- focal(r[[4]], w = 5, fun = "min", expand = T)

    r[["blue_max"]] <- focal(r[[1]], w = 5, fun = "max", expand = T)
    r[["infrared_max"]] <- focal(r[[4]], w = 5, fun = "max", expand = T)
    
    if(file.exists(gsub("_trainpoints.shp","_mask.shp",i))){
      
      msk <- st_read(gsub("_trainpoints.shp","_mask.shp",i))
      
      if(class(msk$geometry)[1] == "sfc_GEOMETRYCOLLECTION"){
        clouds <- r[[1]]
        clouds[] <- 0
        names(clouds) <- "clouds"
      } else {
        msk <- msk %>% mutate(X1 = as.numeric(substr(X1, 1, 1))) %>% 
          mutate(X1 = ifelse(is.na(X1), 2, X1))
        
        clouds <- fasterize(msk, raster(chm), field = "X1", fun = "max")
        clouds <- rast(clouds)
        names(clouds) <- "clouds"
        clouds[is.na(clouds)] <- 0
      }
    }
    
    rs <- c(r, chm, chm_conif, slope, pisr, clouds)
    
    p <- p %>% mutate(X1 = as.numeric(substr(X1, 1, 1))) %>% 
      filter(!is.na(X1))
    
    if(grepl(".tif_", i)){
      imageid <- gsub("_trainpoints.shp","",tail(strsplit(i, "/")[[1]],1))
    } else {
      imageid <- gsub("_trainpoints.shp",".tif",tail(strsplit(i, "/")[[1]],1))
    }
    
    p <- bind_cols(st_drop_geometry(p) %>% slice(st_coordinates(p) %>% as.data.frame() %>% pull(L1)),
                   terra::extract(rs, st_coordinates(p) %>% as.data.frame() %>% select(X,Y)) %>% select(-ID)) %>% 
      filter(!is.na(blue)) %>% 
      mutate(imageid = imageid)
    
    write_csv(p, paste0("extracted/", imageid, ".csv"))
  }
}

closeCluster(cl)
mpi.quit()