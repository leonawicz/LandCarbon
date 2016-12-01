setwd("/atlas_scratch/mfleonawicz/projects/SNAPQAQC/workspaces")
library(parallel); library(raster); library(purrr)
rasterOptions(chunksize=10e10, maxmemory=10e11)

vars <- c("tas", "pr")
gcms <- c("cccma-cgcm3-1-t47", "mpi-echam5")
yrs.hist <- 1950:2013
yrs.proj <- 2090:2099

cruDir <- "/Data/Base_Data/Climate/AK_CAN_2km/historical/CRU/CRU_TS32"
gcmDir <- "/Data/Base_Data/Climate/AK_CAN_2km/projected/AR4_CMIP3_models/sresa1b"
inDirs <- map(vars, ~c(file.path(cruDir, .x), file.path(gcmDir, gcms, .x))) %>% setNames(vars)

# monthly or seasonal (DJF, MAM, JJA, SON) climatology layers
get_clim <- function(dir, years, seasons=FALSE, prev.december=TRUE, decimals=0){
    p <- if(seasons) list(Winter=c(1,2,12), Spring=3:5, Summer=6:8, Fall=9:11) else split(1:12, factor(month.abb, levels=month.abb))
    files <- list.files(dir, pattern="\\.tif$", full=T)
    mo.vec <- substr(files, nchar(files)-10, nchar(files)-9)
    f <- function(x, idx, yrs, seas, pre){
      y <- as.numeric(substr(x, nchar(x)-7, nchar(x)-4))
      if(seas && pre && idx==12) yrs <- yrs - 1
      x[y %in% yrs]
    }
    files <- split(files, mo.vec) %>% map2(1:12, ~f(.x, .y, years, seasons, prev.december))
    files <- map(p, ~unlist(files[.x]))
    round(stack(mclapply(files, function(x) calc(stack(x), mean), mc.cores=length(p))), decimals) %>% setNames(names(p))
}

ids <- c(basename(cruDir), gcms)
yrs.list <- list(yrs.hist, yrs.proj, yrs.proj)
b.t <- map2(inDirs$tas, yrs.list, ~get_clim(.x, .y, seasons=T, decimals=1)) %>% setNames(ids)
b.p <- map2(inDirs$pr, yrs.list, ~get_clim(.x, .y, seasons=T)) %>% setNames(ids)
save(list=ls(pattern="^b\\."), file="/atlas_scratch/mfleonawicz/projects/LandCarbon/workspaces/lc_clim.RData")
