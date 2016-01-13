setwd("/atlas_scratch/mfleonawicz/projects/LandCarbon/workspaces")

library(parallel)
library(raster)
tmpDir <- paste0("/atlas_scratch/mfleonawicz/tmp")
rasterOptions(chunksize=10e10, maxmemory=10e11, tmpdir=tmpDir)

# seasonal temperature climatology layers
get_seasonal_clim <- function(dir, seasons, years, prev.december=TRUE){
    files <- list.files(dir, pattern="\\.tif$", full=T)
    mo.vec <- substr(files, nchar(files)-10, nchar(files)-9)
    files <- lapply(1:12, function(i, x, years, pre=prev.december){
        yr.vec <- as.numeric(substr(x[[i]], nchar(x[[i]])-7, nchar(x[[i]])-4))
        if(pre && i==12) years <- years-1
        x[[i]][yr.vec %in% years]
        }, x=split(files, mo.vec), years=years)
    files <- list(unlist(files[seasons$Winter]), unlist(files[seasons$Spring]), unlist(files[seasons$Summer]), unlist(files[seasons$Fall]))
    b <- round(stack(mclapply(files, function(x) calc(stack(x), mean))), 1)
    names(b) <- c("Winter", "Spring", "Summer", "Fall")
    b
}

seasons <- list(Winter=c(1,2,12), Spring=3:5, Summer=6:8, Fall=9:11)
b.cru <- get_seasonal_clim("/Data/Base_Data/Climate/AK_CAN_2km/historical/CRU/CRU_TS32/tas", seasons, 1950:2013)
b.cccma <- get_seasonal_clim("/Data/Base_Data/Climate/AK_CAN_2km/projected/AR4_CMIP3_models/sresa1b/cccma-cgcm3-1-t47/tas", seasons, 2090:2099)
b.echam <- get_seasonal_clim("/Data/Base_Data/Climate/AK_CAN_2km/projected/AR4_CMIP3_models/sresa1b/mpi-echam5/tas", seasons, 2090:2099)
save(b.cru, b.cccma, b.echam, file="tas_clim.RData")
