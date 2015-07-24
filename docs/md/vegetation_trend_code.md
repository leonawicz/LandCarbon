# R code: Projected Vegetation Trends 2009 - 2100
Matthew Leonawicz  



Note that the code may contain seemingly unnecessary repetition across multiple code chunks.
Sometimes I do this when using `knitr` to knit documents.
If I was strictly outputing graphics files I would remove apparent duplicate code.

Also note in the `modnames <-` assignment that the same script is simply run twice, once for each climate model.
The amount of hardcoding present in general is indicative of the limited time available for the task.

Depending on whether the output documents are md/html or LaTeX/pdf, tables are compiled differently as well using `xtable`.


```r
region.grp <- "LCC Regions"
mainDir <- "X:/leonawicz/projects/SNAPQAQC/data/final/alfresco"
ak.statsVeg.file <- file.path(mainDir, "stats/Political/Alaska/stats_veg.RData")
statDir <- file.path(mainDir, "stats", region.grp)
library(data.table)
library(reshape2)
library(plyr)
library(xtable)
```


```r
# Projected vegetation change, Figure 6.2
years.all <- 2009:2100
years <- range(years.all)
modnames <- "MPIecham5"  # 
files <- list.files(statDir, pattern = "^stats_veg.RData$", full = TRUE, recursive = TRUE)
files <- c(files, ak.statsVeg.file)
regions <- basename(dirname(files))
d <- vector("list", length(files))
for (i in 1:length(files)) {
    load(files[i])
    d[[i]] <- region.dat
}
rm(region.dat)
d <- rbindlist(d)
d <- subset(d, Model %in% modnames & !(Vegetation %in% c("Barren lichen-moss", 
    "Temperate Rainforest", "Wetland Tundra")))
d$Location <- gsub(" S", " South", gsub(" N", " North", gsub("W ", "Western ", 
    gsub("N ", "North ", gsub("NW ", "Northwest ", d$Location)))))  # Special name changes
regions <- unique(d$Location[d$Location != "Alaska"])
d[, `:=`(Decade, Year - Year%%10)]

d$Scenario <- factor(as.character(d$Scenario), levels = c("SRES B1", "SRES A1B", 
    "SRES A2"))
d2 <- subset(d, Year %in% years.all)
d <- subset(d, Year %in% years)
d.agg1 <- ddply(d, c("Scenario", "Location", "Vegetation", "Year", "Decade"), 
    summarise, Avg = mean(Mean))
d.agg2 <- ddply(d2, c("Scenario", "Location", "Vegetation", "Year", "Decade"), 
    summarise, Avg = mean(Mean))
# Send table to file
write.csv(d.agg2, file = paste0("C:/github/LandCarbon/data/", modnames, "_annual_veg_2009_2100.csv"))

fac <- 1000
d.agg1.sub <- d.agg1  #subset(d.agg1, Location=='Alaska')
d.agg2.sub <- d.agg2  #subset(d.agg2, Location=='Alaska')
d.agg1.sub$Avg <- d.agg1.sub$Avg/fac
d.agg2.sub$Avg <- d.agg2.sub$Avg/fac

d.agg1.b1 <- subset(d.agg1.sub, Year == years[1] & Scenario == "SRES B1")
d.agg1.a1b <- subset(d.agg1.sub, Year == years[1] & Scenario == "SRES A1B")
d.agg1.a2 <- subset(d.agg1.sub, Year == years[1] & Scenario == "SRES A2")
d.agg1.b1.2 <- subset(d.agg1.sub, Year == years[2] & Scenario == "SRES B1")
d.agg1.a1b.2 <- subset(d.agg1.sub, Year == years[2] & Scenario == "SRES A1B")
d.agg1.a2.2 <- subset(d.agg1.sub, Year == years[2] & Scenario == "SRES A2")

vc_barplot <- function(d.list, loc, dodge = FALSE, y.n = 5, prop = TRUE, main.title = "", 
    fix.scale = FALSE) {
    n <- length(d.list)
    for (i in 1:n) d.list[[i]] <- subset(d.list[[i]], Location == loc)
    if (!dodge) 
        layout(matrix(1:n, nrow = n, byrow = FALSE)) else layout(matrix(1, 1))
    par(mar = c(3, 4, 1, 1))
    yaxis.brk.opts <- c(5, seq(10, 90, by = 10), seq(100, 900, by = 100), seq(1000, 
        9000, by = 1000), seq(10000, 90000, by = 10000), seq(1e+05, 9e+05, by = 1e+05))
    if (prop) 
        yaxis.brk.opts <- seq(0, 2, by = 0.01)
    if (prop) 
        ylb <- "Area percent change" else ylb <- expression("Area (1000" ~ km^2 ~ ")")
    y.max <- max(sapply(d.list, function(x) max(abs(x$Avg), na.rm = TRUE)), 
        na.rm = TRUE)
    if (fix.scale) {
        y.gap <- y.max/y.n
        ind <- which.min(abs(y.gap - yaxis.brk.opts))
        x <- yaxis.brk.opts[c(ind, ind + 1)]
        y.gap <- if (x[1] < y.gap) 
            x[2] else x[1]
    } else y.gap <- y.max/y.n
    ylm <- y.gap * y.n * c(-1, 1)
    if (!prop) 
        ylm[1] <- 0
    seq.at <- seq(ylm[1], ylm[2], by = y.gap)
    if (prop & all(do.call(rbind, d.list)$Avg <= 1, na.rm = TRUE)) 
        seq.lab <- seq.at * 100 else seq.lab <- seq.at
    if (dodge) {
        d <- do.call(rbind, lapply(d.list, function(x) x$Avg))
        clr <- colorRampPalette(c("deepskyblue", "dodgerblue3", "dodgerblue4"))(nrow(d))
        bp <- barplot(d, beside = TRUE, col = clr, names.arg = NULL, axes = FALSE, 
            ylab = ylb, main = main.title, ylim = ylm, cex.axis = 0.8, cex.lab = 0.7)
        axis(2, at = seq.at, labels = seq.lab, cex.axis = 0.6, las = 1)
        labs <- d.list[[1]]$Vegetation
        axis(1, at = bp[ceiling(nrow(d)/2), 1:length(labs)], labels = labs, 
            tick = FALSE, cex.axis = 0.4)
        legend("topright", levels(d.list[[1]]$Scenario), pch = 22, pt.bg = clr, 
            horiz = TRUE, bty = "n", cex = 0.7)
    } else {
        for (p in 1:n) {
            d <- d.list[[p]]
            bp <- barplot(d$Avg, names.arg = NULL, axes = FALSE, ylab = ylb, 
                main = main.title, ylim = ylm, cex.axis = 0.8, cex.lab = 0.7)
            axis(2, at = seq.at, labels = seq.lab, cex.axis = 0.6, las = 1)
            if (p == n) {
                labs <- d$Vegetation
                axis(1, at = bp[1:length(labs)], labels = labs, tick = FALSE, 
                  cex.axis = 0.6)
            }
        }
    }
}
```

## Area Trends by Vegetation Class and Scenario
### Alaska

```r
# Projected vegetation trend, Figure 6.3
vc_tsplot <- function(d, loc, alpha = NULL) {
    d <- subset(d, Location == loc)
    veg <- unique(d$Vegetation)
    n <- length(veg)
    nc <- 3
    layout(matrix(1:6, 2, nc, byrow = T))
    par(mar = c(3, 4, 1, 1))
    years <- unique(d$Year)
    xlm <- range(years)
    scen <- levels(d$Scenario)
    clr <- colorRampPalette(c("lightgreen", "dodgerblue", "magenta"))(length(scen))
    if (is.integer(alpha) && alpha < 100) 
        clr <- paste0(clr, alpha)
    for (i in 1:n) {
        di <- subset(d, Vegetation == veg[i])
        ylm <- range(di$Avg, na.rm = TRUE)
        if (any(ylm == Inf)) 
            ylm <- c(0, 1)
        if (i == nc + 1) 
            ylb <- expression("Area (1000" ~ km^2 ~ ")") else ylb <- ""
        plot(0, 0, xaxt = "n", xlim = xlm, ylim = ylm, type = "n", ylab = ylb, 
            main = veg[i], cex.main = 0.7, cex.axis = 0.8, cex.lab = 0.8, las = 1)
        box()
        for (j in 1:length(scen)) if (any(!is.na(di$Avg[di$Scenario == scen[j]]))) 
            lines(years, di$Avg[di$Scenario == scen[j]], col = clr[j], lwd = 2)
        lab <- as.numeric(paste0(substr(as.character(seq(years[1], tail(years, 
            1), by = 20)), 1, 3), 0))
        if (i > n - nc) 
            axis(1, at = lab, labels = lab, cex.axis = 0.8, cex.lab = 0.8) else axis(1, at = lab, labels = rep("", length(lab)), cex.axis = 0.8, 
            cex.lab = 0.8)
    }
    if (n < nc^2) {
        plot(0, 0, axes = F, type = "n", ylab = "")
        legend("center", rev(scen), lty = 1, lwd = 2, col = rev(clr), bty = "n", 
            cex = 1)
    }
}

vc_tsplot(d = d.agg2.sub, loc = "Alaska")
```

### Arctic

```r
# Projected vegetation trend, Figure 6.3
vc_tsplot(d = d.agg2.sub, loc = regions[1])
```

### North Pacific

```r
# Projected vegetation trend, Figure 6.3
vc_tsplot(d = d.agg2.sub, loc = regions[2])
```

### Northwest Interior Forest North

```r
# Projected vegetation trend, Figure 6.3
vc_tsplot(d = d.agg2.sub, loc = regions[3])
```

### Northwest Interior Forest South

```r
# Projected vegetation trend, Figure 6.3
vc_tsplot(d = d.agg2.sub, loc = regions[4])
```

### Western Alaska

```r
# Projected vegetation trend, Figure 6.3
vc_tsplot(d = d.agg2.sub, loc = regions[5])
```

