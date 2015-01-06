# R code: Projected Vegetation Change 2009 vs. 2100
Matthew Leonawicz  



Note that the code may contain seemingly unnecessary repetition across multiple code chunks.
Sometimes I do this when using `knitr` to knit documents.
If I was strictly outputing graphics files I would remove apparent duplicate code.

Also note in the `modnames <-` assignment that the same script is simply run twice, once for each climate model.
The amount of hardcoding present in general is indicative of the limited time available for the task.

Depending on whether the output documents are md/html or LaTeX/pdf, tables are compiled differently as well using `xtable`.


```r
region.grp <- "LCC Regions"
# mainDir <-
# '/workspace/UA/mfleonawicz/Leonawicz/Projects/2014/AR4_AR5_comparisons/data/final'
mainDir <- "X:/Leonawicz/Projects/2014/AR4_AR5_comparisons/data/final"
ak.statsVeg.file <- file.path(mainDir, "region_files_GCM/stats/Political/Alaska/stats_veg.RData")
# ak.samples.file <- file.path(mainDir,
# 'region_files_GCM/samples/Political/Alaska/xxxxxxxxxxxx.RData')
# load(file.path(mainDir, 'meta.RData'))
load("C:/github/shiny-apps/cmip3_cmip5/external/meta.RData")
statDir <- file.path(mainDir, "region_files_GCM/stats", region.grp)
sampDir <- file.path(mainDir, "region_files_GCM/samples", region.grp)
library(plyr)
library(xtable)
```


```r
# Projected vegetation change, Figure 6.2
years.all <- 2009:2100
years <- range(years.all)
modnames <- "MPIecham5"  # 'CCCMAcgcm31' # 
keep.cols <- c(2:stats.columns[1], (tail(stats.columns, 1) + 1):ncol(alf.vegStats.df))
files <- list.files(statDir, pattern = "^stats_veg.RData$", full = TRUE, recursive = TRUE)
files <- c(files, ak.statsVeg.file)
regions <- basename(dirname(files))
for (i in 1:length(files)) {
    load(files[i])
    alf.vegStats.df$Location <- regions[i]
    alf.vegStats.df[, stats.columns] <- region.dat
    if (i == 1) 
        d <- alf.vegStats.df else d <- rbind(d, alf.vegStats.df)
}
rm(region.dat)

d <- subset(d, Model %in% modnames)
d$Location <- gsub(" S", " South", gsub(" N", " North", gsub("W ", "Western ", 
    gsub("N ", "North ", gsub("NW ", "Northwest ", d$Location)))))  # Special name changes
regions <- unique(d$Location[d$Location != "Alaska"])

levels(d$Scenario) <- c("SRES B1", "SRES A1B", "SRES A2")
d2 <- subset(d, Year %in% years.all, select = keep.cols)
d <- subset(d, Year %in% years, select = keep.cols)
d.agg1 <- ddply(d, c("Scenario", "Location", "Vegetation", "Year", "Decade"), 
    summarise, Avg = mean(Mean))
d.agg2 <- ddply(d2, c("Scenario", "Location", "Vegetation", "Year", "Decade"), 
    summarise, Avg = mean(Mean))

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

## Percent Change by Vegetation Class
### 2009 Baseline by Scenario
#### Alaska

```r
# Baseline vegetation cover, Figure 6.2a1
vc_barplot(d.list = list(d.agg1.b1, d.agg1.a1b, d.agg1.a2), loc = "Alaska", 
    y.n = 8, prop = F, fix.scale = T)
```

```r
# Baseline vegetation cover, Figure 6.2a2
vc_barplot(d.list = list(d.agg1.b1, d.agg1.a1b, d.agg1.a2), loc = "Alaska", 
    y.n = 8, dodge = T, prop = F, fix.scale = T)
```

#### Arctic

```r
# Baseline vegetation cover, Figure 6.2a1
vc_barplot(d.list = list(d.agg1.b1, d.agg1.a1b, d.agg1.a2), loc = regions[1], 
    y.n = 8, prop = F, fix.scale = T)
```

```r
# Baseline vegetation cover, Figure 6.2a2
vc_barplot(d.list = list(d.agg1.b1, d.agg1.a1b, d.agg1.a2), loc = regions[1], 
    y.n = 8, dodge = T, prop = F, fix.scale = T)
```

#### North Pacific

```r
# Baseline vegetation cover, Figure 6.2a1
vc_barplot(d.list = list(d.agg1.b1, d.agg1.a1b, d.agg1.a2), loc = regions[2], 
    y.n = 8, prop = F, fix.scale = T)
```

```r
# Baseline vegetation cover, Figure 6.2a2
vc_barplot(d.list = list(d.agg1.b1, d.agg1.a1b, d.agg1.a2), loc = regions[2], 
    y.n = 8, dodge = T, prop = F, fix.scale = T)
```

#### Northwest Interior Forest North

```r
# Baseline vegetation cover, Figure 6.2a1
vc_barplot(d.list = list(d.agg1.b1, d.agg1.a1b, d.agg1.a2), loc = regions[3], 
    y.n = 8, prop = F, fix.scale = T)
```

```r
# Baseline vegetation cover, Figure 6.2a2
vc_barplot(d.list = list(d.agg1.b1, d.agg1.a1b, d.agg1.a2), loc = regions[3], 
    y.n = 8, dodge = T, prop = F, fix.scale = T)
```

#### Northwest Interior Forest South

```r
# Baseline vegetation cover, Figure 6.2a1
vc_barplot(d.list = list(d.agg1.b1, d.agg1.a1b, d.agg1.a2), loc = regions[4], 
    y.n = 8, prop = F, fix.scale = T)
```

```r
# Baseline vegetation cover, Figure 6.2a2
vc_barplot(d.list = list(d.agg1.b1, d.agg1.a1b, d.agg1.a2), loc = regions[4], 
    y.n = 8, dodge = T, prop = F, fix.scale = T)
```

#### Western Alaska

```r
# Baseline vegetation cover, Figure 6.2a1
vc_barplot(d.list = list(d.agg1.b1, d.agg1.a1b, d.agg1.a2), loc = regions[5], 
    y.n = 8, prop = F, fix.scale = T)
```

```r
# Baseline vegetation cover, Figure 6.2a2
vc_barplot(d.list = list(d.agg1.b1, d.agg1.a1b, d.agg1.a2), loc = regions[5], 
    y.n = 8, dodge = T, prop = F, fix.scale = T)
```

### 2100 Percent Change by Scenario
#### Alaska

```r
# Projected vegetation change, Figure 6.2b1
d.agg1.b1$Avg <- d.agg1.b1.2$Avg/d.agg1.b1$Avg - 1
d.agg1.a1b$Avg <- d.agg1.a1b.2$Avg/d.agg1.a1b$Avg - 1
d.agg1.a2$Avg <- d.agg1.a2.2$Avg/d.agg1.a2$Avg - 1

vc_barplot(d.list = list(d.agg1.b1, d.agg1.a1b, d.agg1.a2), loc = "Alaska", 
    y.n = 4, fix.scale = T)
```

```r
# Projected vegetation change, Figure 6.2b2
vc_barplot(d.list = list(d.agg1.b1, d.agg1.a1b, d.agg1.a2), loc = "Alaska", 
    y.n = 4, dodge = T, fix.scale = T)
```

#### Arctic

```r
# Projected vegetation change, Figure 6.2a1
vc_barplot(d.list = list(d.agg1.b1, d.agg1.a1b, d.agg1.a2), loc = regions[1], 
    y.n = 4, fix.scale = T)
```

```r
# Projected vegetation change, Figure 6.2a2
vc_barplot(d.list = list(d.agg1.b1, d.agg1.a1b, d.agg1.a2), loc = regions[1], 
    y.n = 4, dodge = T, fix.scale = T)
```

#### North Pacific

```r
# Projected vegetation change, Figure 6.2a1
vc_barplot(d.list = list(d.agg1.b1, d.agg1.a1b, d.agg1.a2), loc = regions[2], 
    y.n = 4, fix.scale = T)
```

```r
# Projected vegetation change, Figure 6.2a2
vc_barplot(d.list = list(d.agg1.b1, d.agg1.a1b, d.agg1.a2), loc = regions[2], 
    y.n = 4, dodge = T, fix.scale = T)
```

#### Northwest Interior Forest North

```r
# Projected vegetation change, Figure 6.2a1
vc_barplot(d.list = list(d.agg1.b1, d.agg1.a1b, d.agg1.a2), loc = regions[3], 
    y.n = 4, fix.scale = T)
```

```r
# Projected vegetation change, Figure 6.2a2
vc_barplot(d.list = list(d.agg1.b1, d.agg1.a1b, d.agg1.a2), loc = regions[3], 
    y.n = 4, dodge = T, fix.scale = T)
```

#### Northwest Interior Forest South

```r
# Projected vegetation change, Figure 6.2a1
vc_barplot(d.list = list(d.agg1.b1, d.agg1.a1b, d.agg1.a2), loc = regions[4], 
    y.n = 4, fix.scale = T)
```

```r
# Projected vegetation change, Figure 6.2a2
vc_barplot(d.list = list(d.agg1.b1, d.agg1.a1b, d.agg1.a2), loc = regions[4], 
    y.n = 4, dodge = T, fix.scale = T)
```

#### Western Alaska

```r
# Projected vegetation change, Figure 6.2a1
vc_barplot(d.list = list(d.agg1.b1, d.agg1.a1b, d.agg1.a2), loc = regions[5], 
    y.n = 4, fix.scale = T)
```

```r
# Projected vegetation change, Figure 6.2a2
vc_barplot(d.list = list(d.agg1.b1, d.agg1.a1b, d.agg1.a2), loc = regions[5], 
    y.n = 4, dodge = T, fix.scale = T)
```

