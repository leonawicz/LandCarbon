# R code: Baseline Fire 1950 - 2009
Matthew Leonawicz  



Note that the code may contain seemingly unnecessary repetition across multiple code chunks.
Sometimes I do this when using `knitr` to knit documents.
If I was strictly outputing graphics files I would remove apparent duplicate code.

Also note in the `modnames <-` assignment that the same script is simply run twice, once for each climate model.
The amount of hardcoding present in general is indicative of the limited time available for the task.

Depending on whether the output documents are md/html or LaTeX/pdf, tables are compiled differently as well using `xtable`.


```r
wd <- basename(getwd())
region.grp <- "LCC Regions"
# mainDir <-
# '/workspace/UA/mfleonawicz/Leonawicz/Projects/2014/AR4_AR5_comparisons/data/final'
mainDir <- "X:/Leonawicz/Projects/2014/AR4_AR5_comparisons/data/final"
ak.statsFire.file <- file.path(mainDir, "region_files_GCM/stats/Political/Alaska/stats_fire.RData")
# ak.samples.file <- file.path(mainDir,
# 'region_files_GCM/samples/Political/Alaska/xxxxxxxxxxxx.RData')
# load(file.path(mainDir, 'meta.RData'))
load("C:/github/shiny-apps/cmip3_cmip5/external/meta.RData")
statDir <- file.path(mainDir, "region_files_GCM/stats", region.grp)
sampDir <- file.path(mainDir, "region_files_GCM/samples", region.grp)
library(plyr)
library(xtable)
```

## Baseline Fire Tables

```r
# Baseline wildland fire, Table 3.1
years <- 1950:2009
modnames <- "CCCMAcgcm31"  # 'MPIecham5' # 
keep.cols <- c(2:stats.columns[1], (tail(stats.columns, 1) + 1):ncol(alf.fireStats.df))
files <- list.files(statDir, pattern = "^stats_fire.RData$", full = TRUE, recursive = TRUE)
files <- c(files, ak.statsFire.file)
regions <- basename(dirname(files))
for (i in 1:length(files)) {
    load(files[i])
    alf.fireStats.df$Location <- regions[i]
    alf.fireStats.df[, stats.columns] <- region.dat
    if (i == 1) 
        d <- alf.fireStats.df else d <- rbind(d, alf.fireStats.df)
}
rm(region.dat)

d <- subset(d, Model %in% modnames)
d$Location <- gsub(" S", " South", gsub(" N", " North", gsub("W ", "Western ", 
    gsub("N ", "North ", gsub("NW ", "Northwest ", d$Location)))))  # Special name changes
regions <- unique(d$Location[d$Location != "Alaska"])
reg.split2 <- reg.split <- regions[2:5]
reg.split2[1] <- "\\\\parbox[t]{3cm}{\\\\centering North\\\\\\\\Pacific}"
reg.split2[2] <- "\\\\parbox[t]{3cm}{\\\\centering Northwest Interior\\\\\\\\Forest North}"
reg.split2[3] <- "\\\\parbox[t]{3cm}{\\\\centering Northwest Interior\\\\\\\\Forest South}"
reg.split2[4] <- "\\\\parbox[t]{3cm}{\\\\centering Western\\\\\\\\Alaska}"

d <- subset(d, Year %in% years, select = keep.cols)
d.agg1 <- ddply(d, c("Var", "Location", "Year", "Decade"), summarise, Avg = mean(Mean))
d.agg2 <- ddply(d.agg1, c("Var", "Location"), summarise, Mean = mean(Avg), Standard_deviation = sd(Avg), 
    Minimum = min(Avg), Median = median(Avg), X95th_quantile = quantile(Avg, 
        probs = 0.95), Maximum = max(Avg))

ba <- subset(d.agg2, Var == "Burn Area", 2:ncol(d.agg2))
rownames(ba) <- ba$Location
ba <- t(ba[-1])
ba <- ba[, c(2:ncol(ba), 1)]
rownames(ba) <- gsub("X", "", gsub("_", " ", rownames(ba)))

fc <- subset(d.agg2, Var == "Fire Count", 2:ncol(d.agg2))
rownames(fc) <- fc$Location
fc <- t(fc[-1])
fc <- fc[, c(2:ncol(fc), 1)]
rownames(fc) <- gsub("X", "", gsub("_", " ", rownames(fc)))

ba.tab <- xtable(ba, digits = 1, align = c("l", rep("c", ncol(ba))))  #, caption='[km^2, square kilometers]')
fc.tab <- xtable(fc, digits = 1, align = c("l", rep("c", ncol(fc))))

bf <- rbind(fc, ba)
rownames(bf)[1:nrow(fc)] <- paste0("ZZZ", rownames(fc))
bf.tab <- xtable(bf, digits = 1, align = c("l", rep("c", ncol(bf))))  #, caption='[km^2, square kilometers]')
```

### Fire frequency (fires/year)

```r
if (wd == "Rmd") print(fc.tab, type = "html")
tmp <- print(fc.tab, booktabs = TRUE, print.results = FALSE, add.to.row = list(pos = list(-1, 
    0), command = c("\\headcol \n", "\\midrule \n\\rowcol \\multicolumn{7}{c}{Number of wildfires per year} \\\\ \n")))
tmp <- gsub("ZZZ", "", tmp)
for (i in 1:length(reg.split)) tmp <- gsub(reg.split[i], reg.split2[i], tmp)
# tmp <- gsub('\\[ht\\]', '', tmp) tmp <- gsub('begin\\{tabular\\}',
# 'begin\\{tabularx\\}\\{\\\\textwidth\\}', tmp) tmp <-
# gsub('end\\{tabular\\}', 'end\\{tabularx\\}', tmp)
tmp <- gsub("\\\\begin\\{tabular\\}", "\\\\hspace*\\{-100pt\\}\\\\begin\\{tabular\\}", 
    tmp)
tmp <- gsub("end\\{tabular\\}", "end\\{tabular\\}\\\\hspace\\{-100pt\\}", tmp)
```

### Burn area (km^2/year)

```r
if (wd == "Rmd") print(ba.tab, type = "html")
tmp <- print(ba.tab, booktabs = TRUE, print.results = FALSE, add.to.row = list(pos = list(-1, 
    0), command = c("\\headcol \n", "\\midrule \n\\rowcol \\multicolumn{7}{c}{Area burned per year \\{km^2\\}} \\\\ \n")))
tmp <- gsub("ZZZ", "", tmp)
for (i in 1:length(reg.split)) tmp <- gsub(reg.split[i], reg.split2[i], tmp)
# tmp <- gsub('\\[ht\\]', '', tmp) tmp <- gsub('begin\\{tabular\\}',
# 'begin\\{tabularx\\}\\{\\\\textwidth\\}', tmp) tmp <-
# gsub('end\\{tabular\\}', 'end\\{tabularx\\}', tmp)
tmp <- gsub("\\\\begin\\{tabular\\}", "\\\\hspace*\\{-100pt\\}\\\\begin\\{tabular\\}", 
    tmp)
tmp <- gsub("end\\{tabular\\}", "end\\{tabular\\}\\\\hspace\\{-100pt\\}", tmp)
```

## Baseline Fire Graphs
### Alaska

```r
# Baseline wildland fire, Figures 3.2-3.3
bf_barplot <- function(d, loc, main.title = "", fix.scale.ba = FALSE, fix.scale.fc = FALSE) {
    ba <- subset(d, Location == loc & Var == "Burn Area")
    fc <- subset(d, Location == loc & Var == "Fire Count")
    yr.seq.lab <- seq(1, length(fc$Year), by = 10)
    layout(matrix(1:2, nrow = 2))
    par(mar = c(3, 4, 1, 1))
    yaxis.brk.opts <- c(5, seq(10, 90, by = 10), seq(100, 900, by = 100), seq(1000, 
        9000, by = 1000))
    y.n <- 5
    if (fix.scale.ba) {
        y.gap <- max(ba$Avg/y.n)
        ind <- which.min(abs(y.gap - yaxis.brk.opts))
        x <- yaxis.brk.opts[c(ind, ind + 1)]
        y.gap <- if (x[1] < y.gap) 
            x[2] else x[1]
    } else y.gap <- max(ba$Avg)/y.n
    bp <- barplot(ba$Avg, names.arg = NULL, axes = FALSE, ylab = expression("Area burned " ~ 
        (km^2) ~ ""), main = main.title, ylim = c(0, y.gap * y.n), space = 0.8, 
        cex.axis = 0.7, cex.lab = 0.7)
    axis(2, at = seq(0, y.gap * y.n, by = round(y.gap)), cex.axis = 0.6, las = 1)
    y.max <- max(fc$Avg)
    y.gap <- max(y.max/y.n)
    if (fix.scale.fc) {
        if (y.max <= 1) 
            y.gap <- 0.2 else if (y.max <= 2) 
            y.gap <- 0.4 else if (y.max <= 5) 
            y.gap <- 1 else if (y.max <= 10) 
            y.gap <- 2 else if (y.max <= 20) 
            y.gap <- 4 else {
            ind <- which.min(abs(y.gap - yaxis.brk.opts))
            x <- yaxis.brk.opts[c(ind, ind + 1)]
            y.gap <- if (x[1] < y.gap) 
                x[2] else x[1]
        }
    }
    bp <- barplot(fc$Avg, names.arg = NULL, axes = FALSE, xlab = "Year", ylab = "Number of wildfires", 
        ylim = c(0, y.gap * y.n), space = 0.8, cex.axis = 0.7, cex.lab = 0.7)
    axis(2, at = seq(0, y.gap * y.n, by = round(y.gap, 1)), cex.axis = 0.6, 
        las = 1)
    brks <- bp[yr.seq.lab]
    brks <- c(brks, tail(brks, 1) + diff(brks)[1] - 1)
    labs <- fc$Year[yr.seq.lab]
    labs <- c(labs, tail(labs, 1) + diff(labs)[1] - 1)
    axis(1, at = brks, labels = labs, tick = FALSE, cex.axis = 0.7)
}

bf_barplot(d = d.agg1, loc = "Alaska", fix.scale.ba = T, fix.scale.fc = T)
```

### LCC Regions
#### Arctic

```r
bf_barplot(d = d.agg1, loc = regions[1], fix.scale.ba = T)
```

#### North Pacific

```r
bf_barplot(d = d.agg1, loc = regions[2])
```

#### Northwest Interior Forest North

```r
bf_barplot(d = d.agg1, loc = regions[3], fix.scale.ba = T)
```

#### Northwest Interior Forest South

```r
bf_barplot(d = d.agg1, loc = regions[4], fix.scale.ba = T)
```

#### Western Alaska

```r
bf_barplot(d = d.agg1, loc = regions[5], fix.scale.fc = T)
```

