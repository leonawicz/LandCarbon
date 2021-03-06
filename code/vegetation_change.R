# @knitr setup
region.grp <- "LCC Regions"
mainDir <- "X:/projects/SNAPQAQC/data/final/alfresco"
ak.statsVeg.file <- file.path(mainDir, "stats/Political Boundaries/Alaska/vegarea.RData")
statDir <- file.path(mainDir, "stats", region.grp)
library(data.table)
library(reshape2)
library(dplyr)
library(xtable)

# @knitr veg_change_setup
# Projected vegetation change, Figure 6.2
years.all <- 2009:2100
years <- range(years.all)
modnames <- "CCCMAcgcm31" # "MPIecham5" # 
files <- list.files(statDir, pattern="^vegarea.RData$", full=TRUE, recursive=TRUE)
files <- c(files, ak.statsVeg.file)
regions <- basename(dirname(files))
d <- vector("list", length(files))
for(i in 1:length(files)){
	load(files[i])
	d[[i]] <- stats.alf.vegarea
}
rm(stats.alf.vegarea)
d <- rbindlist(d)
d <- filter(d, Model %in% modnames & !(Vegetation %in% c("Barren lichen-moss", "Temperate Rainforest", "Wetland Tundra"))) %>%
  mutate(Vegetation=factor(Vegetation, levels=sort(unique(Vegetation))))
d$Location <- gsub(" S", " South", gsub(" N", " North", gsub("W ", "Western ", gsub("N ", "North ", gsub("NW ", "Northwest ", d$Location))))) # Special name changes
regions <- unique(d$Location[d$Location!="Alaska"])
d[, Decade:=Year-Year %% 10]
d$Scenario <- factor(as.character(d$Scenario), levels=c("SRES B1","SRES A1B","SRES A2"))
d <- group_by(d, Scenario, Location, Vegetation, Year, Decade)

d2 <- filter(d, Year %in% years.all)
d <- filter(d, Year %in% years)
d.agg1 <- d %>% summarise(Avg=mean(Mean))
d.agg2 <- d2 %>% summarise(Avg=mean(Mean))
# Send table to file
write.csv(d.agg2, file=paste0("C:/github/LandCarbon/data/", modnames, "_annual_veg_2009_2100.csv"))

fac <- 1000
d.agg1.sub <- d.agg1 %>% complete(Scenario, Location, Vegetation, Year)
d.agg2.sub <- d.agg2 %>% complete(Scenario, Location, Vegetation, Year)
d.agg1.sub$Avg <- d.agg1.sub$Avg/fac
d.agg2.sub$Avg <- d.agg2.sub$Avg/fac

d.agg1.b1 <- filter(d.agg1.sub, Year==years[1] & Scenario=="SRES B1")
d.agg1.a1b <- filter(d.agg1.sub, Year==years[1] & Scenario=="SRES A1B")
d.agg1.a2 <- filter(d.agg1.sub, Year==years[1] & Scenario=="SRES A2")
d.agg1.b1.2 <- filter(d.agg1.sub, Year==years[2] & Scenario=="SRES B1")
d.agg1.a1b.2 <- filter(d.agg1.sub, Year==years[2] & Scenario=="SRES A1B")
d.agg1.a2.2 <- filter(d.agg1.sub, Year==years[2] & Scenario=="SRES A2")

vc_barplot <- function(d.list, loc, dodge=FALSE, y.n=5, prop=TRUE, main.title="", fix.scale=FALSE){
	n <- length(d.list)
	for(i in 1:n) d.list[[i]] <- subset(d.list[[i]], Location==loc)
	if(!dodge) layout(matrix(1:n, nrow=n, byrow=FALSE)) else layout(matrix(1,1))
	par(mar=c(3,4,1,1))
	yaxis.brk.opts <- c(5, seq(10,90, by=10), seq(100,900, by=100), seq(1000,9000,by=1000), seq(10000,90000,by=10000), seq(100000,900000,by=100000))
	if(prop) yaxis.brk.opts <- seq(0, 2, by=0.01)
	if(prop) ylb <- "Area percent change" else ylb <- expression("Area (1000"~km^2~")")
	y.max <- max(sapply(d.list, function(x) max(abs(x$Avg), na.rm=TRUE)), na.rm=TRUE)
	if(fix.scale){
		y.gap <- y.max/y.n
		ind <- which.min(abs(y.gap - yaxis.brk.opts))
		x <- yaxis.brk.opts[c(ind, ind+1)]
		y.gap <- if(x[1] < y.gap) x[2] else x[1]
	} else y.gap <- y.max/y.n
	ylm <- y.gap*y.n*c(-1, 1)
	if(!prop) ylm[1] <- 0
	seq.at <- seq(ylm[1], ylm[2], by=y.gap)
	if(prop & all(do.call(rbind, d.list)$Avg <= 1, na.rm=TRUE)) seq.lab <- seq.at*100 else seq.lab <- seq.at
	if(dodge){
		d <- do.call(rbind, lapply(d.list, function(x) x$Avg))
		clr <- colorRampPalette(c("deepskyblue", "dodgerblue3", "dodgerblue4"))(nrow(d))
		bp <- barplot(d, beside=TRUE, col=clr, names.arg=NULL, axes=FALSE, ylab=ylb, main=main.title, ylim=ylm, cex.axis=0.8, cex.lab=0.7)
		axis(2, at=seq.at, labels=seq.lab, cex.axis=0.6, las=1)
		labs <- d.list[[1]]$Vegetation
		axis(1, at=bp[ceiling(nrow(d)/2),1:length(labs)], labels=labs, tick=FALSE, cex.axis=0.4)
		legend("topright", levels(d.list[[1]]$Scenario), pch=22, pt.bg=clr, horiz=TRUE, bty="n", cex=0.7)
	} else {
		for(p in 1:n){
			d <- d.list[[p]]
			bp <- barplot(d$Avg, names.arg=NULL, axes=FALSE, ylab=ylb, main=main.title, ylim=ylm, cex.axis=0.8, cex.lab=0.7)
			axis(2, at=seq.at, labels=seq.lab, cex.axis=0.6, las=1)
			if(p==n){
				labs <- d$Vegetation
				axis(1, at=bp[1:length(labs)], labels=labs, tick=FALSE, cex.axis=0.6)
			}
		}
	}
}

# @knitr baseline_veg_barplot_AK1
# Baseline vegetation cover, Figure 6.2a1
vc_barplot(d.list=list(d.agg1.b1, d.agg1.a1b, d.agg1.a2), loc="Alaska", y.n=8, prop=F, fix.scale=T)
# @knitr baseline_veg_barplot_AK2
# Baseline vegetation cover, Figure 6.2a2
vc_barplot(d.list=list(d.agg1.b1, d.agg1.a1b, d.agg1.a2), loc="Alaska", y.n=8, dodge=T, prop=F, fix.scale=T)

# @knitr baseline_veg_barplot_LCC1a
# Baseline vegetation cover, Figure 6.2a1
vc_barplot(d.list=list(d.agg1.b1, d.agg1.a1b, d.agg1.a2), loc=regions[1], y.n=8, prop=F, fix.scale=T)
# @knitr baseline_veg_barplot_LCC1b
# Baseline vegetation cover, Figure 6.2a2
vc_barplot(d.list=list(d.agg1.b1, d.agg1.a1b, d.agg1.a2), loc=regions[1], y.n=8, dodge=T, prop=F, fix.scale=T)

# @knitr baseline_veg_barplot_LCC2a
# Baseline vegetation cover, Figure 6.2a1
vc_barplot(d.list=list(d.agg1.b1, d.agg1.a1b, d.agg1.a2), loc=regions[2], y.n=8, prop=F, fix.scale=T)
# @knitr baseline_veg_barplot_LCC2b
# Baseline vegetation cover, Figure 6.2a2
vc_barplot(d.list=list(d.agg1.b1, d.agg1.a1b, d.agg1.a2), loc=regions[2], y.n=8, dodge=T, prop=F, fix.scale=T)

# @knitr baseline_veg_barplot_LCC3a
# Baseline vegetation cover, Figure 6.2a1
vc_barplot(d.list=list(d.agg1.b1, d.agg1.a1b, d.agg1.a2), loc=regions[3], y.n=8, prop=F, fix.scale=T)
# @knitr baseline_veg_barplot_LCC3b
# Baseline vegetation cover, Figure 6.2a2
vc_barplot(d.list=list(d.agg1.b1, d.agg1.a1b, d.agg1.a2), loc=regions[3], y.n=8, dodge=T, prop=F, fix.scale=T)

# @knitr baseline_veg_barplot_LCC4a
# Baseline vegetation cover, Figure 6.2a1
vc_barplot(d.list=list(d.agg1.b1, d.agg1.a1b, d.agg1.a2), loc=regions[4], y.n=8, prop=F, fix.scale=T)
# @knitr baseline_veg_barplot_LCC4b
# Baseline vegetation cover, Figure 6.2a2
vc_barplot(d.list=list(d.agg1.b1, d.agg1.a1b, d.agg1.a2), loc=regions[4], y.n=8, dodge=T, prop=F, fix.scale=T)

# @knitr baseline_veg_barplot_LCC5a
# Baseline vegetation cover, Figure 6.2a1
vc_barplot(d.list=list(d.agg1.b1, d.agg1.a1b, d.agg1.a2), loc=regions[5], y.n=8, prop=F, fix.scale=T)
# @knitr baseline_veg_barplot_LCC5b
# Baseline vegetation cover, Figure 6.2a2
vc_barplot(d.list=list(d.agg1.b1, d.agg1.a1b, d.agg1.a2), loc=regions[5], y.n=8, dodge=T, prop=F, fix.scale=T)

# @knitr veg_change_barplot_AK1
# Projected vegetation change, Figure 6.2b1
d.agg1.b1$Avg <- d.agg1.b1.2$Avg/d.agg1.b1$Avg - 1
d.agg1.a1b$Avg <- d.agg1.a1b.2$Avg/d.agg1.a1b$Avg - 1
d.agg1.a2$Avg <- d.agg1.a2.2$Avg/d.agg1.a2$Avg - 1

# Send table to file
write.csv(dcast(rbind(d.agg1.b1, d.agg1.a1b, d.agg1.a2), Vegetation ~ Location + Scenario)[c(1,5,2:4), c(5:19, 2:4)], file=paste0("C:/github/LandCarbon/data/", modnames, "_veg_pct_change_2009vs2100.csv"))

vc_barplot(d.list=list(d.agg1.b1, d.agg1.a1b, d.agg1.a2), loc="Alaska", y.n=4, fix.scale=T)
# @knitr veg_change_barplot_AK2
# Projected vegetation change, Figure 6.2b2
vc_barplot(d.list=list(d.agg1.b1, d.agg1.a1b, d.agg1.a2), loc="Alaska", y.n=4, dodge=T, fix.scale=T)

# @knitr veg_change_barplot_LCC1a
# Projected vegetation change, Figure 6.2a1
vc_barplot(d.list=list(d.agg1.b1, d.agg1.a1b, d.agg1.a2), loc=regions[1], y.n=4, fix.scale=T)
# @knitr veg_change_barplot_LCC1b
# Projected vegetation change, Figure 6.2a2
vc_barplot(d.list=list(d.agg1.b1, d.agg1.a1b, d.agg1.a2), loc=regions[1], y.n=4, dodge=T, fix.scale=T)

# @knitr veg_change_barplot_LCC2a
# Projected vegetation change, Figure 6.2a1
vc_barplot(d.list=list(d.agg1.b1, d.agg1.a1b, d.agg1.a2), loc=regions[2], y.n=4, fix.scale=T)
# @knitr veg_change_barplot_LCC2b
# Projected vegetation change, Figure 6.2a2
vc_barplot(d.list=list(d.agg1.b1, d.agg1.a1b, d.agg1.a2), loc=regions[2], y.n=4, dodge=T, fix.scale=T)

# @knitr veg_change_barplot_LCC3a
# Projected vegetation change, Figure 6.2a1
vc_barplot(d.list=list(d.agg1.b1, d.agg1.a1b, d.agg1.a2), loc=regions[3], y.n=4, fix.scale=T)
# @knitr veg_change_barplot_LCC3b
# Projected vegetation change, Figure 6.2a2
vc_barplot(d.list=list(d.agg1.b1, d.agg1.a1b, d.agg1.a2), loc=regions[3], y.n=4, dodge=T, fix.scale=T)

# @knitr veg_change_barplot_LCC4a
# Projected vegetation change, Figure 6.2a1
vc_barplot(d.list=list(d.agg1.b1, d.agg1.a1b, d.agg1.a2), loc=regions[4], y.n=4, fix.scale=T)
# @knitr veg_change_barplot_LCC4b
# Projected vegetation change, Figure 6.2a2
vc_barplot(d.list=list(d.agg1.b1, d.agg1.a1b, d.agg1.a2), loc=regions[4], y.n=4, dodge=T, fix.scale=T)

# @knitr veg_change_barplot_LCC5a
# Projected vegetation change, Figure 6.2a1
vc_barplot(d.list=list(d.agg1.b1, d.agg1.a1b, d.agg1.a2), loc=regions[5], y.n=4, fix.scale=T)
# @knitr veg_change_barplot_LCC5b
# Projected vegetation change, Figure 6.2a2
vc_barplot(d.list=list(d.agg1.b1, d.agg1.a1b, d.agg1.a2), loc=regions[5], y.n=4, dodge=T, fix.scale=T)

# @knitr veg_change_ts_AK
# Projected vegetation trend, Figure 6.3
vc_tsplot <- function(d, loc, alpha=NULL){
	d <- subset(d, Location==loc)
	veg <- unique(d$Vegetation)
	n <- length(veg)
	nc <- 3
	layout(matrix(1:6,2,nc, byrow=T))
	par(mar=c(3,4,1,1))
	years <- unique(d$Year)
	xlm <- range(years)
	scen <- levels(d$Scenario)
	clr <- colorRampPalette(c("lightgreen", "dodgerblue", "magenta"))(length(scen))
	if(is.integer(alpha) && alpha < 100) clr <- paste0(clr, alpha)
	for(i in 1:n){
		di <- subset(d, Vegetation==veg[i])
		ylm <- range(di$Avg, na.rm=TRUE)
		if(any(ylm==Inf)) ylm <- c(0,1)
		if(i==nc+1) ylb <- expression("Area (1000"~km^2~")") else ylb <- ""
		plot(0, 0, xaxt="n", xlim=xlm, ylim=ylm, type="n", ylab=ylb, main=veg[i], cex.main=0.7, cex.axis=0.8, cex.lab=0.8, las=1)
		box()
		for(j in 1:length(scen)) if(any(!is.na(di$Avg[di$Scenario==scen[j]]))) lines(years, di$Avg[di$Scenario==scen[j]], col=clr[j], lwd=2)
		lab <- as.numeric(paste0(substr(as.character(seq(years[1], tail(years,1), by=20)), 1, 3), 0))
		if(i > n-nc) axis(1, at=lab, labels=lab, cex.axis=0.8, cex.lab=0.8) else axis(1, at=lab, labels=rep("", length(lab)), cex.axis=0.8, cex.lab=0.8)
	}
	if(n < nc^2){
		plot(0, 0, axes=F, type="n", ylab="")
		legend("center", rev(scen), lty=1, lwd=2, col=rev(clr), bty="n", cex=1)
	}
}

vc_tsplot(d=d.agg2.sub, loc="Alaska")

# @knitr veg_change_ts_LCC1
# Projected vegetation trend, Figure 6.3
vc_tsplot(d=d.agg2.sub, loc=regions[1])

# @knitr veg_change_ts_LCC2
# Projected vegetation trend, Figure 6.3
vc_tsplot(d=d.agg2.sub, loc=regions[2])

# @knitr veg_change_ts_LCC3
# Projected vegetation trend, Figure 6.3
vc_tsplot(d=d.agg2.sub, loc=regions[3])

# @knitr veg_change_ts_LCC4
# Projected vegetation trend, Figure 6.3
vc_tsplot(d=d.agg2.sub, loc=regions[4])

# @knitr veg_change_ts_LCC5
# Projected vegetation trend, Figure 6.3
vc_tsplot(d=d.agg2.sub, loc=regions[5])
