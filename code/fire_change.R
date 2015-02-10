# @knitr setup
wd <- basename(getwd())
region.grp <- "LCC Regions"
#mainDir <- "/workspace/UA/mfleonawicz/leonawicz/projects/SNAPQAQC/data/final"
mainDir <- "X:/leonawicz/projects/SNAPQAQC/data/final"
ak.statsFire.file <- file.path(mainDir, "region_files_GCM/stats/Political/Alaska/stats_fire.RData")
#ak.samples.file <- file.path(mainDir, "region_files_GCM/samples/Political/Alaska/xxxxxxxxxxxx.RData")
#load(file.path(mainDir, "meta.RData"))
load("C:/github/shiny-apps/cmip3_cmip5/external/meta.RData")
statDir <- file.path(mainDir, "region_files_GCM/stats", region.grp)
sampDir <- file.path(mainDir, "region_files_GCM/samples", region.grp)
library(reshape2)
library(plyr)
library(xtable)

# @knitr fire_change_setup
# Projected wildland fire change, Table 8.1
years.all <- 2000:2099
years <- range(years.all)
years1 <- 2000:2009
years2 <- 2090:2099
modnames <- "CCCMAcgcm31" # "MPIecham5" # 
keep.cols <- c(2:stats.columns[1], stats.columns[1] + c(2,5,8), (tail(stats.columns, 1) + 1):ncol(alf.fireStats.df))
files <- list.files(statDir, pattern="^stats_fire.RData$", full=TRUE, recursive=TRUE)
files <- c(files, ak.statsFire.file)
regions <- basename(dirname(files))
for(i in 1:length(files)){
	load(files[i])
	alf.fireStats.df$Location <- regions[i]
	alf.fireStats.df[,stats.columns] <- region.dat
	if(i==1) d <- alf.fireStats.df else d <- rbind(d, alf.fireStats.df)
}
rm(region.dat)

d <- subset(d, Model %in% modnames)
d$Location <- gsub(" S", " South", gsub(" N", " North", gsub("W ", "Western ", gsub("N ", "North ", gsub("NW ", "Northwest ", d$Location))))) # Special name changes
regions <- unique(d$Location[d$Location!="Alaska"])
reg.split2 <- reg.split <- regions[2:5]
reg.split2[1] <- "\\\\parbox[t]{3cm}{\\\\centering North\\\\\\\\Pacific}"
reg.split2[2] <- "\\\\parbox[t]{3cm}{\\\\centering Northwest Interior\\\\\\\\Forest North}"
reg.split2[3] <- "\\\\parbox[t]{3cm}{\\\\centering Northwest Interior\\\\\\\\Forest South}"
reg.split2[4] <- "\\\\parbox[t]{3cm}{\\\\centering Western\\\\\\\\Alaska}"

d$Scenario <- factor(d$Scenario, levels=c("SRES B1","SRES A1B","SRES A2"))
d.all <- subset(d, Year %in% years.all, select=keep.cols)
d1 <- subset(d, Year %in% years1, select=keep.cols)
d2 <- subset(d, Year %in% years2, select=keep.cols)
#d.agg1 <- ddply(d, c("Scenario", "Location", "Vegetation", "Year", "Decade"), summarise, Avg=mean(Mean))
#d.agg2 <- ddply(d2, c("Scenario", "Location", "Vegetation", "Year", "Decade"), summarise, Avg=mean(Mean))

d.all.agg1 <- ddply(d.all, c("Scenario", "Var", "Location", "Decade"), summarise, ZZZ5th=quantile(Pct_05, probs=0.05), ZZZ50th=median(Pct_50), ZZZ95th=quantile(Pct_95, probs=0.95))

hack_plot <- function(d, type, add.legend=FALSE){
	if(type=="FC") ylb <- "Number of wildland fires per year" else if(type=="AB") ylb <- expression("Area burned per year (1000"~km^2~")")
	if(type=="AB") m <- 1000 else m <- 1
	scen <- levels(d$Scenario)
	n <- length(scen)
	scen.colors <- colorRampPalette(c("lightgreen", "dodgerblue", "magenta"))(n)
	decs <- as.numeric(unique(d$Decade))
	plot(0,0, xlim=range(decs), ylim=c(0, max(d$ZZZ95th/m)), type="n", xlab="", ylab=ylb, las=1)
	for(i in 1:n){
		d2 <- subset(d, Scenario==scen[i])
		lines(decs, d2$ZZZ5th/m, col=scen.colors[i], lwd=2, lty=3)
		lines(decs, d2$ZZZ95th/m, col=scen.colors[i], lwd=2, lty=3)
		lines(decs, d2$ZZZ50th/m, col=scen.colors[i], lwd=2, lty=1)
	}
	if(add.legend){
		par(mar=c(2,0,2,1))
		plot(0, 0, axes=F, type="n", xlab="", ylab="", main="Climate-change scenario", cex.main=0.7)
		legend("bottom", c("5th/95th\nPercentile", "Median"), title=scen[1], lty=c(3,1), lwd=2, col=scen.colors[1], bty="n", cex=0.8, seg.len=3)
		legend("center", c("5th/95th\nPercentile", "Median"), title=scen[2], lty=c(3,1), lwd=2, col=scen.colors[2], bty="n", cex=0.8, seg.len=3)
		legend("top", c("5th/95th\nPercentile", "Median"), title=scen[3], lty=c(3,1), lwd=2, col=scen.colors[3], bty="n", cex=0.8, seg.len=3)
	}
}

d1.agg1 <- ddply(d1, c("Scenario", "Var", "Location"), summarise, ZZZ50th=median(Pct_50), ZZZ95th=quantile(Pct_95, probs=0.95))
d2.agg1 <- ddply(d2, c("Scenario", "Var", "Location"), summarise, ZZZ50th=median(Pct_50), ZZZ95th=quantile(Pct_95, probs=0.95))

morph_df <- function(d){
	d <- melt(d, id.vars=c("Scenario","Var","Location"))
	d <- dcast(d, Scenario + Location + variable ~ Var)[,-2][c(1,2,4,3)]
	names(d) <- c("Climate-change scenario", "Percentile", "Ignitions", "Area burned")
	d$Percentile <- gsub("ZZZ", "", d$Percentile)
	d["Area burned"] <- round(d["Area burned"])
	d
}

org_prop_df <- function(d1, d2){
	d <- d1
	d["Ignitions"] <- round(100*(d2["Ignitions"]/d1["Ignitions"] - 1), 2)
	d["Area burned"] <- round(100*(d2["Area burned"]/d1["Area burned"] - 1), 2)
	ind <- which(is.nan(d$Ignitions) | is.na(d$Ignitions))
	if(length(ind)) { d$Ignitions[ind] <- d[["Area burned"]][ind] <- "-" }
	d
}

print_tab <- function(d){
	print(d, booktabs=TRUE, print.results=FALSE, include.rownames=FALSE,
	add.to.row=list(pos=list(-1), command=c("\\headcol \n") #, "\\midrule \n\\rowcol \\multicolumn{7}{c}{Number of wildfires per year} \\\\ \n")
	))
}

#########################################################################################################

# @knitr fire_change_table1_AK
d1.agg1.sub <- morph_df(subset(d1.agg1, Location=="Alaska"))
d2.agg1.sub <- morph_df(subset(d2.agg1, Location=="Alaska"))
d.prop <- org_prop_df(d1.agg1.sub, d2.agg1.sub)
d1.tab <- xtable(d1.agg1.sub, digits=0, align=c("l", "l", rep("c", ncol(d1.agg1.sub)-1)))
d2.tab <- xtable(d2.agg1.sub, digits=0, align=c("l", "l", rep("c", ncol(d2.agg1.sub)-1)))
d.diff.tab <- xtable(d.prop, digits=1, align=c("l", "l", rep("c", ncol(d2.agg1.sub)-1)))
#names(d.diff.tab)[3:4] <- paste("% change in", tolower(names(d.diff.tab)[3:4]))
if(wd=="Rmd") print(d1.tab, type="html")
tmp <- print_tab(d1.tab)
# @knitr fire_change_table2_AK
if(wd=="Rmd") print(d2.tab, type="html")
tmp <- print_tab(d2.tab)
# @knitr fire_change_table_dif_AK
if(wd=="Rmd") print(d.diff.tab, type="html")
tmp <- print_tab(d.diff.tab)

# @knitr fire_change_table1_LCC1
d1.agg1.sub <- morph_df(subset(d1.agg1, Location==regions[1]))
d2.agg1.sub <- morph_df(subset(d2.agg1, Location==regions[1]))
d.prop <- org_prop_df(d1.agg1.sub, d2.agg1.sub)
d1.tab <- xtable(d1.agg1.sub, digits=0, align=c("l", "l", rep("c", ncol(d1.agg1.sub)-1)))
d2.tab <- xtable(d2.agg1.sub, digits=0, align=c("l", "l", rep("c", ncol(d2.agg1.sub)-1)))
d.diff.tab <- xtable(d.prop, digits=1, align=c("l", "l", rep("c", ncol(d2.agg1.sub)-1)))
if(wd=="Rmd") print(d1.tab, type="html")
tmp <- print_tab(d1.tab)
# @knitr fire_change_table2_LCC1
if(wd=="Rmd") print(d2.tab, type="html")
tmp <- print_tab(d2.tab)
# @knitr fire_change_table_dif_LCC1
if(wd=="Rmd") print(d.diff.tab, type="html")
tmp <- print_tab(d.diff.tab)

# @knitr fire_change_table1_LCC2
d1.agg1.sub <- morph_df(subset(d1.agg1, Location==regions[2]))
d2.agg1.sub <- morph_df(subset(d2.agg1, Location==regions[2]))
d.prop <- org_prop_df(d1.agg1.sub, d2.agg1.sub)
d1.tab <- xtable(d1.agg1.sub, digits=0, align=c("l", "l", rep("c", ncol(d1.agg1.sub)-1)))
d2.tab <- xtable(d2.agg1.sub, digits=0, align=c("l", "l", rep("c", ncol(d2.agg1.sub)-1)))
d.diff.tab <- xtable(d.prop, digits=1, align=c("l", "l", rep("c", ncol(d2.agg1.sub)-1)))
if(wd=="Rmd") print(d1.tab, type="html")
tmp <- print_tab(d1.tab)
# @knitr fire_change_table2_LCC2
if(wd=="Rmd") print(d2.tab, type="html")
tmp <- print_tab(d2.tab)
# @knitr fire_change_table_dif_LCC2
if(wd=="Rmd") print(d.diff.tab, type="html")
tmp <- print_tab(d.diff.tab)

# @knitr fire_change_table1_LCC3
d1.agg1.sub <- morph_df(subset(d1.agg1, Location==regions[3]))
d2.agg1.sub <- morph_df(subset(d2.agg1, Location==regions[3]))
d.prop <- org_prop_df(d1.agg1.sub, d2.agg1.sub)
d1.tab <- xtable(d1.agg1.sub, digits=0, align=c("l", "l", rep("c", ncol(d1.agg1.sub)-1)))
d2.tab <- xtable(d2.agg1.sub, digits=0, align=c("l", "l", rep("c", ncol(d2.agg1.sub)-1)))
d.diff.tab <- xtable(d.prop, digits=1, align=c("l", "l", rep("c", ncol(d2.agg1.sub)-1)))
if(wd=="Rmd") print(d1.tab, type="html")
tmp <- print_tab(d1.tab)
# @knitr fire_change_table2_LCC3
if(wd=="Rmd") print(d2.tab, type="html")
tmp <- print_tab(d2.tab)
# @knitr fire_change_table_dif_LCC3
if(wd=="Rmd") print(d.diff.tab, type="html")
tmp <- print_tab(d.diff.tab)

# @knitr fire_change_table1_LCC4
d1.agg1.sub <- morph_df(subset(d1.agg1, Location==regions[4]))
d2.agg1.sub <- morph_df(subset(d2.agg1, Location==regions[4]))
d.prop <- org_prop_df(d1.agg1.sub, d2.agg1.sub)
d1.tab <- xtable(d1.agg1.sub, digits=0, align=c("l", "l", rep("c", ncol(d1.agg1.sub)-1)))
d2.tab <- xtable(d2.agg1.sub, digits=0, align=c("l", "l", rep("c", ncol(d2.agg1.sub)-1)))
d.diff.tab <- xtable(d.prop, digits=1, align=c("l", "l", rep("c", ncol(d2.agg1.sub)-1)))
if(wd=="Rmd") print(d1.tab, type="html")
tmp <- print_tab(d1.tab)
# @knitr fire_change_table2_LCC4
if(wd=="Rmd") print(d2.tab, type="html")
tmp <- print_tab(d2.tab)
# @knitr fire_change_table_dif_LCC4
if(wd=="Rmd") print(d.diff.tab, type="html")
tmp <- print_tab(d.diff.tab)

# @knitr fire_change_table1_LCC5
d1.agg1.sub <- morph_df(subset(d1.agg1, Location==regions[5]))
d2.agg1.sub <- morph_df(subset(d2.agg1, Location==regions[5]))
d.prop <- org_prop_df(d1.agg1.sub, d2.agg1.sub)
d1.tab <- xtable(d1.agg1.sub, digits=0, align=c("l", "l", rep("c", ncol(d1.agg1.sub)-1)))
d2.tab <- xtable(d2.agg1.sub, digits=0, align=c("l", "l", rep("c", ncol(d2.agg1.sub)-1)))
d.diff.tab <- xtable(d.prop, digits=1, align=c("l", "l", rep("c", ncol(d2.agg1.sub)-1)))
if(wd=="Rmd") print(d1.tab, type="html")
tmp <- print_tab(d1.tab)
# @knitr fire_change_table2_LCC5
if(wd=="Rmd") print(d2.tab, type="html")
tmp <- print_tab(d2.tab)
# @knitr fire_change_table_dif_LCC5
if(wd=="Rmd") print(d.diff.tab, type="html")
tmp <- print_tab(d.diff.tab)

#########################################################################################################

# @knitr fire_change_ts_AK
# Projected wildland fire change, figure 8.2
d.all.agg1.sub <- subset(d.all.agg1, Location=="Alaska")

layout(matrix(c(1,1,2,2,3), nrow=1))
par(mar=c(3,5,1,0))
hack_plot(subset(d.all.agg1.sub, Var=="Fire Count"), type="FC")
par(mar=c(3,5,1,0))
hack_plot(subset(d.all.agg1.sub, Var=="Burn Area"), type="AB", add.legend=TRUE)

# Projected wildland fire change, figure 8.3, continuing

# @knitr fire_change_ts_LCC1
d.all.agg1.sub <- subset(d.all.agg1, Location==regions[1])

layout(matrix(c(1,1,2,2,3), nrow=1))
par(mar=c(3,5,1,0))
hack_plot(subset(d.all.agg1.sub, Var=="Fire Count"), type="FC")
par(mar=c(3,5,1,0))
hack_plot(subset(d.all.agg1.sub, Var=="Burn Area"), type="AB", add.legend=TRUE)

# @knitr fire_change_ts_LCC2
d.all.agg1.sub <- subset(d.all.agg1, Location==regions[2])

layout(matrix(c(1,1,2,2,3), nrow=1))
par(mar=c(3,5,1,0))
hack_plot(subset(d.all.agg1.sub, Var=="Fire Count"), type="FC")
par(mar=c(3,5,1,0))
hack_plot(subset(d.all.agg1.sub, Var=="Burn Area"), type="AB", add.legend=TRUE)

# @knitr fire_change_ts_LCC3
d.all.agg1.sub <- subset(d.all.agg1, Location==regions[3])

layout(matrix(c(1,1,2,2,3), nrow=1))
par(mar=c(3,5,1,0))
hack_plot(subset(d.all.agg1.sub, Var=="Fire Count"), type="FC")
par(mar=c(3,5,1,0))
hack_plot(subset(d.all.agg1.sub, Var=="Burn Area"), type="AB", add.legend=TRUE)

# @knitr fire_change_ts_LCC4
d.all.agg1.sub <- subset(d.all.agg1, Location==regions[4])

layout(matrix(c(1,1,2,2,3), nrow=1))
par(mar=c(3,5,1,0))
hack_plot(subset(d.all.agg1.sub, Var=="Fire Count"), type="FC")
par(mar=c(3,5,1,0))
hack_plot(subset(d.all.agg1.sub, Var=="Burn Area"), type="AB", add.legend=TRUE)

# @knitr fire_change_ts_LCC5
d.all.agg1.sub <- subset(d.all.agg1, Location==regions[5])

layout(matrix(c(1,1,2,2,3), nrow=1))
par(mar=c(3,5,1,0))
hack_plot(subset(d.all.agg1.sub, Var=="Fire Count"), type="FC")
par(mar=c(3,5,1,0))
hack_plot(subset(d.all.agg1.sub, Var=="Burn Area"), type="AB", add.legend=TRUE)

# End figure 8.3
