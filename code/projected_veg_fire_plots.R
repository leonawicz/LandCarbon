setwd("/atlas_scratch/mfleonawicz/projects/LandCarbon/workspaces")
inDir <- "/workspace/UA/mfleonawicz/projects/SNAPQAQC/data/final/alfresco/samples/Political Boundaries/Alaska"
dir.create(outDir <- "/workspace/UA/mfleonawicz/tmpDir/plots/test", recursive=TRUE, showWarnings=FALSE)
lapply(c("data.table", "dplyr", "ggplot2", "rvtable"), library, character.only=T)
#source("../../SNAPQAQC/code/alfresco/functions.R")
vegtypes <- c("Black Spruce", "White Spruce", "Deciduous", "Shrub Tundra", "Graminoid Tundra")
lev <- c("Spruce", vegtypes[3:5])
load(file.path(inDir, "vegarea.RData"))
d <- filter(d.alf.vegarea, Scenario=="SRES A1B" & Model %in% c("CCCMAcgcm31", "MPIecham5") & Vegetation %in% vegtypes) %>%
  mutate(Val=Val/10000) %>% group_by(Phase, Scenario, Model, Location, Var, Year)
d <- filter(d, Vegetation %in% vegtypes[1:2]) %>% rvtable %>% marginalize("Vegetation") %>% mutate(Vegetation=factor("Spruce", levels=lev)) %>%
    bind_rows(filter(d, Vegetation %in% vegtypes[3:5]) %>% mutate(Vegetation=factor(Vegetation, levels=lev))) %>% data.table %>%
    group_by(Phase, Scenario, Model, Location, Var, Vegetation, Year) %>% rvtable %>% sample_rvtable

lb <- 0.025
ub <- 0.975
dx <- summarise(d, LB=quantile(Val, lb), Mean=mean(Val), UB=quantile(Val, ub)) %>% mutate(Magnitude=UB-LB)

load(file.path(inDir, "baByVeg.RData"))
d2 <- filter(d.alf.ba, Scenario=="SRES A1B" & Model %in% c("CCCMAcgcm31", "MPIecham5") & Vegetation=="All") %>%
    group_by(Phase, Scenario, Model, Location, Var, Vegetation, Year) %>% mutate(Val=Val/10000) %>%
    group_by(Year, add=T) %>% rvtable
d2b <- ungroup(d2) %>% mutate(Decade = Year - Year %% 10) %>% filter(Decade >= 2010 & Decade < 2100) %>%
  group_by(Phase, Scenario, Model, Location, Var, Vegetation, Decade) %>% rvtable %>% marginalize("Year") %>% sample_rvtable
d2 <- sample_rvtable(d2)
d2a <- summarise(d2, LB=quantile(Val, lb), Mean=mean(Val), UB=quantile(Val, ub)) %>% mutate(Magnitude=UB-LB)
d2b <- summarise(d2b, LB=quantile(Val, lb), Mean=mean(Val), UB=quantile(Val, ub)) %>% mutate(Magnitude=UB-LB)

save(dx, d2, d2a, d2b, file="/workspace/UA/mfleonawicz/tmpDir/projected_veg_fire.RData")

#### LOCAL ####
setwd("C:/github/LandCarbon/workspaces")
dir.create(outDir <- "C:/github/LandCarbon/plots/test", recursive=TRUE, showWarnings=FALSE)
lapply(c("data.table", "dplyr", "ggplot2", "rvtable"), library, character.only=T)

load("projected_veg_fire.RData")

e <- element_blank()
tp_theme <- theme(panel.grid=e, strip.background=e,
    panel.background=element_rect(fill="transparent"), plot.background=element_rect(fill="transparent"))

insert_minor <- function(major_labs, n_minor) {
  labs <- c( sapply( major_labs, function(x) c(x, rep("", n_minor) ) ) )
  labs[1:(length(labs)-n_minor)]
}

fmt_decimals <- function(decimals=0){
  function(x) format(x, nsmall=decimals, scientific=FALSE)
}

tp_theme <-
  #increase size of gridlines
  theme(panel.grid.major = element_line(size = .5, color = "grey"),
  plot.title=element_text(hjust=0.5),
  #increase size of axis lines
  axis.line = element_line(size=.7, color = "black"),
  #Adjust legend position to maximize space, use a vector of proportion
  #across the plot and up the plot where you want the legend.
  #You can also use "left", "right", "top", "bottom", for legends on t
  #he side of the plot
  axis.ticks.length=unit(0.35,"cm"),

  #legend.position = c(.075,.9),
  legend.position = c(.075,.325),
  #legend.position = c(.9125,.9125),

  #increase the font size
  text = element_text(size=14),
  panel.spacing.x=unit(0.25,"cm"),
  plot.margin=unit(c(0.5, 1, 0.5, 0.5),"cm"),
  strip.text=element_text(size=14))


png(file.path(outDir, "ts_veg_fixedY.png"), height=2000, width=4000, res=300)
g <- ggplot(dx %>% filter(Year >= 2010), aes(Year, Mean, colour=Model)) +
  geom_ribbon(aes(ymin=LB, ymax=UB, colour=NULL, group=Model), fill="#00000020") + geom_line(size=2) + geom_line(aes(y=LB)) + geom_line(aes(y=UB)) +
  theme_bw(base_size = 14) +
  #theme(legend.position="bottom", legend.box="horizontal", legend.title=element_blank(), strip.text=element_text(size=12)) +
  labs(title="Projected vegetation area", y=expression(Area~(Million~Hectares))) +
  scale_x_continuous(expand = c(0,0), breaks=seq(2010, 2100, by=5),
    minor_breaks=seq(2010, 2100, by=2), labels=insert_minor(seq(2010, 2100, by=10), 1)) +
  scale_y_continuous(expand=c(0,0.5), labels=fmt_decimals) +
  facet_wrap(~Vegetation, scales="fixed") + tp_theme
g
dev.off()

png(file.path(outDir, "ts_veg_freeY.png"), height=2000, width=4000, res=300)
g <- ggplot(dx %>% filter(Year >= 2010), aes(Year, Mean, colour=Model)) +
  geom_ribbon(aes(ymin=LB, ymax=UB, colour=NULL, group=Model), fill="#00000020") + geom_line(size=2) + geom_line(aes(y=LB)) + geom_line(aes(y=UB)) +
  theme_bw(base_size = 14) +
  #theme(legend.position="bottom", legend.box="horizontal", legend.title=element_blank(), strip.text=element_text(size=12)) +
  labs(title="Projected vegetation area", y=expression(Area~(Million~Hectares))) +
  scale_x_continuous(expand = c(0,0), breaks=seq(2010, 2100, by=5),
    minor_breaks=seq(2010, 2100, by=2), labels=insert_minor(seq(2010, 2100, by=10), 1)) +
  scale_y_continuous(expand=c(0,0.5), labels=fmt_decimals(1)) +
  facet_wrap(~Vegetation, scales="free") + tp_theme
g
dev.off()




png(file.path(outDir, "ts_ba.png"), height=2000, width=4000, res=300)
ggplot(d2a %>% filter(Year >= 2010), aes(Year, Mean, fill=Model)) + geom_bar(stat="identity", position=position_dodge()) + geom_errorbar(aes(ymin=LB, ymax=UB), position=position_dodge(width=0.9)) +
  #geom_ribbon(aes(ymin=LB, ymax=UB, colour=NULL, group=Model), fill="#00000020") + geom_point(size=2) + geom_line(aes(y=LB)) + geom_line(aes(y=UB)) +
  theme_bw(base_size = 14) +
  #theme(legend.position="bottom", legend.box="horizontal", legend.title=element_blank(), strip.text=element_text(size=12)) +
  labs(title="Projected burn area", y=expression(Area~(Million~Hectares))) +
  scale_x_continuous(expand = c(0,0), breaks=seq(2010, 2100, by=5),
    minor_breaks=seq(2010, 2100, by=2), labels=insert_minor(seq(2010, 2100, by=10), 1)) +
  scale_y_continuous(expand=c(0,0)) +
  coord_cartesian(ylim=c(0,6)) +
  #facet_wrap(~Vegetation, scales="free") +
  tp_theme
dev.off()

labs.dec <- paste0(seq(2010, 2090, by=10), "s")
png(file.path(outDir, "ts_ba2.png"), height=2000, width=4000, res=300)
ggplot(d2b, aes(Decade, Mean, fill=Model)) + geom_bar(stat="identity", position="dodge") + geom_errorbar(aes(ymin=LB, ymax=UB), position=position_dodge()) +
  #geom_ribbon(aes(ymin=LB, ymax=UB, colour=NULL, group=Model), fill="#00000020") + geom_point(size=2) + geom_line(aes(y=LB)) + geom_line(aes(y=UB)) +
  theme_bw() + theme(legend.position="bottom", legend.box="horizontal", legend.title=element_blank(),
      strip.text=element_text(size=12)) +
  labs(title="Projected burn area", y=expression(Area~(Million~Hectares))) +
  scale_x_continuous(expand = c(0,0), breaks=c(2008, seq(2010, 2100, by=5)),
    minor_breaks=seq(2010, 2100, by=2), labels=c(2008, "", "", insert_minor(seq(2020, 2100, by=10), 1))) +
  scale_y_continuous(expand=c(0,0.5)) +
  #facet_wrap(~Vegetation, scales="free") +
  tp_theme
dev.off()

png(file.path(outDir, "ts_ba3.png"), height=2000, width=4000, res=300)
ggplot(d2 %>% sample_frac(0.2), aes(Year, Val, fill=Model, colour=Model)) + geom_point(size=1, alpha=0.1, position=position_jitterdodge(jitter.width=0.5)) +
    #geom_ribbon(aes(ymin=LB, ymax=UB, colour=NULL, group=Model), fill="#00000020") + geom_point(size=2) + geom_line(aes(y=LB)) + geom_line(aes(y=UB)) +
    theme_bw() + theme(legend.position="bottom", legend.box="horizontal", legend.title=element_blank(),
        strip.text=element_text(size=12)) +
    labs(title="Projected burn area", y=expression(Area~(Million~Hectares))) +
    scale_x_continuous(expand = c(0,0)) +
    #facet_wrap(~Vegetation, scales="free") +
    tp_theme + guides(colour=guide_legend(override.aes=list(alpha=1)))
dev.off()
