setwd("/atlas_scratch/mfleonawicz/projects/LandCarbon/workspaces")
inDir <- "/workspace/UA/mfleonawicz/projects/SNAPQAQC/data/final/alfresco/samples/Political Boundaries/Alaska"
lapply(c("data.table", "dplyr", "ggplot2"), library, character.only=T)
source("../../SNAPQAQC/code/alfresco/functions.R")
vegtypes <- c("Black Spruce", "White Spruce", "Deciduous", "Shrub Tundra", "Graminoid Tundra")
lev <- c("Spruce", vegtypes[3:5])
load(file.path(inDir, "vegarea.RData"))
d <- filter(d.alf.vegarea, Scenario=="SRES A1B" & Model %in% c("CCCMAcgcm31", "MPIecham5") & Vegetation %in% vegtypes) %>%
    disttable %>% mutate(Val=Val/10000) %>% group_by(Phase, Scenario, Model, Location, Var, Vegetation, Year)
d <- filter(d, Vegetation %in% vegtypes[1:2]) %>% marginalize("Vegetation") %>% mutate(Vegetation=factor("Spruce", levels=lev)) %>%
    bind_rows(filter(d, Vegetation %in% vegtypes[3:5]) %>% mutate(Vegetation=factor(Vegetation, levels=lev))) %>% data.table %>%
    group_by(Phase, Scenario, Model, Location, Var, Vegetation, Year) %>% disttable %>% uc_table

e <- element_blank()
tp_theme <- theme(panel.grid=e, strip.background=e,
    panel.background=element_rect(fill="transparent"), plot.background=element_rect(fill="transparent"))

png("../plots/test/ts_veg.png", height=2000, width=4000, res=300)
ggplot(d, aes(Year, Mean, colour=Model)) +
    geom_ribbon(aes(ymin=LB, ymax=UB, colour=NULL, group=Model), fill="#00000020") + geom_line(size=2) + geom_line(aes(y=LB)) + geom_line(aes(y=UB)) + 
    theme_bw() + theme(legend.position="bottom", legend.box="horizontal", legend.title=element_blank(),
        strip.text=element_text(size=12)) +
    labs(title="Projected vegetation area", y=expression(Area~(Million~Hectares))) +
    facet_wrap(~Vegetation, scales="free") + tp_theme
dev.off()

load(file.path(inDir, "baByVeg.RData"))
d2 <- filter(d.alf.ba, Scenario=="SRES A1B" & Model %in% c("CCCMAcgcm31", "MPIecham5") & Vegetation=="All") %>%
    group_by(Phase, Scenario, Model, Location, Var, Vegetation, Year) %>% disttable %>% mutate(Val=Val/10000) %>%
    group_by(Year, add=T)
d2a <- sample_densities(d2)
d2b <- byDecade(d2, decade.start.years=seq(2010,2090,by=10))
d2 <- uc_table(d2)
d2b <- uc_table(d2b)

png("../plots/test/ts_ba.png", height=2000, width=4000, res=300)
ggplot(d2, aes(Year, Mean, fill=Model)) + geom_bar(stat="identity", position=position_dodge()) + geom_errorbar(aes(ymin=LB, ymax=UB), position=position_dodge(width=0.9)) +
    #geom_ribbon(aes(ymin=LB, ymax=UB, colour=NULL, group=Model), fill="#00000020") + geom_point(size=2) + geom_line(aes(y=LB)) + geom_line(aes(y=UB)) + 
    theme_bw() + theme(legend.position="bottom", legend.box="horizontal", legend.title=element_blank(),
        strip.text=element_text(size=12)) +
    labs(title="Projected burn area", y=expression(Area~(Million~Hectares))) +
    #facet_wrap(~Vegetation, scales="free") +
    tp_theme
dev.off()

png("../plots/test/ts_ba2.png", height=2000, width=4000, res=300)
ggplot(d2b, aes(Decade, Mean, fill=Model)) + geom_bar(stat="identity", position=position_dodge()) + geom_errorbar(aes(ymin=LB, ymax=UB), position=position_dodge(.9), width=0.5) +
    #geom_ribbon(aes(ymin=LB, ymax=UB, colour=NULL, group=Model), fill="#00000020") + geom_point(size=2) + geom_line(aes(y=LB)) + geom_line(aes(y=UB)) + 
    theme_bw() + theme(legend.position="bottom", legend.box="horizontal", legend.title=element_blank(),
        strip.text=element_text(size=12)) +
    labs(title="Projected burn area", y=expression(Area~(Million~Hectares))) +
    #facet_wrap(~Vegetation, scales="free") +
    tp_theme
dev.off()

png("../plots/test/ts_ba3.png", height=2000, width=4000, res=300)
ggplot(d2a %>% sample_frac(0.2), aes(Year, Val, fill=Model, colour=Model)) + geom_point(size=1, alpha=0.1, position=position_jitterdodge(jitter.width=0.5)) +
    #geom_ribbon(aes(ymin=LB, ymax=UB, colour=NULL, group=Model), fill="#00000020") + geom_point(size=2) + geom_line(aes(y=LB)) + geom_line(aes(y=UB)) + 
    theme_bw() + theme(legend.position="bottom", legend.box="horizontal", legend.title=element_blank(),
        strip.text=element_text(size=12)) +
    labs(title="Projected burn area", y=expression(Area~(Million~Hectares))) +
    #facet_wrap(~Vegetation, scales="free") +
    tp_theme + guides(colour=guide_legend(override.aes=list(alpha=1)))
dev.off()
