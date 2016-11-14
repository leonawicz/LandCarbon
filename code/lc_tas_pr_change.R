setwd("C:/github/LandCarbon/workspaces")
#setwd("/atlas_scratch/mfleonawicz/projects/LandCarbon/workspaces")

library(rasterVis)
library(grid)
library(gridExtra)
library(ggplot2)
library(data.table)
library(dplyr)
library(gtable)

# Data prep
load("lc_clim.RData")
shp <- shapefile("C:/github/DataExtraction/data/shapefiles/Political/Alaska.shp")
#shp <- shapefile("/atlas_scratch/mfleonawicz/projects/DataExtraction/data/shapefiles/Political/Alaska.shp")
b.t <- purrr::map(b.t, ~mask(crop(.x, shp), shp))
b.p <- purrr::map(b.p, ~mask(crop(.x, shp), shp))

id.cru <- "CRU_TS32"
id.cccma <- "cccma-cgcm3-1-t47"
id.echam <- "mpi-echam5"

cccma.delta <- list(tas=b.t[[id.cccma]] - b.t[[id.cru]], pr=b.p[[id.cccma]] / b.p[[id.cru]])
echam.delta <- list(tas=b.t[[id.echam]] - b.t[[id.cru]], pr=b.p[[id.echam]] / b.p[[id.cru]])
d.shp <- fortify(shp)

brick_to_dt <- function(b, model){
    data.table(rasterToPoints(b)) %>% melt(id.vars=c("x", "y"), variable.name="Season", value.name="Val") %>% mutate(Model=model)
}

d.tas <- brick_to_dt(cccma.delta$tas, "CCCMA") %>% bind_rows(brick_to_dt(echam.delta$tas, "ECHAM")) %>% data.table
d.pr <- brick_to_dt(cccma.delta$pr, "CCCMA") %>% bind_rows(brick_to_dt(echam.delta$pr, "ECHAM")) %>% data.table

library(purrr)
seasons <- c("Winter", "Spring", "Summer", "Fall")
tas <-bind_rows(
  as.data.frame(map(b.t, ~cellStats(.x, median))) %>% mutate(Var="Median", Season=seasons),
  as.data.frame(map(b.t, ~cellStats(.x, mean))) %>% mutate(Var="Mean", Season=seasons),
  as.data.frame(map(b.t, ~cellStats(.x, sd))) %>% mutate(Var="SD", Season=seasons)
)
tas <- melt(tas, id.vars=c("Var", "Season"), variable.name="Model", value.name="Precipitation")
tas <- dcast(tas, Season + Model ~ Var, value.var="Precipitation")
tas <- mutate(tas, Median=round(Median,1), Mean=round(Mean,1), SD=round(SD,1))

seasons <- c("Winter", "Spring", "Summer", "Fall")
pr <-bind_rows(
  as.data.frame(map(b.p, ~cellStats(.x, median))) %>% mutate(Var="Median", Season=seasons),
  as.data.frame(map(b.p, ~cellStats(.x, mean))) %>% mutate(Var="Mean", Season=seasons),
  as.data.frame(map(b.p, ~cellStats(.x, sd))) %>% mutate(Var="SD", Season=seasons)
)
pr <- melt(pr, id.vars=c("Var", "Season"), variable.name="Model", value.name="Precipitation")
pr <- dcast(pr, Season + Model ~ Var, value.var="Precipitation")
pr <- mutate(pr, Median=round(Median), Mean=round(Mean), SD=round(SD))
save(tas, pr, file="C:/leonawicz/tas_pr_table.RData")

# Prep ggplot theme settings
e <- element_blank()
tp_theme <- theme(axis.title.x=e, axis.title.y=e, axis.text.x=e, axis.text.y=e, axis.ticks=e, panel.grid=e, axis.line=e, strip.background=e,
    panel.background=element_rect(fill="transparent", colour=NA), plot.background=element_rect(fill="transparent", colour=NA))

# Store ggplot objects
make_plot <- function(d, variable, file, pal="Blues"){
  stopifnot(variable %in% c("Temperature", "Precipitation"))
  et <- expression(atop("Seasonal temperature deltas"~(degree~C), "2090-2099 model mean - 1950-2013 CRU 3.2 mean"))
  ep <- expression(atop("Seasonal precipitation deltas"~(mm), "2090-2099 model mean / 1950-2013 CRU 3.2 mean"))
  if(variable=="Temperature") e <- et else e <- ep
  rng.all <- range(d$Val)

  g1a <- ggplot(d, aes(x, y)) + geom_raster(aes(fill=Val)) +
      geom_polygon(data=shp, aes(long, lat, group=group), color="gray40", fill="transparent") +
      theme(panel.margin=unit(0, "cm"), plot.margin=unit(c(0,0,0,0), "mm"),
          legend.position="bottom", legend.box="horizontal", legend.title=element_blank(), legend.key.width=unit(2, "cm"),
          strip.text.x=element_text(size=12), strip.text.y=element_text(size=12, angle=180)) +
      facet_grid(Season~Model, switch="both") + tp_theme +
      scale_fill_distiller(palette=pal, breaks=seq(rng.all[1], rng.all[2], length=5)) +
      labs(title=e)

  rng <- group_by(d, Season) %>% summarise(Min=min(Val), Max=max(Val))
  rng2 <- group_by(d, Season, Model) %>% summarise(Min=min(Val), Max=max(Val)) %>% melt(id.vars=c("Season", "Model"), value.name="Val")
  get_seq <- function(i) round(seq(rng$Min[i], rng$Max[i], length=6), 1)
  get_ylim <- function(i){
    x <- as.numeric(unlist(dplyr::select(slice(rng, i), Min, Max)))
    ylim(x + c(-1, 1)*0.1*diff(x))
  }
  val.sq <- lapply(1:4, get_seq)
  ggpt <- lapply(1:4, function(i) geom_point(data=rng2 %>% filter(Season==c("Winter", "Spring", "Summer", "Fall")[i])))
  ggbp <- geom_boxplot(fill="gray", width=0.5, outlier.colour=NA)
  ylm <- lapply(1:4, get_ylim)
  ggann <- lapply(1:4, function(i) annotate("text", label=val.sq[[i]], x=1.5, y=val.sq[[i]], size=3))
  #ggann <- lapply(1:4, function(i) annotate("text", label=paste0(" - ", val.sq[[i]], " - "), x=1.5, y=val.sq[[i]], size=4))
  g2a <- ggplot(d %>% filter(Season=="Winter"), aes(x=Model, y=Val)) + ggbp + ggpt[[1]] + ylm[[1]] + ggann[[1]] + tp_theme
  g2b <- ggplot(d %>% filter(Season=="Spring"), aes(x=Model, y=Val)) + ggbp + ggpt[[2]] + ylm[[2]] + ggann[[2]] + tp_theme
  g2c <- ggplot(d %>% filter(Season=="Summer"), aes(x=Model, y=Val)) + ggbp + ggpt[[3]] + ylm[[3]] + ggann[[3]] + tp_theme
  g2d <- ggplot(d %>% filter(Season=="Fall"), aes(x=Model, y=Val)) + ggbp + ggpt[[4]] + ylm[[4]] + ggann[[4]] + tp_theme

  # Make plot grob and extract component grobs
  gt <- ggplotGrob(g1a)
  dev.off()
  panel <- gtable_filter(gt, "panel")
  stripleft <- gtable_filter(gt, "strip-left")
  stripbottom <- gtable_filter(gt, "strip-bottom")
  guidebox <- gtable_filter(gt, "guide-box")
  top <- gtable_filter(gt, "title")

  #### Make viewports
  # boxplots
  just <- c("center", "center")
  sq <- c(0.25, 0.5, 0.75, 1)-0.1
  w <- 0.3
  h <- 0.15
  x <- 0.5
  vp1 <- viewport(width=w, height=h, x=x, y=sq[1], just=just)
  vp2 <- viewport(width=w, height=h, x=x, y=sq[2], just=just)
  vp3 <- viewport(width=w, height=h, x=x, y=sq[3], just=just)
  vp4 <- viewport(width=w, height=h, x=x, y=sq[4], just=just)
  # outer box
  outerBox <- viewport(width = unit(1, "npc"), height = unit(1, "npc"))
  # inner box
  innerBox <- viewport(x=unit(0.525, "npc"), y=unit(0.5, "npc"), width=unit(0.95, "npc"), height=unit(0.9, "npc"))
  # left panel labels
  Vleft <- viewport(x=unit(0.05, "npc"), y=unit(0.5, "npc"), width=unit(0.05, "npc"), height=unit(0.9, "npc"))
  # bottom panel labels and color key
  Vbottom <- viewport(x=unit(0.5, "npc"), y=unit(0.025, "npc"), width=unit(1, "npc"), height=unit(1, "npc"))
  # top title
  Vtop <- viewport(x=unit(0.5, "npc"), y=unit(0.975, "npc"), width=unit(1, "npc"), height=unit(1, "npc"))

  # draw plot
  png(file, height=3600, width=3000, res=300)
  grid.newpage()
  pushViewport(outerBox)
  pushViewport(innerBox)
  grid.draw(panel)
  print(g2a, vp=vp1)
  print(g2b, vp=vp2)
  print(g2c, vp=vp3)
  print(g2d, vp=vp4)
  upViewport()
  pushViewport(Vbottom)
  grid.draw(stripbottom)
  grid.draw(guidebox)
  upViewport()
  pushViewport(Vleft)
  grid.draw(stripleft)
  upViewport()
  pushViewport(Vtop)
  grid.draw(top)
  popViewport()
  popViewport()
  dev.off()
}

make_plot(d.tas, "Temperature", "../plots/test/boxplots_on_maps_tas.png", pal="Oranges")
make_plot(d.pr, "Precipitation", "../plots/test/boxplots_on_maps_pr.png")
