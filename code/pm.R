# @knitr create_project
source("C:/github/ProjectManagement/code/rpm.R") # eventually load a package instead of source script
proj.name <- "LandCarbon" # Project name
proj.location <- matt.proj.path # Use default file location

docDir <- c("Rmd/include", "md", "html", "Rnw", "pdf", "timeline")
newProject(proj.name, proj.location, docs.dirs=docDir, overwrite=T) # create a new project

rfile.path <- file.path(proj.location, proj.name, "code") # path to R scripts
docs.path <- file.path(proj.location, proj.name, "docs")
rmd.path <- file.path(docs.path, "Rmd")

# generate Rmd files from existing R scripts using default yaml front-matter
genRmd(path=rfile.path, header=rmdHeader())

# @knitr update_project
# update yaml front-matter only
genRmd(path=rfile.path, header=rmdHeader(), update.header=TRUE)

# obtain knitr code chunk names in existing R scripts
chunkNames(path=file.path(proj.location, proj.name, "code"))

# append new knitr code chunk names found in existing R scripts to any Rmd files which are outdated
chunkNames(path=file.path(proj.location, proj.name, "code"), append.new=TRUE)

# @knitr website
# Setup for generating a project website
user <- "leonawicz"
proj.github <- file.path("https://github.com", user, proj.name)
index.url <- "index.html"
#file.copy(index.url, "index.html")

proj.title <- "Land Carbon"
proj.menu <- c("Fire", "Vegetation", "R Code", "All Projects")

proj.submenu <- list(
	c("Model: CCCMA", "Baseline fire", "Projected change", "divider", "Model: ECHAM5", "Baseline fire", "Projected change"),
	c("Model: CCCMA", "Vegetation change", "Vegetation trends", "divider", "Model: ECHAM5", "Vegetation change", "Vegetation trends"),
	c("Main code", "Baseline fire", "Projected fire", "Vegetation change", "Vegetation trends"),
	c("empty")
)

proj.files <- list(
	c("header", "baseline_fire_cccma.html", "fire_change_cccma.html", "divider", "header", "baseline_fire_echam.html", "fire_change_echam.html"),
	c("header", "vegetation_change_cccma.html", "vegetation_trend_cccma.html", "divider", "header", "vegetation_change_echam.html", "vegetation_trend_echam.html"),
	c("header", "baseline_fire_code.html", "fire_change_code.html", "vegetation_change_code.html", "vegetation_trend_code.html"),
	c("http://leonawicz.github.io")
)

# generate navigation bar html file common to all pages
genNavbar(htmlfile=file.path(proj.location, proj.name, "docs/Rmd/include/navbar.html"), title=proj.title, menu=proj.menu, submenus=proj.submenu, files=proj.files, title.url="index.html", home.url="index.html", site.url=proj.github, include.home=FALSE)

# generate _output.yaml file
# Note that external libraries are expected, stored in the "libs" below
yaml.out <- file.path(proj.location, proj.name, "docs/Rmd/_output.yaml")
libs <- "libs"
common.header <- "include/in_header.html"
genOutyaml(file=yaml.out, lib=libs, header=common.header, before_body="include/navbar.html")

# @knitr knit_setup
library(rmarkdown)
library(knitr)

# if also making PDFs for a project, speed up the Rmd to Rnw file conversion/duplication
rnw.path <- file.path(docs.path, "Rnw")
setwd(rnw.path)
#themes <- knit_theme$get()
highlight <- "tango"
convertDocs(path=rnw.path, emphasis="replace", overwrite=TRUE)#, highlight=highlight) # Take care not to reverse write
highlight <- "solarized-dark"
#convertDocs(path=rmd.path, emphasis="replace", overwrite=TRUE, highlight=highlight) # Take care not to reverse write
lapply(list.files(pattern=".Rnw$"), knit2pdf)
moveDocs(path.docs=docs.path, type="pdf", remove.latex=FALSE)

# Rmd files
setwd(rmd.path)
code.Rmd <- c("index.Rmd", list.files(pattern="code.Rmd$", full=T))
cccma.Rmd <- list.files(pattern="cccma.Rmd$", full=T)
echam.Rmd <- list.files(pattern="echam.Rmd$", full=T)

# @knitr save
# write all yaml front-matter-specified outputs to Rmd directory for all Rmd files
# Make sure to switch models and resave R scripts between following two lines
lapply(code.Rmd, render, output_format="all")
lapply(cccma.Rmd, render, output_format="all")
lapply(echam.Rmd, render, output_format="all")
insert_gatc(list.files(pattern=".html$"))
moveDocs(path.docs=docs.path)

