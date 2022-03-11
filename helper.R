#######################################################################
# Title: helper.R
# Author: B. Hetman
# Date: 2020-03-08
# Description: This file contains custom functions and code used in 
# the analysis of C. jejuni source attribution from S. Alberta, Canada
#######################################################################


#The following packages are required for the analysis. 
#Load libraries if installed. If not, install then load.


# install.packages("pacman")


pacman::p_load(tidyverse,
               extrafont,
               devtools,
               ggsci,
               ggthemes,
               hrbrthemes,
               ggpubr,
               flextable,
               clipr,
               officer,
               here 
)

trace(grDevices:::png, quote({
  if (missing(type) && missing(antialias)) {
    type <- "cairo-png"
    antialias <- "subpixel"
  }
}), print = FALSE)

# Note: several of the plots below make use of the hrbrthemes() package and extrafonts
# Needed to troubleshoot to obtain the correct fonts
# remotes::install_version("Rttf2pt1", version = "1.3.8")
# extrafont::loadfonts()
# extrafont::font_import()


###################################################
##### Define Custom Functions: 
###################################################



upper.ci <- function(p, n){
  p.hat <- p/n
  ul <- p.hat + 1.96*sqrt(((p.hat*(1-p.hat))/n))
  ul <- round(ul, 3)
  return(ul)
}

lower.ci <- function(p, n){
  p.hat <- p/n
  ll <- p.hat - 1.96*sqrt(((p.hat*(1-p.hat))/n))
  ll <- ifelse(ll < 0.0001, 0, round(ll, 3))
  return(ll)
}
