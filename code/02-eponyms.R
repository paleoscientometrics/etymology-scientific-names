# Load packages -----------------------------------------------------------
library(tidyverse)
library(ggthemes)
library(extrafont)
library(patchwork)

# Plot option -------------------------------------------------------------
extrafont::loadfonts()

theme_set(theme_hc(base_family = "Roboto") +
		  	theme(axis.title.y = element_text(angle=90, vjust=1),
		  		  axis.title = element_text(face="bold"),
		  		  legend.title = element_text(face="bold"),
		  		  plot.tag = element_text(size=10))
)

pal <- c("#f0ead2", "#dde5b6", "#adc178", "#a98467", "#6c584c")
