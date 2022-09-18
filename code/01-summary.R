## ---------------------------
##
## Project: Etymology of Dinosaur Names
##
## Purpose of script: 
## Summarises the different etymologies of dinosaur names overall and over time
##
## Author: Nussaïbah B. Raja
## Copyright (c) N. Raja, 2021
## Email: nussaibah.raja.schoob@fau.de
##
## Date Created: 2021-08-16
## Last Modified: 2021-08-17
##
## ---------------------------
##
## Notes:
##   
##
## ---------------------------

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

# Load data ---------------------------------------------------------------

dat <- read.csv(file.path("data", "dino_data_cleaned.csv")) %>% 
	filter(data_enterer != "" & group != "trace")

dat$ref_pubyr[dat$taxon_name=="Apatosaurus excelsus"] <- 1879

summ_sp <- dat %>% group_by(sp_named_after) %>% 
	tally() %>% 
	na.omit() %>% 
	ungroup() %>% 
	mutate(prop = n/sum(n))

summ_sp$sp_named_after <- factor(summ_sp$sp_named_after, 
								 levels=(summ_sp %>% arrange(-prop) %>% 
								 	pull(sp_named_after))
)

p1 <- ggplot(summ_sp, aes(x=reorder(sp_named_after, prop), 
						  y=prop*100, fill=sp_named_after)) +
	geom_bar(stat="identity") +
	geom_text(aes(label=n,
				  hjust=ifelse(prop >0.1, 1, 0),
				  col=ifelse(prop>0.1, "white", "black")), fontface=2) +
	labs(x="species named after", y="percentage") +
	coord_flip() +
	scale_fill_manual(values=pal[c(4,3,2,rep(1, 8))]) +
	scale_color_manual(values=c("black", "white"))+
	theme(legend.position = "none")

p1

p2 <- dat %>% mutate(group = ifelse(sp_named_after %in% c("person", "location/geography", "morphological characteristics"), sp_named_after, "other")) %>% 
	bind_rows(dat %>% mutate(group = "all" )) %>% 
	group_by(group, ref_pubyr) %>% 
	tally() %>% 
	group_by(group) %>% 
	mutate(ra = caTools::runmean(n, 3, alg="C"),
		   group= factor(group, levels=c("all", "person", "location/geography", 
		   							  "morphological characteristics", "other"))) %>% 
	ggplot(aes(x=ref_pubyr, y=ra, col=group)) +
	geom_line(size=0.8) +
	labs(x="year", y="number of species described") +
	scale_color_manual(values=pal[c(5:1)]) +
	guides(col = guide_legend(ncol = 1)) +
	theme(legend.position = c(0, 1),
		  legend.justification = c(0, 1),
		  legend.title = element_blank(),
		  legend.background = element_rect(colour = NA, fill = "white"))

svg("plots/Fig_01_summary.svg", w=10,h=4)
p2 + p1 +
	plot_layout(ncol=2, widths = c(1,0.7)) +
	plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")")
dev.off()

