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
## Last Modified: 2022-09-18
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
library(patchwork)

# Plot option -------------------------------------------------------------

theme_set(theme_hc() +
	theme(axis.title.y = element_text(angle=90, vjust=1),
		  axis.title = element_text(face="bold"),
		  legend.title = element_text(face="bold"),
		  plot.tag = element_text(size=10))
)

pal <- c("#f0ead2", "#dde5b6", "#adc178", "#a98467", "#6c584c")

# Load data ---------------------------------------------------------------

dat <- read.csv(file.path("data", "dino_data_cleaned.csv"), fileEncoding = "UTF-8") %>% 
  filter(data_enterer != "" & !group %in% c("trace", "egg"))

levs_old <- sort(unique(dat$sp_named_after))
levs_old <- levs_old[levs_old != ""]
levs <- c("factual", "hypothesis", 
                "honouring", "hypothesis",
                "other", "honouring", 
                "other", "factual", 
                "factual", "other",
                "honouring", "factual", "unknown")

names(levs) <- levs_old

sp <- dat %>% 
  filter(category=="sp" & !taxon_status %in% c("recombined",
                                               "corrected", 
                                               "corrected to")) %>% 
  select(taxon_name, ref_pubyr, named_after=sp_named_after)

gen <- dat %>% 
  arrange(ref_pubyr) %>% 
  filter(gen_named_after != "") %>%  # TODO: maybe also check the missing ones
  distinct(genus, gen_named_after, .keep_all = T) %>% # TODO: check if getting the oldest one
  select(taxon_name=genus, ref_pubyr, named_after=gen_named_after)

janitor::get_dupes(gen, taxon_name) # no duplicates

summ_sp <- sp %>% 
  bind_rows(gen)

summ_sp$named_after_broad <- recode(summ_sp$named_after, !!!levs)
summ_sp <- summ_sp[summ_sp$named_after_broad != "unknown",]

summ_sp$named_after_broad <- factor(summ_sp$named_after_broad,
                                    levels=rev(c("honouring", "hypothesis", "factual", "other")))

pal <- RColorBrewer::brewer.pal(5, "PRGn")[-1]
pal <- pal[c(2,1,3,4)]
pal <- scales::alpha(pal, 0.9)

p1 <- ggplot(data=summ_sp) + 
  geom_density(aes(x = ref_pubyr, after_stat(count), fill = named_after_broad),
               position = "fill",
               colour="grey80") +
  scale_fill_manual(values=pal) +
  labs(x="Year of publication", y="Percentage of\ndinosaurs described",
       fill="Named after") +
  scale_y_continuous(breaks=seq(0,1, 0.25),
                     labels=seq(0,1, 0.25)*100)

p2 <- ggplot(data=summ_sp) +
  geom_density(aes(ref_pubyr, after_stat(count), fill = named_after_broad),
               position = "stack", colour="grey80")+
  scale_fill_manual(values=pal) +
  labs(x="Year of publication", y="Number of\ndinosaurs described",
       fill="Named after") 

p <- p1 + p2 +
  plot_layout(ncol=2, guides = "collect") +
  plot_annotation(tag_prefix="(", tag_suffix=")", tag_levels = "a")

ggsave("figs/Fig_02_time.svg", p, width=8, height = 5)
