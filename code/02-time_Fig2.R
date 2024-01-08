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

dat_sp <- readRDS("data/species.rds")
dat_gen <- readRDS("data/genus.rds")

levs_old <- sort(unique(dat_sp$sp_named_after))
levs_old <- levs_old[levs_old != ""]
levs <- c("factual", "hypothesis", 
                "honouring", "hypothesis",
                "other", "honouring", 
                "other", "factual", 
                "factual", "other",
                "honouring", "factual", "unknown")

names(levs) <- levs_old

sp <- dat_sp %>% 
  filter(!taxon_status %in% c("recombined",
                                               "corrected", 
                                               "corrected to")) %>% 
  select(genus, species, ref_pubyr, named_after=sp_named_after)

gen <- dat_gen %>% 
  arrange(ref_pubyr) %>% 
  distinct(genus, gen_named_after, .keep_all = T) %>% 
  select(genus, ref_pubyr, named_after=gen_named_after)

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
ggsave("figs/Fig_02_time.png", p, width=8, height = 5)

# Supplement
summ_ind <- list(gen, sp)

pp <- map2(summ_ind, c("genera", "species"), ~{
  .x$named_after_broad <- recode(.x$named_after, !!!levs)
  .x <- .x[.x$named_after_broad != "unknown",]
  
  .x$named_after_broad <- factor(.x$named_after_broad,
                                      levels=rev(c("honouring", "hypothesis", "factual", "other")))
  
  pal <- RColorBrewer::brewer.pal(5, "PRGn")[-1]
  pal <- pal[c(2,1,3,4)]
  pal <- scales::alpha(pal, 0.9)
  
  p1 <- ggplot(data=.x) + 
    geom_density(aes(x = ref_pubyr, after_stat(count), fill = named_after_broad),
                 position = "fill",
                 colour="grey80") +
    scale_fill_manual(values=pal) +
    labs(x="Year of publication", y=glue::glue("Percentage of\n{.y} described"),
         fill="Named after") +
    scale_y_continuous(breaks=seq(0,1, 0.25),
                       labels=seq(0,1, 0.25)*100)
  
  p2 <- ggplot(data=.x) +
    geom_density(aes(ref_pubyr, after_stat(count), fill = named_after_broad),
                 position = "stack", colour="grey80")+
    scale_fill_manual(values=pal) +
    labs(x="Year of publication", y=glue::glue("Number of\n{.y} described"),
         fill="Named after") 
  
  list(p1, p2)
  
})

p <- pp[[1]][[1]] + pp[[2]][[1]] +
  pp[[1]][[2]] + pp[[2]][[2]] +
  plot_layout(ncol=2, guides = "collect") +
  plot_annotation(tag_prefix="(", tag_suffix=")", tag_levels = "a")

ggsave("figs/Fig_S_time.png", p, width=8, height = 8, units="in")

