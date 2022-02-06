## ---------------------------
##
## Project: Etymology of Dinosaur Names
##
## Purpose of script: 
## Compares the country of the eponym to the type country and the affiliation(s) of
## the "namer(s)".
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
library(countrycode)
library(grid)

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

dat <- read.csv(file.path("data", "dino_data.csv")) %>% 
	filter(data_enterer != "" & group != "trace")

# Country -----------------------------------------------------------------

## Where are dinosaurs from
df.dino <- dat %>%  group_by(type_country) %>% 
	tally()

df.dino$code <- countrycode(df.dino$type_country, "country.name", "iso3c")

## Eponyms: People and where they are from ----
df.person <- dat %>%  filter(sp_named_after == "person") %>% 
	select(ref_pubyr, taxon_name, type_country, sp_if_person_country, 
		   sp_if_person_gender,
		   author_aff = affiliations_of_authors..separated.by...,
		   author_country = countries_of_authors..separated.by...)

# Note: need to sort out some country name, old names, disputed

df.person$type_code <- countrycode(df.person$type_country, "country.name", "iso3c") 
df.person$person_code <-    countrycode(df.person$sp_if_person_country, "country.name", "iso3c")

## Countries of researchers ----
affs <- strsplit(df.person$author_aff, ";")
counts <- strsplit(df.person$author_country, ";")

n <- unlist(lapply(counts, length))

for(i in which(n==1)){
	if(!is.na(counts[[i]])){
		counts[[i]] <- rep(counts[[i]], length(affs[[i]]))
	}
}

# Plot --------------------------------------------------------------------
world_map <- map_data("world")
world_map$code <- countrycode(world_map$region, "country.name", "iso3c")

world_map <- left_join(world_map, df.dino %>% 
					   	mutate(n=n/sum(n)), by="code")

## Map showing countries where dinos were found
p0 <- ggplot(world_map, aes(long, lat, group = group))+
	geom_polygon(aes(fill = n), color = "white")

## Add how many have eponyms
tot <-sum(df.person %>% 
	group_by(person_code) %>% 
	tally() %>% pull(n)
)

eponyms <- df.person %>% 
	group_by(person_code) %>% 
	tally(name="person") %>% 
	ungroup() %>% 
	mutate(person=person/tot) %>% 
	na.omit() %>% 
	left_join(world_map %>% 
			  	group_by(code) %>% 
			  	summarise(long=mean(long), lat=mean(lat)), by=c("person_code"="code"))


local_eponyms <- df.person %>% 
	filter(type_code!=person_code) %>% 
	group_by(person_code) %>% 
	tally(name="person") %>% 
	ungroup() %>% 
	mutate(person=person/tot) %>% 
	na.omit() %>% 
	left_join(world_map %>% 
			  	group_by(code) %>% 
			  	summarise(long=mean(long), lat=mean(lat)), by=c("person_code"="code"))


pal <- c("#d8f3dc",
		 "#40916c",
		 "#081c15")

p0 +	
	geom_point(data=eponyms,
			   aes(x=long, y=lat, size=person),
			   col="white",stroke=1.8,
			   inherit.aes = F, shape=1) +
	geom_point(data=eponyms,
			   aes(x=long, y=lat, size=person, col=person),
			   stroke=1, fill="white",
			   inherit.aes = F, shape=21) +
	geom_point(data=local_eponyms,
			   aes(x=long, y=lat, size=person),
			   col="darkorange",
			   stroke=0.8, linetype="dashed",
			   inherit.aes = F, shape=21) +
	scale_color_gradient2(limits=c(0,0.30),
						 low=pal[1],
						 mid=pal[2],
						 high=pal[3],
						 midpoint = 0.10) +
	scale_fill_gradient2(limits=c(0,0.30),
						low=pal[1],
						mid=pal[2],
						high=pal[3],
						midpoint = 0.10,
						 na.value = "grey80") +
	scale_size(range=c(1,15))
