## ---------------------------
##
## Project: Etymology of Dinosaur Names
##
## Purpose of script: Summarise the languages used for species names and compare to
## authors country
##
## Author: Nussaïbah B. Raja
## Copyright (c) N. Raja, 2021
## Email: nussaibah.raja.schoob@fau.de
##
## Date Created: 2021-08-18
## Last Modified: 2021-08-18
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
	filter(data_enterer != "" &  #empty data
		   	group != "trace" #trace fossils omitted
		   )

dat$type_code <- countrycode(dat$type_country, "country.name", "iso3c")

# affiliations with only one country added
affs <- strsplit(dat$affiliations_of_authors..separated.by..., ";")
counts <- strsplit(dat$countries_of_authors..separated.by..., ";")
counts <- lapply(counts, function(x) gsub("\\(.*\\)", "", x))
counts <- lapply(counts, function(x) gsub("not reported", "", x))

n <- unlist(lapply(counts, length))

for(i in which(n==1)){
	if(!is.na(counts[[i]])){
		counts[[i]] <- rep(counts[[i]], length(affs[[i]]))
	}
}

# first author 
n <- grep("\\(", dat$affiliations_of_authors..separated.by...) # contains bracket, multiple affiliations

first <- list()
first2 <- rep(NA, length(affs)) # firstauthor local
alla <- rep(NA, length(affs)) # all authors local

for(i in 1:length(affs)){
	if(i %in% n){
		temp <- do.call(rbind, strsplit(affs[[i]], "\\("))
		
		ff <- temp[1,2]
		loc <- which(temp[,2]==ff)
		
		first[[i]] <- unique(countrycode(counts[[i]][loc],
										 "country.name", "iso3c"))
	} else {
		first[[i]] <- countrycode(counts[[i]][1], 
								  "country.name", "iso3c")
	}
	
	first2[i] <- ifelse(dat$type_code[i] %in% first[[i]], "yes", "no")
	alla[i] <- ifelse(dat$type_code[i] %in% countrycode(counts[[i]], "country.name", "iso3c"), "yes", "no")
}

dat$first <- first2
dat$all <- alla

# Languages ---------------------------------------------------------------
df <- dat %>% filter(!sp_named_after %in% c("person", "company/corporation"))
write.csv(data.frame(input=sort(names(table(df$sp_language..if.another.language.but.latin.used.)))),
		  "data/languages.csv", row.names = F)

