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
df.person <- dat %>%  filter(sp_named_after == "person") %>% 
	select(ref_pubyr, taxon_name, type_country, sp_if_person_country, 
		   sp_if_person_gender,
		   author_aff = affiliations_of_authors..separated.by...,
		   author_country = countries_of_authors..separated.by...)

# Note: need to sort out some country name, old names, disputed

df.person$type_code <- countrycode(df.person$type_country, "country.name", "iso3c") 
df.person$person_code <-    countrycode(df.person$sp_if_person_country, "country.name", "iso3c")

# affiliations with only one country added
affs <- strsplit(df.person$author_aff, ";")
counts <- strsplit(df.person$author_country, ";")

n <- unlist(lapply(counts, length))

for(i in which(n==1)){
	if(!is.na(counts[[i]])){
		counts[[i]] <- rep(counts[[i]], length(affs[[i]]))
	}
}

# first author 
n <- grep("\\(", df.person$author_aff) # contains bracket, multiple affiliations

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
	
	first2[i] <- ifelse(df.person$type_code[i] %in% first[[i]], "yes", "no")
	alla[i] <- ifelse(df.person$type_code[i] %in% countrycode(counts[[i]], "country.name", "iso3c"), "yes", "no")
}

df.person$first <- first2
df.person$all <- alla

df.person <- na.omit(df.person)
df.person <- df.person %>% 
	mutate(local=ifelse(person_code==type_code, "yes", "no"))

t1 <- df.person %>% group_by(ref_pubyr, local) %>% 
	tally() %>% 
	mutate(local=factor(local, c("yes", "no"))) %>% 
	ggplot(aes(x=ref_pubyr, y=n, col=local)) +
	geom_line(size=0.6) +
	scale_color_manual(values=pal[c(3,5)]) +
	labs(x="year", y="number of species described")

df.prop <- df.person %>% 
	select(ref_pubyr, local, all, first) %>% 
	pivot_longer(cols=all:first) %>% 
	group_by(name, value, local) %>% 
	tally() %>% 
	ungroup() %>% 
	mutate(cat=paste0(name, value)) %>% 
	group_by(cat) %>% 
	mutate(prop=n/sum(n))

g1 <- ggplot(df.prop, aes(x=cat, y=prop, fill=local)) +
	geom_bar(stat="identity") +
	scale_fill_manual(values=pal[c(5,3)]) +
	scale_x_discrete(label=rep(c("non-local", "local"),2)) +
	geom_segment(aes(x=2.5, xend=2.5, y=1.05, yend=-0.2), col="darkgrey", linetype="dashed") +
	labs(x="", y="proportion", fill="local eponym") +
	coord_cartesian(ylim = c(0, 1), clip="off") +
	annotate("text", x = c(1.5, 3.5), y = -0.15, 
			 label = c("all authors", "first author"), fontface=2) +
	theme(legend.position = "top",
		  plot.margin = unit(c(1, 1, 3, 1), "lines"))

# remove clipping of x axis labels
g2 <- ggplot_gtable(ggplot_build(g1))
g2$layout$clip[g2$layout$name == "panel"] <- "off"

svg("plots/Fig_03_authors.svg", w=5, h=6)
grid.draw(g2)
dev.off()

allt <- prop.test(x=df.prop %>% filter(name=="all" & local=="yes") %>% pull(n), 
				  n=df.prop %>% filter(name=="all") %>% group_by(value) %>% summarise(n=sum(n)) %>%  pull(n)
)

firstt <- prop.test(x=df.prop %>% filter(name=="first" & local=="yes") %>% pull(n), 
					n=df.prop %>% filter(name=="first") %>% group_by(value) %>% summarise(n=sum(n)) %>%  pull(n)
)

propt <- rbind(
	c(allt$estimate, allt$statistic, "p-value"=allt$p.value),
	c(firstt$estimate, firstt$statistic, "p-value"=firstt$p.value)
)

propt <- signif(propt, 3)
colnames(propt)[1:2] <- c("non-local", "local")
row.names(propt) <- c("all authors", "first author")

write.csv(propt, "output/table1.csv")


# Gender ------------------------------------------------------------------
nm <- df.person$taxon_name
gender <- rep(NA, length(nm))

gender[grep(".+i$", nm)] <- "male"
gender[grep(".+ae$", nm)] <- "female"

n <- which(is.na(gender))
gender[n] <- df.person$sp_if_person_gender[n]
df.person$gender <- gender

g3 <- df.person %>% group_by(gender) %>% 
	tally() %>% 
	ungroup() %>% 
	mutate(prop=n/sum(n)) %>% 
	ggplot(aes(x=gender, y=prop, fill=gender)) +
	geom_bar(stat = "identity") +
	scale_fill_manual(values=pal[c(3,5)]) +
	theme(legend.position = "none") +
	labs(y="proportion", x="")

t2 <- df.person %>% group_by(ref_pubyr, gender) %>% 
	tally() %>%  
	ggplot(aes(x=ref_pubyr, y=n, col=gender)) +
	geom_line(size=0.6) +
	scale_color_manual(values=pal[c(3,5)]) +
	labs(x="year", y="number of species described")

png("plots/Fig_S_time_eponym.png", w=8, h=4, res=300, units="in")
t1 + t2 +
	plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")")
dev.off()

pdata <- read.csv("data/person_data.csv")
df.person <- df.person %>% 
	left_join(pdata, by="taxon_name")

prop.table(table(df.person$reason))

# Where are dinosaurs found -----------------------------------------------
# and who are dinosaurs named after
df.dino <- dat %>% select(type_country) %>% 
	mutate(type_code = countrycode(type_country, "country.name", "iso3c")) %>% 
	na.omit() %>% 
	group_by(type_code) %>% 
	tally() %>% 
	ungroup %>% 
	mutate(n=n/sum(n))

worldtilegrid <- read.csv("data/worldtilegrid.csv") %>% 
	left_join(df.dino, by=c("alpha.3"="type_code")) %>% 
	left_join(df.person %>% group_by(person_code) %>%  summarise(person=n()) %>% 
			  	ungroup() %>%  mutate(person=person/sum(person)), 
			  by=c("alpha.3"="person_code"))

worldtilegrid$lab <- worldtilegrid$alpha.2
worldtilegrid$lab[is.na(worldtilegrid$n)] <- ""

#worldtilegrid$person[!is.na(worldtilegrid$n) & is.na(worldtilegrid$person)] <- 0

mid <- 0.10
ggplot(worldtilegrid, aes(xmin = x, ymin = y, xmax = x + 1, ymax = y + 1))  +
	geom_rect(color = "#ffffff", aes(fill=person*100)) + 
	geom_text(aes(x = x, y = y, label = lab,
				  fontface=ifelse(person < n, 3,2),
				  col=ifelse(person > 0.19, "black", "white")), 
			  alpha = 0.8, nudge_x = 0.5, nudge_y = -0.5, size = 3) +
	scale_fill_gradient2(na.value="grey80",limits=c(0,30), 
						 high=pal[1], low=pal[5], mid=pal[4], midpoint=mid*100) +
	scale_color_manual(values=c("black", "white"), guide="none") +
	scale_y_reverse() +
	labs(fill="Percentage") +
	theme(panel.grid = element_blank(), 
		  axis.text = element_blank(), 
		  axis.title.x = element_blank(),
		  axis.title.y=element_blank(),
		  axis.ticks = element_blank(),
		  panel.grid.major.y = element_blank()) +
	coord_equal()

ggsave("plots/Fig_02_map_person.svg", w=8,h=7)

