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

# alla2 <- list()

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
	# alla2[[i]] <- countrycode(counts[[i]], "country.name", "iso3c") 
}

df.person$first <- first2
df.person$all <- alla
df.person$first_code <- lapply(first, function(x) x[1]) #get only first aff
#df.person$all_code <- alla2

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

svg("figs/Fig_S_authors.svg", w=5, h=6)
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

write.csv(propt, "output/local_prob_test.csv", row.names=F)

# MOngolia
n <- which(df.person$type_code == "MNG")

mng_aut <- unlist(counts[n])
mng_aut <- gsub("\\(.+\\)", "", mng_aut)
mng_aut <- countrycode(trimws(mng_aut), "country.name", "iso3c")
sort(table(mng_aut))

df.person %>% filter(type_code == "TZA") %>% group_by(person_code) %>%  tally()
df.person %>% filter(type_code == "ZAF") %>% group_by(person_code) %>%  tally()

# over time
coi <- c("USA", "CHN", "ARG", "CAN", "GBR", "BRA", "MNG")

tmp <- df.person %>% filter(type_code %in% coi) %>% 
  group_by(type_code, ref_pubyr, local) %>% 
  tally() %>% 
  ungroup() %>% 
  filter(local=="yes") %>% 
  group_by(type_code) %>% 
  mutate(local_n = cumsum(n))

tmp_tot <- df.person %>% filter(type_code %in% coi) %>% 
  group_by(type_code, ref_pubyr) %>% 
  tally() %>% 
  group_by(type_code) %>% 
  mutate(tot2 = cumsum(n)) %>% 
  select(type_code, ref_pubyr, tot2)
  
tmp <- tmp %>% left_join(tmp_tot) %>% 
  mutate(prop = local_n / tot2)

tmp <- tmp %>% mutate(country = countrycode(type_code, "iso3c", "country.name"))

ggplot(tmp, aes(x = ref_pubyr, y=prop, col=tot2)) + 
  geom_line()+
  geom_point() +
  facet_wrap(~country) +
  labs(x="Year of publication",
       y="Cumulative proportion of local honorees", 
       col = "Total eponyms") +
  scale_color_viridis_c(option="magma", end=0.9, begin=0.1, direction=-1)

ggsave("figs/Fig_S_local_prop.png", width=8, h=6, units=c("in"))

# DO proportions of high vs low income