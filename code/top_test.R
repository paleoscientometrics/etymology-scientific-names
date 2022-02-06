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


# Dinosaurs ---------------------------------------------------------------
dat$code <- countrycode(dat$type_country, "country.name", "iso3c")

dino5 <- dat %>%  group_by(code) %>% 
  tally() %>% 
  slice_max(order_by=n, n=5) %>% 
  mutate(x=1,
         y=nrow(.):1,
         country=countrycode(code, "iso3c", "country.name"))

ggplot(dino5, aes(x=x, y=y)) +
  geom_point(aes(size=n), alpha=0.5) +
  scale_size(range=c(1,50),
  		   limits=c(1,600)) +
  
  geom_text(aes(label=paste(country, "\n", n)),
            nudge_x = -0.01, hjust=1, vjust=-1) +
  scale_y_continuous(breaks=1:5,
                     limits=c(0,7))+
  xlim(0.98, 1.01) +
  theme(legend.position = "none")


ggsave("figs/top5/dino.svg", w=5, h=5)

# Researchers -------------------------------------------------------------

# affiliations with only one country added
affs <- strsplit(dat$affiliations_of_authors..separated.by...,";")
counts <- strsplit(dat$countries_of_authors..separated.by..., ";")

n <- unlist(lapply(counts, length))

for(i in which(n==1)){
  if(!is.na(counts[[i]])){
    counts[[i]] <- rep(counts[[i]], length(affs[[i]]))
  }
}


u_count <- lapply(counts, function(x) {x <- countrycode(x, "country.name", "iso3c")
unique(x)})

u_count2 <- data.frame(table(unlist(u_count)))
colnames(u_count2) <- c("code", "n")

count5 <- u_count2 %>% slice_max(order_by = n, n=5) %>% 
  mutate(x=1,
         y=nrow(.):1,
         country=countrycode(code, "iso3c", "country.name"))

ggplot(count5, aes(x=x, y=y)) +
  geom_point(aes(size=n), alpha=0.5) +
	scale_size(range=c(1,50),
			   limits=c(1,600)) +
  
  geom_text(aes(label=paste(country, "\n", n)),
            nudge_x = -0.01, hjust=1, vjust=-1) +
  scale_y_continuous(breaks=1:5,
                     limits=c(0,7))+
  xlim(0.98, 1.01) +
  theme(legend.position = "none")

ggsave("figs/top5/author_all.svg", w=5, h=5)


# First author ------------------------------------------------------------
f_count <- lapply(u_count, function(x) x[1])

f_count2 <- data.frame(table(unlist(f_count)))
colnames(f_count2) <- c("code", "n")

fcount5 <- f_count2 %>% slice_max(order_by = n, n=5) %>% 
  mutate(x=1,
         y=nrow(.):1,
         country=countrycode(code, "iso3c", "country.name"))

ggplot(fcount5, aes(x=x, y=y)) +
  geom_point(aes(size=n), alpha=0.5) +
  scale_size(range=c(1,50), 			   limits=c(1,600)) +
  
  geom_text(aes(label=paste(country, "\n", n)),
            nudge_x = -0.01, hjust=1, vjust=-1) +
  scale_y_continuous(breaks=1:5,
                     limits=c(0,7))+
  xlim(0.98, 1.01) +
  theme(legend.position = "none")

ggsave("figs/top5/author_first.svg", w=5, h=5)

# Eponyms -----------------------------------------------------------------
df.person <- dat %>%  filter(sp_named_after == "person") %>% 
  select(ref_pubyr, taxon_name, type_country, sp_if_person_country, 
         sp_if_person_gender,
         author_aff = affiliations_of_authors..separated.by...,
         author_country = countries_of_authors..separated.by...)

# Note: need to sort out some country name, old names, disputed

df.person$type_code <- countrycode(df.person$type_country, "country.name", "iso3c") 
df.person$person_code <-    countrycode(df.person$sp_if_person_country, "country.name", "iso3c")

                      
person5 <- df.person %>% group_by(person_code) %>% 
  tally() %>% 
  na.omit() %>% 
  slice_max(order_by = n, n=5) %>% 
  mutate(x=1,
         y=nrow(.):1,
         country=countrycode(person_code, "iso3c", "country.name"))

ggplot(person5, aes(x=x, y=y)) +
  geom_point(aes(size=n), alpha=0.5) +
  scale_size(range=c(1,50), 			   limits=c(1,600)) +
  
  geom_text(aes(label=paste(country, "\n", n)),
            nudge_x = -0.01, hjust=1, vjust=-1) +
  scale_y_continuous(breaks=1:5,
                     limits=c(0,7))+
  xlim(0.98, 1.01) +
  theme(legend.position = "none")

ggsave("figs/top5/eponym_all.svg", w=5, h=5)


# Eponym-foreign ----------------------------------------------------------

fperson5 <- df.person %>% 
  na.omit() %>% 
  filter(type_code != person_code) %>% 
  group_by(person_code) %>% 
  tally() %>% 
  na.omit() %>% 
  slice_max(order_by = n, n=5) %>% 
  mutate(x=1,
         y=nrow(.):1,
         country=countrycode(person_code, "iso3c", "country.name"))

ggplot(fperson5, aes(x=x, y=y)) +
  geom_point(aes(size=n), alpha=0.5) +
  scale_size(range=c(1,50), 			   limits=c(1,600)) +
  
  geom_text(aes(label=paste(country, "\n", n)),
            nudge_x = -0.01, hjust=1, vjust=-1) +
  scale_y_continuous(breaks=1:5,
                     limits=c(0,7))+
  xlim(0.98, 1.01) +
  theme(legend.position = "none")

ggsave("figs/top5/foreign_eponym.svg", w=5, h=5)