
### Introduction

Historical challenges faced by southern sea otters (Enhydra lutris), in particular habitat loss and degradation due to human activities, 
have raised concerns about their population stagnation and limited genetic diversity in concentrated areas (Larson et al., 2012). 
There has also been a rise in recent years of white shark bites being the most common cause of death along the outer edges of the growing 
southern sea otter population (Miller et al., 2020), as sharks seem to confuse otters for young seals which are the shark’s main prey source. 
The increasing occurrence of white sharks mistargeting southern sea otters is intensifying throughout the year, disrupting sea otter recovery 
efforts and necessitating a reevaluation of management strategies that traditionally overlook such dynamic predator-prey conflicts 
(Moxley et al., 2019). San Francisco Bay (SFB) may provide a vital population localization that can bypass the shark/sea otter conflict. 
This is because SFB is predominantly a shallow estuary with an average depth of 6m (Conomos et al., 1985), while white sharks avoid the 
surface and use water to 50m depth when near pinniped rookeries as part of their hunting strategy (Moxley et al., 2019). However, the 
successful re-colonization of sea otters in SFB, and the appropriateness of instigating a formal reintroduction effort, hinges on an 
understanding of how the animals can succeed in establishing their habitat in low risk areas away from vessel traffic with adequate foraging 
possibilities (Rudebusch et al., 2020). The purpose of this thesis study is to assess the benthic invertebrate communities across ten sites 
within the envisioned foraging range of California sea otters within SFB, mirroring their natural feeding behaviors. This data will be compared 
with invertebrate assemblages from Elkhorn Slough to evaluate the potential of these sites to support sustainable sea otter populations and to 
inform future conservation management. The project for class 710 in specific is to assess (1) diversity indexes and (2) abundances of benthic 
invertebrates using a fake dataset modelled to be a prediction of the data to be collected within the next year. 

### Analysis

The Null hypothesis is that all sites will have the same species diversity.
The Alternate hypothesis is that some sites will have different species diversity.

The best statistical approach would be the Shannon diversity index. The higher the value of the Shannon index, the higher the diversity of species 
in a particular community. The lower the value of the Shannon index, the lower the diversity. A value of 0 indicates a community that only has one 
species. The best associated visualization would be a bar graph, to easily see the difference of the numerical output the Shannon index has.

```{r load-packages, message=FALSE}
library(tidyverse)
library(openintro)
library(plyr)
library(naniar)
library(vegan)
library(ggpubr)
library(labdsv)

# Importing data
benthic <- read.csv("FakeBenthicDataset.csv",header=TRUE)
# viewing the data
benthic

summary(benthic)

# change - to NA
benthic$Carapace.Length..mm._NA <- gsub("-", 0, benthic$Carapace.Length..mm.) %>% as.numeric() %>% tidyr::replace_na(0)
benthic$Shell.Length_NA <- gsub("-", 0, benthic$Shell.Length) %>% as.numeric() %>% tidyr::replace_na(0)
benthic$Worm.Length_NA <- gsub("-", 0, benthic$Worm.Length) %>% as.numeric() %>% tidyr::replace_na(0)

benthic %>% naniar::replace_with_na(replace = list(x = 0))

numspecies <- benthic %>% dplyr::count (Location, Species)
numspecies

data_wide <- spread(numspecies, Species, n)
data_wide

data_wide[is.na(data_wide)] <- 0

rownames(data_wide) <- data_wide$Location
data_wide$Location <- NULL

#shannon diversity test on species present
shannon <- diversity(data_wide,index = "shannon") %>% as.data.frame()
shannon$Location <- rownames(shannon)

plot_shandiv <- ggplot(shannon, aes(x = Location, y = ., fill = Location)) +
   geom_text(aes(label=round(., 2)), vjust=-0.5) +
  geom_col(color = "black") +
  theme(legend.position = "none") +
  labs(x = "Data Collection Site",
       y = "Mean Shannon Diversity",
       title = "Shannon Diversity Per Site")
plot_shandiv

```
We reject the Null hypothesis as each site clearly have different species diversity index values. Richardson Bay has the highest value of 2.19 
which means it is the most species diverse out of all the sites. Pinole Shores has the least amount of diversity with a value of 1.21. Interestingly 
enough, Point Pinole and Pinole Shores have vastly different species diversity even though they are located directly beside one another. Further 
research into the habitats of these location may provide insights for the difference in diversity.

...
