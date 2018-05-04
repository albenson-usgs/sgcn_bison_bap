#' BISON - SGCN Data Mashup Report for the Midwestern Association of Fish and Wildlife Agencies (MAFWA)
#' Abby Benson  
#' April 30, 2018  


# Data for this notebook was brought together and managed using the R script MAFWA_SGCN_Analysis.R

#' ## General information about the data
#' The data contained in this report are a combination of the Species of Greatest Conservation Need (SGCN) as identified by the states in the
#' Midwestern Association of Fish and Wildlife Agences (MAFWA). SGCN lists were collated by USGS and matched to the Integrated 
#' Taxonomic Information System (ITIS), the World Register of Marine Species (WoRMS), Fish and Wildlife Service listing status information, 
#' and the NatureServe database. Species that were not found in ITIS or WoRMS are not considered part of the SGCN National List but are still 
#' included in this analysis.
#' #' List of states included in this analysis: Illinois, Indiana, Iowa, Kansas, Kentucky, Michigan, Minnesota, Missouri, Nebraska, North Dakota,
#' Ohio, South Dakota, Wisconsin

#+ Load libraries, include=F
library(scales)
library(tidyverse)
library(ggplot2)
library(knitr)
library(gridExtra)
library(kableExtra)
library(gtable)
library(reshape2)
library(plotly)

#+ SGCN_number, include=FALSE
#' ### Number of Species of Greatest Conservation Need in MAFWA:
nrow(MAFWA_list)

#+ SGCN_number_sansPlants, include=FALSE
#' ### Number of Species of Greatest Conservation Need in MAFWA excluding plants:
nrow(MAFWA_list[which(MAFWA_list$kingdom != "Plantae"),])

#+ FWSListing_number, include=FALSE
#' ### Number of SGCN that are considered Endangered, Threatened, Candidate, or Under Review by the Fish and Wildlife Service:
nrow(MAFWA_list[which(MAFWA_list$ListingStatus == "Candidate" | MAFWA_list$ListingStatus == "Endangered" | 
                             MAFWA_list$ListingStatus == "Threatened" | 
                             MAFWA_list$ListingStatus == "Under Review in the Candidate or Petition Process"),])



lsp1 <- ggplot(ls_r1, aes(x=count, y=class)) + geom_point(shape=1)

# Divide by levels of FWS listing status, in the vertical direction
lsp1 + facet_grid(ListingStatus ~ .)

#' #### ========= Note: From this point forward, plants are removed from the analysis as requested by the MAFWA representative because plants 
#' ####                 are not eligible for grants under the Fish and Wildlife Service State Wildlife Grant Program ========================

#+ BISON_data, include = TRUE
#' ### BISON Occurrence Data Available for these SGCN grouped by the Class level of the taxonomic hierarchy
#' First we'll examine what the breakdown looks like for the types of observations for these species for all observations from all locations.
#' But as you can see, it's difficult to discern what's available for anything but birds.
op1 <- ggplot(o1_gather, aes(class, `Number of Occurrences` ))
op1 + geom_bar(aes(fill = type), position = position_stack(reverse = TRUE), stat = "identity") +
  coord_flip() +
  theme(legend.position = "top") 

#' Therefore we take birds out and see what is available for the rest of the classes.
o1_1_gather <- o1_gather[which(o1_gather$class != "Aves (252)"),]
op2 <- ggplot(o1_1_gather, aes(class, `Number of Occurrences` ))
op2 + geom_bar(aes(fill = type), position = position_stack(reverse = TRUE), stat = "identity") +
  coord_flip() +
  theme(legend.position = "top") 


#' Next we'll see how this breaks down for the region of interest- MAFWA
p1 <- ggplot(r1, aes(x=class, y=Total)) + 
  geom_point(stat = "identity") +
  geom_point(col="tomato2", size=3) +   # Draw points
  geom_segment(aes(x=class, 
                   xend=class, 
                   y=min(Total), 
                   yend=max(Total)), 
               linetype="dashed", 
               size=0.1) +   # Draw dashed lines
  labs(title="BISON occurrences of SGCN", 
       subtitle="MAFWA", 
       caption="") +  
  theme(axis.title.x = element_blank()) +
  ylim(0,100000) +
  coord_flip()
p1 <- ggplotGrob(p1)

p2 <- ggplot(r1, aes(x=class, y=Total)) + 
  geom_point(stat = "identity") +
  geom_point(col="tomato2", size=3) +   # Draw points
  geom_segment(aes(x=class, 
                   xend=class, 
                   y=min(Total), 
                   yend=max(Total)), 
               linetype="dashed", 
               size=0.1) + 
  labs(title="", 
       subtitle="", 
       caption="") + 
  theme(axis.title.y = element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x = element_blank()) +
  ylim(100001,1000000) +
  coord_flip()
p2 <- ggplotGrob(p2)

p3 <- ggplot(r1, aes(x=class, y=Total)) + 
  geom_point(stat = "identity") +
  geom_point(col="tomato2", size=3) +   # Draw points
  geom_segment(aes(x=class, 
                   xend=class, 
                   y=min(Total), 
                   yend=max(Total)), 
               linetype="dashed", 
               size=0.1) +
  labs(title="", 
       subtitle="", 
       caption="source: SGCN") + 
  theme(axis.title.y = element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x = element_blank()) +
  ylim(1000001,16000000) + 
  coord_flip()
p3 <- ggplotGrob(p3)

# grid.arrange(p1, p2, p3, ncol=3)
g <- cbind(p1, p2, p3, size = "first")
grid::grid.draw(g)

#+ Stacked bar chart, include = FALSE
#' Stack bar chart showing the proportion of records that are in each state by class
r1_gather <- gather(r1, key = "state", value = "occCount", -class)
r1_gather <- r1_gather[which(r1_gather$state != "classtotal" & r1_gather$state != "Total"),]
r1_1_gather <- r1_gather[which(r1_gather$class != "Aves (252)"),]
r1_2_gather <- r1_gather[which(r1_gather$class == "Aves (252)"),]
r1_1_gather$class <- factor(r1_1_gather$class, levels = r1_1_gather$class[order(r1$Total)])
g1 <- ggplot(r1_1_gather, aes(class, occCount))
g1 + geom_bar(aes(fill = state), position = position_stack(reverse = TRUE), stat = "identity") +
  coord_flip() +
  theme(legend.position = "top") + ylim(0,250000) #doesn't show Aves
