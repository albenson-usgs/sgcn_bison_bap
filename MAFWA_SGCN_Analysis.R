library(jsonlite)
library(tidyverse)
library(mongolite)
library(dplyr)
library(kableExtra)
library(plyr)


# Pulls the SGCN National List from MongoDB
bis_sgcnsynthesis2 <- mongo(collection = "xxxxxxxxxxxxx", db = "xxxxxxxxxxxxxxxxxx", url = "xxxxxxxxxxxxxxxxxxx")
mongo_sgcn2 <- bis_sgcnsynthesis2$find('{}', fields = '{"_id":1, "Common Name":1, "Taxonomic Group":1, "Match Method":1, "Taxonomy":1, "TESS":1, "NatureServe":1, "Source Data Summary":1, "ITIS":1, "WoRMS":1}')


sgcn_natlist <- mongo_sgcn2
sgcn_natlist$ScientificName <- sgcn_natlist$`_id`

# Let's break this down into manageable chunks first starting with MAFWA. But in order to do that we need the state list.
# This bit of code is extremely slow. Would be good to figure out how to speed this up. Takes hours to run.
sgcn_natlist$statelist2015 <- NA
sgcn_natlist$statelist2005 <- NA
for (i in 1:nrow(sgcn_natlist)){
  if(!is.null(sgcn_natlist$`Source Data Summary`[[i]]$`2015`$States)){
    staterow <- unlist(sgcn_natlist$`Source Data Summary`[[i]]$`2015`$States[[1]])
    sgcn_natlist[i,]$statelist2015 <- paste(staterow, collapse = ",")
  }
}

for (i in 1:nrow(sgcn_natlist)){
  if(!is.null(sgcn_natlist$`Source Data Summary`[[i]]$`2005`$States)){
    staterow <- unlist(sgcn_natlist$`Source Data Summary`[[i]]$`2005`$States)
    sgcn_natlist[i,]$statelist2005 <- paste(staterow, collapse = ",")
  }
}

### Subset the data to just the national lists species for a particular region
toMatchMAWFA <- c("Illinois","Indiana","Iowa","Kansas","Kentucky","Michigan","Minnesota","Missouri","Nebraska","North Dakota","Ohio",
                  "South Dakota", "Wisconsin") 
MAFWA_list <- sgcn_natlist[grep(paste(toMatchMAWFA,collapse = "|"),sgcn_natlist$statelist2015),]


# Extract out the FWS listing status from the document structure coming back from Mongodb2.0
MAFWA_list$ListingStatus <- NA
MAFWA_list$ListingStatus2 <- NA
MAFWA_list$ListingStatus3 <- NA
MAFWA_list$ListingStatus4 <- NA
for (i in 1:nrow (MAFWA_list)) {
  if (!is.null(MAFWA_list[i,]$TESS$listingStatus[[1]])){
    tessrow <- as.data.frame(MAFWA_list[i,]$TESS$listingStatus, simplifyDataFrame=T)
    if (nrow(tessrow) == 1) {
      MAFWA_list[i,]$ListingStatus <- tessrow$STATUS[[1]]
    }
    if (nrow(tessrow) == 2){
    MAFWA_list[i,]$ListingStatus <- tessrow$STATUS[[1]] 
    MAFWA_list[i,]$ListingStatus2 <- tessrow$STATUS[[2]]
    }
    if (nrow(tessrow) == 3){
      MAFWA_list[i,]$ListingStatus <- tessrow$STATUS[[1]] 
      MAFWA_list[i,]$ListingStatus2 <- tessrow$STATUS[[2]]
      MAFWA_list[i,]$ListingStatus3 <- tessrow$STATUS[[3]]
    }
    if (nrow(tessrow) == 4){
      MAFWA_list[i,]$ListingStatus <- tessrow$STATUS[[1]] 
      MAFWA_list[i,]$ListingStatus2 <- tessrow$STATUS[[2]]
      MAFWA_list[i,]$ListingStatus3 <- tessrow$STATUS[[3]]
      MAFWA_list[i,]$ListingStatus4 <- tessrow$STATUS[[4]]
    }
  } else {
    MAFWA_list[i,]$ListingStatus <- NA
  }
}

MAFWA_list$TESS <- NULL

# Swap higher priority listing status for lower priority ones
for (i in 1:nrow(MAFWA_list)) {
  if (!is.na(MAFWA_list[i,]$ListingStatus2) & MAFWA_list[i,]$ListingStatus2 == "Endangered") {
    MAFWA_list[i,]$ListingStatus <- paste(MAFWA_list[i,]$ListingStatus,MAFWA_list[i,]$ListingStatus2, sep = "")
    MAFWA_list[i,]$ListingStatus2 <- substr(MAFWA_list[i,]$ListingStatus,0,nchar(MAFWA_list[i,]$ListingStatus) - nchar(MAFWA_list[i,]$ListingStatus2))
    MAFWA_list[i,]$ListingStatus <- substr(MAFWA_list[i,]$ListingStatus,nchar(MAFWA_list[i,]$ListingStatus2) + 1, nchar(MAFWA_list[i,]$ListingStatus))
  }
}

for (i in 1:nrow(MAFWA_list)) {
  if (!is.na(MAFWA_list[i,]$ListingStatus3) & MAFWA_list[i,]$ListingStatus3 == "Endangered") {
    MAFWA_list[i,]$ListingStatus <- paste(MAFWA_list[i,]$ListingStatus,MAFWA_list[i,]$ListingStatus3, sep = "")
    MAFWA_list[i,]$ListingStatus3 <- substr(MAFWA_list[i,]$ListingStatus,0,nchar(MAFWA_list[i,]$ListingStatus) - nchar(MAFWA_list[i,]$ListingStatus3))
    MAFWA_list[i,]$ListingStatus <- substr(MAFWA_list[i,]$ListingStatus,nchar(MAFWA_list[i,]$ListingStatus3) + 1, nchar(MAFWA_list[i,]$ListingStatus))
  }
}

for (i in 1:nrow(MAFWA_list)) {
  if (!is.na(MAFWA_list[i,]$ListingStatus3) & MAFWA_list[i,]$ListingStatus3 == "Recovery") {
    MAFWA_list[i,]$ListingStatus <- paste(MAFWA_list[i,]$ListingStatus,MAFWA_list[i,]$ListingStatus3, sep = "")
    MAFWA_list[i,]$ListingStatus3 <- substr(MAFWA_list[i,]$ListingStatus,0,nchar(MAFWA_list[i,]$ListingStatus) - nchar(MAFWA_list[i,]$ListingStatus3))
    MAFWA_list[i,]$ListingStatus <- substr(MAFWA_list[i,]$ListingStatus,nchar(MAFWA_list[i,]$ListingStatus3) + 1, nchar(MAFWA_list[i,]$ListingStatus))
  }
}

# Now we want to grab what we need from the NatureServe rank data
MAFWA_list$NSGlobalDescription <- MAFWA_list$NatureServe$conservationStatus$natureServeStatus$globalStatus$roundedRank$description
MAFWA_list$NSGlobalReviewDate <- MAFWA_list$NatureServe$conservationStatus$natureServeStatus$globalStatus$statusLastReviewed

# Not grabbing the NS state ranks for now. Try and come back to that later.
MAFWA_list$NatureServe <- NULL

# Populating the taxonomic hierarchy from the Taxonomy information from Mongodb
MAFWA_list$kingdom <- NA
MAFWA_list$phylum <- NA
MAFWA_list$class <- NA
MAFWA_list$order <- NA
MAFWA_list$family <- NA
MAFWA_list$genus <- NA
for (i in 1: nrow(MAFWA_list)) {
  if (!is.null(MAFWA_list$Taxonomy[[i]])) {
  taxonomy_row <- MAFWA_list[i,]$Taxonomy
  df <- ldply(taxonomy_row, data.frame)
  if (is.na(MAFWA_list[i,]$kingdom)) {
    MAFWA_list[i,]$kingdom <- df$name[which(df$rank == "Kingdom")]
    MAFWA_list[i,]$phylum <- df$name[which(df$rank == "Phylum" | df$rank == "Division")]
    if (nrow(df[which(df$rank == "Class"),]) >0) { 
      MAFWA_list[i,]$class <- df$name[which(df$rank == "Class")]
    } 
    if (nrow(df[which(df$rank == "Order"),]) >0) {
      MAFWA_list[i,]$order <- df$name[which(df$rank == "Order")]
    } 
    if (nrow(df[which(df$rank == "Family"),]) >0) {
      MAFWA_list[i,]$family <- df$name[which(df$rank == "Family")]
    } 
    if (nrow(df[which(df$rank == "Genus"),]) >0) {
      MAFWA_list[i,]$genus <- df$name[which(df$rank == "Genus")]
    }
    }
  }
}

# Grab taxonomic ranks so we can filter out any taxonomic identifications made at a level higher than species because they'll bring in too
# many records from BISON and result in duplication

MAFWA_list$rank <- NA
for (i in 1:nrow(MAFWA_list)) {
  if (is.na(MAFWA_list[i,]$rank)) {
    if (!is.null(MAFWA_list[i,]$ITIS[[1]])){
      itis <- as.data.frame(MAFWA_list[i,]$ITIS)
      MAFWA_list[i,]$rank <- itis[which(itis$usage == "valid" | itis$usage == "accepted"),]$rank
    }
  }
}

for (i in 1:nrow(MAFWA_list)){
  if (is.na(MAFWA_list[i,]$rank)){
    if(!is.null(MAFWA_list[i,]$WoRMS[[1]])){
      worms <- as.data.frame(MAFWA_list[i,]$WoRMS)
      if (!is.null(worms$rank)){
        if (!is.na(worms[1,]$rank) | !is.na(worms[2,]$rank)) {
          if (any(worms$status == "accepted")){
            MAFWA_list[i,]$rank <- worms[which(worms$status == "accepted" ),]$rank
          } else if(worms$status == "uncertain" | worms$status == "nomen dubium" & worms$status != "accepted") {
            MAFWA_list[i,]$rank <- worms$status
          }
        }
      }
    }
  }
}


MAFWA_list_speciesonly <- MAFWA_list[which(MAFWA_list$rank != "Family" & MAFWA_list$rank != "Genus" & MAFWA_list$rank != "Order" & MAFWA_list$rank != "Subclass" & MAFWA_list$rank != "Suborder"),]
MAFWA_list_speciesonly$ITIS <- NULL
MAFWA_list_speciesonly$WoRMS <- NULL


# Need to unload the package "plyr" here because it causes problems for summarizing the data below
detach(package:plyr)

### See what BISON has for these species
MAFWA_list_speciesonly$bisonQuery <- NA
for(i in 1:nrow(MAFWA_list_speciesonly)){
  if (is.na(MAFWA_list_speciesonly[i,]$bisonQuery)){
      MAFWA_list_speciesonly[i,]$bisonQuery <- paste0("https://data.usgs.gov/solr/occurrences/select?q=scientificName:(%22", (URLencode(MAFWA_list_speciesonly[i,]$ScientificName)))
    }
  }
MAFWA_list_speciesonly$bisonQuery <- paste0(MAFWA_list_speciesonly$bisonQuery, "%22)&facet.mincount=1&rows=0&facet=true&facet.missing=true&facet.limit=-1&wt=json&indent=true&facet.field=basisOfRecord")

MAFWA_list_speciesonly$bisontotal <- NA
for(i in 1:nrow(MAFWA_list_speciesonly)){
  if (is.na(MAFWA_list_speciesonly[i,]$bisontotal)){
    bisonrow <- fromJSON(MAFWA_list_speciesonly[i,]$bisonQuery)
    MAFWA_list_speciesonly[i,]$bisontotal <- bisonrow$response$numFound
  }
}

N <- nrow(MAFWA_list_speciesonly)
bisondata <- vector(mode="list", length=N)

df <- data.frame(X_id=NA, bisonQuery=NA, total=NA, literature=NA, fossil=NA, observation = NA, specimen=NA, unknown=NA )
df_total <- data.frame(X_id=NA, bisonQuery=NA, total=NA, literature=NA, fossil=NA, observation = NA, specimen=NA, unknown=NA )
for(i in 1:N){
  query  <- toString(MAFWA_list_speciesonly$bisonQuery[i])
  bisondata[[i]] <- fromJSON(query)
  xid = bisondata[[i]]$responseHeader$params$q
  bisondata[[i]] <- bisondata[[i]]$facet_counts$facet_fields$basisOfRecord
  dummy_df <- as.data.frame(t(matrix(unlist(bisondata[[i]]), nrow=length(bisondata[[i]])/2, byrow=T)), stringsAsFactors = F)
  colnames(dummy_df) = dummy_df[1, ]
  dummy_df <- dummy_df[complete.cases(dummy_df), ]
  # merge with df with all columns
  if(!is.null(dim(dummy_df))){
    merged <- bind_rows(df, dummy_df)
    merged[2,"X_id"] = xid 
    merged[2,"bisonQuery"] = query
  }
  else{ 
    dummy_df <- df
    dummy_df[1,"X_id"] = xid
    dummy_df[1,"bisonQuery"] = query
    merged <- bind_rows(df, dummy_df)
  }
  merged <- merged[c("X_id", "bisonQuery", "total", "literature", "fossil", "observation", "specimen", "unknown")]
  merged <- merged[-1, ]
  # stack bison query into one final df
  df_total <- rbind(df_total, merged)
  
}
# final df
df_total <- df_total[-1, ]  # first row is NA's
df_total$total <- NULL

# Merge df with original data
MAFWA_list_speciesonly <- merge(MAFWA_list_speciesonly, df_total, by = "bisonQuery", all.x = T)
MAFWA_list_speciesonly[,26:30] <- lapply(MAFWA_list_speciesonly[,26:30], function(x) as.numeric(x))

# Now that we have the data for each SGCN species as the type of data (basis of record), let's figure out how many records are available in each
# state for each species listed
MAFWA_list_speciesonly$bisonStateQuery <- NA
for(i in 1:nrow(MAFWA_list_speciesonly)){
  if (is.na(MAFWA_list_speciesonly[i,]$bisonStateQuery)){
    MAFWA_list_speciesonly[i,]$bisonStateQuery <- paste0("https://data.usgs.gov/solr/occurrences/select?q=scientificName:(%22", (URLencode(MAFWA_list_speciesonly[i,]$ScientificName)))
  }
}
MAFWA_list_speciesonly$bisonStateQuery <- paste0(MAFWA_list_speciesonly$bisonStateQuery, "%22)%20AND%20calculatedState:(%22Illinois%22%20%22Indiana%22%20%22Iowa%22%20%22Kansas%22%20%22Kentucky%22%20%22Michigan%22%20%22Missouri%22%20%22Minnesota%22%20%22Nebraska%22%20%22North%20Dakota%22%20%22Ohio%22%20%22South%20Dakota%22%20%22Wisconsin%22)&facet.mincount=1&rows=0&facet=true&facet.missing=true&facet.limit=-1&wt=json&indent=true&facet.field=calculatedState")

N <- nrow(MAFWA_list_speciesonly)
bisonStatedata <- vector(mode="list", length=N)

df <- data.frame(X_id=NA, bisonStateQuery=NA, Illinois=NA, Indiana=NA, Iowa=NA, Kansas=NA, Kentucky=NA, Michigan=NA, Missouri=NA, Minnesota=NA, Nebraska=NA, Ohio=NA, Wisconsin=NA )
df$`South Dakota` <- NA
df$`North Dakota` <- NA
df_total <- data.frame(X_id=NA, bisonStateQuery=NA, Illinois=NA, Indiana=NA, Iowa=NA, Kansas=NA, Kentucky=NA, Michigan=NA, Missouri=NA, Minnesota=NA, Nebraska=NA, Ohio=NA, Wisconsin=NA)
df_total$`South Dakota` <- NA
df_total$`North Dakota` <- NA
for(i in 1:N){
  query  <- toString(MAFWA_list_speciesonly$bisonStateQuery[i])
  bisondata[[i]] <- fromJSON(query)
  xid = bisondata[[i]]$responseHeader$params$q
  bisondata[[i]] <- bisondata[[i]]$facet_counts$facet_fields$calculatedState
  dummy_df <- as.data.frame(t(matrix(unlist(bisondata[[i]]), nrow=length(bisondata[[i]])/2, byrow=T)), stringsAsFactors = F)
  colnames(dummy_df) = dummy_df[1, ]
  dummy_df <- dummy_df[complete.cases(dummy_df), ]
  # merge with df with all columns
  if(!is.null(dim(dummy_df))){
    merged <- bind_rows(df, dummy_df)
    merged[2,"X_id"] = xid 
    merged[2,"bisonStateQuery"] = query
  }
  else{ 
    dummy_df <- df
    dummy_df[1,"X_id"] = xid
    dummy_df[1,"bisonStateQuery"] = query
    merged <- bind_rows(df, dummy_df)
  }
  merged <- merged[c("X_id", "bisonStateQuery", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Michigan", "Missouri", "Minnesota", "Nebraska", "North Dakota", "Ohio", "South Dakota", "Wisconsin")]
  merged <- merged[-1, ]
  # stack bison query into one final df
  df_total <- rbind(df_total, merged)
  
}
# final df
df_total <- df_total[-1, ]  # first row is NA's

# Merge df with original data
MAFWA_list_speciesonly <- merge(MAFWA_list_speciesonly, df_total, by = "bisonStateQuery", all.x = T)
MAFWA_list_speciesonly[,33:45] <- lapply(MAFWA_list_speciesonly[,33:45], function(x) as.numeric(x))

# Group by class

r1 <- MAFWA_list_speciesonly %>%
  group_by(class) %>%
  summarize(Total = sum(bisontotal), Illinois = sum(Illinois, na.rm = T), Indiana = sum(Indiana, na.rm = T), Iowa = sum(Iowa, na.rm = T), 
            Kansas = sum(Kansas, na.rm = T), Kentucky = sum(Kentucky, na.rm = T), Michigan = sum(Michigan, na.rm = T), 
            Missouri = sum(Missouri, na.rm = T), Minnesota = sum(Minnesota, na.rm = T), Nebraska = sum(Nebraska, na.rm = T), 
            `North Dakota` = sum(`North Dakota`, na.rm = T), Ohio = sum(Ohio, na.rm = T), `South Dakota` = sum(`South Dakota`, na.rm = T), 
            Wisconsin = sum(Wisconsin, na.rm = T), Count = n())
r1wbisonTotal <- r1
r1$Total <- NA
for (i in 1:nrow(r1)){
  r1[i,]$Total <- (r1[i,]$Illinois + r1[i,]$Indiana + r1[i,]$Iowa + r1[i,]$Michigan + r1[i,]$Missouri + r1[i,]$Minnesota + r1[i,]$Ohio + r1[i,]$Wisconsin)
}
r1$class <- factor(r1$class, levels = r1$class[order(r1$Total)])
r1 <- r1[order(r1$Total),] 


# Move the count of the number of species within that class to the class field
r1$class <- paste(r1$class, r1$Count, sep = " (")
r1$class <- paste(r1$class, ")", sep = "")
r1 <- r1[,-16] #Removes "Count" column

# # Create a log version of the data to prep for a dot plot since Aves makes it impossible to see the other classes
# r_log <- r
# r_log$logTotal <- log10(r_log$FWSRegionTotal)
# r_log <- r_log[order(r_log$logTotal),]

ls_r1 <- MAFWA_list %>%
  group_by(class, ListingStatus) %>%
  summarize(count = n())
ls_r1 <- ls_r1[which(!is.na(ls_r1$ListingStatus)),]
ls_r1 <- ls_r1[which(ls_r1$ListingStatus == "Candidate" | ls_r1$ListingStatus == "Endangered" | 
                       ls_r1$ListingStatus == "Threatened" | 
                       ls_r1$ListingStatus == "Under Review in the Candidate or Petition Process"),]

ListingStatus2 <- MAFWA_list[which(!is.na(FWSRegion1_list$ListingStatus2)),]

EndangeredList <- MAFWA_list[which(FWSRegion1_list$ListingStatus == "Endangered"),]

# What can we say about the distribution of observation types at the class level?

o1 <- MAFWA_list_speciesonly %>%
  group_by(class) %>%
  summarise(Count = n(), Literature = sum(literature, na.rm = T), Observation = sum(observation, na.rm = T), 
            Specimen = sum(specimen, na.rm = T), Fossil = sum(fossil, na.rm = T), Unknown = sum(unknown, na.rm = T))
o1$class <- paste(o1$class, o1$Count, sep = " (")
o1$class <- paste(o1$class, ")", sep = "")
o1 <- o1[,-2] #Removes "Count" column
o1$class <- factor(o1$class, levels = o1$class[order(o1$Observation)])
o1 <- o1[order(o1$Observation),]
o1_gather <- gather(o1, key = "type", value = "Number of Occurrences", -class)






