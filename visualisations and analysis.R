library(synthesisr)
library(revtools)

rayyan <- read_refs("visualisations and analysis/provisional includes 21.12.24.ris")

# remove references from abstracts
rayyan$abstract <- sub(". References .*", ".", rayyan$abstract)

# load in keywords
terms <- read.csv("visualisations and analysis/Salmon scoping review keywords - Sheet1.csv")
pathogen_terms <- terms$Keyword.1

# create new dataframe for topics
topics <- data.frame(RayyanID=rayyan$accession_zr, title=rayyan$title, abstract=rayyan$abstract)

# check pathogen terms
topics$pathogenLogic <- grepl(paste(pathogen_terms, collapse="|"), paste(topics$title, topics$abstract))

