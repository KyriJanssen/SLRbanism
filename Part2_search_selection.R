### Code to exemplify Part 2 with Xiaoxia's query


# Section 1: Retrieve references from WoS and Scopus (Shuyu) 
library(wosr)
library(rscopus)

# Section 2: Combine tables, deduplicate references and summarise (Kyri?) 
#install.packages("revtools")
#install.packages("here")
#install.packages("tidyverse")
library(revtools)
library(here)
library(tidyvertse)

# Read the bib or ris file (depending on Shuyu's part - may not be necessary)
scopus_data <- read_bibliography(here("Data", "scopus.bib"))
wos_data <-read_bibliography(here("Data", "web_of_science.bib"))

# save variable names of dataframes is object
unique_vars_scopus <- colnames(scopus_data)
unique_vars_wos <- colnames(wos_data)

#identify which columns the scopus and wos dataframes have in common
common_vars <- intersect(unique_vars_scopus, unique_vars_wos)
print(common_vars)

#identify which columns are unique for the scopus and wos references
unique_vars_only_scopus <- setdiff(unique_vars_scopus, unique_vars_wos)
print(unique_vars_only_scopus)

unique_vars_only_wos <- setdiff(unique_vars_wos, unique_vars_scopus)
print(unique_vars_only_wos)

# Rename the column in the wos_data from "issue" to "number"
colnames(wos_data)[colnames(wos_data) == "issue"] <- "number"

#select only the variables: label, author, type, title, year, volume and number in the scopus dataframe
scopus_selection <- scopus_data%>%
  select(c("label",  "title", "year", "journal", "volume", "number"))

#do the same for the wos dataframe
wos_selection <- wos_data%>%
  select(c("label", "title", "year", "journal", "volume", "number"))

#have a look at the subsetted dataframes 
head(scopus_selection)
head(wos_selection)

#to make sure that our variables in the scopus and wos dataframes are represented in a similar way, we can remove all punctuations and capital letters from relevant character variables

# First create function called `preprocess` where
preprocess <- function(text) {
  text <- tolower(text) #all characters are transformed to lower-case
  text <- gsub("[[:punct:]]", "", text) #all punctuations are removed from the characters
  return(text)
}

#apply this function to the scopus_selection dataframe
scopus_selection$journal <- sapply(scopus_selection$journal, preprocess)
scopus_selection$title <- sapply(scopus_selection$title, preprocess)
scopus_selection$label <- sapply(scopus_selection$label, preprocess)

#examine the scopus_selection dataframe
head(scopus_selection)

#do the same with wos dataframe
wos_selection$journal <- sapply(wos_selection$journal, preprocess)
wos_selection$title <- sapply(wos_selection$title, preprocess)
wos_selection$label <- sapply(wos_selection$label, preprocess)

#examine the wos_selection dataframe
head(wos_selection)

#combine the wos dataframe with the scopus dataframe
all_references <- rbind(wos_selection, scopus_selection)

# locate and extract unique references
check_duplicates <- find_duplicates(all_references)
unique_references <- extract_unique_references(all_references, matches = check_duplicates)

#compare the total number of references to the total number of unique references
#count the number of rows in the all_references dataframe
nrow(all_references)
#do the same for the references_unique dataframe
nrow(unique_references)


# Section 3: Suggest new keywords (Clementine) 

# install.packages("remotes")
# library(remotes)
# install_github("elizagrames/litsearchr", ref="main")
library(litsearchr)
library(igraph)

#Import .bib or .ris database
refs <- litsearchr::import_results("data")

# identify frequent terms
raked_terms <- extract_terms(text = refs, 
                             method = "fakerake",
                             min_freq=2,
                             min_n=2)

# identify frequent keywords tagged by authors
keywords <- extract_terms(text = refs, 
                          method = "tagged",
                          keywords = refs$keywords,
                          ngrams=T,
                          min_n=2)

# create document-feature matrix
dfm <- create_dfm(elements = refs$title, 
                  features = c(raked_terms, keywords))

# create network
net <- create_network(search_dfm = dfm,
                      min_studies = 3,
                      min_occ = 3)


hist(igraph::strength(net), 
     main="Histogram of node strengths", 
     xlab="Node strength")


cutoffs_cumulative <- find_cutoff(net, method = "cumulative") 

reduced_graph <- reduce_graph(net, cutoff_strength = cutoffs_cumulative)

plot(reduced_graph)

search_terms <- litsearchr::get_keywords(reduced_graph)
head(sort(search_terms), 20)

# identify isolated components of graph
components(reduced_graph)
grouped <- split(names(V(reduced_graph)), components(reduced_graph)$membership)

litsearchr::write_search(grouped,
                         API_key = NULL,
                         languages = "English",
                         exactphrase = FALSE,
                         stemming = TRUE,
                         directory = "./",
                         writesearch = FALSE,
                         verbose = TRUE,
                         closure = "left")

# Section 4: build PRISMA figure (Xiaoxia)
library(PRISMA2020)
