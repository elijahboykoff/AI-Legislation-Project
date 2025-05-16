## Project work

library(stm)
library(quanteda)
library(quanteda.corpora)
library(readr)
library(dplyr)
library(tidyr)
# install.packages("tidytext")
library(tidytext)
# install.packages("stringr")
library(stringr)
# install.packages("textTinyR")
library(textTinyR)
library(ggplot2)
# install.packages("umap")
library(umap)

# setwd("..")
getwd()

cleaned_data <- read.csv("clean_ai_leg.csv")
head(cleaned_data, 2)

# How many rows of data? 
length(cleaned_data$BillNumber)
# 475

# How many unique bills are there?
length(unique(cleaned_data$BillNumber))
# 466 unique bills

# Which bills are duplicated?
unique(cleaned_data$BillNumber[duplicated(cleaned_data$BillNumber)])
# [1] "H 1361"                      "None"                        "H 1814"                     
# [4] "No 2024 legislative session" "S 217"                       "Not available"              
# [7] "H 249" 


# Which categories of bills are most likely to pass?

# Chopping up Category col
cleaned_data_long <- cleaned_data %>%
  separate_rows(Category, sep = ";") %>%
  mutate(Category = trimws(Category)) 

# Calculating the proportion of enacted bills within each unique category
category_enacted_proportion <- cleaned_data_long %>%
  group_by(Category) %>%
  summarize(
    total_bills = n(),
    enacted_bills = sum(BillStatus == "Enacted"),
    enactment_rate = enacted_bills / total_bills
  ) %>%
  arrange(desc(enactment_rate))

print(category_enacted_proportion, n = 28)

# 'Appropriations' bills have a 46% chance of enactment! Pretty interesting
# 'Private Sector Use' bills have the lowest chance of enactment - 7.7%
# This isn't a perfect proportion, as 'pending' bills are still involved

# How about the language within these categories?

word_counts <- cleaned_data_long %>%
  unnest_tokens(word, text_clean, token = "words", strip_numeric = TRUE) %>% 
  anti_join(stop_words, by = "word") %>% 
  filter(!grepl("[[:punct:]]", word)) %>% 
  count(Category, word, sort = TRUE)    

top_words_by_category <- word_counts %>%
  group_by(Category) %>%
  slice_max(n, n = 20, with_ties = FALSE) %>% 
  ungroup()

# Step 4: Create a list of top words by category
top_words_list <- top_words_by_category %>%
  group_by(Category) %>%
  summarize(top_words = paste(word, collapse = ", "), top_counts = paste(n, collapse = ", "), .groups = "drop")


# View the list
print(top_words_list)








# CORRECT DATA (!!)
# Word embeddings: let's get started

# pkgbuild::check_build_tools(debug = TRUE)

# remotes::install_github("bmschmidt/wordVectors")

#install.packages("wordVectors", repos = "http://cran.us.r-project.org")

library(wordVectors)
library(magrittr)
library(tsne)
library(ggplot2)
library(dplyr)

setwd("..")
# getwd()
correct_data <- read.csv("all_ai_bills.csv", header = TRUE, sep = ",")
head(correct_data, 2)

# BillStatus column seems like it was scraped incorrectly in certain cases
unique(correct_data$BillStatus)

#Subsetting for only relevant bill statuses
desired_statuses <- c("Enacted", "Failed - Adjourned", "Pending", "Failed", "To governor")
ai_leg <- subset(correct_data, BillStatus %in% desired_statuses)
dim(ai_leg)
unique(ai_leg$BillStatus)

# Renaming BillStatus labels for consistency
ai_leg$BillStatus <- ifelse(ai_leg$BillStatus == "Failed - Adjourned", 
                                   "Failed", 
                                   ifelse(ai_leg$BillStatus == "To governor", 
                                          "Pending", 
                                          ai_leg$BillStatus))
unique(ai_leg$BillStatus)
# Now, all the bills are labelled as "Enacted", "Failed", or "Pending"

# How about all cols? 16,000+ columns?
colnames(ai_leg)
desired_columns <- c("Jurisdiction", "BillNumber", "BillTitle", "BillStatus", "BillSummary", "Category", "BillURL", "VersionDate", "Pages", "BillText", "VersionType", "VersionURL")
ai_leg <- ai_leg[, desired_columns]
colnames(ai_leg)
dim(ai_leg)
# 1046 observations, 12 columns

# Getting just the last version of each bill and saving the number of iterations
ai_leg <- ai_leg %>%
  group_by(BillTitle) %>%
  mutate(BillIterations = n()) %>%
  # Keep only the last version of each bill
  slice_tail(n = 1) %>%
  ungroup()
dim(ai_leg)
# 363 different bills

# Cleaning of bill text
ai_leg$BillText <- tolower(ai_leg$BillText)
ai_leg$BillText <- gsub("[0-9]", "", ai_leg$BillText)
ai_leg$BillText <- gsub("[[:punct:]]", "", ai_leg$BillText)


# Cleaned df metadata
jurisdictions <- unique(ai_leg$Jurisdiction)
length(jurisdictions) 
# 46 jurisdictions

sort(jurisdictions)
# Run the above line for getting all jurisdictions included in dataset

# Bills per jurisdiction
bills_per_jurisdiction <- ai_leg %>%
  group_by(Jurisdiction) %>%
  summarise(NumBills = n()) %>%
  arrange(Jurisdiction) # Sorts by Jurisdiction alphabetically

print(bills_per_jurisdiction,n = 46)
# unsurprisingly, New York (60), California (30), and Illinois (24) have the most number of bills




# Now, let's get into word embeddings
# My first approach is to use Law2Vec to get the word vectors for every word in each document,
# and then average those word vectors to get a document vector. 

library(data.table)

# Load the word embeddings
embeddings <- fread("Law2Vec.200d.txt", header = FALSE, sep = " ", quote = "", fill = TRUE, skip = 1)

embedding_list <- split(embeddings[, -1, with = FALSE], embeddings$V1)

embedding_list <- lapply(embedding_list, function(vec) {
  as.numeric(vec)  # Convert each word vector to numeric
})

embedding_list[["legislation"]]

# Tokenized bill text
bill_text_tokens <- strsplit(ai_leg$BillText, split = " ")


embedding_matrix <- rbindlist(embedding_list, fill = TRUE)
word_vectors <- lapply(bill_text_tokens, function(words) {
  word_vecs <- sapply(words, function(word) {
    if (word %in% names(embedding_list)) {
      return(embedding_list[[word]])
    } else {
      return(rep(NA, ncol(embedding_matrix)))  # For unknown words
    }
  })
  rowMeans(word_vecs, na.rm = TRUE)  # Average the word vectors to get a document vector
})


# Convert word_vectors (list) to a matrix, then a data frame
document_matrix <- do.call(rbind, word_vectors)  # Combine vectors into a matrix
document_vectors <- as.data.frame(document_matrix)  # Convert to a data frame

# Add BillNumber to the document_vectors data frame
document_vectors$BillNumber <- ai_leg$BillNumber

# Optional: Inspect the resulting data frame
head(document_vectors)




# Trying again to see if I can get the bill number to stay attached to the document vectors


word_vectors2 <- lapply(seq_along(bill_text_tokens), function(i) {
  words <- bill_text_tokens[[i]]  # Tokenized words for the i-th document
  word_vecs <- sapply(words, function(word) {
    if (word %in% names(embedding_list)) {
      return(embedding_list[[word]])
    } else {
      return(rep(NA, ncol(embedding_matrix)))  # For unknown words
    }
  })
  
  # Average the word vectors to get the document vector
  doc_vector <- rowMeans(word_vecs, na.rm = TRUE)
  
  # Add the BillNumber as a feature to the document vector
  doc_vector_with_billnumber <- c(doc_vector, BillNumber = ai_leg$BillNumber[i])
  
  return(doc_vector_with_billnumber)
})

# Convert the list of vectors into a data frame
document_vectors_df <- as.data.frame(do.call(rbind, word_vectors2))

colnames(document_vectors_df) <- c(paste("Vector", 1:200, sep = "_"), "BillNumber")

head(document_vectors_df)
dim(document_vectors_df)

# Now, I have a document vector for each of my bills, stored in document_vectors_df
# Time to get into UMAP!

vectors <- document_vectors_df[, 1:200]  # leaving out the BillNumber column
# str(vectors)
vectors[] <- lapply(vectors, as.numeric) # have to change these to be numeric structure, they're in character form rn
umap <- umap(vectors)


# what does umap look like?
head(umap$layout)
# A bunch of XY coordinates - pefect!

# Let's visualize this

umap_df <- data.frame(umap$layout)
colnames(umap_df) <- c("V1", "V2")
umap_df$BillNumber <- document_vectors_df$BillNumber  # Adding BillNumber for labeling


ggplot(umap_df, aes(x = V1, y = V2)) +
  geom_point(color = "blue", alpha = 0.7) +  # Set a single color (e.g., "blue")
  labs(title = "UMAP AI Bill Landscape\n(averaged word2vecs)", x = "Dimension 1", y = "Dimension 2") +
  theme_minimal() +
  guides(color = "none")  # Remove color legend


# Dope. How about breaking each bill down by certain characteristics? Enactment/Failure,
# pre-defined category (from NCSL), jurisdiction?

# Only want to grab the first category listed in the 'Category' col
ai_leg$Category_clean <- sapply(strsplit(ai_leg$Category, ";"), function(x) x[1])

umap_df <- merge(umap_df, ai_leg[, c("BillNumber", "Jurisdiction", "Category_clean", "BillStatus")], 
                 by = "BillNumber", all.x = TRUE)

head(umap_df)

# Plot broken down by jurisdiction

ggplot(umap_df, aes(x = V1, y = V2, color = Jurisdiction)) +
  geom_point(alpha = 0.7) +  
  labs(title = "UMAP AI Bill Landscape by Jurisdiction\n(Averaged word2vecs)", x = "Dimension 1", y = "Dimension 2") +
  theme_minimal()

# Plot broken down by category

ggplot(umap_df, aes(x = V1, y = V2, color = Category_clean)) +
  geom_point(alpha = 0.7) +  
  labs(title = "UMAP AI Bill Landscape by Category\n(Averaged word2vecs)", x = "Dimension 1", y = "Dimension 2") +
  theme_minimal()

# Plot broken down by Bill status

ggplot(umap_df, aes(x = V1, y = V2, color = BillStatus)) +
  geom_point(alpha = 0.7) +  
  labs(title = "UMAP AI Bill Landscape by Bill Status\n(Averaged word2vecs)", x = "Dimension 1", y = "Dimension 2") +
  theme_minimal()





# Alright, now I am going to a try a different way of vectorizing my documents.
# The last approach was ok for experimenting with these methods, but I lose a lot of context
# and complex relationships between terms this way.

# Alex Newhouse shared this method with me, trying it out

library(tidyverse)
library(reticulate)
py_install("google-generativeai")

# just saving ai_leg here so I can have a fresh df. Not necessary, just so I can organize my data a bit
write.csv(ai_leg, "ai_leg_clean.csv", row.names = FALSE)

docs <- read_csv("ai_leg_clean.csv") 

gm <- import("google.generativeai")
gm$configure(api_key="AIzaSyCCBLiP_aSpPqrlxsS59Aln1YiYVIFsl1I")
response <- gm$embed_content(
  model='models/text-embedding-004',
  content=docs$BillText,
  task_type='SEMANTIC_SIMILARITY'
)

# Cool!

document_vectors <- response$embedding
# str(response)
str(document_vectors)
document_vectors_df <- as.data.frame(do.call(rbind, document_vectors))
# document_vectors_df$BillNumber <- docs$BillNumber
str(document_vectors_df)
document_vectors_df[] <- lapply(document_vectors_df, as.numeric)

umap2 <- umap(document_vectors_df)

# Get the reduced 2D dimensions
umap_df2 <- as.data.frame(umap2$layout)
colnames(umap_df2) <- c("V1", "V2") 
umap_df2$BillNumber <- docs$BillNumber
head(umap_df2)


ggplot(umap_df2, aes(x = V1, y = V2)) +
  geom_point(color = "blue", alpha = 0.7) +  
  labs(title = "UMAP AI Bill Landscape\n(Full document vectors)", x = "Dimension 1", y = "Dimension 2") +
  theme_minimal()

# UMAP by features

umap_df2 <- merge(umap_df2, docs[, c("BillNumber", "Jurisdiction", "Category_clean", "BillStatus")], 
                 by = "BillNumber", all.x = TRUE)

# By Jurisdiction

ggplot(umap_df2, aes(x = V1, y = V2, color = Jurisdiction)) +
  geom_point(alpha = 0.7) +  
  labs(title = "UMAP AI Bill Landscape by Jurisdiction\n(Full document vectors)", x = "Dimension 1", y = "Dimension 2") +
  theme_minimal()

# By category

ggplot(umap_df2, aes(x = V1, y = V2, color = Category_clean)) +
  geom_point(alpha = 0.7) +  
  labs(title = "UMAP AI Bill Landscape by Category\n(Full document vectors)", x = "Dimension 1", y = "Dimension 2") +
  theme_minimal()

# By bill status

ggplot(umap_df2, aes(x = V1, y = V2, color = BillStatus)) +
  geom_point(alpha = 0.7) +  
  labs(title = "UMAP AI Bill Landscape by Bill Status\n(Full document vectors)", x = "Dimension 1", y = "Dimension 2") +
  theme_minimal()


# Next step: calculating average similarity between bills of each grouping (category, jurisdiction, etc)

# I had to restart my laptop and then I dealt with a ton of issues with the google generative AI stuff. 
# To circumvent this, I did the work in a Jupyter Notebook and saved the vectors as a csv, which I'll import here.


vecs <- read_csv("please_work.csv")
# head(vecs, 2)

vecs_with_md <- merge(vecs, docs, by.x = "Bill Name", by.y = "BillNumber", all = TRUE)
head(vecs_with_md, 2)

# Ok, now I've got that saved and merged. Time to fuck around with the similarity stuff
# This is uncharted territory. I'm just playing to see what is possible. Chat GPT was very helpful here :)




# JURISDICTION FIRST

# install.packages("lsa")
library(lsa)

# Group the data by 'Jurisdiction'
grouped_by_jurisdiction <- split(vecs_with_md, vecs_with_md$Jurisdiction)

# Function to compute the average cosine similarity for a group
compute_avg_cosine_similarity <- function(group) {
  # Extract the embeddings
  embeddings <- group[, grepl("Vec", names(group))]
  
  # Compute pairwise cosine similarity matrix
  sim_matrix <- cosine(as.matrix(embeddings))
  
  # Extract the upper triangle of the similarity matrix (excluding diagonal)
  sim_values <- sim_matrix[lower.tri(sim_matrix)]
  
  # Compute the average cosine similarity
  avg_similarity <- mean(sim_values, na.rm = TRUE)
  return(avg_similarity)
}

# Apply the function to each group
avg_cosine_similarities <- sapply(grouped_by_jurisdiction, compute_avg_cosine_similarity)

# Normalize the 'AvgCosineSimilarity' column
avg_cosine_similarities_df$NormalizedAvgCosineSimilarity <- 
  (avg_cosine_similarities_df$AvgCosineSimilarity - min(avg_cosine_similarities_df$AvgCosineSimilarity)) / 
  (max(avg_cosine_similarities_df$AvgCosineSimilarity) - min(avg_cosine_similarities_df$AvgCosineSimilarity))

# Sort the dataframe by the normalized values in descending order
avg_cosine_similarities_df <- avg_cosine_similarities_df[order(-avg_cosine_similarities_df$NormalizedAvgCosineSimilarity), ]
avg_cosine_similarities_with_bill_count <- avg_cosine_similarities_df %>%
  left_join(bills_per_jurisdiction, by = "Jurisdiction")

print(avg_cosine_similarities_with_bill_count)







# NOW, BY CATEGORY

# Group the data by 'Category_clean'
grouped_by_category <- split(vecs_with_md, vecs_with_md$Category_clean)

# Function to compute the average cosine similarity for a group
compute_avg_cosine_similarity <- function(group) {
  # Extract the embeddings (assuming your embeddings are columns with 'Vec' in their name)
  embeddings <- group[, grepl("Vec", names(group))]
  
  # Compute pairwise cosine similarity matrix
  sim_matrix <- cosine(as.matrix(embeddings))
  
  # Extract the upper triangle of the similarity matrix (excluding diagonal)
  sim_values <- sim_matrix[lower.tri(sim_matrix)]
  
  # Compute the average cosine similarity
  avg_similarity <- mean(sim_values, na.rm = TRUE)
  return(avg_similarity)
}

# Apply the function to each group
avg_cosine_similarities <- sapply(grouped_by_category, compute_avg_cosine_similarity)

# Convert the result into a data frame
avg_cosine_similarities_df <- data.frame(
  Category_clean = names(avg_cosine_similarities),
  AvgCosineSimilarity = avg_cosine_similarities
)

# Normalize the 'AvgCosineSimilarity' column
avg_cosine_similarities_df$NormalizedAvgCosineSimilarity <- 
  (avg_cosine_similarities_df$AvgCosineSimilarity - min(avg_cosine_similarities_df$AvgCosineSimilarity)) / 
  (max(avg_cosine_similarities_df$AvgCosineSimilarity) - min(avg_cosine_similarities_df$AvgCosineSimilarity))

# Sort the dataframe by the normalized values in descending order
avg_cosine_similarities_df <- avg_cosine_similarities_df[order(-avg_cosine_similarities_df$NormalizedAvgCosineSimilarity), ]

# Count the number of bills per Category_clean
bill_counts_by_category <- vecs_with_md %>%
  group_by(Category_clean) %>%
  summarize(num_bills = n())

# Merge the count of bills with the average cosine similarities
similarity_by_category_with_bill_count <- avg_cosine_similarities_df %>%
  left_join(bill_counts_by_category, by = "Category_clean")

print(similarity_by_category_with_bill_count)



# LAST, BILL STATUS

# Group the data by 'BillStatus'
grouped_by_bill_status <- split(vecs_with_md, vecs_with_md$BillStatus)

# Function to compute the average cosine similarity for a group
compute_avg_cosine_similarity <- function(group) {
  # Extract the embeddings (assuming your embeddings are columns with 'Vec' in their name)
  embeddings <- group[, grepl("Vec", names(group))]
  
  # Compute pairwise cosine similarity matrix
  sim_matrix <- cosine(as.matrix(embeddings))
  
  # Extract the upper triangle of the similarity matrix (excluding diagonal)
  sim_values <- sim_matrix[lower.tri(sim_matrix)]
  
  # Compute the average cosine similarity
  avg_similarity <- mean(sim_values, na.rm = TRUE)
  return(avg_similarity)
}

# Apply the function to each group
avg_cosine_similarities <- sapply(grouped_by_bill_status, compute_avg_cosine_similarity)

# Convert the result into a data frame
avg_cosine_similarities_df <- data.frame(
  BillStatus = names(avg_cosine_similarities),
  AvgCosineSimilarity = avg_cosine_similarities
)

# Normalize the 'AvgCosineSimilarity' column
avg_cosine_similarities_df$NormalizedAvgCosineSimilarity <- 
  (avg_cosine_similarities_df$AvgCosineSimilarity - min(avg_cosine_similarities_df$AvgCosineSimilarity)) / 
  (max(avg_cosine_similarities_df$AvgCosineSimilarity) - min(avg_cosine_similarities_df$AvgCosineSimilarity))

# Sort the dataframe by the normalized values in descending order
avg_cosine_similarities_df <- avg_cosine_similarities_df[order(-avg_cosine_similarities_df$NormalizedAvgCosineSimilarity), ]

# Count the number of bills per BillStatus
bill_counts_by_bill_status <- vecs_with_md %>%
  group_by(BillStatus) %>%
  summarize(num_bills = n())

# Merge the count of bills with the average cosine similarities
bill_status_cosine_similarities <- avg_cosine_similarities_df %>%
  left_join(bill_counts_by_bill_status, by = "BillStatus")

print(bill_status_cosine_similarities)

# This marks the end of my work on this project ... for now. I have a lot more to do, but
# seeing as this is a proof of concept project, I will use what I've discovered thus far to make a case
# for the direction I plan to take this work moving forward. 
