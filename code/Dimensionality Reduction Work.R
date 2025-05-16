# Load the libraries

# Load the libraries
library(quanteda)
library(readr)
library(dplyr)
library(tibble) # found this online to help retain indexes during random sampling
library(quanteda.textmodels)
#install.packages("caret")
library(caret)
library(groupdata2)
library(quanteda.classifiers)
library(wordVectors)
library(magrittr)
library(tsne)
library(ggplot2)
library(dplyr)
library(umap)
# install.packages("plotly")
library(plotly)
library(htmlwidgets)
library(ggplot2)
# install.packages("randomForest")
library(randomForest)
library(stringr)
library(tidyr)
library(lsa)
# install.packages("gridExtra")
# install.packages("grid")
library(gridExtra)
library(grid)
library(scales)
library(randomForest)
# install.packages("pheatmap")
library(pheatmap)
# install.packages("e1071")
library(e1071)

all_leg <- read_csv("all_leg.csv")

all_leg_vecs <- read_csv("all_leg_vecs.csv")

all_leg$BillText[2]
all_leg$BillNumber[2]
dim(all_leg)

# Getting bill authors
all_leg <- all_leg %>%
  mutate(Author = str_extract(BillText, "(?<=author)[a-zA-Z]+(?=version)"))

# Results
head(all_leg[c("BillText", "Author")], 5)

# How many missing?
sum(!is.na(all_leg$Author))
# Answer: too many.
# Gonna do this the hard way I guess

colnames(all_leg)
# write.csv(all_leg[, c("BillNumber", "year", "Jurisdiction")], "finding_authors.csv", row.names = FALSE)

unique(all_leg$BillStatus)
filtered_row <- all_leg[which(all_leg$BillNumber == "S 523"), ]
filtered_row$year

authors <- read_csv("authors_complete.csv")

# Adding authors back in

all_leg <- all_leg %>%
  left_join(authors, by = c("BillNumber", "Jurisdiction", "year"))

colnames(all_leg)

# Getting rid of empty bill text

# rows where BillText is "no url available"
all_leg <- all_leg %>%
  filter(BillText != "no url available")

# rows where BillText is "there are no text versions currently associated with this id"
all_leg <- all_leg %>%
  filter(BillText != "there are no text versions currently associated with this id")


dim(all_leg)

# Checking out duplicated bills across years
duplicates <- all_leg %>%
  group_by(BillURL) %>%
  filter(n() > 1) %>%
  ungroup()

# View the first few rows of duplicates
head(duplicates)

# Removing duplicates if they have the same on BillURL and BillStatus
all_leg <- all_leg %>%
  distinct(BillURL, BillStatus, .keep_all = TRUE)

# Final df info:

dim(all_leg)

table(all_leg$Jurisdiction)
length(unique(all_leg$PoliticalAffiliation))


# fixing author column

all_leg <- all_leg %>%
  select(-Author.x) %>% 
  rename(Author = Author.y)  


sum(table(all_leg$Author) > 3)
sort(table(all_leg$Author), decreasing = TRUE)

category_popularity <- all_leg %>%
  group_by(PoliticalAffiliation, Category_clean) %>%
  summarise(count = n()) %>%
  arrange(PoliticalAffiliation, desc(count))

# View the result
head(category_popularity)


# Great. Now, let's recreate those umap visuals and save them.
# We'll add political affiliation as a new feature

vectors <- all_leg_vecs %>%
  select(-`Bill Name`)  # Exclude non-numeric columns

vectors[] <- lapply(vectors, as.numeric)

umap <- umap(vectors)

umap_df <- as.data.frame(umap$layout)
colnames(umap_df) <- c("V1", "V2")

umap_df <- cbind(all_leg_vecs["Bill Name"], umap_df)

umap_df <- umap_df %>%
  rename(BillNumber = `Bill Name`)

ggplot(umap_df, aes(x = V1, y = V2)) +
  geom_point(color = "blue", alpha = 0.7) +  
  labs(title = "UMAP AI Bill Landscape\n(Full document vectors)", x = "Dimension 1", y = "Dimension 2") +
  theme_minimal()

# UMAP by features

umap_df <- merge(
  umap_df, 
  all_leg[, c("BillNumber", "Jurisdiction", "Category_clean", "BillStatus", "year", "BillText", "Author", "PoliticalAffiliation")], 
  by = "BillNumber", 
  all = FALSE  # Performs an inner join, keeping only matching rows
)

dim(umap_df)

# By Jurisdiction

ggplot(umap_df, aes(x = V1, y = V2, color = Jurisdiction)) +
  geom_point(alpha = 0.7) +  
  labs(title = "UMAP AI Bill Landscape by Jurisdiction\n(Full document vectors)", x = "Dimension 1", y = "Dimension 2") +
  theme_minimal()

# By category

umap_df <- umap_df %>%
  mutate(Category_clean = replace_na(as.character(Category_clean), "Other"))

ggplot(umap_df, aes(x = V1, y = V2, color = Category_clean)) +
  geom_point(alpha = 0.7) +  
  labs(title = "UMAP AI Bill Landscape by Category\n(Full document vectors)", x = "Dimension 1", y = "Dimension 2", color = "Bill Category") +
  theme_minimal()

# By bill status

ggplot(umap_df, aes(x = V1, y = V2, color = BillStatus)) +
  geom_point(alpha = 0.7) +  
  labs(title = "UMAP AI Bill Landscape by Bill Status\n(Full document vectors)", x = "Dimension 1", y = "Dimension 2", color = "Bill Status") +
  theme_minimal()

# By year

ggplot(umap_df, aes(x = V1, y = V2, color = as.factor(year))) +
  geom_point(alpha = 0.7) +  
  labs(title = "UMAP AI Bill Landscape by Legislative Year\n(Full document vectors)", x = "Dimension 1", y = "Dimension 2", color = "Year of Proposal") +
  theme_minimal()

# By author political affiliation

umap_df <- umap_df %>%
  mutate(PoliticalAffiliation = replace_na(as.character(PoliticalAffiliation), "Other"))


ggplot(umap_df, aes(x = V1, y = V2, color = factor(PoliticalAffiliation, levels = c("D", "R", "Other")))) +
  geom_point(alpha = 0.7) +  
  scale_color_manual(
    name = "Principal Bill Author\nPolitical Affiliation",  # Legend title
    values = c("D" = "blue", "R" = "red", "Other" = "gray")  # Custom colors
  ) +
  labs(title = "UMAP AI Bill Landscape by Legislative Author Political Affiliation\n(Full document vectors)", 
       x = "Dimension 1", 
       y = "Dimension 2") +
  theme_minimal()


# Interactive plot
plot <- plot_ly(
  data = umap_df,
  x = ~V1,  # UMAP dimension 1
  y = ~V2,  # UMAP dimension 2
  type = 'scatter',
  mode = 'markers',
  text = ~paste(
    "Bill Number: ", BillNumber, "<br>",
    "Jurisdiction: ", Jurisdiction, "<br>",
    "Status: ", BillStatus, "<br>",
    "Category: ", Category_clean, "<br>",
    "Year: ", year, "<br>",
    "Author: ", Author, "<br>",
    "Author Political Affiliation: ", PoliticalAffiliation
  ),  # Metadata to display on hover
  hoverinfo = 'text',  # Show the text on hover
  marker = list(size = 6, opacity = 0.7, color = 'blue')  # Customize marker appearance
)

# Add layout for better visualization
plot <- plot %>%
  layout(
    title = "State-Level AI Legislation Diversity \n(UMAP Dimensionality Reduction)",
    xaxis = list(title = "Dimension 1"),
    yaxis = list(title = "Dimension 2")
  )

# Display the plot
plot


# Save the plot as an HTML file
saveWidget(plot, "umap_plot.html", selfcontained = TRUE)
saveWidget(plot, "doc_vecs_plot.html", selfcontained = FALSE)
