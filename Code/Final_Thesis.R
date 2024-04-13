
# Install the required packages
install.packages(c("twitteR", "ROAuth", "hms", "lubridate", "tidytext", "tm", "wordcloud", 
                   "igraph", "glue", "networkD3", "devtools", "plyr", "stringr", "ggplot2", 
                   "ggeasy", "plotly", "dplyr", "hms", "lubridate", "magrittr", "tidyverse", 
                   "janeaustenr", "widyr"))

install.packages("reshape2")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("tidyquant")
install.packages("potly")
install.packages("timetk")
install.packages("tidyr")

# Install 'rtweet' from GitHub
devtools::install_github("mkearney/rtweet")

# Load the installed packages
library(twitteR)
library(ROAuth)
library(hms)
library(lubridate) 
library(tidytext)
library(tm)
library(wordcloud)
library(igraph)
library(glue)
library(networkD3)
library(rtweet) 
library(plyr)
library(stringr)
library(ggplot2)
library(ggeasy)
library(plotly)
library(dplyr)  
library(hms)
library(lubridate) 
library(magrittr)
library(tidyverse)
library(janeaustenr)
library(widyr)
library(reshape2)
library(ggplot2)
library(dplyr)
library(tidyquant) 
library(plotly) 
library(timetk) 
library(tidyr)
library(readr)

install.packages("pacman")
pacman::p_load(tidyverse, quanteda, tm, randomForest, varImp, ggwordcloud, kableExtra)



################################################################################
#                                 Import Data                                  #
################################################################################

library(readxl)
Data_Thesis <- read_excel("C:/Users/jwelch2/Desktop/Data_Thesis.xlsx")
View(Data_Thesis)

library(readxl)
ML_Classified_Sentiment_Data <- read_excel("C:/Users/jwelch2/Desktop/ML_Classified_Sentiment_Data.xlsx")
View(ML_Classified_Sentiment_Data)


################################################################################
#                     Data Sentiment Analysis And Cleaning                     #
################################################################################


# Check column names
colnames(Data_Thesis)

#Clean Announcements
clean_Announcement <- function(text) {
  require(stringr)
  
  cleaned_text <- sapply(text, function(Announcement) {
    
    # Remove URLs
    Announcement <- gsub('https://|http://', '', Announcement)
    
    # Remove non-graph characters
    Announcement <- gsub('[^[:graph:]]', ' ', Announcement)
    
    # Remove punctuation
    Announcement <- gsub('[[:punct:]]', '', Announcement)
    
    # Remove control characters
    Announcement <- gsub('[[:cntrl:]]', '', Announcement)
    
    # Remove digits
    Announcement <- gsub('\\d+', '', Announcement)
    
    # Convert to lowercase
    Announcement <- tolower(Announcement)
    
    # Remove emotes
    Announcement <- str_replace_all(Announcement, "[\\x{1F600}-\\x{1F64F}\\x{1F300}-\\x{1F5FF}\\x{1F680}-\\x{1F6FF}\\x{1F700}-\\x{1F77F}\\x{1F780}-\\x{1F7FF}\\x{1F800}-\\x{1F8FF}\\x{1F900}-\\x{1F9FF}\\x{1FA00}-\\x{1FA6F}\\x{1FA70}-\\x{1FAFF}\\x{2600}-\\x{26FF}\\x{2700}-\\x{27BF}\\x{2300}-\\x{23FF}]", "")
    
    # Replace consecutive spaces with a single space
    Announcement <- str_replace_all(Announcement, "\\s+", " ")
    
    # Remove non-English words and characters
    Announcement <- str_replace_all(Announcement, "[^a-z\\s]", "")
    
    # Replace return spaces with a single space
    Announcement <- str_replace_all(Announcement, "\n", " ")
    
    # Remove words that begin with "nasdaq"
    Announcement <- str_replace_all(Announcement, "\\bnasdaq\\w*\\b", "")
    
    # Remove words that begin with "nyse"
    Announcement <- str_replace_all(Announcement, "\\bnyse\\w*\\b", "")
    
    # Remove month names
    Announcement <- str_replace_all(Announcement, "\\b(january|february|march|april|may|june|july|august|september|october|november|december)\\b", "")
    
    # Remove specific words like "the," "to," "about", and "and". ect. 
    Announcement <- str_replace_all(Announcement, "\\b(the|to|and|about|or|of|for|because|that|a|an|we|company|said|with|in|by|these|this|globe|newswire|inc|at|has|is|our|they|it|as|us|its|nasdaq|nys|jan|feb|mar|apr|may|jun|jul|aug|sep|oct|nov|dec|their|on|which|prnewswire|sept|whose|are|from|chief|is|was|were|study|fda|also|will|president|studies|therapy|who|whom|data|executive|administration|have|drug|disease|research|results|all|officer|today|compared|patients|announced|can|dose|food|based|observed|phd|placebo|than|therapies|through|two|one|three|patient|vice|need|cell|diseases|been|randomized|overall|week|after|biopharmaceuticals|combination|cancer|people|months|into|pharmaceuticals|product|generally|had|medical|medicine|medicines|scientific|there|type|weeks|where|years|biotechnology|following|oncology|therapeutics|use|annual|oral|products|biopharmaceutical|date|agent|alone|body|during|metastatic|placebocontrolled|arm|year|tumors|see|when|additionally|clinically|due|while.|cancers|those|seeN|next.|antibody)\\b", "")
    
    # Remove cities and countries (you can extend this list as needed)
    Announcement <- str_replace_all(Announcement, "\\b(new york|london|paris|tokyo|berlin|beijing|boston|netherlands|amsterdam|irvine|calif|california|mass|cambridge|massachusetts|austin|texas|dallas|reykjavik|iceland|waltham|stamford|conn|connecticut|new have|united states|canada|redwood city|salt lake city|plymouth|san diego|chatham|nj|pa|san diego|ca|charlottesville|va|md|phase|fla|vancouver|wash|bethesda|minneapolis|seattle|palo alto|clinical|trials|trial|thousand oaks|newtown|newark|san francisco|melbourne|australia|bothell|zug|switzerland|indianapolis|pittsburgh|st paul|minn|new haven|lexington|needham|watertown|tel aviv israel|weston|westlake village|rockville|berkeley heights|taicang|china|houston|tx|bedford|massbusiness|ma|accesswire|new england journal|england|dublin|san carlos|ind|alpharetta|ga|woodcliff lake|clinicalstage|headquarters|ceo|across|both|including|mg|vs|chicago|th|preclinical|clinicalstage)\\b", "")
    
    # Remove cities and countries (you can extend this list as needed)
    Announcement <- str_replace_all(Announcement, "\\b(indications|additionsuch|did|adx|iop|bimatoprost|approximately|days|function.|dmt|million|day|solid|stage|university|half|human|mean|lung|world|four|tumor|vitro|chemotherapy|therapeutic|david|step|seen|six|pharmaceutical|bring|administered|participants|protein|uisng|american|such|vivo|cells|treatment|biib|biogen|develeopment|als|alzheimers|glaucoma|amyloid|diroximel|spinraza|margenza|nsclc|difelikefalin|tcs|cda|trastuzumab|clene|eisai|program|fumarate|eye|development|ata|cft|tau|vtx|nda|easi)\\b", "")
    
    return(Announcement)
  })
  
  return(cleaned_text)
}

# Assuming the Text column is named 'Announcement' in Training_Data_Day
Data_Thesis$clean_Announcement <- clean_Announcement(Data_Thesis$Announcement)

# View the updated data
View(Data_Thesis)

################################################################################
#                                 Build Matrix                                 #
################################################################################

# cast text data into corpus
corpus <- Corpus(VectorSource(Data_Thesis$clean_Announcement)) %>% 
  tm_map(stripWhitespace)

# Create document term feature matrix
dtm <- DocumentTermMatrix(corpus) %>% 
  removeSparseTerms(., 0.85) %>% #80% of words kept
  as.matrix() %>% 
  as.data.frame()

# Add column names to matrix
colnames(dtm) = make.names(colnames(dtm)) # Make Syntactically Valid Names

# Illustration: Show subset of matrix
# show first, last columns
dtm %>% 
  select(1:5, tail(names(.), 4)) %>% 
  slice(1:8) %>%
  kable("html") %>%
  kable_styling(font_size = 9)


#merge data
Data_Thesis <- bind_cols(Data_Thesis, dtm)

# Illustration: Show subset of data
# show first, last columns
Data_Thesis %>% 
  select(1:5, tail(names(.), 4)) %>% 
  slice(1:8) %>%
  kable("html") %>%
  kable_styling(font_size = 9)

#check for duplicates
duplicated_vars <- duplicated(names(Data_Thesis))

# Keep the first occurrence of each variable, drop duplicates
Data_Thesis <- Data_Thesis[, !duplicated_vars]

view(Data_Thesis)

# Assuming your dataset is named Data_Thesis_for_forest
observation_index <- 38
variable_name <- "clean_Announcement"

# Convert the list to character and print the value
cat("Value of", variable_name, "for observation", observation_index, "is:\n")
cat(paste(Data_Thesis[observation_index, variable_name], collapse = "\n"), "\n")


################################################################################
#                          Create Training and Test Data                       #
################################################################################

#Check target results
result <- table(Data_Thesis$Human_Learning_Sentiment)

# Display the result
print(result)

#fix mispellings
library(dplyr)

Data_Thesis <- Data_Thesis %>%
  mutate(Human_Learning_Sentiment = case_when(
    Human_Learning_Sentiment == "Positve" ~ "Positive",
    TRUE ~ Human_Learning_Sentiment  # Keep the original value if no condition is met
  ))

#Check target results
result <- table(Data_Thesis$Human_Learning_Sentiment)

# Display the result
print(result)

# Convert variables into factor
# Convert other attributes which really are categorical data but in the form of numbers
Data_Thesis$Human_Learning_Sentiment <- as.factor(Data_Thesis$Human_Learning_Sentiment)

# Convert other attributes which really are categorical data but in the form of numbers
Data_Thesis$Field <- as.factor(Data_Thesis$Field)

# Convert other attributes which really are categorical data but in the form of numbers
Data_Thesis$Ticker <- as.factor(Data_Thesis$Ticker)

# Convert other attributes which really are categorical data but in the form of numbers
Data_Thesis$Stage <- as.factor(Data_Thesis$Stage)

# Confirm variable types
sapply(Data_Thesis, class)

# Display summary statistics
summary(Data_Thesis)

# Assuming Data_Thesis is your data frame
library(dplyr)

# Specify the variables to drop
vars_to_drop <- c("Count", "Field", "Stage", "Ticker", "Name", "Drug", "Indication","Indication", "Date", "Announcement", "T_1_Day_Before", "T", "T_1_Day_After", "T_14", "T_30", "T_60", "clean_Announcement")

# Drop the specified variables from Data_Thesis
Data_Thesis_for_forest <- Data_Thesis %>% select(-one_of(vars_to_drop))

# Now Data_Thesis_for_forest does not contain these variables
view(Data_Thesis_for_forest)


# Set seed for reproducibility
set.seed(123)

install.packages('caTools')
library(caTools)

# Split the data into a training set (75%) and a test set (25%)
split = sample.split(Data_Thesis_for_forest$Human_Learning_Sentiment, SplitRatio = 0.75)
training_set = subset(Data_Thesis_for_forest, split == TRUE)
test_set = subset(Data_Thesis_for_forest, split == FALSE)

#Check target results
result <- table(training_set$Human_Learning_Sentiment)

# Display the result
print(result)

#Check target results
result <- table(test_set$Human_Learning_Sentiment)

# Display the result
print(result)

# Load necessary packages
library(randomForest)
library(dplyr)

# Check the levels of Human_Learning_Sentiment
levels(training_set$Human_Learning_Sentiment)

# Set seed for reproducibility
set.seed(123)

# Display the structure of the training_set dataset
str(training_set)

# Display the structure of the test_set dataset
str(test_set)

# Load necessary packages
library(randomForest)
library(party)

install.packages("rpart")
library(rpart)

install.packages("rpart.plot")
library(rpart.plot)

# Set seed for reproducibility
set.seed(123)

# Install and load the required packages
install.packages(c("randomForest", "caret", "ggplot2"))
library(randomForest)
library(caret)
library(ggplot2)

################################################################################
#                          Actual Classification Model                         #
################################################################################

# Set seed for reproducibility
set.seed(123)

# Fit a decision tree model with the 'Human_Learning_Sentiment' variable as the response
# Using the rpart function with a smaller complexity parameter (cp) and smaller minsplit
tree <- rpart(Human_Learning_Sentiment ~ ., data = training_set, cp = 0.00000001, minsplit = 9)

# Specify the file name and open the PNG device
png("Tree.png", width = 6.5, height = 5, units = "in", res = 400)

# Plot the decision tree with increased complexity and larger size
library(rpart.plot)

# Customize the plot
prp(tree, 
    box.col = c("#FF0000", "#CCCCCC", "#00FF00")[tree$frame$yval],  # Set node colors based on yval
    branch.lty = 1,        # Set line type for branches (1 for squared lines)
    branch.lwd = 3,        # Set line width for branches
    tweak = 1,             # Adjust text size and encourage more branching
    shadow.col = "#333333",   # Set shadow color
    branch.tweak = 4,      # Adjust branch length
    faclen = 0,            # Set faclen to 0 to increase the size of the tree
    fallen.leaves = TRUE,  # Show percentages at the leaves
    compress = TRUE        # Compress the plot to fit
)

# Close the PNG device to save the plot
dev.off()





# Train the random forest classifier
rf_model <- randomForest(Human_Learning_Sentiment ~ ., data = training_set, ntree = 19)

library(tibble)
library(dplyr)
install.packages('ggraph')
library(ggraph)
install.packages('igraph')
library(igraph)


y_pred1 <- predict(rf_model, newdata = test_set)

# Calculate accuracy
accuracy <- mean(y_pred1 == test_set$Human_Learning_Sentiment)

# Print the accuracy
cat("Accuracy:", accuracy, "\n")


# Assuming 'test_set' is your test dataset and 'survived' is the target variable
conf.matrix <- confusionMatrix(data = y_pred1, reference = test_set$Human_Learning_Sentiment)

# Convert the confusion matrix to a data frame for ggplot2
conf.matrix_df <- as.data.frame(as.table(conf.matrix))

# Create a more sophisticated heatmap of the confusion matrix using ggplot2
heatmap_plot <- ggplot(data = conf.matrix_df, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile() +
  theme_minimal() +
  scale_fill_gradientn(colors = brewer.pal(9, "Blues")) +
  labs(title = "Confusion Matrix",
       x = "Reference",
       y = "Prediction",
       fill = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 15),  # Adjusting x-axis label size
        axis.text.y = element_text(size = 15),  # Adjusting y-axis label size
        plot.title = element_text(hjust = 0.5, size = 20),  # Adjusting plot title size
        axis.title.x = element_text(size = 15),  # Adjusting x-axis title size
        axis.title.y = element_text(size = 15),  # Adjusting y-axis title size
        legend.text = element_text(size = 11),  # Adjusting legend text size
        legend.title = element_text(size = 15))  # Adjusting legend title size

# Save the plot as a PNG file
ggsave("confusion_matrix.png", plot = heatmap_plot, width = 8, height = 8, units = "in")

################################################################################
#                                   word Clouds                                #
################################################################################

install.packages("wordcloud2")
library(wordcloud2)

install.packages("htmlwidgets")
library(htmlwidgets)

install.packages("webshot2")
library(webshot2)


# Assuming your original dataset is named Data_Thesis_for_forest
# Create a new dataset with only "Negative" observations
Sampled_Negative_Data <- Data_Thesis[Data_Thesis$Human_Learning_Sentiment == "Negative", ]

# View the entire new dataset
View(Sampled_Negative_Data)

text_corpus <- Corpus(VectorSource(Sampled_Negative_Data$clean_Announcement))
tdm <- TermDocumentMatrix(text_corpus)
tdm <- as.matrix(tdm)
tdm <- sort(rowSums(tdm), decreasing = TRUE)
tdm <- data.frame(word = names(tdm), freq = tdm)

# Set the maximum number of words to include in the word cloud
max_words <- 80

red_palette <- colorRampPalette(c("#FF0000", "#800000"))

# Subset the data to include only the top words
top_words <- head(tdm, max_words)

# Create word cloud using wordcloud2
wordcloud <- wordcloud2(data = top_words, size = 0.8,  color = red_palette(nrow(top_words)))

# Save the word cloud as an HTML file
output_file <- "wordcloud_Negative.html"
saveWidget(wordcloud, output_file, selfcontained = TRUE)

# Capture a screenshot and save as PNG
webshot(output_file, "wordcloud_Negative.png")








# Assuming your original dataset is named Data_Thesis_for_forest
# Create a new dataset with only "Positive" observations
Sampled_Positive_Data <- Data_Thesis[Data_Thesis$Human_Learning_Sentiment == "Positive", ]

# View the entire new dataset
View(Sampled_Positive_Data)

text_corpus <- Corpus(VectorSource(Sampled_Positive_Data$clean_Announcement))
tdm <- TermDocumentMatrix(text_corpus)
tdm <- as.matrix(tdm)
tdm <- sort(rowSums(tdm), decreasing = TRUE)
tdm <- data.frame(word = names(tdm), freq = tdm)

# Set the maximum number of words to include in the word cloud
max_words <- 80

# Define a color palette with green gradient
green_palette <- colorRampPalette(c("#00FF00", "#006400"))

# Subset the data to include only the top words
top_words <- head(tdm, max_words)

# Create word cloud using wordcloud2
wordcloud <- wordcloud2(data = top_words, size = 0.8,  color = green_palette(nrow(top_words)))

# Save the word cloud as an HTML file
output_file <- "wordcloud_Positive.html"
saveWidget(wordcloud, output_file, selfcontained = TRUE)

# Capture a screenshot and save as PNG
webshot(output_file, "wordcloud_Positive.png")




# Assuming your original dataset is named Data_Thesis_for_forest
# Create a new dataset with only "Positive" observations
Sampled_Neutral_Data <- Data_Thesis[Data_Thesis$Human_Learning_Sentiment == "Neutral", ]

# View the entire new dataset
View(Sampled_Neutral_Data)

text_corpus <- Corpus(VectorSource(Sampled_Neutral_Data$clean_Announcement))
tdm <- TermDocumentMatrix(text_corpus)
tdm <- as.matrix(tdm)
tdm <- sort(rowSums(tdm), decreasing = TRUE)
tdm <- data.frame(word = names(tdm), freq = tdm)

# Set the maximum number of words to include in the word cloud
max_words <- 80

# Define a grayscale color palette
gray_palette <- colorRampPalette(c("#333333", "#CCCCCC"))

# Subset the data to include only the top words
top_words <- head(tdm, max_words)

# Create word cloud using wordcloud2
wordcloud <- wordcloud2(data = top_words, size = 0.8,  color = gray_palette(nrow(top_words)))

# Save the word cloud as an HTML file
output_file <- "wordcloud_Neutral.html"
saveWidget(wordcloud, output_file, selfcontained = TRUE)

# Capture a screenshot and save as PNG
webshot(output_file, "wordcloud_Neutral.png")

view(Data_Thesis)

################################################################################
#                                Word Importance                               #
################################################################################

# Plotting function from randomForest package
varImpPlot(rf_model)

library(ggplot2)

# Assuming rf_model is your random forest model
var_importance <- randomForest::importance(rf_model)

# Convert the importance matrix to a data frame
importance_df <- data.frame(Variable = rownames(var_importance), Importance = var_importance[, 1])

# Sort by importance
importance_df <- importance_df[order(importance_df$Importance, decreasing = TRUE), ]

# Create the variable importance scatter plot using ggplot2
p <- ggplot(importance_df, aes(x = Importance, y = reorder(Variable, Importance))) +
  geom_point(size = 2, shape = 1, stroke= 0.4, colour = rgb(8, 48, 107, maxColorValue = 255)) +  # Hollow dots with navy outline
  labs(title = "Word Importance Plot", x = "Importance (Mean Decrease in Gini Impurity)", y = "Words") +
  theme(axis.text.y = element_text(size = 5, color = "black"))

# Print the plot
print(p)

# Save the plot as a .png file
ggsave("word_importance_plot.png", plot = p, width = 9, height = 6, dpi = 2000)


################################################################################
#                             Top 15 Word Importance                           #
################################################################################

# Load necessary libraries
library(randomForest)
library(ggplot2)

# Assuming rf_model is your random forest model
var_importance <- randomForest::importance(rf_model)

# Convert the importance matrix to a data frame
importance_df <- data.frame(Variable = rownames(var_importance), Importance = var_importance[, 1])

# Sort by importance
importance_df <- importance_df[order(importance_df$Importance, decreasing = TRUE), ]

# Subset to include only the top 15 most important words
top_15_importance <- importance_df[1:15, ]

# Create the variable importance scatter plot using ggplot2
p <- ggplot(top_15_importance, aes(x = Importance, y = reorder(Variable, Importance))) +
  geom_point(size = 4, shape = 1, stroke = 0.5, colour = rgb(8, 48, 107, maxColorValue = 255)) +  # Larger solid dots with navy outline
  labs(title = "Word Importance Plot: Top 15 ", x = "Importance (Mean Decrease in Gini Impurity)", y = "Words") +
  theme(axis.text = element_text(size = 21, color = "black"),        # Larger axis text
        axis.title = element_text(size = 23, color = "black"),       # Larger title text
        plot.title = element_text(size = 25, hjust = 0.5, color = "black"))  # Larger plot title text

# Print the plot
print(p)

# Save the plot as a .png file
ggsave("word_importance_top_15_plot.png", plot = p, width = 12, height = 8)




################################################################################
#              Data Sentiment Analysis And Cleaning for ML Data                #
################################################################################


# Check column names
colnames(ML_Classified_Sentiment_Data)

#Clean Announcements
clean_Announcement <- function(text) {
  require(stringr)
  
  cleaned_text <- sapply(text, function(Announcement) {
    
    # Remove URLs
    Announcement <- gsub('https://|http://', '', Announcement)
    
    # Remove non-graph characters
    Announcement <- gsub('[^[:graph:]]', ' ', Announcement)
    
    # Remove punctuation
    Announcement <- gsub('[[:punct:]]', '', Announcement)
    
    # Remove control characters
    Announcement <- gsub('[[:cntrl:]]', '', Announcement)
    
    # Remove digits
    Announcement <- gsub('\\d+', '', Announcement)
    
    # Convert to lowercase
    Announcement <- tolower(Announcement)
    
    # Remove emotes
    Announcement <- str_replace_all(Announcement, "[\\x{1F600}-\\x{1F64F}\\x{1F300}-\\x{1F5FF}\\x{1F680}-\\x{1F6FF}\\x{1F700}-\\x{1F77F}\\x{1F780}-\\x{1F7FF}\\x{1F800}-\\x{1F8FF}\\x{1F900}-\\x{1F9FF}\\x{1FA00}-\\x{1FA6F}\\x{1FA70}-\\x{1FAFF}\\x{2600}-\\x{26FF}\\x{2700}-\\x{27BF}\\x{2300}-\\x{23FF}]", "")
    
    # Replace consecutive spaces with a single space
    Announcement <- str_replace_all(Announcement, "\\s+", " ")
    
    # Remove non-English words and characters
    Announcement <- str_replace_all(Announcement, "[^a-z\\s]", "")
    
    # Replace return spaces with a single space
    Announcement <- str_replace_all(Announcement, "\n", " ")
    
    # Remove words that begin with "nasdaq"
    Announcement <- str_replace_all(Announcement, "\\bnasdaq\\w*\\b", "")
    
    # Remove words that begin with "nyse"
    Announcement <- str_replace_all(Announcement, "\\bnyse\\w*\\b", "")
    
    # Remove month names
    Announcement <- str_replace_all(Announcement, "\\b(january|february|march|april|may|june|july|august|september|october|november|december)\\b", "")
    
    # Remove specific words like "the," "to," "about", and "and". ect. 
    Announcement <- str_replace_all(Announcement, "\\b(the|to|and|about|or|of|for|because|that|a|an|we|company|said|with|in|by|these|this|globe|newswire|inc|at|has|is|our|they|it|as|us|its|nasdaq|nys|jan|feb|mar|apr|may|jun|jul|aug|sep|oct|nov|dec|their|on|which|prnewswire|sept|whose|are|from|chief|is|was|were|study|fda|also|will|president|studies|therapy|who|whom|data|executive|administration|have|drug|disease|research|results|all|officer|today|compared|patients|announced|can|dose|food|based|observed|phd|placebo|than|therapies|through|two|one|three|patient|vice|need|cell|diseases|been|randomized|overall|week|after|biopharmaceuticals|combination|cancer|people|months|into|pharmaceuticals|product|generally|had|medical|medicine|medicines|scientific|there|type|weeks|where|years|biotechnology|following|oncology|therapeutics|use|annual|oral|products|biopharmaceutical|date|agent|alone|body|during|metastatic|placebocontrolled|arm|year|tumors|see|when|additionally|clinically|due|while.|cancers|those|seeN|next.|antibody)\\b", "")
    
    # Remove cities and countries (you can extend this list as needed)
    Announcement <- str_replace_all(Announcement, "\\b(new york|london|paris|tokyo|berlin|beijing|boston|netherlands|amsterdam|irvine|calif|california|mass|cambridge|massachusetts|austin|texas|dallas|reykjavik|iceland|waltham|stamford|conn|connecticut|new have|united states|canada|redwood city|salt lake city|plymouth|san diego|chatham|nj|pa|san diego|ca|charlottesville|va|md|phase|fla|vancouver|wash|bethesda|minneapolis|seattle|palo alto|clinical|trials|trial|thousand oaks|newtown|newark|san francisco|melbourne|australia|bothell|zug|switzerland|indianapolis|pittsburgh|st paul|minn|new haven|lexington|needham|watertown|tel aviv israel|weston|westlake village|rockville|berkeley heights|taicang|china|houston|tx|bedford|massbusiness|ma|accesswire|new england journal|england|dublin|san carlos|ind|alpharetta|ga|woodcliff lake|clinicalstage|headquarters|ceo|across|both|including|mg|vs|chicago|th|preclinical|clinicalstage)\\b", "")
    
    # Remove cities and countries (you can extend this list as needed)
    Announcement <- str_replace_all(Announcement, "\\b(indications|additionsuch|did|adx|iop|bimatoprost|approximately|days|function.|million|day|solid|stage|university|half|human|mean|lung|world|four|tumor|vitro|chemotherapy|therapeutic|david|step|seen|six|pharmaceutical|bring|administered|participants|protein|uisng|american|such|vivo|cells|treatment|biib|biogen|develeopment|als|alzheimers|glaucoma|amyloid|diroximel|spinraza|margenza|nsclc|difelikefalin|tcs|cda|trastuzumab|clene|eisai|program|fumarate|eye|development)\\b", "")
    
    return(Announcement)
  })
  
  return(cleaned_text)
}

# Assuming the Text column is named 'Announcement' in Training_Data_Day
ML_Classified_Sentiment_Data$clean_Announcement <- clean_Announcement(ML_Classified_Sentiment_Data$Announcement)

# View the updated data
View(ML_Classified_Sentiment_Data)

################################################################################
#                     Build Matrix for ML Data                                 #
################################################################################

# cast text data into corpus
corpus <- Corpus(VectorSource(ML_Classified_Sentiment_Data$clean_Announcement)) %>% 
  tm_map(stripWhitespace)

# Create document term feature matrix
dtm <- DocumentTermMatrix(corpus) %>% 
  removeSparseTerms(., 0.85) %>% #85% of words kept
  as.matrix() %>% 
  as.data.frame()

# Add column names to matrix
colnames(dtm) = make.names(colnames(dtm)) # Make Syntactically Valid Names

# Illustration: Show subset of matrix
# show first, last columns
dtm %>% 
  select(1:5, tail(names(.), 4)) %>% 
  slice(1:8) %>%
  kable("html") %>%
  kable_styling(font_size = 9)


#merge data
ML_Classified_Sentiment_Data <- bind_cols(ML_Classified_Sentiment_Data, dtm)

# Illustration: Show subset of data
# show first, last columns
ML_Classified_Sentiment_Data %>% 
  select(1:5, tail(names(.), 4)) %>% 
  slice(1:8) %>%
  kable("html") %>%
  kable_styling(font_size = 9)

#check for duplicates
duplicated_vars <- duplicated(names(ML_Classified_Sentiment_Data))

# Keep the first occurrence of each variable, drop duplicates
ML_Classified_Sentiment_Data <- ML_Classified_Sentiment_Data[, !duplicated_vars]

view(ML_Classified_Sentiment_Data)


################################################################################
#            make sure that they all have the same variables                   #
################################################################################

# Suppose you have your training data in a dataframe called Data_Thesis
# and your unclassified data in a dataframe called ML_Classified_Sentiment_Data

# Get the unique words from your training data
train_words <- colnames(Data_Thesis)[-17]  # Assuming the first column is not a word column

# Create a dataframe for your unclassified data with the same columns as your training data
unclassified_data_processed <- data.frame(matrix(0, ncol = length(train_words), nrow = nrow(ML_Classified_Sentiment_Data)))
colnames(unclassified_data_processed) <- train_words

# Now fill in the frequencies for the words that overlap
overlapping_words <- intersect(colnames(unclassified_data_processed), colnames(ML_Classified_Sentiment_Data))
unclassified_data_processed[, overlapping_words] <- ML_Classified_Sentiment_Data[, overlapping_words]

# Now you have to deal with the words in unclassified data that were not in the training data
# You can simply ignore them or set their frequencies to 0, as you suggested
new_words <- setdiff(colnames(unclassified_data_processed), colnames(Data_Thesis))

# Set the frequencies of new words to 0
unclassified_data_processed[, new_words] <- 0

# Now unclassified_data_processed contains word frequencies for both overlapping words and new words
# You can use this processed data in your random forest classifier

view(unclassified_data_processed)

ML_Sentiment_Classification <- predict(rf_model, unclassified_data_processed)
print(ML_Sentiment_Classification)

View(ML_Sentiment_Classification)

# Merge the prediction vector into the original dataset under the variable ML_classification
ML_Classified_Sentiment_Data$ML_classification <- ML_Sentiment_Classification

# View the updated dataset
View(ML_Classified_Sentiment_Data)

# List of variables to keep
variables_to_keep <- c("ML_classification", "Count", "Ticker", "Name","Drug", "Field", "Indication", "Stage", "Date", "Announcement", "T","clean_Announcement")  # Adjust this list according to your variables of interest

# Subset the dataset to keep only the specified variables
ML_Classified_Sentiment_Data <- ML_Classified_Sentiment_Data[, c("ML_classification", variables_to_keep)]

# View the updated dataset
View(ML_Classified_Sentiment_Data)

# Remove one of the duplicated variables (ML_classification) from the dataset
ML_Classified_Sentiment_Data <- ML_Classified_Sentiment_Data[, !duplicated(colnames(ML_Classified_Sentiment_Data))]

# View the updated dataset
View(ML_Classified_Sentiment_Data)

#Repeat for data_thesis 
view(Data_Thesis)

# List of variables to keep
variables_to_keep <- c("Human_Learning_Sentiment", "Count", "Ticker", "Name","Drug", "Field", "Indication", "Stage", "Date", "Announcement", "T","clean_Announcement")  # Adjust this list according to your variables of interest

# Subset the dataset to keep only the specified variables
Data_Thesis <- Data_Thesis[, c("Human_Learning_Sentiment", variables_to_keep)]

# View the updated dataset
View(Data_Thesis)

# Remove one of the duplicated variables (Human_Learning_Sentiment) from the dataset
Data_Thesis <- Data_Thesis[, !duplicated(colnames(Data_Thesis))]

# View the updated dataset
View(Data_Thesis)


################################################################################
#                              Combine data sets                               #
################################################################################

# Create "Sentiment" variable in Data_Thesis
Data_Thesis$Sentiment <- Data_Thesis$Human_Learning_Sentiment

# Create "Sentiment" variable in ML_Classified_Sentiment_Data
ML_Classified_Sentiment_Data$Sentiment <- ML_Classified_Sentiment_Data$ML_classification

View(ML_Classified_Sentiment_Data)
View(Data_Thesis)

# Show the names of variables in Data_Thesis
print("Variable names in Data_Thesis:")
print(names(Data_Thesis))

# Show the names of variables in ML_Classified_Sentiment_Data
print("Variable names in ML_Classified_Sentiment_Data:")
print(names(ML_Classified_Sentiment_Data))


# Drop several variables from Data_Thesis
Data_Thesis <- Data_Thesis[, !names(Data_Thesis) %in% c("Count", "Human_Learning_Sentiment")]

# Drop several variables from ML_Classified_Sentiment_Data
ML_Classified_Sentiment_Data <- ML_Classified_Sentiment_Data[, !names(ML_Classified_Sentiment_Data) %in% c("Count", "ML_classification")]

# Combine the datasets by stacking them vertically
final_data <- rbind(Data_Thesis, ML_Classified_Sentiment_Data)

# View the final dataset
View(final_data)


################################################################################
#                             Announcement trackers                            #
################################################################################


# Graph Tweets per day
plt <- final_data %>% 
  count(Date) %>% 
  ggplot(mapping = aes(x = Date, y = n)) +
  theme_light() +
  geom_bar(stat = "identity", fill = rgb(8, 48, 107, maxColorValue = 255)) +  # Bar plot
  xlab(label = 'Date') +
  ylab(label = 'Number of Announcements') +
  ggtitle(label = 'Number of Announcements per Day')

# Save the plot as a PNG file
ggsave("announcements_per_day_plot.png", plt, width = 6, height = 4)

# Display the plot using ggplotly
plt_ggplotly <- ggplotly(plt)
plt_ggplotly

################################################################################
#                               Announcements by firm                          #
################################################################################

# Assuming final_data is your dataset

# Count occurrences of each name
name_counts <- table(final_data$Name)

# Sort name counts in descending order
sorted_name_counts <- sort(name_counts, decreasing = TRUE)

# Convert sorted name counts to a data frame
name_counts_df <- data.frame(Name = names(sorted_name_counts), Count = as.numeric(sorted_name_counts))
print(name_counts_df)

# Create the plot
plt1 <- ggplot(name_counts_df, aes(x = reorder(Name, Count), y = Count)) +
  geom_bar(stat = "identity", fill = rgb(8, 48, 107, maxColorValue = 255), color = "white", width = 1) +
  labs(title = "Announcements by Firm", x = "Firm", y = "Number of Announcements") +
  theme(axis.text.x = element_text(angle = 68, vjust = 0.8, hjust = 0.8, size = 5, color = "black"),  # Rotate x-axis labels and decrease text size
        axis.text.y = element_text(size = 8, color = "black"),  # Decrease y-axis text size
        axis.title.y = element_text(size = 8, color = "black"),  # Decrease y-axis title size
        axis.ticks.y = element_line(size = 0.5, color = "black"),  # Adjust y-axis ticks size
        panel.grid.major.y = element_line(color = "white", size = 0.25),  # Adjust major grid lines
        panel.grid.minor.y = element_blank())  # Remove minor grid lines

# Rescale y-axis by increments of 1
plt1 <- plt1 + scale_y_continuous(breaks = seq(0, max(name_counts_df$Count), by = 1))

# Save the plot as a PNG file
ggsave("Announcements_by_firm_plot.png", plt1, width = 7.5, height = 5, dpi = 2800)

# Display the plot
print(plt1)

################################################################################
#          Counting and comparing Observations by Sentiment of Training        #
################################################################################

library(dplyr)
library(ggplot2)

# Assuming 'training_set' is your training dataset and 'Human_Learning_Sentiment' is the variable of interest

# Count occurrences of each human learning sentiment classification in the training set
sentiment_counts <- training_set %>% 
  group_by(Human_Learning_Sentiment) %>% 
  summarise(Count = n())

# Create a data frame with Human_Learning_Sentiment and Count columns
output <- data.frame(Human_Learning_Sentiment = sentiment_counts$Human_Learning_Sentiment, Count = sentiment_counts$Count)

# Reorder Human_Learning_Sentiment factor levels to ensure consistent ordering in the plot
output$Human_Learning_Sentiment <- factor(output$Human_Learning_Sentiment, levels = c("Negative", "Neutral", "Positive"))

# Create the bar plot
p <- ggplot(output, aes(x = Human_Learning_Sentiment, y = Count, fill = Human_Learning_Sentiment)) +
  geom_bar(stat = "identity") +
  ggtitle("Human Learning Sentiment Classification in Training Data") +  # Adjusted title
  xlab("Sentiment") +
  ylab("Count") +
  scale_fill_manual(values = c("Negative" = "#FF0000", "Neutral" = "#cccccc", "Positive" = "#00FF00")) +
  labs(fill = "Human Classified Sentiment") +  # Change the legend label
  theme_minimal()

# Save the plot as a .png file
ggsave("human_learning_sentiment_training_plot.png", plot = p, width = 3.5, height = 2.5)

# Print the plot
print(p)

# Count occurrences of each human learning sentiment classification in the training set
sentiment_counts <- training_set %>% 
  group_by(Human_Learning_Sentiment) %>% 
  summarise(Count = n())

# Print the counts
print("Number of Positive, Negative, and Neutral Observations in the Training Set:")
print(sentiment_counts)




################################################################################
#             Counting and comparing Observations by Sentiment                 #
################################################################################

library(dplyr)
library(ggplot2)

# Assuming 'final_data' is your dataset and 'classification' is the variable of interest

# Count occurrences of each sentiment classification
sentiment_counts <- final_data %>% 
  group_by(Sentiment) %>% 
  summarise(Count = n())

# Create a data frame with Sentiment and Count columns
output <- data.frame(Sentiment = sentiment_counts$Sentiment, Count = sentiment_counts$Count)

# Reorder Sentiment factor levels to ensure consistent ordering in the plot
output$Sentiment <- factor(output$Sentiment, levels = c("Negative", "Neutral", "Positive"))

# Create the bar plot
p <- ggplot(output, aes(x = Sentiment, y = Count, fill = Sentiment)) +
  geom_bar(stat = "identity") +
  ggtitle("Announcements by Sentiment Classification") +
  xlab("Sentiment") +
  ylab("Count") +
  scale_fill_manual(values = c("Negative" = "#FF0000", "Neutral" = "#cccccc", "Positive" = "#00FF00")) +
  theme_minimal()

# Save the plot as a .png file
ggsave("sentiment_classification_plot.png", plot = p, width = 3.5, height = 2.5)

# Print the plot
print(p)

# Count occurrences of each sentiment classification
sentiment_counts <- final_data %>% 
  group_by(Sentiment) %>% 
  summarise(Count = n())

# Print the counts
print("Number of Positive, Negative, and Neutral Observations:")
print(sentiment_counts)

################################################################################
#                            Pie Chart for Stage                          #
################################################################################


library(ggplot2)
library(RColorBrewer)

# Count the frequency of each value in the variable
variable_counts <- table(final_data$Stage)

# Create a data frame for plotting
pie_data <- data.frame(
  Stage = factor(names(variable_counts), levels = names(variable_counts)),
  Count = as.numeric(variable_counts)
)

# Calculate percentages
pie_data$Percentage <- round(pie_data$Count / sum(pie_data$Count) * 100, 1)

# Rename stages
pie_data$Stage <- factor(pie_data$Stage, labels = c("Preclinical", "Phase 1", "Phase 2", "Phase 3", "Phase 4"))

# Create the pie chart
p <- ggplot(pie_data, aes(x = "", y = Count, fill = Stage)) +
  geom_bar(stat = "identity", color = "white") +
  coord_polar("y", start = 0) +
  labs(title = "Announcements at Different Pipeline Stages", fill = "Stage") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),  # Remove x-axis labels
        axis.title.x = element_blank(),  # Remove x-axis title
        axis.text.y = element_blank(),  # Remove y-axis labels
        axis.title.y = element_blank(),  # Remove y-axis title
        legend.position = "right") +
  scale_fill_brewer(palette = "Blues", labels = c("Preclinical", "Phase 1", "Phase 2", "Phase 3", "Phase 4"))

# Add percentages to the chart
p <- p + geom_text(aes(label = ifelse(Percentage == 0, "0.0%", paste0(Percentage, "%"))), position = position_stack(vjust = 0.5))

# Save the plot as a .png file
ggsave("pie_chart_stage.png", plot = p, width = 8, height = 6, units = "in", dpi = 300)

# Print the plot
print(p)


################################################################################
#                             Pie Chart for Field                             #
################################################################################

library(ggplot2)
library(dplyr)

# Assuming 'final_data' is your dataset and 'Field' is the variable of interest

# Count the frequency of each value in the variable
variable_counts <- table(final_data$Field)

# Calculate percentages
percentages <- prop.table(variable_counts) * 100

# Identify values with less than 4% frequency
threshold <- 4
other_values <- names(percentages[percentages < threshold])

# Combine counts for values less than 3.5%
other_count <- sum(variable_counts[other_values])

# Check if the combined count is less than the threshold count
if (other_count < threshold) {
  # Include the combined count in "Other" category
  other_values <- c(other_values, "Other")
}

# Update counts for "Other" category
variable_counts["Other"] <- other_count

# Remove counts for values less than 4%
variable_counts <- variable_counts[!(names(variable_counts) %in% other_values)]

# Create a data frame for plotting
pie_data <- data.frame(
  Field = factor(names(variable_counts)),
  Count = as.numeric(variable_counts)
)

# Calculate percentages for the modified data
total_count <- sum(pie_data$Count)
pie_data$Percentage <- round(pie_data$Count / total_count * 100, 1)

# Reorder pie_data to build clockwise from least to greatest
pie_data <- pie_data %>%
  arrange(Count) %>%
  mutate(Field = factor(Field, levels = unique(Field)))

# Create the pie chart
p <- ggplot(pie_data, aes(x = "", y = Count, fill = Field)) +
  geom_bar(stat = "identity", color = "white") +
  coord_polar("y", start = 0) +
  labs(title = "Distribution of Fields", fill = "Field") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),  # Remove x-axis labels
        axis.title.x = element_blank(),  # Remove x-axis title
        axis.text.y = element_blank(),  # Remove y-axis labels
        axis.title.y = element_blank(),  # Remove y-axis title
        legend.position = "right") +
  scale_fill_brewer(palette = "Blues")

# Add percentages to the chart
p <- p + geom_text(aes(label = ifelse(Percentage == 0, "0.0%", paste0(Percentage, "%"))), position = position_stack(vjust = 0.5))

# Save the plot as a .png file
ggsave("pie_chart.png", plot = p, width = 8, height = 6, units = "in", dpi = 300)

# Print the plot
print(p)

################################################################################
#                          Pull in Stock price for data                        #
################################################################################

# Load necessary packages
install.packages("tidyquant")
install.packages("corrplot")
library(tidyverse)
library(tidyquant)
library(ggplot2)
library(corrplot)

# Define GetMySymbols function with modification
GetMySymbols <- function(x) {
  tryCatch({
    data <- getSymbols(x, src = "yahoo", from = "2014-01-01", to = "2024-03-03", auto.assign = FALSE)
    if (inherits(data, "xts")) {
      # Fill missing values with NA
      data <- na.fill(data, fill = NA)
    }
    data
  }, error = function(e) {
    cat("Error importing ticker:", x, "\n")
    cat("Error message:", conditionMessage(e), "\n")
    NULL
  })
}

# Define tickers
tickers <- c("EGRX", "REGN", "HRTX", "ANIP", "CRSP", "MRK", "TARS", "AMGN", "ABBV", 
             "AZN", "BIIB", "LLY", "VTRS", "AGIO", "MRNA", "DARE", "BHC", "OCUL", "MGNX", 
             "AQST", "VRTX", "GILD", "AERI", "ALVO", "ALVR", "SNY", "ARGX", "IVVD", "CARA", 
             "NVS", "SGEN", "TVTX", "HRMY", "SAVA", "IONS", "BHVN", "FGEN", "MAIA", 
             "CNSP",
             "ALLK",
             "CLNN",
             "OKYO",
             "INCY",
             "BMEA",
             "ALDX",
             "SNDX",
             "CNTB",
             "ICVX",
             "CRIS",
             "CADL",
             "BCTX",
             "NBIX",
             "ATRA",
             "LUMO",
             "VTYX",
             "TNXP",
             "DRMA",
             "ATAI",
             "CYCC",
             "ALNY",
             "CGEM",
             "HOOK",
             "NTLA",
             "CCCC",
             "ABOS",
             "VKTX",
             "RLAY",
             "PFE",
             "RXRX",
             "RGNX", 
             "JANX",
             "GNLX",
             "VTGN",
             "ENTA",
             "MRSN",
             "FHTX",
             "ELEV",
             "RAIN",
             "IXHL",
             "FULC",
             "NKTR",
             "ONTX", 
             "CMND",
             "TNGX",
             "ATHA",
             "ABSI",
             "ZVSA",
             "RPTX",
             "OCEA",
             "GANX",
             "PBLA",
             "RDHL",
             "PLUR",
             "PALI",
             "SCYX",
             "DTIL",
             "ARTL",
             "NRBO",
             "RNAZ",
             "SANA",
             "VINC",
             "ENVB",
             "SRRK",
             "JNJ",
             "RCUS",
             "BIVI",
             "CPRX",
             "CTXR", "SWTX", "ASND")

# Retrieve data for tickers
symbol_list <- map(tickers, GetMySymbols) %>% reduce(merge.xts) 

Closing <- symbol_list[, !grepl("\\.Open$|\\.High$|\\.Low$|\\.Adjusted$|\\.Volume$", colnames(symbol_list))]

# Assuming Closing is your dataframe

Volume <- symbol_list[, !grepl("\\.Open$|\\.High$|\\.Low$|\\.Adjusted$|\\.Close$", colnames(symbol_list))]
# Continue with your analysis...

# Remove spaces from column names
colnames(Closing) <- make.names(colnames(Closing))

# Remove spaces from row names
rownames(Closing) <- make.names(rownames(Closing))



view(Closing)

view(Volume)

# View the modified final_data dataframe
view(final_data)


##################################################
# find individual data for tickers AERI and SGEN and HOOK #
# also pull in Biotech index                     #
##################################################


################################################################################
#                   Make new variable for stock Price T                        #
################################################################################
# Create 'Day_T' variable if not already present
final_data$Day_T <- weekdays(final_data$T)

# Create 'T_Adj' variable based on conditions
final_data$T_Adj <- final_data$T
final_data$T_Adj[final_data$Day_T == "Saturday"] <- final_data$T[final_data$Day_T == "Saturday"] + days(2)
final_data$T_Adj[final_data$Day_T == "Sunday"] <- final_data$T[final_data$Day_T == "Sunday"] + days(1)

# Convert 'T' to Date format
final_data$T_Adj <- as.Date(final_data$T_Adj)

# Create 'Day_T' variable if not already present
final_data$Day_T_Adj <- weekdays(final_data$T_Adj)

library(dplyr)

# Assuming your Closing data frame is available and properly formatted

# Define a function to get the closing price for a given ticker and date
get_closing_price <- function(ticker, date) {
  column_name <- paste0(ticker, ".Close")
  if (column_name %in% colnames(Closing)) {
    closing_price <- Closing[date, column_name]
    return(closing_price)
  } else {
    return(NA_real_)
  }
}

# Create Closing_Price_T variable in final_data
final_data <- final_data %>%
  mutate(Closing_Price_T_Adj = mapply(get_closing_price, Ticker, T_Adj))

numeric_columns <- c("Closing_Price_T_Adj")  # List of column names that should be numeric
final_data[numeric_columns] <- lapply(final_data[numeric_columns], as.numeric)

view(final_data)

################################################################################
#              Make new variable for T  -7 -6 -5 -4 -3 -2 -1 +1 +2 +3          #
################################################################################

# Load the lubridate package
library(lubridate)

# Add a day to the values of the variable 'T' and store it in a new variable 'T_Plus_1'
final_data$T_Minus_1 <- final_data$T - days(1)

# Create 'Day_T' variable if not already present
final_data$Day_T_Minus_1 <- weekdays(final_data$T_Minus_1)

# Create 'T_Adj' variable based on conditions
final_data$T_Adj_Minus_1 <- final_data$T_Minus_1
final_data$T_Adj_Minus_1[final_data$Day_T_Minus_1 == "Saturday"] <- final_data$T_Minus_1[final_data$Day_T_Minus_1 == "Saturday"] - days(1)
final_data$T_Adj_Minus_1[final_data$Day_T_Minus_1 == "Sunday"] <- final_data$T_Minus_1[final_data$Day_T_Minus_1 == "Sunday"] - days(2)

# Convert 'T' to Date format
final_data$T_Adj_Minus_1 <- as.Date(final_data$T_Adj_Minus_1)

# Create 'Day_T' variable if not already present
final_data$Day_T_Adj_Minus_1 <- weekdays(final_data$T_Adj_Minus_1)

###


# Add a day to the values of the variable 'T' and store it in a new variable 'T_Plus_1'
final_data$T_Minus_2 <- final_data$T_Adj_Minus_1 - days(1)

# Create 'Day_T' variable if not already present
final_data$Day_T_Minus_2 <- weekdays(final_data$T_Minus_2)

# Create 'T_Adj' variable based on conditions
final_data$T_Adj_Minus_2 <- final_data$T_Minus_2
final_data$T_Adj_Minus_2[final_data$Day_T_Minus_2 == "Saturday"] <- final_data$T_Minus_2[final_data$Day_T_Minus_2 == "Saturday"] - days(1)
final_data$T_Adj_Minus_2[final_data$Day_T_Minus_2 == "Sunday"] <- final_data$T_Minus_2[final_data$Day_T_Minus_2 == "Sunday"] - days(2)

# Convert 'T' to Date format
final_data$T_Adj_Minus_2 <- as.Date(final_data$T_Adj_Minus_2)

# Create 'Day_T' variable if not already present
final_data$Day_T_Adj_Minus_2 <- weekdays(final_data$T_Adj_Minus_2)

###


# Add a day to the values of the variable 'T' and store it in a new variable 'T_Plus_1'
final_data$T_Minus_3 <- final_data$T_Adj_Minus_2 - days(1)

# Create 'Day_T' variable if not already present
final_data$Day_T_Minus_3 <- weekdays(final_data$T_Minus_3)

# Create 'T_Adj' variable based on conditions
final_data$T_Adj_Minus_3 <- final_data$T_Minus_3
final_data$T_Adj_Minus_3[final_data$Day_T_Minus_3 == "Saturday"] <- final_data$T_Minus_3[final_data$Day_T_Minus_3 == "Saturday"] - days(1)
final_data$T_Adj_Minus_3[final_data$Day_T_Minus_3 == "Sunday"] <- final_data$T_Minus_3[final_data$Day_T_Minus_3 == "Sunday"] - days(2)

# Convert 'T' to Date format
final_data$T_Adj_Minus_3 <- as.Date(final_data$T_Adj_Minus_3)

# Create 'Day_T' variable if not already present
final_data$Day_T_Adj_Minus_3 <- weekdays(final_data$T_Adj_Minus_3)

###


# Add a day to the values of the variable 'T' and store it in a new variable 'T_Plus_1'
final_data$T_Minus_4 <- final_data$T_Adj_Minus_3 - days(1)

# Create 'Day_T' variable if not already present
final_data$Day_T_Minus_4 <- weekdays(final_data$T_Minus_4)

# Create 'T_Adj' variable based on conditions
final_data$T_Adj_Minus_4 <- final_data$T_Minus_4
final_data$T_Adj_Minus_4[final_data$Day_T_Minus_4 == "Saturday"] <- final_data$T_Minus_4[final_data$Day_T_Minus_4 == "Saturday"] - days(1)
final_data$T_Adj_Minus_4[final_data$Day_T_Minus_4 == "Sunday"] <- final_data$T_Minus_4[final_data$Day_T_Minus_4 == "Sunday"] - days(2)

# Convert 'T' to Date format
final_data$T_Adj_Minus_4 <- as.Date(final_data$T_Adj_Minus_4)


# Create 'Day_T' variable if not already present
final_data$Day_T_Adj_Minus_4 <- weekdays(final_data$T_Adj_Minus_4)


###


# Add a day to the values of the variable 'T' and store it in a new variable 'T_Plus_1'
final_data$T_Minus_5 <- final_data$T_Adj_Minus_4 - days(1)

# Create 'Day_T' variable if not already present
final_data$Day_T_Minus_5 <- weekdays(final_data$T_Minus_5)

# Create 'T_Adj' variable based on conditions
final_data$T_Adj_Minus_5 <- final_data$T_Minus_5
final_data$T_Adj_Minus_5[final_data$Day_T_Minus_5 == "Saturday"] <- final_data$T_Minus_5[final_data$Day_T_Minus_5 == "Saturday"] - days(1)
final_data$T_Adj_Minus_5[final_data$Day_T_Minus_5 == "Sunday"] <- final_data$T_Minus_5[final_data$Day_T_Minus_5 == "Sunday"] - days(2)

# Convert 'T' to Date format
final_data$T_Adj_Minus_5 <- as.Date(final_data$T_Adj_Minus_5)


# Create 'Day_T' variable if not already present
final_data$Day_T_Adj_Minus_5 <- weekdays(final_data$T_Adj_Minus_5)


###


# Add a day to the values of the variable 'T' and store it in a new variable 'T_Plus_1'
final_data$T_Minus_6 <- final_data$T_Adj_Minus_5 - days(1)

# Create 'Day_T' variable if not already present
final_data$Day_T_Minus_6 <- weekdays(final_data$T_Minus_6)

# Create 'T_Adj' variable based on conditions
final_data$T_Adj_Minus_6 <- final_data$T_Minus_6
final_data$T_Adj_Minus_6[final_data$Day_T_Minus_6 == "Saturday"] <- final_data$T_Minus_6[final_data$Day_T_Minus_6 == "Saturday"] - days(1)
final_data$T_Adj_Minus_6[final_data$Day_T_Minus_6 == "Sunday"] <- final_data$T_Minus_6[final_data$Day_T_Minus_6 == "Sunday"] - days(2)

# Convert 'T' to Date format
final_data$T_Adj_Minus_6 <- as.Date(final_data$T_Adj_Minus_6)


# Create 'Day_T' variable if not already present
final_data$Day_T_Adj_Minus_6 <- weekdays(final_data$T_Adj_Minus_6)


###


# Add a day to the values of the variable 'T' and store it in a new variable 'T_Plus_1'
final_data$T_Minus_7 <- final_data$T_Adj_Minus_6 - days(1)

# Create 'Day_T' variable if not already present
final_data$Day_T_Minus_7 <- weekdays(final_data$T_Minus_7)

# Create 'T_Adj' variable based on conditions
final_data$T_Adj_Minus_7 <- final_data$T_Minus_7
final_data$T_Adj_Minus_7[final_data$Day_T_Minus_7 == "Saturday"] <- final_data$T_Minus_7[final_data$Day_T_Minus_7 == "Saturday"] - days(1)
final_data$T_Adj_Minus_7[final_data$Day_T_Minus_7 == "Sunday"] <- final_data$T_Minus_7[final_data$Day_T_Minus_7 == "Sunday"] - days(2)

# Convert 'T' to Date format
final_data$T_Adj_Minus_7 <- as.Date(final_data$T_Adj_Minus_7)


# Create 'Day_T' variable if not already present
final_data$Day_T_Adj_Minus_7 <- weekdays(final_data$T_Adj_Minus_7)


###


# Load the lubridate package
library(lubridate)

# Add a day to the values of the variable 'T' and store it in a new variable 'T_Plus_1'
final_data$T_Plus_1 <- final_data$T + days(1)

# Create 'Day_T' variable if not already present
final_data$Day_T_Plus_1 <- weekdays(final_data$T_Plus_1)

# Create 'T_Adj' variable based on conditions
final_data$T_Adj_Plus_1 <- final_data$T_Plus_1
final_data$T_Adj_Plus_1[final_data$Day_T_Plus_1 == "Saturday"] <- final_data$T_Plus_1[final_data$Day_T_Plus_1 == "Saturday"] + days(2)
final_data$T_Adj_Plus_1[final_data$Day_T_Plus_1 == "Sunday"] <- final_data$T_Plus_1[final_data$Day_T_Plus_1 == "Sunday"] + days(1)

# Convert 'T' to Date format
final_data$T_Adj_Plus_1 <- as.Date(final_data$T_Adj_Plus_1)


# Create 'Day_T' variable if not already present
final_data$Day_T_Adj_Plus_1 <- weekdays(final_data$T_Adj_Plus_1)


###


# Add a day to the values of the variable 'T' and store it in a new variable 'T_Plus_1'
final_data$T_Plus_2 <- final_data$T_Adj_Plus_1 + days(1)

# Create 'Day_T' variable if not already present
final_data$Day_T_Plus_2 <- weekdays(final_data$T_Plus_2)

# Create 'T_Adj' variable based on conditions
final_data$T_Adj_Plus_2 <- final_data$T_Plus_2
final_data$T_Adj_Plus_2[final_data$Day_T_Plus_2 == "Saturday"] <- final_data$T_Plus_2[final_data$Day_T_Plus_2 == "Saturday"] + days(2)
final_data$T_Adj_Plus_2[final_data$Day_T_Plus_2 == "Sunday"] <- final_data$T_Plus_2[final_data$Day_T_Plus_2 == "Sunday"] + days(1)

# Convert 'T' to Date format
final_data$T_Adj_Plus_2 <- as.Date(final_data$T_Adj_Plus_2)


# Create 'Day_T' variable if not already present
final_data$Day_T_Adj_Plus_2 <- weekdays(final_data$T_Adj_Plus_2)

###


# Add a day to the values of the variable 'T' and store it in a new variable 'T_Plus_1'
final_data$T_Plus_3 <- final_data$T_Adj_Plus_2 + days(1)

# Create 'Day_T' variable if not already present
final_data$Day_T_Plus_3 <- weekdays(final_data$T_Plus_3)

# Create 'T_Adj' variable based on conditions
final_data$T_Adj_Plus_3 <- final_data$T_Plus_3
final_data$T_Adj_Plus_3[final_data$Day_T_Plus_3 == "Saturday"] <- final_data$T_Plus_3[final_data$Day_T_Plus_3 == "Saturday"] + days(2)
final_data$T_Adj_Plus_3[final_data$Day_T_Plus_3 == "Sunday"] <- final_data$T_Plus_3[final_data$Day_T_Plus_3 == "Sunday"] + days(1)

# Convert 'T' to Date format
final_data$T_Adj_Plus_3 <- as.Date(final_data$T_Adj_Plus_3)


# Create 'Day_T' variable if not already present
final_data$Day_T_Adj_Plus_3 <- weekdays(final_data$T_Adj_Plus_3)

###

view(final_data)

#T-7

# Assuming your Closing data frame is available and properly formatted

# Define a function to get the closing price for a given ticker and date
get_closing_price <- function(ticker, date) {
  column_name <- paste0(ticker, ".Close")
  if (column_name %in% colnames(Closing)) {
    closing_price <- Closing[date, column_name]
    return(closing_price)
  } else {
    return(NA_real_)
  }
}

# Create Closing_Price_T_Minus_7 variable in final_data
final_data <- final_data %>%
  mutate(Closing_Price_T_Adj_Minus_7 = mapply(get_closing_price, Ticker, T_Adj_Minus_7))

view(final_data)

#T-6

# Assuming your Closing data frame is available and properly formatted

# Define a function to get the closing price for a given ticker and date
get_closing_price <- function(ticker, date) {
  column_name <- paste0(ticker, ".Close")
  if (column_name %in% colnames(Closing)) {
    closing_price <- Closing[date, column_name]
    return(closing_price)
  } else {
    return(NA_real_)
  }
}

# Create Closing_Price_T_Minus_6 variable in final_data
final_data <- final_data %>%
  mutate(Closing_Price_T_Adj_Minus_6 = mapply(get_closing_price, Ticker, T_Adj_Minus_6))

#T-5

# Assuming your Closing data frame is available and properly formatted

# Define a function to get the closing price for a given ticker and date
get_closing_price <- function(ticker, date) {
  column_name <- paste0(ticker, ".Close")
  if (column_name %in% colnames(Closing)) {
    closing_price <- Closing[date, column_name]
    return(closing_price)
  } else {
    return(NA_real_)
  }
}

# Create Closing_Price_T_Minus_5 variable in final_data
final_data <- final_data %>%
  mutate(Closing_Price_T_Adj_Minus_5 = mapply(get_closing_price, Ticker, T_Adj_Minus_5))

#T-4

# Assuming your Closing data frame is available and properly formatted

# Define a function to get the closing price for a given ticker and date
get_closing_price <- function(ticker, date) {
  column_name <- paste0(ticker, ".Close")
  if (column_name %in% colnames(Closing)) {
    closing_price <- Closing[date, column_name]
    return(closing_price)
  } else {
    return(NA_real_)
  }
}

# Create Closing_Price_T_Minus_4 variable in final_data
final_data <- final_data %>%
  mutate(Closing_Price_T_Adj_Minus_4 = mapply(get_closing_price, Ticker, T_Adj_Minus_4))

#T-3

# Assuming your Closing data frame is available and properly formatted

# Define a function to get the closing price for a given ticker and date
get_closing_price <- function(ticker, date) {
  column_name <- paste0(ticker, ".Close")
  if (column_name %in% colnames(Closing)) {
    closing_price <- Closing[date, column_name]
    return(closing_price)
  } else {
    return(NA_real_)
  }
}

# Create Closing_Price_T_Minus_3 variable in final_data
final_data <- final_data %>%
  mutate(Closing_Price_T_Adj_Minus_3 = mapply(get_closing_price, Ticker, T_Adj_Minus_3))

#T-2

# Assuming your Closing data frame is available and properly formatted

# Define a function to get the closing price for a given ticker and date
get_closing_price <- function(ticker, date) {
  column_name <- paste0(ticker, ".Close")
  if (column_name %in% colnames(Closing)) {
    closing_price <- Closing[date, column_name]
    return(closing_price)
  } else {
    return(NA_real_)
  }
}

# Create Closing_Price_T_Minus_2 variable in final_data
final_data <- final_data %>%
  mutate(Closing_Price_T_Adj_Minus_2 = mapply(get_closing_price, Ticker, T_Adj_Minus_2))

#T-1

# Assuming your Closing data frame is available and properly formatted

# Define a function to get the closing price for a given ticker and date
get_closing_price <- function(ticker, date) {
  column_name <- paste0(ticker, ".Close")
  if (column_name %in% colnames(Closing)) {
    closing_price <- Closing[date, column_name]
    return(closing_price)
  } else {
    return(NA_real_)
  }
}

# Create Closing_Price_T_Minus_1 variable in final_data
final_data <- final_data %>%
  mutate(Closing_Price_T_Adj_Minus_1 = mapply(get_closing_price, Ticker, T_Adj_Minus_1))

#T+1

# Assuming your Closing data frame is available and properly formatted

# Define a function to get the closing price for a given ticker and date
get_closing_price <- function(ticker, date) {
  column_name <- paste0(ticker, ".Close")
  if (column_name %in% colnames(Closing)) {
    closing_price <- Closing[date, column_name]
    return(closing_price)
  } else {
    return(NA_real_)
  }
}

# Create Closing_Price_T_Plus_1 variable in final_data
final_data <- final_data %>%
  mutate(Closing_Price_T_Adj_Plus_1 = mapply(get_closing_price, Ticker, T_Adj_Plus_1))

#T+2

# Assuming your Closing data frame is available and properly formatted

# Define a function to get the closing price for a given ticker and date
get_closing_price <- function(ticker, date) {
  column_name <- paste0(ticker, ".Close")
  if (column_name %in% colnames(Closing)) {
    closing_price <- Closing[date, column_name]
    return(closing_price)
  } else {
    return(NA_real_)
  }
}

# Create Closing_Price_T_Plus_2 variable in final_data
final_data <- final_data %>%
  mutate(Closing_Price_T_Adj_Plus_2 = mapply(get_closing_price, Ticker, T_Adj_Plus_2))

#T+3

# Assuming your Closing data frame is available and properly formatted

# Define a function to get the closing price for a given ticker and date
get_closing_price <- function(ticker, date) {
  column_name <- paste0(ticker, ".Close")
  if (column_name %in% colnames(Closing)) {
    closing_price <- Closing[date, column_name]
    return(closing_price)
  } else {
    return(NA_real_)
  }
}

# Create Closing_Price_T_Plus_3 variable in final_data
final_data <- final_data %>%
  mutate(Closing_Price_T_Adj_Plus_3 = mapply(get_closing_price, Ticker, T_Adj_Plus_3))


numeric_columns <- c("Closing_Price_T_Adj", "Closing_Price_T_Adj_Plus_3", "Closing_Price_T_Adj_Plus_2", "Closing_Price_T_Adj_Plus_1", "Closing_Price_T_Adj_Minus_1", "Closing_Price_T_Adj_Minus_2", "Closing_Price_T_Adj_Minus_3", "Closing_Price_T_Adj_Minus_4", "Closing_Price_T_Adj_Minus_5", "Closing_Price_T_Adj_Minus_6", "Closing_Price_T_Adj_Minus_7")  # List of column names that should be numeric
final_data[numeric_columns] <- lapply(final_data[numeric_columns], as.numeric)

view(final_data)

# Assuming final_data is your dataframe

# Assuming 'final_data' is your dataframe
final_data$Count <- seq.int(nrow(final_data))

view(final_data)

################################################################################
#                       Calculate stocks Returns                    #
################################################################################

# Calculate percentage change in closing prices and assign it to R_0
final_data$R_0 <- ((final_data$Closing_Price_T_Adj - final_data$Closing_Price_T_Adj_Minus_1) / final_data$Closing_Price_T_Adj_Minus_1) * 100

# Calculate percentage change in closing prices and assign it to R_N1
final_data$R_N1 <- ((final_data$Closing_Price_T_Adj_Minus_1 - final_data$Closing_Price_T_Adj_Minus_2) / final_data$Closing_Price_T_Adj_Minus_2) * 100

# Calculate percentage change in closing prices and assign it to R_N2
final_data$R_N2 <- ((final_data$Closing_Price_T_Adj_Minus_2 - final_data$Closing_Price_T_Adj_Minus_3) / final_data$Closing_Price_T_Adj_Minus_3) * 100

# Calculate percentage change in closing prices and assign it to R_N3
final_data$R_N3 <- ((final_data$Closing_Price_T_Adj_Minus_3 - final_data$Closing_Price_T_Adj_Minus_4) / final_data$Closing_Price_T_Adj_Minus_4) * 100

# Calculate percentage change in closing prices and assign it to R_N4
final_data$R_N4 <- ((final_data$Closing_Price_T_Adj_Minus_4 - final_data$Closing_Price_T_Adj_Minus_5) / final_data$Closing_Price_T_Adj_Minus_5) * 100

# Calculate percentage change in closing prices and assign it to R_N5
final_data$R_N5 <- ((final_data$Closing_Price_T_Adj_Minus_5 - final_data$Closing_Price_T_Adj_Minus_6) / final_data$Closing_Price_T_Adj_Minus_6) * 100

# Calculate percentage change in closing prices and assign it to R_N6
final_data$R_N6 <- ((final_data$Closing_Price_T_Adj_Minus_6 - final_data$Closing_Price_T_Adj_Minus_7) / final_data$Closing_Price_T_Adj_Minus_7) * 100

# Calculate percentage change in closing prices and assign it to R_P1
final_data$R_P1 <- ((final_data$Closing_Price_T_Adj_Plus_1 - final_data$Closing_Price_T_Adj) / final_data$Closing_Price_T_Adj) * 100

# Calculate percentage change in closing prices and assign it to R_P2
final_data$R_P2 <- ((final_data$Closing_Price_T_Adj_Plus_2 - final_data$Closing_Price_T_Adj_Plus_1) / final_data$Closing_Price_T_Adj_Plus_1) * 100

# Calculate percentage change in closing prices and assign it to R_P3
final_data$R_P3 <- ((final_data$Closing_Price_T_Adj_Plus_3 - final_data$Closing_Price_T_Adj_Plus_2) / final_data$Closing_Price_T_Adj_Plus_2) * 100

view(final_data)



################################################################################
#               Read in Nasdaq biotech index for Excess Returns                #
################################################################################

library(readxl)
NBI <- read_excel("NBI.xlsx")
View(NBI)

# Extract column names from 'NBI'
column_names <- names(NBI)

# Filter for variables containing "close" or "Returns"
close_columns <- grep("close|Returns|Date", column_names, ignore.case = TRUE)

# Keep only the columns containing "close"
NBI <- NBI[, close_columns]

View(NBI)

# Assuming 'final_data' and 'NBI' datasets are already loaded and 'final_data' has a column 'T'
# Ensure 'final_data' has a column 'T' and 'NBI' has a column 'Date'

# Merge 'final_data' with 'NBI' based on matching 'T' and 'Date' values
final_data <- merge(final_data, NBI, by.x = "T_Adj", by.y = "Date", all.x = TRUE)

# Rename the variable 'Returns NBI' to 'NBI Returns' in the merged dataset
names(final_data)[which(names(final_data) == "Returns NBI")] <- "NBI_Returns_0"

# Merge 'final_data' with 'NBI' based on matching 'T' and 'Date' values
final_data <- merge(final_data, NBI, by.x = "T_Adj_Minus_1", by.y = "Date", all.x = TRUE)

# Rename the variable 'Returns NBI' to 'NBI Returns' in the merged dataset
names(final_data)[which(names(final_data) == "Returns NBI")] <- "NBI_Returns_N1"

# Merge 'final_data' with 'NBI' based on matching 'T' and 'Date' values
final_data <- merge(final_data, NBI, by.x = "T_Adj_Minus_2", by.y = "Date", all.x = TRUE)

# Rename the variable 'Returns NBI' to 'NBI Returns' in the merged dataset
names(final_data)[which(names(final_data) == "Returns NBI")] <- "NBI_Returns_N2"

# Merge 'final_data' with 'NBI' based on matching 'T' and 'Date' values
final_data <- merge(final_data, NBI, by.x = "T_Adj_Minus_3", by.y = "Date", all.x = TRUE)

# Rename the variable 'Returns NBI' to 'NBI Returns' in the merged dataset
names(final_data)[which(names(final_data) == "Returns NBI")] <- "NBI_Returns_N3"

# Merge 'final_data' with 'NBI' based on matching 'T' and 'Date' values
final_data <- merge(final_data, NBI, by.x = "T_Adj_Minus_4", by.y = "Date", all.x = TRUE)

# Rename the variable 'Returns NBI' to 'NBI Returns' in the merged dataset
names(final_data)[which(names(final_data) == "Returns NBI")] <- "NBI_Returns_N4"

# Merge 'final_data' with 'NBI' based on matching 'T' and 'Date' values
final_data <- merge(final_data, NBI, by.x = "T_Adj_Minus_5", by.y = "Date", all.x = TRUE)

# Rename the variable 'Returns NBI' to 'NBI Returns' in the merged dataset
names(final_data)[which(names(final_data) == "Returns NBI")] <- "NBI_Returns_N5"

# Merge 'final_data' with 'NBI' based on matching 'T' and 'Date' values
final_data <- merge(final_data, NBI, by.x = "T_Adj_Minus_6", by.y = "Date", all.x = TRUE)

# Rename the variable 'Returns NBI' to 'NBI Returns' in the merged dataset
names(final_data)[which(names(final_data) == "Returns NBI")] <- "NBI_Returns_N6"

# Merge 'final_data' with 'NBI' based on matching 'T' and 'Date' values
final_data <- merge(final_data, NBI, by.x = "T_Adj_Plus_1", by.y = "Date", all.x = TRUE)

# Rename the variable 'Returns NBI' to 'NBI Returns' in the merged dataset
names(final_data)[which(names(final_data) == "Returns NBI")] <- "NBI_Returns_P1"

# Merge 'final_data' with 'NBI' based on matching 'T' and 'Date' values
final_data <- merge(final_data, NBI, by.x = "T_Adj_Plus_2", by.y = "Date", all.x = TRUE)

# Rename the variable 'Returns NBI' to 'NBI Returns' in the merged dataset
names(final_data)[which(names(final_data) == "Returns NBI")] <- "NBI_Returns_P2"

# Merge 'final_data' with 'NBI' based on matching 'T' and 'Date' values
final_data <- merge(final_data, NBI, by.x = "T_Adj_Plus_3", by.y = "Date", all.x = TRUE)

# Rename the variable 'Returns NBI' to 'NBI Returns' in the merged dataset
names(final_data)[which(names(final_data) == "Returns NBI")] <- "NBI_Returns_P3"

# View the merged dataset
View(final_data)

################################################################################
#                           Calculate Excess Returns                           #
################################################################################

# Calculate the excess returns
final_data$Excess_Returns_0 <- final_data$R_0 - final_data$NBI_Returns_0

# Calculate the excess returns
final_data$Excess_Returns_N1 <- final_data$R_N1 - final_data$NBI_Returns_N1

# Calculate the excess returns
final_data$Excess_Returns_N2 <- final_data$R_N2 - final_data$NBI_Returns_N2

# Calculate the excess returns
final_data$Excess_Returns_N3 <- final_data$R_N3 - final_data$NBI_Returns_N3

# Calculate the excess returns
final_data$Excess_Returns_N4 <- final_data$R_N4 - final_data$NBI_Returns_N4

# Calculate the excess returns
final_data$Excess_Returns_N5 <- final_data$R_N5 - final_data$NBI_Returns_N5

# Calculate the excess returns
final_data$Excess_Returns_N6 <- final_data$R_N6 - final_data$NBI_Returns_N6

# Calculate the excess returns
final_data$Excess_Returns_P1 <- final_data$R_P1 - final_data$NBI_Returns_P3

# Calculate the excess returns
final_data$Excess_Returns_P2 <- final_data$R_P2 - final_data$NBI_Returns_P2

# Calculate the excess returns
final_data$Excess_Returns_P3 <- final_data$R_P3 - final_data$NBI_Returns_P3
view(final_data)

# Assuming "Sentiment" is a factor variable with levels "Negative", "Neutral", and "Positive"
# You can use recode_factor() from dplyr to recode it to numeric values


################################################################################
#                           Numeric Sentiment                                  #
################################################################################

# Assuming 'final_data' is your dataset
# Create a new variable 'Numeric_Sentiment'
final_data$Numeric_Sentiment <- NA

# Assign numeric values based on 'Sentiment'
final_data$Numeric_Sentiment[final_data$Sentiment == "Negative"] <- 1
final_data$Numeric_Sentiment[final_data$Sentiment == "Neutral"] <- 2
final_data$Numeric_Sentiment[final_data$Sentiment == "Positive"] <- 3

view(final_data)

# Assuming your dataset is named "final_data"
final_data$Negative_Dummy <- ifelse(final_data$Numeric_Sentiment == 1, 1, 0)

# Assuming your dataset is named "final_data"
final_data$Neutral_Dummy <- ifelse(final_data$Numeric_Sentiment == 2, 1, 0)

# Assuming your dataset is named "final_data"
final_data$Positive_Dummy <- ifelse(final_data$Numeric_Sentiment == 3, 1, 0)

# View the updated dataset
view(final_data)


################################################################################
#                         Smaller # of Fields                                  #
################################################################################

# Assuming your list of words is stored in a vector called "word_list"
word_list <- c("Dermatology", "Endocrinology", "Gastroenterology", "Immunology", "Opthalmology", "Infectious Disease", "Neurology", "Cancer")  # Add your list of words here

# Convert the "TopFields" variable to character type to ensure it does not get converted to factors
final_data$Field <- as.character(final_data$Field)

# Assuming your dataset is named "final_data"
final_data$TopFields <- ifelse(final_data$Field %in% word_list, final_data$Field, "Other")

# View the updated dataset
View(final_data)

# Pulling up columns X, Y, and Z
selected_columns <- subset(final_data, select = c(T_Adj, Ticker, Name))

# Displaying the selected columns
view(selected_columns)

library(readxl)
SOS_Data <- read_excel("SOS Data.xlsx")
View(SOS_Data)

# Assuming "Count" is a variable in SOS_Data and final_data is a data frame
# Create a column in final_data representing row numbers
final_data$Count <- seq_len(nrow(final_data))

# Merge the datasets by the "Count" variable in SOS_Data and the row numbers of final_data
Data_almost <- merge(SOS_Data, final_data, by.x = "Count", by.y = "Count")

# Print the merged dataset
view(Data_almost)

# Convert the "MarketCap" variable to numeric
Data_almost$Market_Cap <- as.numeric(Data_almost$Market_Cap)

# Convert the "MarketCap" variable to numeric
Data_almost$SOSadj <- as.numeric(Data_almost$SOSadj)

view(Data_almost)

# Calculate the excess returns
Data_almost$Market_Cap <- Data_almost$SOSadj * Data_almost$Closing_Price_T_Adj_Minus_1

# Create a new variable called 'MCQuintiles' based on the quintiles of 'Market_Cap'
Data_almost <- Data_almost %>%
  mutate(MCQuintiles = ntile(Market_Cap, 5))

# Filter rows where MCQuintiles is 1
quintile_1_data <- subset(Data_almost, MCQuintiles == 1)

# Print the minimum and maximum values of Market_Cap
cat("Minimum Market_Cap when MCQuintiles = 1:", min(quintile_1_data$Market_Cap), "\n")
cat("Maximum Market_Cap when MCQuintiles = 1:", max(quintile_1_data$Market_Cap), "\n")

# Filter rows where MCQuintiles is 2
quintile_2_data <- subset(Data_almost, MCQuintiles == 2)

# Print the minimum and maximum values of Market_Cap
cat("Minimum Market_Cap when MCQuintiles = 2:", min(quintile_2_data$Market_Cap), "\n")
cat("Maximum Market_Cap when MCQuintiles = 2:", max(quintile_2_data$Market_Cap), "\n")

# Filter rows where MCQuintiles is 3
quintile_3_data <- subset(Data_almost, MCQuintiles == 3)

# Print the minimum and maximum values of Market_Cap
cat("Minimum Market_Cap when MCQuintiles = 3:", min(quintile_3_data$Market_Cap), "\n")
cat("Maximum Market_Cap when MCQuintiles = 3:", max(quintile_3_data$Market_Cap), "\n")


# Filter rows where MCQuintiles is 4
quintile_4_data <- subset(Data_almost, MCQuintiles == 4)

# Print the minimum and maximum values of Market_Cap
cat("Minimum Market_Cap when MCQuintiles = 4:", min(quintile_4_data$Market_Cap), "\n")
cat("Maximum Market_Cap when MCQuintiles = 4:", max(quintile_4_data$Market_Cap), "\n")


# Filter rows where MCQuintiles is 5
quintile_5_data <- subset(Data_almost, MCQuintiles == 5)

# Drop observation where the variable equals "7.657151e+12"
Data_almost <- subset(Data_almost, Market_Cap != 7.657151e+12)

# Print the minimum and maximum values of Market_Cap
cat("Minimum Market_Cap when MCQuintiles = 5:", min(quintile_5_data$Market_Cap), "\n")
cat("Maximum Market_Cap when MCQuintiles = 5:", max(quintile_5_data$Market_Cap), "\n")

view(Data_almost)



####################

#variables_of_interest <- c("Ticker", "Name", "Drug", "Field", "Indication", "Stage", "Sentiment", "Excess_Returns_0", "Excess_Returns_P1", "Excess_Returns_P2", "Excess_Returns_P3", "Numeric_Sentiment", "Negative_Dummy", "Neutral_Dummy", "Positive_Dummy")  # Replace with your variable names

# Subset the dataframe to keep only the variables of interest
#final_data <- final_data[, variables_of_interest, drop = FALSE]

#view(final_data)



# Pulling up columns X, Y, and Z
#selected_columns <- subset(final_data, select = c(T_Adj_Minus_1, Ticker, Day_T_Adj_Minus_1, Excess_Returns_P1, Numeric_Sentiment))

# Displaying the selected columns
#view(selected_columns)

################################################################################
#                             Rescale Stage variable                           #
################################################################################

view(Data_almost)

# Assuming "Data_almost" is your dataset

# Create a new variable "Stage" based on the values of "stage"
Data_almost$Stage <- ifelse(Data_almost$Stage == 0, "Pre-Clinical",
                            ifelse(Data_almost$Stage == 1, "Phase 1",
                                   ifelse(Data_almost$Stage == 2, "Phase 2",
                                          ifelse(Data_almost$Stage == 3, "Phase 3",
                                                 ifelse(Data_almost$Stage == 4, "Phase 4", NA)))))

# NA will be assigned if the value of "stage" is not 0, 1, 2, 3, or 4

view(Data_almost)


# Convert MCQuintiles to a categorical variable
Data_almost$MCQuintiles <- factor(Data_almost$MCQuintiles,
                                  levels = c(1, 2, 3, 4, 5),
                                  labels = c("First Quintile", "Second Quintile", "Third Quintile", "Fourth Quintile", "Fifth Quintile"))


view(Data_almost)


################################################################################
#                         Clean Data for Stata results                         #
################################################################################

# Subset columns 'MCQuintiles' and 'Excess_Returns_P1' into a smaller dataset
smaller_dataset <- Data_almost[, c("MCQuintiles", "Excess_Returns_P1")]

# View the first few rows of the smaller dataset
head(smaller_dataset)


################################################################################
#                                Visualization                                 #
################################################################################




# Create the new variable Ln_Market_Cap
Data_almost$Ln_Market_Cap <- log(Data_almost$Market_Cap)

# Define equations for the lines
line1 <- function(x) { return(0.5*x - 2) }  # Example equation 1
line2 <- function(x) { return(0.3*x + 1) }  # Example equation 2
line3 <- function(x) { return(0.8*x - 3) }  # Example equation 3

# Create a scatter plot
scatter_plot <- ggplot(Data_almost, aes(x = Ln_Market_Cap, y = Excess_Returns_P1, color = factor(Sentiment), shape = factor(TopFields))) +
  
  # Add points with size based on Stage
  geom_point(aes(size = Stage)) +
  
  # Change shape of points based on TopFields
  scale_shape_manual(values = c(8, 15, 16, 17, 18, 4, 21, 13, 6, 7)) +  # Change values as per your requirement
  
  # Change colors for Sentiment
  scale_color_manual(values = c("#FF0000", "#333333", "#00FF00")) +
  
  # Add a legend
  labs(color = "Sentiment", shape = "Top Fields") +
  guides(size = guide_legend(title = "Stage")) + # Add legend for size
  
  # Add title and axis labels
  ggtitle("Excess Returns One Day after Announcement vs Market Cap") +
  xlab("Log-Scaled Market Cap") +
  ylab("Excess Returns (Percent, %)")+
  
  # Increase text size for axes, axis titles, plot title, and legend
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 15),
        plot.title = element_text(size = 20,),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 11))

# Save the plot as a .png file
ggsave("scatter_plot_results_RP1.png", plot = scatter_plot, width = 12, height = 7, units = "in", dpi = 300)



# Create a scatter plot
scatter_plot <- ggplot(Data_almost, aes(x = Ln_Market_Cap, y = Excess_Returns_0, color = factor(Sentiment), shape = factor(TopFields))) +
  
  # Add points with size based on Stage
  geom_point(aes(size = Stage)) +
  
  # Change shape of points based on TopFields
  scale_shape_manual(values = c(8, 15, 16, 17, 18, 4, 21, 13, 6, 7)) +  # Change values as per your requirement
  
  # Change colors for Sentiment
  scale_color_manual(values = c("#FF0000", "#333333", "#00FF00")) +
  
  # Add a legend
  labs(color = "Sentiment", shape = "Top Fields") +
  guides(size = guide_legend(title = "Stage")) + # Add legend for size
  
  # Add title and axis labels
  ggtitle("Excess Returns Day of Announcement vs Market Cap") +
  xlab("Log-Scaled Market Cap") +
  ylab("Excess Returns (Percent, %)")+
  
  # Increase text size for axes, axis titles, plot title, and legend
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 15),
        plot.title = element_text(size = 20,),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 11))


# Save the plot as a .png file
ggsave("scatter_plot_results_R0.png", plot = scatter_plot, width = 12, height = 7, units = "in", dpi = 300)



# Create a scatter plot
scatter_plot <- ggplot(Data_almost, aes(x = Ln_Market_Cap, y = Excess_Returns_P2, color = factor(Sentiment), shape = factor(TopFields))) +
  
  # Add points with size based on Stage
  geom_point(aes(size = Stage)) +
  
  # Change shape of points based on TopFields
  scale_shape_manual(values = c(8, 15, 16, 17, 18, 4, 21, 13, 6, 7)) +  # Change values as per your requirement
  
  # Change colors for Sentiment
  scale_color_manual(values = c("#FF0000", "#333333", "#00FF00")) +
  
  # Add a legend
  labs(color = "Sentiment", shape = "Top Fields") +
  guides(size = guide_legend(title = "Stage")) + # Add legend for size
  
  # Add title and axis labels
  ggtitle("Excess Returns Two Days after Announcement vs Market Cap") +
  xlab("Log-Scaled Market Cap") +
  ylab("Excess Returns (Percent, %)")+
  
  # Increase text size for axes, axis titles, plot title, and legend
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 15),
        plot.title = element_text(size = 20,),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 11))

# Save the plot as a .png file
ggsave("scatter_plot_results_P2.png", plot = scatter_plot, width = 12, height = 7, units = "in", dpi = 300)



# Create a scatter plot
scatter_plot <- ggplot(Data_almost, aes(x = Ln_Market_Cap, y = Excess_Returns_P3, color = factor(Sentiment), shape = factor(TopFields))) +
  
  # Add points with size based on Stage
  geom_point(aes(size = Stage)) +
  
  # Change shape of points based on TopFields
  scale_shape_manual(values = c(8, 15, 16, 17, 18, 4, 21, 13, 6, 7)) +  # Change values as per your requirement
  
  # Change colors for Sentiment
  scale_color_manual(values = c("#FF0000", "#333333", "#00FF00")) +
  
  # Add a legend
  labs(color = "Sentiment", shape = "Top Fields") +
  guides(size = guide_legend(title = "Stage")) + # Add legend for size
  
  # Add title and axis labels
  ggtitle("Excess Returns Three Days after Announcement vs Market Cap") +
  xlab("Log-Scaled Market Cap") +
  ylab("Excess Returns (Percent, %)")+
  
  # Increase text size for axes, axis titles, plot title, and legend
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 15),
        plot.title = element_text(size = 20,),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 11))


# Save the plot as a .png file
ggsave("scatter_plot_results_P3.png", plot = scatter_plot, width = 12, height = 7, units = "in", dpi = 300)

################################################################################
#                                    Model                                     #
################################################################################


install.packages("stargazer")
library(stargazer)
install.packages("webshot")
webshot::install_phantomjs()
library(webshot)
# Install and load necessary packages
install.packages("stargazer")
install.packages("webshot")
install.packages("writexl")
library(stargazer)
library(webshot)
library(writexl)

# Fit the regression model
model_1 <- lm(Excess_Returns_0 ~ Negative_Dummy * Stage + Negative_Dummy * TopFields + Negative_Dummy * MCQuintiles + Positive_Dummy * Stage + Positive_Dummy * TopFields + Positive_Dummy * MCQuintiles, data = Data_almost)
model_2 <- lm(Excess_Returns_P1 ~ Negative_Dummy * Stage + Negative_Dummy * TopFields + Negative_Dummy * MCQuintiles + Positive_Dummy * Stage + Positive_Dummy * TopFields + Positive_Dummy * MCQuintiles, data = Data_almost)
model_3 <- lm(Excess_Returns_P2 ~ Negative_Dummy * Stage + Negative_Dummy * TopFields + Negative_Dummy * MCQuintiles + Positive_Dummy * Stage + Positive_Dummy * TopFields + Positive_Dummy * MCQuintiles, data = Data_almost)
model_4 <- lm(Excess_Returns_P3 ~ Negative_Dummy * Stage + Negative_Dummy * TopFields + Negative_Dummy * MCQuintiles + Positive_Dummy * Stage + Positive_Dummy * TopFields + Positive_Dummy * MCQuintiles, data = Data_almost)


################################################################################
#                 Test Models for 5 out of sample Announcements                #
################################################################################

library(readxl)
OutofSampleData <- read_excel("OutofSampleData.xlsx")
View(OutofSampleData)

# Convert the variable from numeric to factor
OutofSampleData$MCQuintiles <- factor(OutofSampleData$MCQuintiles)



# Predict the value of the dependent variable for the new observation
predicted_value_Model_1 <- predict(model_1, newdata = OutofSampleData)

# Add the predicted values as a new column in the dataset
OutofSampleData$predicted_value_Model_1 <- predicted_value_Model_1

# Predict the value of the dependent variable for the new observation
predicted_value_Model_2 <- predict(model_2, newdata = OutofSampleData)

# Add the predicted values as a new column in the dataset
OutofSampleData$predicted_value_Model_2 <- predicted_value_Model_2

# Predict the value of the dependent variable for the new observation
predicted_value_Model_3 <- predict(model_3, newdata = OutofSampleData)

# Add the predicted values as a new column in the dataset
OutofSampleData$predicted_value_Model_3 <- predicted_value_Model_3

# Predict the value of the dependent variable for the new observation
predicted_value_Model_4 <- predict(model_4, newdata = OutofSampleData)

# Add the predicted values as a new column in the dataset
OutofSampleData$predicted_value_Model_4 <- predicted_value_Model_4

view(OutofSampleData)

# Subset the data where TopFields equals "Other"
subset_data1 <- subset(OutofSampleData, TopFields == "Other")

view(subset_data1)


# Subset the data where TopFields equals "Other"
subset_data2 <- subset(OutofSampleData, TopFields == "Neurology")

view(subset_data2)

################################################################################
#    Visualize Excess Return Prediction for 2 out of sample Announcements      #
################################################################################

library(readxl)
MRNAPredicteddata <- read_excel("MRNAPredicteddata.xlsx")
View(MRNAPredicteddata)

# Convert the variable from numeric to factor
MRNAPredicteddata$Actual <- as.numeric(as.character(MRNAPredicteddata$Actual))
MRNAPredicteddata$Predicted <- as.numeric(MRNAPredicteddata$Predicted)

View(MRNAPredicteddata)


gg1 <- ggplot(MRNAPredicteddata, aes(x = Time)) +
  geom_vline(xintercept = 0, color = "green", size = 1, linetype = "longdash") + 
  geom_point(aes(y = Predicted, color = "Predicted"), size = 2, na.rm = TRUE) +
  geom_point(aes(y = Actual, color = "Actual"), size = 2, na.rm = TRUE) +
  geom_line(aes(y = Actual, color = "Actual"), size = 1, na.rm = TRUE) +
  geom_line(aes(y = Predicted, color = "Predicted", linetype = "Predicted"), size = 1,  linetype = "dashed", na.rm = TRUE) +
  labs(x = "Time (@ Time = 0, Date = 04-03-2024)", y = "Excess Returns", title = "Actual vs Predicted Excess Returns of Moderna (Ticker: MRNA)") +
  scale_y_continuous(breaks = seq(-7, 7, by = 1)) +
  theme_minimal() +
  scale_color_manual(name = "Data Type", 
                     values = c("Actual" = "orange", "Predicted" = "purple"),
                     labels = c("Actual", "Predicted")) +
  scale_linetype_manual(name = "Data Type",
                        values = c("Actual" = "solid", "Predicted" = "dashed"),
                        labels = c("Actual", "Predicted")) +
  theme(axis.title.x = element_text(size = 8),  # Adjust x-axis title size
        axis.title.y = element_text(size = 8),  # Adjust y-axis title size
        plot.title = element_text(size = 10))   # Adjust main title size

# Save the plot as a .png file
ggsave("ActualvsPredictionMRNA.png", plot = gg1, width = 5, height = 3, units = "in", dpi = 300)

## Company #2 ##

library(readxl)
AZNPredicteddata <- read_excel("AZNPredicteddata.xlsx")
View(AZNPredicteddata)

# Convert the variable from numeric to factor
AZNPredicteddata$Actual <- as.numeric(as.character(AZNPredicteddata$Actual))
AZNPredicteddata$Predicted <- as.numeric(AZNPredicteddata$Predicted)

View(AZNPredicteddata)


gg2 <- ggplot(AZNPredicteddata, aes(x = Time)) +
  geom_vline(xintercept = 0, color = "green", size = 1, linetype = "longdash") + 
  geom_point(aes(y = Predicted, color = "Predicted"), size = 2, na.rm = TRUE) +
  geom_point(aes(y = Actual, color = "Actual"), size = 2, na.rm = TRUE) +
  geom_line(aes(y = Actual, color = "Actual"), size = 1, na.rm = TRUE) +
  geom_line(aes(y = Predicted, color = "Predicted", linetype = "Predicted"), size = 1,  linetype = "dashed", na.rm = TRUE) +
  labs(x = "Time (@ Time = 0, Date = 03-25-2024)", y = "Excess Returns", title = "Actual vs Predicted Excess Returns of AstraZeneca  (Ticker: AZN)") +
  scale_y_continuous(breaks = seq(-7, 7, by = 1)) +
  theme_minimal() +
  scale_color_manual(name = "Data Type", 
                     values = c("Actual" = "orange", "Predicted" = "purple"),
                     labels = c("Actual", "Predicted")) +
  scale_linetype_manual(name = "Data Type",
                        values = c("Actual" = "solid", "Predicted" = "dashed"),
                        labels = c("Actual", "Predicted")) +
  theme(axis.title.x = element_text(size = 8),  # Adjust x-axis title size
        axis.title.y = element_text(size = 8),  # Adjust y-axis title size
        plot.title = element_text(size = 10))   # Adjust main title size

# Save the plot as a .png file
ggsave("ActualvsPredictionAZN.png", plot = gg2, width = 5, height = 3, units = "in", dpi = 300)

################################################################################
#                           Visualize Market_Cap                               #
################################################################################

gg3 <- ggplot(data = Data_almost, aes(x = Ln_Market_Cap, fill = factor(MCQuintiles))) +
  geom_bar(color = "black", width = 0.5) +  
  scale_fill_manual(values = c("#F7FBFF", "#DEEBF7", "#C6DBEF", "#9ECAE1", "#6BAED6"), 
                    name = "Quintiles",
                    labels = c("1st Quintile (< or = $93,385,502)", "2nd Quintile (< or = $1,066,800,041)", "3rd Quintile (< or = $40,024,602,051)", "4th Quintile (< or = $154,929,603,882)", "5th Quintile (< or = $765,715,100,000)")) +  # Add descriptions to legend here
  labs(title = "Distribution of Market Cap by Quintiles",
       x = "Log-Scaled Market Cap", y = "Count") +
  theme_minimal()

# Save the plot as a .png file
ggsave("qUINTILEMARKETCAP.png", plot = gg3)
