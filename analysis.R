# This script contains exploratory analyses of the search query and results data provided by
# Aslett et al. 
# 
# The calculate_jaccard() function is defined in the accompanying script david_additional_analyses.R
# The code covers analyses of diverse aspects from basic properties of queries, such as length and counts, to linguistic properties of query terms
# including rarity, sentiment, POS-tags and semantic content.
# authors: Markus Bink + David Elsweiler


library(tidyverse)
library(stopwords)
library(mlr3measures)
library(lsa)
library(tm)
library(igraph)
library(qgraph)
library(mclust)
library(stringr)

set.seed(123) # Set seed for reproducibility

# Read data
data <- read.csv("data/All_Search_Results_Combined.csv")
articles <- read.csv("data/Study_5_Articles.csv")

# Only select columns we want to use
filtered_data <- data %>% 
  select(ResponseId, 
         Category, 
         True_Dummy, 
         Seven_Ordinal, 
         Four_Ordinal, 
         Age, 
         Gender, 
         FC_Eval, 
         List_Scores, 
         avg_score, 
         Dummy_Ideology, 
         Ideo_Congruence,
         Income_Score,
         Education_Score,	
         Dig_Lit_Score,
         Search_Term, 
         URL_in_results,
         URL,
         URLs,
         ALL_URLS
  )

# Preprocess Search_Term column (includes special characters e.g. from URL Encoding)
filtered_data$Search_Term <- sapply(filtered_data$Search_Term, URLdecode)


table(filtered_data$FC_Eval, data$Category)

# Get documents that were identified as "False/Misleading" by fact checkers and which participants did not rate as "Could not determine"
filtered_data <- filtered_data %>% 
  filter(FC_Eval == "FM" & Category != "Coul") %>% 
  filter(List_Scores != "[]")

table(filtered_data$FC_Eval, filtered_data$Category)


# Query length in words
filtered_data <- filtered_data %>% 
  mutate(query_length = lengths(strsplit(Search_Term, ' ')))

# Average query length
filtered_data %>% 
  group_by(Category) %>% 
  summarise(
    M = round(mean(query_length), 2),
    SD = round(sd(query_length), 2)
  )

t.test(query_length ~ Category, data = filtered_data)

# Add an internal user id
filtered_data <- filtered_data %>%
  mutate(uid = match(ResponseId, unique(ResponseId)))


# How many documents are there?
filtered_data %>% 
  group_by(URL) %>% 
  count()

filtered_data %>% 
  group_by(URL, ResponseId) %>% 
  count()

# How many users per document?
filtered_data %>% 
  distinct(URL, ResponseId) %>% 
  group_by(URL) %>% 
  count()
  


# ==== IDF over all documents ====
get_user_count <- function(df) {
  return (df %>% 
    pull(uid) %>% 
    unique() %>% 
    length())
}

preprocess_query <- function(query) {
  term = tolower(query)     
  term = gsub("[[:punct:]]", " ", term)  
  term = gsub("\\s+", " ", term)
  term = trimws(term)
  return(term)
}

get_index <- function(df) {
  return (
    filtered_data %>%
      rowwise() %>%
      mutate(
        terms = preprocess_query(Search_Term),
        terms = str_split(terms, " ")           # Split the search term
      ) %>%
      unnest(terms) %>%
      select(uid, terms)
  )
}

# Remove possible empty strings, numeric terms, terms containing numbers, stopwords, single char terms
filter_index <- function(df) {
  return (
    df %>% 
      filter(terms != "") %>% 
      filter(!grepl("\\d", terms)) %>% 
      filter(!terms %in% stopwords("en")) %>% 
      filter(nchar(terms) > 1)
  ) 
}

sort_index <- function(df) {
 return (
   df %>% 
     arrange(terms)
 ) 
}


get_unique_terms <- function(df) {
  return(
    df %>% 
      distinct(terms)
    )
}

get_inverted_index <- function(df) {
  return(
    df %>%
      rowwise() %>%
      mutate(users = list(unique(unlist(list(index.sorted$uid[index.sorted$terms == terms]))))) %>%
      mutate(user_count = length(users)) %>%
      select(terms, user_count, users)
  )
}

get_index_with_idf <- function(index, inverted_index) {
  total_user_count <- get_user_count(index)
  
  return (
    inverted_index %>% 
      mutate(
        idf = log(total_user_count / user_count)
    )
  )
}

get_idf <- function(term) {
  result <- inverted.index %>% 
    filter(terms == term) %>% 
    pull(idf)
  
  # Add check because the passed term could not be in the index because of the filtering we do (e.g. stopword-removal) 
  if (length(result) == 0) {
    return(0)
  } else {
    return(result)
  }
}

get_idf_sum <- function(terms) {
  idf_sum = 0
  for(term in terms) {
    idf_sum = idf_sum + get_idf(term)
  }
  return(idf_sum)
}

get_idf_mean <- function(terms) {
  idf_sum = 0
  for(term in terms) {
    idf_sum = idf_sum + get_idf(term)
  }
  return(idf_sum/length(terms))
}

get_idf_median <- function(terms) {
  idf_sum = 0
  idfs = c()
  for(term in terms) {
    idfs = c(idfs,get_idf(term))
  }
  return(median(idfs))
}

# Create the index
index <- get_index(filtered_data)

index.filtered <- filter_index(index)

# Sort the index based on query terms
index.sorted <- sort_index(index.filtered)

# Get unique terms
unique.terms <- get_unique_terms(index.sorted)

# Create inverted index
inverted.index <- get_inverted_index(unique.terms)

# Calculate idf for each term 
inverted.index <- get_index_with_idf(index, inverted.index)

# Calculate sum of idfs for each query used
filtered_data <- filtered_data %>% 
  mutate(
    terms = preprocess_query(Search_Term),
    terms = str_split(terms, " "),
    sum_idf = sapply(terms, get_idf_sum),
    mean_idf = sapply(terms, get_idf_mean),
    median_idf = sapply(terms, get_idf_median)
  )

# Plot idf scores based on Category
filtered_data %>% 
  ggplot(aes(x = Category, y = sum_idf)) + 
  geom_boxplot()

t.test(filtered_data$sum_idf~filtered_data$Category)
# idf is larger for rare terms. True group seems to copy headline terms, therefore their  
# average idf sum should be lower given that people in that group often use the same words.
# However, this is not revealed in the test with 


# the sum_idf adds weight to longer queries (i.e. copy/pasted)
# let's try the mean and median instead
filtered_data %>% 
  ggplot(aes(x = Category, y = mean_idf)) + 
  geom_boxplot()

filtered_data %>% 
  ggplot(aes(x = Category, y = median_idf)) + 
  geom_boxplot()

t.test(filtered_data$median_idf~filtered_data$Category)
# This makes more sense. Those participants who used their own queries (i.e. didn't copy and paste) 
# used rarer terms and were slightly more likely to identify the misinformation




# ==== Term Document Matrix to calc similarity ====

# Now we want to see how similar the queries are.
# We would expect that, since True group often copied (parts of) the headline, that there's a higher similarity
# between the queries in this group
corpus = Corpus(VectorSource(filtered_data$Search_Term))

# Each column is a query, each row is a term (1 = term is in query, 0 otherwise)
tdm = TermDocumentMatrix(
  corpus,
  control = list(
    removePunctuation=TRUE,
    stopwords = stopwords("en"),
    removeNumbers= TRUE, tolower = TRUE
  )
)

# ==== Cosine similarity for specific document ====
flatten_corr_matrix <- function(correlation_matrix) {
  ut <- upper.tri(correlation_matrix)
  return(correlation_matrix[ut])
}


# ==== Cosine Similarity across all queries in dataset ====
full_similarty <- cosine(as.matrix(tdm))
full_similarty <- flatten_corr_matrix(full_similarty)
summary(full_similarty)


# ==== Get likely terms for document based on misl and true tdms ====
get_likely_terms <- function(misl_tdm, true_tdm, get_top_terms, term_count) {
  misl_term_in_doc_count <- rowSums(misl_tdm)
  true_term_in_doc_count <- rowSums(true_tdm)
  
  misl_total_terms_in_doc_count <- sum(misl_term_in_doc_count)
  true_total_terms_in_doc_count <- sum(true_term_in_doc_count)
  
  lambda <- 0.5
  misl_term_likelihood <- (misl_term_in_doc_count / misl_total_terms_in_doc_count) + ((misl_term_in_doc_count + true_term_in_doc_count)/(misl_total_terms_in_doc_count + true_total_terms_in_doc_count))
  true_term_likelihood <- (true_term_in_doc_count / true_total_terms_in_doc_count) + ((misl_term_in_doc_count + true_term_in_doc_count)/(misl_total_terms_in_doc_count + true_total_terms_in_doc_count))
  
  term_likelihood <- log(misl_term_likelihood / true_term_likelihood)
  
  if(get_top_terms) {
    return(head(sort(term_likelihood, decreasing = TRUE), n = term_count))
  } else {
    return(tail(sort(term_likelihood, decreasing = TRUE), n = term_count))
  }
}


# ==== Query Analysis ====
# This is the average for each document, aggregate the same for each document to one long vector
query.df <- data.frame()

for(document in unique(filtered_data$URL)) {
  print(document)
  document_indices <- which(filtered_data$URL == document)
  document_tdm <- as.matrix(tdm[, document_indices])
  
  # Get indices based on Category
  misl_indices <- which(filtered_data[document_indices,]$Category == "Misl")
  true_indices <- which(filtered_data[document_indices,]$Category != "Misl")
  
  # Get subset of data
  misl_tdm <- document_tdm[, misl_indices]
  true_tdm <- document_tdm[, true_indices]
  
  # Calculate centroid based on Category
  misl_centroid <- rowMeans(misl_tdm)
  true_centroid <- rowMeans(true_tdm)
  
  document_tdm_with_centroid <- document_tdm %>%
    cbind(misl_centroid) %>%
    cbind(true_centroid)
  
  document_similarity_with_centroid <- cosine(document_tdm_with_centroid)
  
  misl_similarty <- as.data.frame(document_similarity_with_centroid[misl_indices, ])
  true_similarty <- as.data.frame(document_similarity_with_centroid[true_indices, ])
  
  within_class_dist <- misl_similarty$misl_centroid
  within_class_dist <- c(within_class_dist, true_similarty$true_centroid)
  
  outer_class_dist <- misl_similarty$true_centroid
  outer_class_dist <- c(outer_class_dist, true_similarty$misl_centroid)
  
  # Get likely terms
  misl_terms <- get_likely_terms(misl_tdm, true_tdm, TRUE, 5)
  true_terms <- get_likely_terms(misl_tdm, true_tdm, FALSE, 5)
  
  document_similarity <- cosine(document_tdm)
  dist_m <- document_similarity
  dist_mi <- 1/dist_m
  
  file_name <- gsub("/", "-", document)
  
  # Red = misleading, Blue = true
  # Numbers in the nodes are the rows in "filtered_data" dataframe of the individual query
  jpeg(paste("graphs/", file_name, '.jpeg', sep =""), width=1000, height=1000, unit='px')
  qgraph(dist_mi, layout='spring', vsize=3, legend = TRUE, groups = list(Misl = misl_indices, True = true_indices), borders = F, arrows = F, directed = F, edge.color = "transparent")
  dev.off()
  
  # K-Means Clustering from https://uc-r.github.io/kmeans_clustering
  # Perform k-means clustering
  k <- 2 # Define the number of clusters
  kmeans_result <- kmeans(document_similarity, centers = k, nstart = 25)
  
  # Assign clusters to each document
  clusters <- kmeans_result$cluster
  
  # Store the cluster information in a dataframe
  clusters_df <- data.frame(
    "query_index" = as.numeric(names(clusters)),
    "cluster" = clusters,
    "category" = filtered_data[document_indices,]$Category
  )
  
  clusters_df <- clusters_df %>% 
    mutate(cluster = ifelse(cluster == 1, "True", "Misl"))
    
  # Calculate accuracy
  doc_matches <- sum(clusters_df$cluster == clusters_df$category)
  total_queries_for_doc <- nrow(clusters_df)
  doc_accuracy <- doc_matches / total_queries_for_doc
  
  # Calculate ARI (Adjusted Rank Index) to evaluate the overlap between two clusterings
  ari_score <- adjustedRandIndex(clusters_df$cluster, clusters_df$category)
  
  # Plotting
  jpeg(paste("graphs/", file_name, '-kmeans.jpeg', sep =""), width=1000, height=1000, unit='px')
  qgraph(dist_mi, layout = 'spring', vsize = 3, legend = TRUE, groups = list(Cluster1 = which(clusters == 2), Cluster2 = which(clusters == 1)), borders = FALSE, arrows = FALSE, directed = FALSE, edge.color = "transparent")
  dev.off()
  
  # Mean Similarity based on Category
  mean_sim_misl_queries = mean(flatten_corr_matrix(document_similarity[misl_indices, misl_indices]))
  mean_sim_true_queries = mean(flatten_corr_matrix(document_similarity[true_indices, true_indices]))
  
  query.df <- rbind(query.df, data.frame(
    "document" = file_name, 
    "num_misl_queries" = length(misl_indices),
    "num_true_queries" = length(true_indices),
    "mean_sim_within_centroid" = mean(within_class_dist), 
    "mean_sim_outer_centroid" = mean(outer_class_dist), 
    "mean_sim_misl_queries" = mean_sim_misl_queries,
    "mean_sim_true_queries" = mean_sim_true_queries,
    "misl_terms" = paste(names(misl_terms), sep = ", ", collapse = ", "),
    "true_terms" = paste(names(true_terms), sep = ", ", collapse = ", "),
    "k_means_acc" = doc_accuracy,
    "ari_score" = ari_score
  )) 
}

# The more diverse users' query behaviour is, the better they are able to correctly identify misinformation as misinformation
# Diverse query behaviour here means, for people who issued multiple queries, cosine similarity should be low (e.g. high variance 1 - cosine_sim)
# Thus, they should have come across a more diverse set of results

# For each user who issued multiple queries, calculate average cosine similarity of queries used
# Need a session_id since in some cases Users judged multiple documents
filtered_data <- filtered_data %>% 
  mutate(
    session_id = paste(ResponseId, URL, sep = "--")
  )

# Sometimes the query is identical, but the URLs are not the same
# This means that people probably went on the 2nd page
# -> add column to indicate this (viewed_2nd_page)
filtered_data <- filtered_data %>% 
  group_by(ResponseId, Search_Term) %>% 
  mutate(viewed_2nd_page = ifelse(n_distinct(URLs) > 1, TRUE, FALSE)) %>% 
  ungroup()

# In some cases the query + urls (from SERP) are identical, so this seems like an issue on the data collection part
# e.g. users double-clicking something and thus storing the same data twice (see R_0ilMeEkp8f8eRJ7) 
# (not sure if that's exactly the case since this is not documented in the paper, no timestamps available either)
# So we have to filter those out where session_id + searchterm + urls are identical
sessions_with_multiple_queries <- filtered_data %>% 
  distinct(session_id, Search_Term, URLs, .keep_all = TRUE)

# Now also filter users who viewed_2nd_page 
# (since the query in this case is identical, so not meaningful to calculate similarity since it's 1 anyways)
sessions_with_multiple_queries <- sessions_with_multiple_queries %>% 
  filter(viewed_2nd_page == FALSE)

# Remove users who only issued one query (since we cannot calculate any similarity with just 1 query)
sessions_ids_with_multiple_queries <- sessions_with_multiple_queries %>% 
  group_by(session_id) %>% 
  count() %>% 
  filter(n > 1) %>% 
  pull(session_id)

multiple_query_data <- data.frame()
for(session_id in sessions_ids_with_multiple_queries) {
  document_indices <- which(filtered_data$session_id == session_id)
  document_tdm <- as.matrix(tdm[, document_indices])
  
  query_sim <- cosine(document_tdm)
  avg_query_sim <- mean(flatten_corr_matrix(query_sim))
  Category <- sessions_with_multiple_queries[which(sessions_with_multiple_queries$session_id == session_id)[1],] %>% 
    pull(Category)
  
  multiple_query_data <- rbind(multiple_query_data, data.frame(
    "ResponseId" = strsplit(session_id, "--")[[1]][1], 
    "URL" = strsplit(session_id, "--")[[1]][2], 
    "Category" = Category,
    "query_variance" = 1 - avg_query_sim
  )) 
}

multiple_query_data %>% 
  group_by(Category) %>% 
  count()

multiple_query_data %>% 
  group_by(Category) %>% 
  summarise(
    M = mean(query_variance),
    SD = sd(query_variance),
  )

multiple_query_data %>% 
  ggplot(aes(x = Category, y = query_variance)) + 
  geom_boxplot()

t.test(multiple_query_data$query_variance ~ multiple_query_data$Category)
# No significant difference there

  
# ==== Query Reformulation ====
# Have query variation on the x-axis (temporal) and NewsGuard Score on the y-axis
# If they get better, with increase in x, NewsGuard score could potentially also go up
# Essentially, them finding better quality results after query reformulation

query_reformulation_data <- sessions_with_multiple_queries %>% 
  select(Category, List_Scores, avg_score, Search_Term, URL, session_id)

query_reformulation_data <- left_join(query_reformulation_data, articles %>% select(URL, Headline), by = "URL")

query_reformulation_data$jaccard <- calculate_jaccard(query_reformulation_data$Search_Term,  query_reformulation_data$Headline)

query_reformulation_jaccard = c()
for (i in 1:nrow(query_reformulation_data)) {
  text1 <- query_reformulation_data[i, "Search_Term"]
  text2 <- query_reformulation_data[i, "Headline"]
  
  jaccard_coefficient <- calculate_jaccard(text1, text2)
  query_reformulation_jaccard=c(query_reformulation_jaccard,jaccard_coefficient)
}
query_reformulation_data$jaccard = query_reformulation_jaccard

query_reformulation_data <- query_reformulation_data %>% 
  group_by(session_id) %>%
  mutate(search_term_number = row_number()) %>%
  ungroup()

query_reformulation_data %>% 
  group_by(search_term_number) %>% 
  count()

# Significant difference b/w the # of queries issued based on whether users identified misinfo or not 
query_reformulation_data %>% 
  group_by(Category) %>% 
  summarise(
    M_Query_Count = round(mean(search_term_number), 2),
    SD_Query_Count = round(sd(search_term_number), 2),
  )

t.test(search_term_number ~ Category, data = query_reformulation_data)
  


query_reformulation_data %>% 
  group_by(as.factor(search_term_number)) %>% 
  summarise(
    M_Score = round(mean(avg_score), 2),
    SD_Score = round(sd(avg_score), 2),
    M_Jacc = round(mean(jaccard), 2),
    SD_Jacc = round(sd(jaccard), 2),
  )

query_reformulation_data %>% 
  ggplot(aes(x = as.factor(search_term_number), y = jaccard)) + 
  geom_boxplot() + 
  facet_wrap(~ Category) +
  ggtitle("Average Jaccard based on # of queries issued by users")

query_reformulation_data %>% 
  ggplot(aes(x = search_term_number, y = avg_score)) + 
  geom_point() +
  geom_smooth(se = FALSE)

query_reformulation_data %>% 
  ggplot(aes(x = as.factor(search_term_number), y = avg_score)) + 
  geom_boxplot()

# Seems that the quality does get better with more queries issued (at least visually)
# Check again using correlation analysis
cor.test(query_reformulation_data$search_term_number, query_reformulation_data$avg_score, method = "spearman")
# Small but significant correlation


# ==== How do words used differ between Misl & True - is it more than just copying headline? ====

# 1. Look at POS Tags in their queries (from https://ghanadatastuff.com/post/text_analysis_covid/)
library(spacyr)
spacy_install()
spacy_initialize(model = "en_core_web_sm")


queries <- filtered_data$Search_Term

query_pos_tags <- spacy_parse(
  queries,
  lemma = TRUE,
  entity = TRUE,
  nounphrase = TRUE
) %>% 
  as.tibble() %>% 
  select(-sentence_id) %>%
  rename(query_number = doc_id) %>%
  mutate(
    query_number = as.numeric(gsub("text", "", query_number))
  )

query_pos_tags$Category <- filtered_data$Category[query_pos_tags$query_number]

query_pos_tags %>%
  group_by(Category, pos) %>%
  summarise(count = n()) %>%
  mutate(percentage = (count / sum(count)) * 100) %>% 
  ggplot(aes(x = pos, y = percentage, fill = Category)) + 
  geom_bar(stat="identity", position = "dodge")
# -> Generally no difference there 
# -> Only Misl uses more NOUN phrases than True, and True uses more Verbs than Misl
# -> Interestingly, True also uses more punctuation marks, indicative of copying headline (e.g. comma, end dot of sentence)

# Also do POS Tagging for headline to see similarities
headline_pos_tags <- spacy_parse(
  articles$Headline,
  lemma = TRUE,
  entity = TRUE,
  nounphrase = TRUE
) %>% 
  as.tibble() %>% 
  select(-sentence_id)

headline_pos_tags %>%
  group_by(pos) %>%
  summarise(count = n()) %>%
  mutate(percentage = (count / sum(count)) * 100) %>% 
  ggplot(aes(x = pos, y = percentage)) + 
  geom_bar(stat="identity")
# Distribution is similar to the one in the queries (not percentage wise but where the most occurrences lie, eg. NOUN, PROPN, VERB)

# 2. Look at sentiment (e.g. is avg. sentiment of True more similar to that of the Headlines)
Afinn <- read.delim("https://raw.githubusercontent.com/fnielsen/afinn/master/afinn/data/AFINN-en-165.txt", header = F) %>%
  as_tibble() %>%
  rename(word = 1,
         sentiment = 2
         )

query_sentiment <- query_pos_tags %>% 
  inner_join(Afinn, by = c("lemma" = "word"))

query_sentiment %>% 
  group_by(Category) %>% 
  summarise(
    M = round(mean(sentiment), 2),
    SD = round(sd(sentiment), 2),
  )

t.test(query_sentiment$sentiment ~ query_sentiment$Category)

headline_sentiment <- headline_pos_tags %>% 
  inner_join(Afinn, by = c("lemma" = "word"))

headline_sentiment %>% 
  summarise(
    M = round(mean(sentiment), 2),
    SD = round(sd(sentiment), 2),
  )


# TODO: Linear Mixed Effects Models
# ==== TODO: Linear Mixed Effects Models ====

library(lme4)

# Use Category as outcome -> mutate(identified = Category == "Misl")
# Then create a multilevel logistic model using glmer
filtered_data <- filtered_data %>% 
  mutate(identified = Category == "Misl")

headline.merge <- headline.merge %>% 
  mutate(
    session_id = paste(ResponseId, URL, sep = "--")
  )

# Models based on user features
multilevel.model.participant.1 = glmer(identified ~ 1 + Age + (1  | URL), data = headline.merge, family = "binomial")
multilevel.model.participant.2 = glmer(identified ~ Education_Score + (1  | URL), data = headline.merge, family = "binomial")
multilevel.model.participant.3 = glmer(identified ~ Dig_Lit_Score + (1 | URL), data = headline.merge, family = "binomial")
multilevel.model.participant.4 = glmer(identified ~ Dig_Lit_Score+Income_Score + (1 | URL), data = headline.merge, family = "binomial")
multilevel.model.participant.5 = glmer(identified ~ Dig_Lit_Score+Ideo_Congruence + (1 | URL), data = headline.merge, family = "binomial")
multilevel.model.participant.6 = glmer(identified ~ Dig_Lit_Score+Ideo_Congruence + Dig_Lit_Score:Ideo_Congruence + (1 | URL), data = headline.merge, family = "binomial")

summary(multilevel.model.participant.1) # 435.7
summary(multilevel.model.participant.2) # 434.7
summary(multilevel.model.participant.3) # 423.1 
summary(multilevel.model.participant.4) # 425.1
summary(multilevel.model.participant.5) # 416.7  
summary(multilevel.model.participant.6) # 418.6
# model 5 best: AIC: 416.7  


# models based on behaviour features
multilevel.model.behav.1 = glmer(identified ~ mean_query_length + (1 | session_id), data = headline.merge, family = "binomial")
multilevel.model.behav.1.a = glmer(identified ~ max_query_length + (1 | session_id), data = headline.merge, family = "binomial")
multilevel.model.behav.1.b = glmer(identified ~ med_query_length + (1 | session_id), data = headline.merge, family = "binomial")
multilevel.model.behav.2 = glmer (identified ~ n + (1 | session_id), data = headline.merge, family = "binomial")
multilevel.model.behav.3 = glmer(identified ~ n+mean_jaccard_q_head + (1 | session_id), data = headline.merge, family = "binomial")
multilevel.model.behav.4 = glmer(identified ~ n+max_jaccard_q_head + (1 | session_id), data = headline.merge, family = "binomial")
multilevel.model.behav.5 = glmer(identified ~ n+max_jaccard_q_head+ n:max_jaccard_q_head + (1 | session_id), data = headline.merge, family = "binomial")
multilevel.model.behav.6 = glmer(identified ~ n+max_jaccard_q_head+ n:max_jaccard_q_head + mean_median_idf + (1 | session_id), data = headline.merge, family = "binomial")
multilevel.model.behav.7 = glmer(identified ~ n+max_jaccard_q_head+ n:max_jaccard_q_head + max_median_idf + (1 | session_id), data = headline.merge, family = "binomial")
multilevel.model.behav.8 = glmer(identified ~ n+max_jaccard_q_head + max_median_idf + (1 | session_id), data = headline.merge, family = "binomial")
multilevel.model.behav.9 = glmer(identified ~ n+max_jaccard_q_head + med_median_idf + (1 | session_id), data = headline.merge, family = "binomial")

summary(multilevel.model.behav.1) # AIC: 444.8
summary(multilevel.model.behav.1.a) # AIC: 445.8
summary(multilevel.model.behav.1.b) # AIC: 444.8
summary(multilevel.model.behav.2) # AIC: 436.8
summary(multilevel.model.behav.3) # AIC: 431.8
summary(multilevel.model.behav.4) # AIC: 430.9
summary(multilevel.model.behav.5) # AIC: 431.3
summary(multilevel.model.behav.6) # AIC: 430.5
summary(multilevel.model.behav.7) # AIC: 430.0
summary(multilevel.model.behav.8) # AIC: 429.2
summary(multilevel.model.behav.9) # AIC: 432.1


## separate analysis ###
# I want to analyse the effect of biases from bias_scanner on search results
# the idea will be to create a column equivalent to List_Scores for each bias
# since the analysis showed `Ad Hominem Bias` to be strongest we will start with that

bias_counts_per_url <- sentences.df %>%
  group_by(url, bias_type) %>%
  summarise(bias_count = n()) %>%
  pivot_wider(names_from = bias_type, values_from = bias_count, values_fill = 0)



# Function to extract URLs enclosed in single quotes, handling URLs with commas inside quotes
extract_urls <- function(url_string) {
  # Extract everything between single quotes, preserving commas within quotes
  matches <- str_match_all(url_string, "'([^']*)'")[[1]][, 2]  # Extract URLs inside quotes
  return(matches)
}

# Function to look up the bias scores for a given set of URLs
get_bias_scores <- function(urls, bias_data) {
  # Extract the URLs correctly
  url_list <- extract_urls(urls)
  
  # Look up the Ad Hominem Bias for each URL
  bias_scores <- bias_data %>%
    filter(url %in% url_list) %>%
    select(`Ad Hominem Bias`) %>%
    pull()
  
  # If no URLs found, return NA
  if (length(bias_scores) == 0) {
    bias_scores <- NA
  }
  
  return(bias_scores)
}

# Function to compute the mean of bias scores, returning NA if the list is empty or all NA
compute_mean <- function(bias_scores) {
  if (length(bias_scores) == 0 || all(is.na(bias_scores))) {
    return(NA)
  } else {
    return(mean(bias_scores, na.rm = TRUE))
  }
}

# Apply the functions to create both columns
data <- data %>%
  rowwise() %>%
  mutate(
    Ad.Hom.Biases = list(get_bias_scores(ALL_URLS, bias_counts_per_url)),  # List of bias scores
    Ad.Hom.Biases_Mean = compute_mean(unlist(Ad.Hom.Biases))               # Mean of the bias scores
  ) %>%
  ungroup()  # Ensure data is no longer treated as rowwise

# let's look at whether the bias score is a good predictor for whether or not participants recognised fake news
summary_by_category <- data %>%
  group_by(Category) %>%
  summarise(
    mean_ad_hom_biases = mean(Ad.Hom.Biases_Mean, na.rm = TRUE),  # Average bias per category
    median_ad_hom_biases = median(Ad.Hom.Biases_Mean, na.rm = TRUE),  # Median bias per category
    count = n()  # Number of articles per category
  )

# View the summary
print(summary_by_category)

# Step 2: Visualize the distribution of Ad.Hom.Biases across categories using a boxplot
ggplot(data, aes(x = Category, y = Ad.Hom.Biases_Mean)) +
  geom_boxplot() +  # Create a boxplot to show distribution
  theme_minimal() +
  labs(
    title = "Distribution of Ad Hominem Bias Scores Across Categories",
    x = "Category",
    y = "Ad Hominem Bias (Mean)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability

# Ensure Category is treated as a factor (required for logistic regression)
data$Category <- as.factor(data$Category)

# Logistic regression model to predict Category based on Ad.Hom.Biases_Mean
model.1 <- glm(Category ~ Ad.Hom.Biases_Mean, data = data, family = binomial)
model.2 <-  glm(Category ~ avg_score, data = data, family = binomial)
# Summary of the model
summary(model.1)
summary(model.2)
