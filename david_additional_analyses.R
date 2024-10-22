
# Attain the number of queries per participant (each query has one line in the dataset)
overview = filtered_data %>%
  group_by(ResponseId) %>% 
  count()

# join the tables to have a count per participant plus the rest of their data.
merged.overview =   left_join(overview %>% group_by(ResponseId) %>% mutate(rid = row_number()),
                                filtered_data %>% group_by(ResponseId) %>% mutate(rid = row_number()), 
            by = c("ResponseId", "rid"))  
  
# visualise and test if successful participants submitted more queries
boxplot(merged.overview$n~merged.overview$Category)
t.test(merged.overview$n~merged.overview$Category)  


# Gain the Jaccard between query and headline
headline.merge = left_join(filtered_data,articles, by = "URL")

# Function to calculate Jaccard coefficient
calculate_jaccard <- function(query, headline) {
  set1 <- strsplit(tolower(query), " ")[[1]]
  set2 <- strsplit(tolower(gsub("[[:punct:]]", "", headline)), " ")[[1]]
  intersection <- length(intersect(set1, set2))
  union <- length(union(set1, set2))
    
  # print(intersection)
  # print(union)
  # print("========")
  
  jaccard_coefficient <- intersection / union
  return(jaccard_coefficient)
}

# Traverse the dataframe and calculate Jaccard coefficient for each row
jaccard_q_headline = c()
for (i in 1:nrow(headline.merge)) {
  text1 <- as.character(headline.merge[i, "Search_Term"])
  text2 <- as.character(headline.merge[i, "Headline"])
  #remove punctuation from the headline
  text2 <- gsub("[[:punct:]]", "", text2)
  
  jaccard_coefficient <- calculate_jaccard(text1, text2)
  jaccard_q_headline=c(jaccard_q_headline,jaccard_coefficient)
}

headline.merge$jaccard_q_headline = jaccard_q_headline

# visualise and test if successful participants submitted more queries
boxplot(headline.merge$jaccard_q_headline~headline.merge$Category)
t.test(headline.merge$jaccard_q_headline~headline.merge$Category) 


# I am interested to know if people did copy in terms from the headline, how likely they were to uncover the article to be fakenews.

cutoff <- quantile(headline.merge$jaccard_q_headline,.85)

copiers <- headline.merge %>%
  filter(jaccard_q_headline > cutoff) %>%
  distinct(ResponseId, .keep_all = TRUE) %>%
  group_by(Category) %>% 
  summarize(count = n(), .groups = "drop")

chisq.test(copiers$count)

copiers.to.view <- headline.merge %>%
  filter(jaccard_q_headline > cutoff) %>%
  distinct(ResponseId, .keep_all = TRUE) %>%
  select(c(ResponseId,Search_Term,Headline))


# Visualize distribution based on different cutoff values
cutoff_thresholds <- seq(0.5, 1, by = 0.05) 

# Initialize an empty data frame to store results
jaccard_q_headline_df <- list()

# Loop through cutoff thresholds
for (threshold in cutoff_thresholds) {
  cutoff <- quantile(headline.merge$jaccard_q_headline, threshold)
  
  copiers <- headline.merge %>%
    filter(jaccard_q_headline > cutoff) %>%
    distinct(ResponseId, .keep_all = TRUE) %>%
    group_by(Category) %>% 
    summarize(count = n(), .groups = "drop")
  
  # Store the results in the list with the threshold as a name
  jaccard_q_headline_df[[as.character(threshold)]] <- copiers
}

# Combine the list of data frames into a single data frame
jaccard_q_headline_df <- bind_rows(jaccard_q_headline_df, .id = "threshold") %>%
  mutate(threshold = as.numeric(threshold))

jaccard_q_headline_df <- jaccard_q_headline_df %>%
  group_by(threshold) %>%
  mutate(percentage = count / sum(count) * 100)

# Plotting using ggplot2
ggplot(jaccard_q_headline_df, aes(x = as.factor(threshold), y = percentage, fill = Category)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Jaccard of Query and Headline based on different Thresholds",
       x = "Threshold",
       y = "Percentage") +
  scale_y_continuous(labels = scales::percent_format(scale = 1))



# modelling:
headline.merge <- filtered_data %>%
  mutate(identified = Category=='Misl') %>%
  mutate(jaccard_q_headline = jaccard_q_headline)


# add the number of queries in session (n) from merged.overview
headline.merge =   left_join(headline.merge,merged.overview %>%
                            select(c("ResponseId","n")), by = c("ResponseId"))


# we need to perform the modelling at a per document level.
headline.merge  <- headline.merge %>%
  group_by(ResponseId) %>%
  summarise(mean_query_length = mean(query_length, na.rm = TRUE),
            med_query_length = median(query_length, na.rm = TRUE),
            max_query_length = max(query_length, na.rm = TRUE),
            mean_median_idf =  mean(median_idf, na.rm = TRUE),
            med_median_idf =  median(median_idf, na.rm = TRUE),
            max_median_idf = max(median_idf, na.rm = TRUE),
            mean_jaccard_q_head = mean(jaccard_q_headline, na.rm=TRUE),
            med_jaccard_q_head = median(jaccard_q_headline, na.rm=TRUE),
            max_jaccard_q_head = max(jaccard_q_headline, na.rm=TRUE),
            across(c(Category,Age,Gender, FC_Eval,List_Scores,avg_score, Ideo_Congruence,Income_Score,Education_Score,Dig_Lit_Score,  
                   URL,identified,n), first))
            

# models based on user features
model.participant.1 = glm(identified ~ Age, data = headline.merge, family = "binomial")
model.participant.2 = glm(identified ~ Education_Score, data = headline.merge, family = "binomial")
model.participant.3 = glm(identified ~ Dig_Lit_Score, data = headline.merge, family = "binomial")
model.participant.4 = glm(identified ~ Dig_Lit_Score+Income_Score, data = headline.merge, family = "binomial")
model.participant.5 = glm(identified ~ Dig_Lit_Score+Ideo_Congruence, data = headline.merge, family = "binomial")
model.participant.6 = glm(identified ~ Dig_Lit_Score+Ideo_Congruence + Dig_Lit_Score:Ideo_Congruence, data = headline.merge, family = "binomial")
# model.participant.5 is the strongest model with an AIC of 422.47

# models based on behaviour features
model.1 = glm(identified ~ mean_query_length, data = headline.merge, family = "binomial")
model.1.a = glm(identified ~ max_query_length, data = headline.merge, family = "binomial")
model.1.b = glm(identified ~ med_query_length, data = headline.merge, family = "binomial")
model.2 = glm (identified ~ n, data = headline.merge, family = "binomial")
model.3 = glm(identified ~ n+mean_jaccard_q_head, data = headline.merge, family = "binomial")
model.4 = glm(identified ~ n+max_jaccard_q_head, data = headline.merge, family = "binomial")
model.5 = glm(identified ~ n+max_jaccard_q_head+ n:max_jaccard_q_head, data = headline.merge, family = "binomial")
model.6 = glm(identified ~ n+max_jaccard_q_head+ n:max_jaccard_q_head + mean_median_idf, data = headline.merge, family = "binomial")
model.7 = glm(identified ~ n+max_jaccard_q_head+ n:max_jaccard_q_head + max_median_idf, data = headline.merge, family = "binomial")
model.8 = glm(identified ~ n+max_jaccard_q_head + max_median_idf, data = headline.merge, family = "binomial")
model.9 = glm(identified ~ n+max_jaccard_q_head + med_median_idf, data = headline.merge, family = "binomial")
# model.8 is the strongest model with an AIC of 429.02

# models combining features








