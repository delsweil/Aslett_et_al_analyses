Hello everyone, 

I put the main data from Aslett et al in a data folder and added my code for context.

data/All_Search_Results_Combined.csv contains the search query information as well as the search results for that query.

Each row in the data.frame represents a query. Here are the columns that are most of interest to us:

ResponseId: this is a key for the participant
Category: this is how the participant classified the article after searching: Coul = “could not decide”, Misl = “the participant thought the article was fake news”, True =“the participant thought the article was trustworthy”
 
FC_Eval: this is how the fact checkers evaluated the article:  CND = “could not decide”      FM= “fake news” No Mode =“neither FM nor True”    True=“can be believed” 
 
List_Scores: this is the newsguard scores for documents which one was available
URLs: these are the URLs matching the newsguard scores             
ALL_URLS: these are all of the URLs returned as search results regardless of whether a newsguard score is there or not.
