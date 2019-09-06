
pacman::p_load("stringr","ggplot2", "tidyr", "ngram", "dplyr", "syuzhet", "plotly", "tm", "wordcloud", "sentimentr", "reshape2", "rlist", "gplots", "plsgenomics")
options(scipen = 999)

IMDB <- read.csv("../data/real_deal.csv")

#Removing Duplicates
IMDB <- IMDB[!duplicated(IMDB), ]

#Removing White-Spaces and Special Characters
IMDB$movie_title <- gsub("Â", "", as.character(factor(IMDB$movie_title)))
str_trim(IMDB$movie_title, side = "right")

IMDB <- subset(IMDB, select = -c(genres))

colSums(sapply(IMDB, is.na))

#Plotting Facebook likes against IMDB score
ggplot(data = IMDB, aes(x = imdb_score, y = movie_facebook_likes)) +
  geom_point() +
  stat_smooth(method="lm")

#Plotting Aspect Ratio against Facebook likes
IMDB %>% 
  filter(aspect_ratio==1.85 | aspect_ratio==2.35) %>%
  mutate(aspect_ratio=as.character((aspect_ratio))) %>%
  ggplot() +
  geom_point(aes(x=aspect_ratio, y=movie_facebook_likes))



keywords_split <- str_split(IMDB$plot_keywords, pattern="[|]", n=5)

keywords_matrix <- do.call(rbind, strsplit(as.character(IMDB$plot_keywords), "[|]"))

keywords_df <- as.data.frame(keywords_matrix)

names(keywords_df) <- c("one", "two", "three", "four", "five")

keywords_one_col <- gather(keywords_df) %>% 
  select(value)

keywords_one_col_freq <- keywords_one_col %>%
  group_by(value) %>%
  tally()

top_20 <- keywords_one_col_freq %>%
  select(value, n) %>%
  top_n(20)


movies_with_keywords <- data.frame()
IMDB_keyword_movie <- data.frame()

keywords_one_col %>%
  group_by(value) %>% 
  tally() %>% 
  filter(n > 30) %>% 
  ggplot() +
  geom_bar(aes(x = value, y=n), stat="identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

top_20

###### Placing Top 20 Words Against Movie Success#############################################################################################

IMDB_true_false <- IMDB 
for (keyword in top_20$value) {
  IMDB_true_false <- cbind(IMDB_true_false, ifelse(str_detect(IMDB$plot_keywords, keyword), "TRUE", "FALSE"))
}

for (i in 1:20) {
  reference <- 27
  names(IMDB_true_false)[reference + i] <- top_20$value[i]  
}

truey <- data.frame()
for (keyword in top_20$value) {
  truey <- rbind(truey, IMDB_true_false %>%
                            filter(get(keyword) == TRUE) %>% select(movie_title, gross, imdb_score, movie_facebook_likes, plot_keywords)) %>%
    distinct(movie_title, .keep_all = T)
}



falsy <- data.frame()
for (keyword in top_20$value) {
  falsy <- rbind(falsy, IMDB_true_false %>%
                            filter(get(keyword) == FALSE) %>% select(movie_title, gross, imdb_score, movie_facebook_likes, plot_keywords)) %>%
    distinct(movie_title, .keep_all = T)
}

truey <- truey %>%
  mutate(tri = "Top 20 Word")

falsy <- falsy %>%
  mutate(tri = "NOT Top 20 Word")

truey_falsy <- full_join(falsy, truey, by = c("movie_title", "gross", "imdb_score", "movie_facebook_likes", "plot_keywords"))

truey_falsy <- truey_falsy %>%
  mutate(tri = coalesce(tri.y, tri.x)) %>%
  select(movie_title, gross, imdb_score, movie_facebook_likes, plot_keywords, tri)

truey_falsy <-  truey_falsy %>%
  group_by(tri) %>%
  na.omit() %>%
  mutate(avg = mean(gross))

truey_falsy_graph <- summarise(truey_falsy, avg = mean(gross))

truey_falsy_graph %>%
  ggplot() +
  geom_bar(aes(x = tri, y=avg, fill=tri), stat="identity", position="stack") +
  theme(axis.text.x = element_blank()) 




###### Sentiment ############################################################################################################################

keywords_from_split <- data.frame(lapply(keywords_split, "length<-", max(lengths(keywords_split))))

#Creating a list of titles and years seperatored by "|"
titles <-  IMDB %>%
  select(movie_title)
years <- IMDB %>%
  select(title_year)

titles_years_list <- list()

for (x in 1:4998) {
  seperator="|"
  titles_years_list <- c(titles_years_list, paste(titles$movie_title[x], years$title_year[x], sep="|"))
}

#Renaming columns to movie names
movie_name_list <- IMDB$movie_title
names(keywords_from_split) <- titles_years_list

#Adding column to the begining
key_names <- c('key_word_1', 'key_word_2', 'key_word_3', 'key_word_4', 'key_word_5')
keywords_from_split <- cbind(test_keys = key_names, keywords_from_split)

#Moving columns to rows
keywords_from_split <- melt(keywords_from_split, id = "test_keys")

#Creating new column for year
keywords_from_split_matrix <- do.call(rbind, strsplit(as.character(keywords_from_split$variable), "[|]"))
keywords_from_split <- cbind(keywords_from_split, keywords_from_split_matrix)
names(keywords_from_split) <- c("Key #", "Combined", "Keyword", "Title", "Year")
keywords_from_split = keywords_from_split %>%
  select(`Key #`, Title, Year, Keyword)

#Replacing blank cells within keywords to NA's
keywords_from_split$Keyword[keywords_from_split$Keyword == ""] <- NA
#Removing rows containing NA's
keywords_from_split <- na.omit(keywords_from_split)

#All unique years
all_years <- keywords_from_split %>% select(Year)
all_years <- distinct(all_years)
all_years <- na.omit(all_years)
all_years <- all_years %>%
  filter(Year != "NA")

#Function for sentiment per year
yearly_sentiment <- function(year, df) {
  amount <- nrow(df %>%
    select(Year) %>%
    filter(Year == year))
  df <- df %>%
    filter(Year == year)
  sentiments <- get_nrc_sentiment(as.character(df[4]))
  for (i in 1:length(sentiments)) {
    sentiments[i] <- sentiments[i]/amount
    print(i)
  }
  year_sentiment <- cbind(year, sentiments)
  return (year_sentiment)
}

sentiments <- data.frame()

#For-loop to capture all years
for (i in all_years$Year) {
  sentiments <- rbind(sentiments,yearly_sentiment(i, keywords_from_split))
}

#Making years integers
sentiments$year <- strtoi(sentiments$year)

#Sort by year
sentiments <- sentiments[with(sentiments, order(year)), ]
#sentiments <- sentiments[order(year), ]

#Heatmap for sentiments
rnames <- sentiments[,1]
mat_sentiments <- data.matrix(sentiments[,2:ncol(sentiments)])
rownames(mat_sentiments) <- rnames
mat_sentiments <- t(mat_sentiments)

df_sentiment <-  as.data.frame(mat_sentiments)
names_emotions <- c("anger", "anticipation", "disgust","fear","joy","sadness","surprise","trust","negative","positive")

sentiments_graph <- cbind(names_emotions, df_sentiment)

#Run if there is a problem with heatmap
dev.off()

#Heatmap
heatmap.2(mat_sentiments, Rowv=NA, Colv=NA, scale="row", col=colorRampPalette(c("white","darkblue")),  margins=c(5,10), trace = "none")

#Filtering sentiments graph
sentiments_graph_filter <- sentiments %>%
  filter(year >= 2000)

#Other graphs
# Showing trends between sentiment from the 2000's 
ggplot(sentiments_graph_filter, aes(x = as.numeric(year), y = anger)) + 
  geom_point(alpha = 0.5) + 
  geom_line()
ggplot(sentiments_graph_filter, aes(x = as.numeric(year), y = anticipation)) + 
  geom_point(alpha = 0.5) + 
  geom_line()
ggplot(sentiments_graph_filter, aes(x = as.numeric(year), y = disgust)) + 
  geom_point(alpha = 0.5) + 
  geom_line()
ggplot(sentiments_graph_filter, aes(x = as.numeric(year), y = fear)) + 
  geom_point(alpha = 0.5) + 
  geom_line()
ggplot(sentiments_graph_filter, aes(x = as.numeric(year), y = joy)) + 
  geom_point(alpha = 0.5) + 
  geom_line()
ggplot(sentiments_graph_filter, aes(x = as.numeric(year), y = sadness)) + 
  geom_point(alpha = 0.5) + 
  geom_line()
ggplot(sentiments_graph_filter, aes(x = as.numeric(year), y = surprise)) + 
  geom_point(alpha = 0.5) + 
  geom_line()
ggplot(sentiments_graph_filter, aes(x = as.numeric(year), y = trust)) + 
  geom_point(alpha = 0.5) + 
  geom_line()
ggplot(sentiments_graph_filter, aes(x = as.numeric(year), y = negative)) + 
  geom_point(alpha = 0.5) + 
  geom_line()
ggplot(sentiments_graph_filter, aes(x = as.numeric(year), y = positive)) + 
  geom_point(alpha = 0.5) + 
  geom_line()


##############################################################################################################################################


