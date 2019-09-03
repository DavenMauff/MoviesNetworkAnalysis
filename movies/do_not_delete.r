
pacman::p_load("stringr","ggplot2", "tidyr", "ngram")
options(scipen = 999)


IMDB <- read.csv("../movie_metadata.csv")



#Removing Duplicates
IMDB <- IMDB[!duplicated(IMDB), ]



#Removing White-Spaces and Special Characters
IMDB$movie_title <- gsub("Â", "", as.character(factor(IMDB$movie_title)))
str_trim(IMDB$movie_title, side = "right")



IMDB <- subset(IMDB, select = -c(genres))



colSums(sapply(IMDB, is.na))



ggplot(data = IMDB, aes(x = imdb_score, y = movie_facebook_likes)) +
  geom_point() +
  stat_smooth(method="lm")




IMDB %>% 
  filter(aspect_ratio==1.85 | aspect_ratio==2.35) %>%
  mutate(aspect_ratio=as.character((aspect_ratio))) %>%
  ggplot() +
  geom_point(aes(x=aspect_ratio, y=movie_facebook_likes))




IMDB %>% 
  separate(plot_keywords, c('word 1', 'word 2', 'word 3', 'word 4', 'word 5'), sep= "|")


keywords_split <- str_split(IMDB$plot_keywords, pattern="[|]", n=5)
keywords_matrix <- do.call(rbind, strsplit(IMDB$plot_keywords, '[|]'))
keywords_df <- as.data.frame(keywords_matrix)

names(keywords_df) <- c("one", "two", "three", "four", "five")

keywords_one_col <- gather(keywords_df) %>% 
  select(value)

keyword_freq_graph <- 
  keywords_one_col %>%
  group_by(value) %>% 
  tally() %>% 
  filter(n > 30) %>% 
  ggplot() +
  geom_bar(aes(x = value, y=n), stat="identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

keyword_freq_graph



keywords_one_col %>% 
  ggplot() +
  geom_bar(aes(x = value)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  NULL

typeof(keywords_one_col$value)



# Claris Shit:


keywords_one_col <- keywords_one_col %>% 
  mutate(wc=str_count(value, '\\s+')+1)


keywords_one_col %>% summarise(count=n())


keywords_flat <- transpose(keywords_split)


test <- keywords_one_col %>% 
  filter(value != "") %>% 
  mutate(wc = wordcount(value)) %>% 
  filter(wc <= 5) %>% 
  mutate(count=count(value)) %>% 
  
  test$wc <- lapply(keywords_one_col, wordcount)  

ggplot() +
  geom_bar(aes(x = value, y=count), stat="identity") +
  theme(axis.text.x %>% = element_text(angle = 90, hjust = 1)) 




temp <- data.frame(one="", two="", three="",four="", five="")


##################################################################################
IMDB %>% 
  group_by(budget) %>% 
  tally()



