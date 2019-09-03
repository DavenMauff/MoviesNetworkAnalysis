pacman::p_load(rjson, jsonlite, DT,  RJSONIO)

metadata = read.csv("../data-raw/movies_metadata.csv")

head(metadata$belongs_to_collection)

metadata$belongs_to_collection <- gsub("'", '"', metadata$belongs_to_collection)
head(metadata$belongs_to_collection)

belongs <- fromJSON(metadata$belongs_to_collection)
head(belongs)

ratings = read.csv('../data-raw/ratings.csv')
average <- aggregate(ratings[,3], list(ratings$movieId), mean)

 