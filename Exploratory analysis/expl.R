#EXPLORATORY ANLYSIS
library(recommenderlab)
library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(DT)
library(knitr)
library(grid)
library(gridExtra)
library(corrplot)
library(qgraph)
library(methods)
library(Matrix)

books <- fread('books.csv')
ratings <- fread('ratings.csv')
book_tags <- fread('book_tags.csv')
tags <- fread('tags.csv')


#Cleaning the dataset


ratings[, N := .N, .(user_id, book_id)]
## corresponding dplyr code
# ratings %>% group_by(user_id, book_id) %>% mutate(n=n())
cat('Number of duplicate ratings: ', nrow(ratings[N > 1]))

#first remove the duplicate ratings

ratings <- ratings[N == 1]

ratings

#Then let's remove users who rated fewer than 3 books

ratings <- ratings[N > 2]


#

set.seed(1)
user_fraction <- 0.2
users <- unique(ratings$user_id)
sample_users <- sample(users, round(user_fraction * length(users)))

cat('Number of ratings (before): ', nrow(ratings))
## Number of ratings (before):  960595
ratings <- ratings[user_id %in% sample_users]
cat('Number of ratings (after): ', nrow(ratings))


# Exploratory 1

ratings %>% 
  group_by(user_id) %>% 
  summarize(mean_user_rating = mean(rating)) %>% 
  ggplot(aes(mean_user_rating)) +
  geom_histogram(fill = "cadetblue3", color = "grey20")

#From this analysis it is clear that the most of the rating are in the range 3-5, with a mean rating of 4.

#Exploratory 2

#By looking at the books that were rated most often we can get an impression of the popularity of a book
books %>% 
  mutate(image = paste0('<img src="', small_image_url, '"></img>')) %>% 
  arrange(-ratings_count) %>% 
  top_n(10,wt = ratings_count) %>% 
  select(image, title, ratings_count, average_rating) %>% 
  datatable(class = "nowrap hover row-border", escape = FALSE, options = list(dom = 't',scrollX = TRUE, autoWidth = TRUE))



#Exploratory 3


#Plot the average rating as a function of the length of the title

books <- books %>% 
  mutate(title_cleaned = str_trim(str_extract(title, '([0-9a-zA-Z]| |\'|,|\\.|\\*)*')),
         title_length = str_count(title_cleaned, " ") + 1) 

tmp <- books %>% 
  group_by(title_length) %>% 
  summarize(n = n()) %>% 
  mutate(ind = rank(title_length))

books %>% 
  ggplot(aes(factor(title_length), average_rating, color=factor(title_length), group=title_length)) +
  geom_boxplot() + guides(color = FALSE) + labs(x = "Title length") + coord_cartesian(ylim = c(2.2,4.7)) + geom_text(aes(x = ind,y = 2.25,label = n), data = tmp)





#  We can see that there is in fact some variation in average rating depending on title length.
#Titles with 3 or 7 words seem to have slightly higher ratings.




#Exploratory 3


#The dataset that we used contains some books in different languages

p1 <- books %>% 
  mutate(language = factor(language_code)) %>% 
  group_by(language) %>% 
  summarize(number_of_books = n()) %>% 
  arrange(-number_of_books) %>% 
  ggplot(aes(reorder(language, number_of_books), number_of_books, fill = reorder(language, number_of_books))) +
  geom_bar(stat = "identity", color = "grey20", size = 0.35) + coord_flip() +
  labs(x = "language", title = "english included") + guides(fill = FALSE)

p2 <- books %>% 
  mutate(language = factor(language_code)) %>% 
  filter(!language %in% c("en-US", "en-GB", "eng", "en-CA", "")) %>% 
  group_by(language) %>% 
  summarize(number_of_books = n()) %>% 
  arrange(-number_of_books) %>% 
  ggplot(aes(reorder(language, number_of_books), number_of_books, fill = reorder(language, number_of_books))) +
  geom_bar(stat = "identity", color = "grey20", size = 0.35) + coord_flip() +
  labs(x = "", title = "english excluded") + guides(fill = FALSE)

grid.arrange(p1,p2, ncol=2)



#Exploratory 4

# Plot average rating as a function of boks having subtitle

books <- books %>% 
  mutate(subtitle = str_detect(books$title, ':') * 1, subtitle = factor(subtitle))

books %>% 
  ggplot(aes(subtitle, average_rating, group = subtitle, color = subtitle)) + 
  geom_boxplot() + guides(color = FALSE)



#We see that books that have a subtitle get rated slightly higher than books without a subtitle.












