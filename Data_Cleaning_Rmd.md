---
layout: page
title: Data Cleaning and Exploration Code
subtitle: In R
use-site-title: true
---


```r
knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE, cache = TRUE, eval = FALSE)
library(data.table); library(knitr); library(kableExtra); library(dplyr); library(ggplot2)
setwd('/Users/maraudersmap/Desktop/Bayes')
```

## Merge Bechdel Data with IMDb Dataset and check 

```r

bechdel = fread('all_bechdel.csv')
keep = c('tconst','primaryTitle','isAdult','startYear',
         'runtimeMinutes','genres', 'titleType')
imdb = fread('title.basics.tsv', select = keep, fill = TRUE)


imdb_mv = imdb[titleType == 'movie']


bechdel[,tconst := imdbid]

bechdel_mv = merge(bechdel, imdb_mv, by = 'tconst')

#fwrite(data, file = 'bechdel_full.csv')

```

## Check against Top Rated Movies in IMDb

##### BarCharts to check top IMDb movies in Bechdel Dataset

```r

ratings = fread('title.ratings.tsv')

ratings = merge(imdb_mv, ratings, by = 'tconst')
ratings = ratings[,.(tconst, primaryTitle, startYear, averageRating, numVotes)]

bechdel_ratings = merge(bechdel_mv, ratings, by = 'tconst')

bechdel_ratings = bechdel_ratings[,.(tconst, rating, title, primaryTitle = primaryTitle.x, 
                                     startYear = startYear.x, runtimeMinutes,
                                    genres, titleType, averageRating, numVotes)]


decade_code = function(data){
  data[startYear %like% '189' , decade := '1890']
  data[startYear %like% '190' , decade := '1900-1920']
  data[startYear %like% '191' , decade := '1900-1920']
  data[startYear %like% '192' , decade := '1900-1920']
  data[startYear %like% '193' , decade := '1930']
  data[startYear %like% '194' , decade := '1940']
  data[startYear %like% '195' , decade := '1950']
  data[startYear %like% '196' , decade := '1960']
  data[startYear %like% '197' , decade := '1970']
  data[startYear %like% '198' , decade := '1980']
  data[startYear %like% '199' , decade := '1990']
  data[startYear %like% '200' , decade := '2000']
  data[startYear %like% '201' , decade := '2010']

}

bechdel_mv = decade_code(bechdel_mv)
ratings = decade_code(ratings)

sum_rate = ratings %>% group_by(decade) %>% 
  summarize(mean=mean(numVotes), max=max(numVotes),
            median = median(numVotes), Q3 = quantile(numVotes, 0.75))

ratings[, Q3 := quantile(numVotes, 0.75), .(decade)]
ratings[, mean := mean(numVotes), .(decade)]
ratings[,order(-numVotes), by = decade]

top_mv = ratings[order(-numVotes), head(tconst, 500), by = decade][, .(decade, tconst = V1)]


sample_check = merge(top_mv, bechdel_mv, by = c('tconst', 'decade'))

top_mv_n = top_mv[,.N, by = decade][,.(decade, imdb_cnt = N)]

sample_check_n = sample_check[,.N, by = decade][,.(decade = decade, bechdel_cnt = N)]
sample_check_n = merge(top_mv_n, sample_check_n, by = 'decade')
sample_check_n[,per := (bechdel_cnt/imdb_cnt) * 100]


sample_check_n = sample_check_n[,.(Decade = decade, `IMDb N` = imdb_cnt, 
                                   `Bechdel N`= bechdel_cnt , `% Captured` = per)]

kable(sample_check_n) %>% 
  add_header_above(c('% of top IMDb movies captured \n in Bechedl Dataset by Decade' = 4)) %>%
  kable_styling('striped', full_width = FALSE, font_size = 10) %>% 
  footnote(symbol = 'Top IMDb movies are chosen as those \n with total number of rating votes \n 
           greater than the average number of votes \n in that decade')

sample_check_n[, per_not := 100 - `% Captured`]
sample_check_n[, IMDb_not := `IMDb N` - `Bechdel N`]
sc_long = melt(sample_check_n, id.vars = c('Decade'), measure.vars = c('IMDb_not','Bechdel N'))
sc_long[,prop := round(value/sum(value), 2), by = c('Decade')]

sc_long[,`:=`(Dataset = variable, `Number of Movies` = value, `Proportion of Movies` = prop, 
              `Percentage of Movies` = paste0(prop*100, '%' ))]
sc_long[variable == 'IMDb_not', Dataset := 'IMDb only']
sc_long[variable == 'Bechdel N', Dataset := 'IMDb and Bechdel']


ggplot(data = sc_long, aes(x = Decade, y = `Number of Movies`, fill = Dataset) ) + 
  geom_bar(stat = 'identity')+ 
    geom_text(aes(label = `Percentage of Movies`), 
              position = position_stack(vjust = 0.5), size = 3)

ggplot(data = sc_long, aes(x = Decade, y = `Proportion of Movies`, fill = Dataset) ) + 
  geom_bar(stat = 'identity')

```

#### Barcharts to check amount of Bechdel data in IMDb data  

```r

bechdel_pop = sample_check_n[,.(Decade, `Bechdel N`)]

bechdel_comp = merge(bechdel_pop, decade_cnt, 
                     by = 'Decade')[, .(Decade, Pop_count = `Bechdel N`, full_count = Count)]

bechdel_comp[, per_pop := (Pop_count/full_count) ]
bechdel_comp[, per_left := 1 - per_pop]
bechdel_comp[, left_count := full_count - Pop_count]
bechdel_comp_l = melt(bechdel_comp, id.vars = 'Decade', 
                      measure.vars = c('left_count','Pop_count'))

bechdel_comp_l[,per := round(value/sum(value), 2), by = c('Decade')]

bechdel_comp_l[, `:=`(`In Top 500 IMDb` = variable, 
                      `Number of Movies` = value,
                      `Proportion of Movies` = per,
                      `Percentage of Movies` = paste0(per*100, '%' ) )]
bechdel_comp_l[variable == 'left_count',`In Top 500 IMDb` := 'No' ]
bechdel_comp_l[variable == 'Pop_count',`In Top 500 IMDb` := 'Yes' ]

ggplot(data = bechdel_comp_l, aes(x = Decade, 
                                  y = `Number of Movies`, 
                                  fill = `In Top 500 IMDb`) ) + 
  geom_bar(stat = 'identity')+ 
    geom_text(aes(label = `Percentage of Movies`), 
              position = position_stack(vjust = 0.5), size = 3) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data = bechdel_comp_l, aes(x = Decade, y = `Proportion of Movies`,
                                  fill = `In Top 500 IMDb`) ) + geom_bar(stat = 'identity')

```

## Data

```r
#original (cleaned) data
bechdel = read.csv("~/Desktop/Stats 551/bechdel_cleaned.csv")
bechdel$pass = ifelse(bechdel$rating == 3, 1, 0)
bechdel$notpass = ifelse(bechdel$pass == 1, 0, 1)

#genre summary data (from Sam's work)
genre_passed = read.csv("~/Desktop/Stats 551/genre_passed.csv")
genre_passed = genre_passed[-23,]
names(genre_passed) = c("genre","num","num_passed")
genre_passed$perc = genre_passed$num_passed/genre_passed$num
genre_passed$size = ifelse(genre_passed$num >= 1000, 1, 
                           ifelse(genre_passed$num >= 500, 2, 3))

#single genre for each movie
genre_unique = read.csv("~/Desktop/Stats 551/genre_unique.csv")
genre_unique$scale_year <- scale(genre_unique$year)
```

##  Figures

#### Pass/fail barchart

```r
plot2 <- ggplot(data = bechdel, aes(pass, fill = as.factor(rating)))+
  geom_bar()+
  theme_linedraw()+
  labs(fill = "Number of criteria met", 
       x= "Fail                                           Pass", 
       y = "Number of Movies")
plot2
```

#### Pass/Fail Barcharts - by decade

```r
plot5 <- ggplot(data = bechdel, aes(x = "",fill = as.factor(pass)))+
  geom_bar(position = "fill")+
  theme_linedraw()+
  facet_wrap(~decade)+
  labs(fill = "Pass", x = "",y = "Proportion")
plot5
```

#### Proportion By Genre - split by size

```r
genre_passed %>%
  filter(size == 3)%>%
  ggplot(aes(x = genre, y = perc))+
  geom_point(colour = "purple", aes(size = num))+
  theme_linedraw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(y = "Proportion of movies that pass",
       x = "Genre", size = "Number of movies",
       title = "Genres with 0 to 499 movies")+
  ylim(0.2,0.8)

genre_passed %>%
  filter(size == 2)%>%
  ggplot(aes(x = genre, y = perc))+
  geom_point(colour = "blue", aes(size = num))+
  theme_linedraw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(y = "Proportion of movies that pass",
       x = "Genre", size = "Number of movies",
       title = "Genres with 500 to 999 movies")+
  ylim(0.2,0.8)

genre_passed %>%
  filter(size == 1)%>%
  ggplot(aes(x = genre, y = perc))+
  geom_point(color = "darkgreen", aes(size = num))+
  theme_linedraw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(y = "Proportion of movies that pass",
       x = "Genre", size = "Number of movies",
       title = "Genres with 1000 or more movies")+
  ylim(0.2,0.8)
```

#### Prop By Year - recreate bechdel.com plot

```r
ggplot(bechdel, aes(startYear, fill = as.factor(rating))) +
  geom_histogram(binwidth = 1, position = "fill")+
  theme_linedraw()+
  labs(x = "Year",y = "Proportion of movies", 
       fill = "Number of criteria met")
```

#### Genre Counts

```r
genre_passed2 = genre_passed %>%
  arrange(num)
plot9 = ggplot(data = genre_passed2, aes(x = genre,y = num))+
  geom_bar(stat = "identity",fill = "purple")+
  theme_linedraw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_x_discrete(limits = genre_passed2$genre)+
  labs(x = "Genre",y = "Number of Movies")
plot9
```

## Kable Tables

#### Snapshot of data frame

```r
snapshot = head(cbind(as.character(bechdel$primaryTitle),
                      bechdel$rating, bechdel$startYear,
                      bechdel$decade, bechdel$Action,
                      bechdel$Adventure, bechdel$Comedy,
                      bechdel$Drama))

columnnames = c("Title","Number of criteria met","Year", 
                "Decade","Action","Adventure","Comedy","Drama")
kable(snapshot,col.names = columnnames)
```


#### Table of Decade Counts

```r
columnnames = c("Decade","Movie Count")
kable(table(bechdel$decade),col.names = columnnames)
```

