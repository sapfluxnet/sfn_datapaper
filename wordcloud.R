
```{r wordclouds_treatments, echo = FALSE, fig.height=7, fig.width=7, message=FALSE, warning=FALSE}
library(wordcloud)
library(wordcloud2)
library(tm)
library(gridGraphics)

set.seed(63025)

# plant wordcloud
corpus_processed <-
  sfn_allplants_tax %>%
  pull(pl_treatment) %>%
  VectorSource() %>%
  Corpus() %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords("english")) %>%
  TermDocumentMatrix() %>%
  as.matrix() %>%
  rowSums() %>%
  sort(decreasing = TRUE)

wordcloud_data <- tibble(
  word = names(corpus_processed),
  freq = corpus_processed
)

wordcloud_plant <- function() {
  wordcloud(
    wordcloud_data[['word']], wordcloud_data[['freq']],
    min.freq = 1,
    max.words = 100,
    random.order = TRUE, rot.per = 0,
    scale = c(2.5, 0.5)
  )
}

# stand wordcloud
corpus_processed_stand <-
  sfn_allstands %>%
  pull(st_treatment) %>%
  VectorSource() %>%
  Corpus() %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords("english")) %>%
  TermDocumentMatrix() %>%
  as.matrix() %>%
  rowSums() %>%
  sort(decreasing = TRUE)

wordcloud_data_stand <- tibble(
  word = names(corpus_processed_stand),
  freq = corpus_processed_stand
)


wordcloud_stand <- function() {
  wordcloud(
    wordcloud_data_stand[['word']], wordcloud_data_stand[['freq']],
    min.freq = 1,
    max.words = 100,
    random.order = TRUE, rot.per = 0.15,
    scale = c(2.5, 0.25)
  )
}

cowplot::plot_grid(
  wordcloud_plant, wordcloud_stand,
  nrow = 2, labels = c('Plant treatments', 'Stand treatments'),
  scale = c(1.2, 1.2)
)
```
**Figure S5.**