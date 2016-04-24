library(tm);
library(RWeka);
library(slam)

cleaned_dir <- "H://Datascience/Capstone/final/cleaned_en_US";

TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))

prepareTrigrams <- function(filename) {
  text <- readLines(paste(cleaned_dir, filename, sep="/"), encoding="UTF-8")
  
  corpus <- Corpus(VectorSource(text));
  corpus <- tm_map(corpus, PlainTextDocument);
  
  tdm3 <-TermDocumentMatrix(corpus, control=list(tokenize = TrigramTokenizer, weighting=weightTf));
  tdm3 <- rollup(tdm3, 2, na.rm=TRUE, FUN = sum);
  tdm3_matrix <- rowSums(as.matrix(tdm3));
  
  remove(corpus)
  remove(tdm3)
  
  tdm3_matrix <- tdm3_matrix[which(tdm3_matrix > 1)]
  tdm3_matrix
}

tdm3_matrix_blogs <- prepareTrigrams("en_US.blogs.txt")
tdm3_matrix_news <- prepareTrigrams("en_US.news.txt") 
tdm3_matrix_twitter <- prepareTrigrams("en_US.twitter.txt")

alltrigrams <- c(tdm3_matrix_blogs, tdm3_matrix_news, tdm3_matrix_twitter)

remove(tdm3_matrix_blogs)
remove(tdm3_matrix_news)
remove(tdm3_matrix_twitter)

alltrigrams <- rowsum(alltrigrams, group=names(combined))
alltrigrams <- alltrigrams[, 1]  # convert into a vector

rdata_dir = "H:/Datascience/Capstone/final/rdata"
save(alltrigrams, file=paste(rdata_dir, "alltrigrams.R", sep="/"))
