

# ----------------------------------------------------------------------------------------------------
# process sample corpus


##------- running out of memory ------------------- so process seperately -------------------

library(e1071)

rdata_dir = "H:/Datascience/Capstone/final/rdata"
load(paste(rdata_dir, "trigramdf.R", sep="/"))
nb_model <- naiveBayes(w ~ f1+f2, trigram_df)


######## ----- use till here ------------------------------------------------------------



# -------------------------------------------------------------------------------------------------


allwords <- scan("H:/Datascience/Capstone/final/cleaned_en_US/en_US.news.txt", what="character");
wordcount1 <- table(allwords);
allwords <- scan("H:/Datascience/Capstone/final/cleaned_en_US/en_US.twitter.txt", what="character");
wordcount2 <- table(allwords);
allwords <- scan("H:/Datascience/Capstone/final/cleaned_en_US/en_US.blogs.txt", what="character");
wordcount3 <- table(allwords);
remove(allwords);

wordc <- rowSums(cbind(wordcount1, wordcount2, wordcount3));
wordc25 <- wordc[which(wordc < 25)];

# --- now remove these words with frequency less than 25 in all combined documents
less_freq_words = names(wordc25);

removeLessFreqWords <- function(filename) {
  file_path = paste(cleaned_dir, filename, sep="/")
  cleanLines <<- scan(file_path, what="character", sep="\n");
  for (i in 1:as.integer(length(less_freq_words)/1000) + 1)
    cleanLines <<- removeWords(cleanLines, less_freq_words[i*1000:(i+1)*1000]);

  conn <- file(paste(file_path, "_cleaned.txt"), open="w");
  writeLines(cleanLines, con=conn);
  close(conn);
  # remove(cleanLines);
}

removeLessFreqWords("en_US.news.txt");
removeLessFreqWords("en_US.blogs.txt");
removeLessFreqWords("en_US.twitter.txt");





### ---------------------------------- sample created -------------------------------

train_dir <- "H://Datascience/Capstone/final/train_en_US";
corpus <- Corpus(DirSource(train_dir));
corpus <- tm_map(corpus, PlainTextDocument);
TrigramTokenizer <- function(x) { gc(); NGramTokenizer(x, Weka_control(min = 3, max = 3)); }
tdm3 <-TermDocumentMatrix(corpus, control=list(tokenize = TrigramTokenizer, weighting=weightTf));
tdm3_matrix <- rowSums(as.matrix(tdm3));
remove(tdm3);
remove(corpus);
remove(train_dir);

### -------------- 3 gram matrix created






removeSpecialChars <- function(x) gsub("[^a-z ]","",x);
delRepeatedWords <- function(x) gsub("(.)\1+","",x);
delOnlyRepeating <- function(x) gsub(pattern = "\\b([a-z])+\\1\\b", replacement = "", x=x);

readSampleCorpus <- function() {
  corpus <- Corpus(DirSource(train_dir));
  corpus <- tm_map(corpus, PlainTextDocument);
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, stemDocument)
  corpus <- tm_map(corpus, removeSpecialChars)
  corpus <- tm_map(corpus, removeWords, stopwords("english"));
  corpus <- tm_map(corpus, PlainTextDocument);
  corpus <- tm_map(corpus, delOnlyRepeating)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, PlainTextDocument);
  corpus
}

corpus <- readSampleCorpus()



BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))

tdm2 <-TermDocumentMatrix(corpus, control=list(tokenize = BigramTokenizer, weighting=weightTf))
tdm3 <-TermDocumentMatrix(corpus, control=list(tokenize = TrigramTokenizer, weighting=weightTf))
tdm2_matrix <- rowSums(as.matrix(tdm2))
tdm3_matrix <- rowSums(as.matrix(tdm3))
