library(NLP);
library(openNLP);
library(tm);
library(RWeka);
library(SnowballC);
library(foreach)

corpus_dir <- "H://Datascience/Capstone/final/en_US";
train_dir <- "H://Datascience/Capstone/final/train_en_US";
test_dir <- "H://Datascience/Capstone/final/test_en_US";
cleaned_dir <- "H://Datascience/Capstone/final/cleaned_en_US";
badwords_file <- "H:/Datascience/Capstone/final/badwords.txt"
sample_file <- "H:/Datascience/Capstone/final/sample/en_US.blogs.txt"

badwords <- readLines(badwords_file, encoding="UTF-8")
badwords = gsub("[^a-z ]","",badwords)
stopwords_list <- stopwords("english")

cleanLines <- function(in_file_path, fname, train_pct) {
  # print(in_file_path);
  sampleLines <- scan(in_file_path, what="character", sep="\n");
  proceed <- length(sampleLines) > 0;
  print(proceed);
  sampleLines <- sample(sampleLines, length(sampleLines) * train_pct) # consider 20 percent sample
  if (proceed) {
    # print("Inside IF")
    sampleLines <- lapply(sampleLines, function(x) { iconv(x, 'UTF-8', 'ASCII', "byte") })
    sampleLines = tolower(sampleLines);
    sampleLines = gsub("[^a-z \\-]","",sampleLines)
    sampleLines = gsub("[\\-]"," ",sampleLines)
    sampleLines = gsub("\\b([a-z])+\\1\\b", " ", sampleLines);
    # sampleLines = gsub("\\b.\\b", " ", sampleLines)
    sampleLines = gsub("^M{0,4}(CM|CD|D?C{0,3})(XC|XL|L?X{0,3})(IX|IV|V?I{0,3})$", " ", sampleLines) # roman numerals
    sampleLines = removeWords(sampleLines, stopwords_list);
    sampleLines = removeWords(sampleLines, badwords);
    sampleLines = stripWhitespace(sampleLines);
    out_fpath = paste(cleaned_dir, fname, sep="/");
    # print(out_fpath)
    conn <- file(out_fpath, open="w");
    # print("connection created");
    writeLines(sampleLines, con=conn);
    close(conn);
  }
  print(length(sampleLines));
  proceed # this will return TRUE or FALSE
}

createSampleCorpus <- function(train_pct) {
  filenames = list.files(corpus_dir);
  foreach(filename = filenames) %do% {
    in_fpath = paste(corpus_dir, filename, sep="/")
    #     out_fpath = paste(train_dir, filename, sep="/")
    #     print(in_fpath)
    #     print(out_fpath)
    
    # fromline = 1;
    if (filename == "en_US.news.txt")
      cleanLines(in_fpath, filename, 1.0)
    else
      cleanLines(in_fpath, filename, train_pct)
  }
}

createSampleCorpus(0.30)