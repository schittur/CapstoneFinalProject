library(shiny);
library(NLP);
library(openNLP);
library(tm);
library(stringr)

load("alltrigrams.Rdat");  # load the saved trigrams as named vector
df <- data.frame(do.call('rbind', strsplit(names(alltrigrams),' ',fixed=TRUE)), 
                 count=alltrigrams, stringsAsFactors = F);

row.names(df) = NULL;
remove(alltrigrams);

badwords <- readLines("badwords.txt", encoding="UTF-8")
badwords = gsub("[^a-z ]","",badwords)
stopwords_list <- stopwords("english")

cleanSentence <- function(str) {
  str = tolower(str);
  str = gsub("[^a-z \\-]","",str)
  str = gsub("[\\-]"," ",str)
  str = gsub("\\b([a-z])+\\1\\b", " ", str);
  str = gsub("^M{0,4}(CM|CD|D?C{0,3})(XC|XL|L?X{0,3})(IX|IV|V?I{0,3})$", " ", str) # roman numerals
  str = removeWords(str, stopwords_list);
  str = removeWords(str, badwords);
  str = stripWhitespace(str);
  str
}

predict_with_2endWords <- function(words, idx) {
  if (idx > 1)
    results <- df[df$X1 == words[idx-1] & df$X2 == words[idx], ]
  else
    results <- df[df$X2 == words[idx], ];
  if (nrow(results) == 0)
    results <- df[df$X2 == words[idx], ];
  results
}

first_time = T
predict_with_trigrams <- function(str, npredict) {
  str <- cleanSentence(str)
  words <- unlist(strsplit(str, ' ', fixed = T));
  for (word_idx in length(words):1) {
    results <- predict_with_2endWords(words, word_idx)
    if (nrow(results) > 0)
      break
  }
  if (nrow(results) > 0) {
    results <- aggregate(count~X3, results, sum)
    top3results <- head(results[order(results$count, decreasing = T), ], npredict);
    top3results$X3
  } else {
    if (first_time) {
      r = "Waiting for a phrase ...."
    } else {
      if (npredict == 1)
        r = "Error: unable to predict"
      else
        r = ''
    }
    first_time <<- F
    "Waiting for a phrase ...."
  }
}

shinyServer(function(input, output) {
    output$PredictedWords <- renderText(predict_with_trigrams(input$sentence, 1));
    output$WordsFound <- renderText(predict_with_trigrams(input$sentence, 5));
    output$servertime <- renderText(format(Sys.time(), "%a %b %d %X %Y"))
})
