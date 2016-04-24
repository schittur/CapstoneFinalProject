library(shiny);
library(NLP);
library(openNLP);
library(tm);

load("alltrigrams.R")

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

# --- do not use this... rewriten the logic using dataframe
predict3Words_old <- function(str) {
  str <- cleanSentence(str)
  words = unlist(strsplit(str, ' ', fixed = T));
  firsttry = '' # this will be last 2 words, for eg. 'this is a test' = 'a test'
  secondtry = '' # this will be last word
  if (length(words) > 1) {
    firsttry = paste(paste(words[length(words)-1], words[length(words)], sep=' '), ' ', sep='')
    secondtry = paste(' ', paste(words[length(words)], ' ', sep=''), sep = '')
  }
  if (length(words) == 1)
    firsttry = paste(paste(' ', words[1], sep=''), ' ', sep='')  # there is no second try
  
  print(firsttry)
  print(secondtry)
  
  results <- alltrigrams[grep(firsttry, names(alltrigrams))];
  if (length(results) > 0) {
    results <- head(sort(results, decreasing = T),3)  # get top 3 - this will follow probability
    results <- unlist(strsplit(names(results), split=firsttry))
    
  } else {
    results <- alltrigrams[grep(secondtry, names(alltrigrams))]
    results <- head(sort(results, decreasing = T),3)  # get top 3 - this will follow probability
    print(results)
    results <- unlist(strsplit(names(results), split=secondtry))
    print(results)
    }
  results = results[results != ""]
  results
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

predict3Words <- function(str) {
  str <- cleanSentence(str)
  words <- unlist(strsplit(str, ' ', fixed = T));
  for (word_idx in length(words):1) {
      results <- predict_with_2endWords(words, word_idx)
      if (nrow(results) > 0)
        break
  }
  if (nrow(results) > 0) {
    results <- aggregate(count~X3, results, sum)
    top3results <- head(results[order(results$count, decreasing = T), ], 1);
    top3results$X3
  } else {
    "Error: unable to predict"
  }
}


printlastwords <- function(str) {
  str <- cleanSentence(str);
  words <- unlist(strsplit(str, ' ', fixed = T));
  for (c in length(words):2) {
    print(words[c])
    print(words[c-1])
  }
}

