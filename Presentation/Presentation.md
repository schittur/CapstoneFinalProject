Next Word Predictor
========================================================
author: Sriram Chitturi
date: April 24, 2016

This is the final Capstone project for DataScience course

Project
========================================================

The goal of the project is to develop an algorithm to predict next word when a phrase is given.

A Shiny App should be hosted to demonstrate the algorithm as part of the project.

The App will be accepting a phrase as input and should display the next predicted word.


Datasources, limitations and assumptions 
========================================================

SwiftKey has provided files from 3 different sources, namely - blogs, news and twitter.

Limitations and Assumptions

- A sample of 30% of the dataset is used to train the model
- Due to constraint of resources only Trigrams were created and used in the model
- The model and algorithm is developed mainly keeping in view the limited memory/processing available on the Shiny App server (the application should respond in time)
- It is assumed that this project is not aimed at developing a perfect model, but to reasonably predict the next word

Model
========================================================

- the 'tm' and 'NLP' packages are used and the text is cleaned and bad words are removed
- trigrams are created the frequencies are saved to a disk file cache 
- this final cache is used on the web site to speed up the prediction

Prediction
- The given phrase is cleaned and send as input to the model
- The model traverses 2 words in the reverse at a time and searches the trigrams for the next word
- it picks up the trigram with highest frequency and display the next word
- upto 5 possible next words are displayed from the top five frequent trigrams

Shiny App
========================================================
The final Shiny App is hosted here

[Next Word Predictor](https://schittur.shinyapps.io/CapstoneApp/)
  
  
