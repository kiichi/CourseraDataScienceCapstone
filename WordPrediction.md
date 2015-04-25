Word Prediction Demo
========================================================
author: Kiichi Takeuchi
date: April 26, 2015

Background and Motivation
========================================================

Word Prediction is important text mining technique in order to speed up user's input. Especially, it's important on smartphone application because the keyboard functionalities and space are limited. The goal of this project is to build an app that takes user input text, and display predicted words next to it.

Data
========================================================

In this demo, you can choose one of three data sources: Twitter, Blog and News.
Each file has been imported and files are processed into unigram, bigram and trigram tables.
Additionally, the profanity filter has been applied before creating n-gram data set. 

Model and Smoothing
========================================================

The demo is calculating the probability of next word based on preceding words using Markov assumption.

Four major options are examined for this demo: 

* Lapsace 
* Back-off
* Interpolation
* Kneser-Ney

In this demo, I decided to implement interpolation smoothing since it's fast to compute; however, the Kneser-Ney seems beat other models at the end.


Future Work
========================================================

If I have extra time, I would definitely consider to implement Generalized Language Model (GLM) since it'll be quick improvement by creating another corpus by replacing rare occuring words with <UNK> symbol. Also, I would spend time to speed up and minize the storage size for the corpus. I can think of better data structure using hash table / index or tree which are based on O(Log) efficiency.







