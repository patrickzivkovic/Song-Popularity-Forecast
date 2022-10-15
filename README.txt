NLP Song Popularity Prediction Using Songlyrics

This project’s goal was to assess if it is possible to predict the popularity
of songs with information that is generally publicly available. The data was
extracted from a Kaggle-dataset that included the artist, the song name and
the corresponding lyrics. We enriched the original data set with metadata from
Spotify and, to get further predictors, used Natural Language Processing-methods
to extract song polarity and readability-indices from the song lyrics.

Further, the 552 provided genres by Spotify were narrowed down to 23 overlying
genres. The data encapsulated 598 artists on 57650 songs, with a mean of 96
songs per artist. Due to problems with missing data from Spotify and failed name
matching the dataset shrunk down to 36889 songs.

In this project 5 different models were trained and 10-fold cross validated: a
baseline model predicting the mean, a simple generalized linear model, a neural
network, a distributed random forest, a gradient boosting machine and a stacked
ensemble model. All resulting models improved the prediction accuracy compared
to the baseline model with a mean absolute error of 12.758 and showed prediction
accuracy between 11.301 and 8.721 on the cross validated data, with the best
performing model being the stacked ensemble model. The results of this project
were satisfactory but problems and ideas for improvements on the underlying
predictors and data for following projects are discussed in the conclusion.
