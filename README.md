# NLP Song Popularity Prediction Using Song Lyrics

## Project Description
This project’s goal is to assess if it is possible to predict the popularity
of songs with information that is generally publicly available. The data was
extracted from a Kaggle-dataset that included the artist, the song name and
the corresponding lyrics. The original data set was enriched with metadata from
Spotify and, to get further predictors, Natural Language Processing-methods were used
to extract song polarity and readability-indices from the song lyrics.

### Methods Used
* Descriptive Statistics
* Data Visualization
* Web Scraping
* Querying big databases
* Predictive Modelling (Regression, Random Forests, Neural Networks)

### Technologies
* R, Python
* spotifyR (for querying Spotify DBs)
* RMarkdown/knitR
* ggplot2
* Pandas,Keras, Tensorflow

### Results

In this project 5 different models were trained and 10-fold cross validated: a
baseline model predicting the mean, a simple generalized linear model, a neural
network, a distributed random forest, a gradient boosting machine and a stacked
ensemble model. All resulting models improved the prediction accuracy compared
to the baseline model with a mean absolute error of 12.758 and showed prediction
accuracy between 11.301 and 8.721 on the cross validated data, with the best
performing model being the stacked ensemble model.

The complete project report can be found in reports/FINAL_REPORT.docx
