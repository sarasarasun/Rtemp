#first install the devtools package!

require(devtools)
install_github("sentiment140", "okugami79")

# sentiment analysis
library(sentiment)

sentiment('I love warm weather')
sentiment('I hate cold weather')

sentiments <- sentiment(cold_weather_df$Tweets)
table(sentiments$polarity)