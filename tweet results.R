# Tweet -------------------------------------------------------------------
library(rtweet)

# Tweet daily chart -------------------------------------------------------

fname <- paste0("chart-", todayDate, ".png")
outfile <- here("output", fname)
tweetText <- "Cases nearly doubled today.  No doubt the result of increased testing. But still, jeesh."
post_tweet(tweetText, media = outfile)


# % change chart ----------------------------------------------------------

fname <- paste0("percent_change-", todayDate, ".png")
outfile <- here("output", fname)
tweetText <- "Here's the daily change since March 1."
post_tweet(tweetText, media = outfile)

# % change chart ----------------------------------------------------------
fname <- paste0("state-comparison-", todayDate, ".png")
outfile<- here("output", fname)
tweetText <- "Here's a comparison of IL to other states with greater than 100 cases"
post_tweet(tweetText, media = outfile)


