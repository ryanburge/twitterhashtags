user <- getUser('barackobama')
user$toDataFrame()


timeline <- userTimeline('edstetzer', n=1000)
timeline <- twListToDF(timeline)
