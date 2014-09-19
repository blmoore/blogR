d <- read.csv2("http://www.ark-genomics.org/tmp/Twitter50.txt", sep="\t")

library("rCharts")

i <- dPlot(Followers ~ Citations, data=d, type="bubble",
      groups="Name", height=480, width=520)
i$yAxis(type = "addLogAxis", overrideMin=1e4)
i$xAxis(type = "addLogAxis", overrideMin=10)

# log
i

i$publish("The top 50 science stars of Twitter", host="gist")

f <- read.csv2("http://files.figshare.com/1679269/top_tweeters.txt", sep="\t")

i2 <- dPlot(Papers ~ Retweets, data=f, groups="Account",
      height=500, width=500)
i2$yAxis(type = "addLogAxis", overrideMin=1)
i2$xAxis(type = "addLogAxis", overrideMin=500)
i2

