d <- read.csv2("http://www.ark-genomics.org/tmp/Twitter50.txt", sep="\t")

library("rCharts")

i <- dPlot(Followers ~ Citations, data=d, type="bubble",
      groups="Name", height=480, width=520)
i$yAxis(type = "addLogAxis", overrideMin=1e4)
i$xAxis(type = "addLogAxis", overrideMin=10)

# log
i


i$publish("The top 50 science stars of Twitter", host="gist")
