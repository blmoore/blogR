library("dplyr")
require("MASS")
library("ggplot2")
library("plotrix")
library("quantmod")
library("rCharts")
library("RColorBrewer")
library("scales")
library("stringr")
options(scipen=99)

imdb <- read.csv("data/imdb_actionHeroes.tsv", sep="\t", 
                 colClasses=c("factor", "character"))

colnames(imdb) <- c("actor", "film", "budget",
                    "gross", "release")

extract.gross <- function(cell){
  ## Regex to get biggest dollar amount from list of
  ## grosses returned from IMDb
  if(cell != "U"){
    matches <- unlist(str_extract_all(cell, "\\$[[:digit:],]*\\w.*?;;"))
    numbers <- as.numeric( gsub(",", "", 
                               gsub("\\$([[:digit:],]*)\\w?.*", "\\1", matches)))
    est.gross <- max(numbers)
  } else {
    est.gross <- "UNKNOWN"
  }
  est.gross
}

extract.gross <- Vectorize(extract.gross)
eg <- extract.gross(imdb$gross)
names(eg) <- NULL
new.gross <- as.numeric(ifelse(eg == "-Inf" | eg == "UNKNOWN", NA, eg))

grepMoney <- function(col=c("budget", "gross")){
  ## Simply pick out the dollar amount from the IMDb data
  col <- imdb[,col]
  # Set unknowns to NA
  col[col == "U"] <- NA
  # Set non-dollar amounts to NA (assume USD)
  col[!grepl("\\$", col)] <- NA
  col <- gsub(" \\(estimated\\)", "", col)
  col <- gsub("\\$|,", "", col)
  col <- str_extract(col, "\\d+")
  return(as.numeric(col))
}

## Old method: get _first_ dollar amount:
#imdb$gross <- grepMoney("gross")
## New method: get _largest_ dollar amount:
imdb$gross <- new.gross
imdb$budget <- grepMoney("budget")

### General Cleanup ###
# try get a date
imdb$release <- gsub(".*::", "", imdb$release)

# remove end things like premiere etc.
imdb$release <- gsub("\\(.*", "", imdb$release)
imdb$release <- str_extract(imdb$release, "\\d+ [A-z]+ \\d+")
imdb$release <- as.Date(imdb$release, format="%d %b %Y")

# This cool regex changes:
#  "Chronicles of Riddick, The (2004)"  to
#  "The Chronicles of Riddick (2004)":
imdb$film <- gsub("(.*), The (\\(\\d+\\))", "The \\1 \\2", imdb$film)

# Remove rows with *any* missing data
imdb <- imdb[complete.cases(imdb),]
imdb <- imdb[!duplicated(imdb),]

# Only Enter the Dragon data left for Bruce Lee :(
imdb <- subset(imdb, actor != "Bruce Lee")

## Inflation adjustment ##
# lose john wayne as no CPI data from back then, sorry John
imdb <- subset(imdb, actor != "John Wayne")

## help from: http://stackoverflow.com/a/12591311/1274516
getSymbols("CPIAUCSL", src='FRED') 
avg.cpi <- apply.yearly(CPIAUCSL, mean)
cf <- avg.cpi/as.numeric(avg.cpi['2014']) #using 2008 as the base year

## Simplifying assumption: Majority of gross earned in year of release, 
## adjust to same inflation as budget 
inf <- as.data.frame(cf)
inf <- data.frame(year=gsub("-.*", "", rownames(inf)),
                  adj=inf$CPIAUCSL)

adj.factors <- inf[with(imdb, match(format(imdb$release, "%Y"), inf$year)),2]
adj.factors[is.na(adj.factors)] <- 1
imdb$adjusted.budget <- signif(imdb$budget / adj.factors, 4)
imdb$adjusted.gross <- signif(imdb$gross / adj.factors, 4)

plotActor <- function(name, col="red")
  # Draw bubble chart +loess of actors movie budgets over
  # time, scale relative to box office success
  ggplot(subset(imdb, actor == name), 
         aes(x=release, y=adjusted.budget/1e6)) +
  geom_line(col=I(col)) + 
  geom_point(aes(size=log10(adjusted.gross/adjusted.budget)), col=I(col)) + 
  theme_bw() +
  geom_text(aes(label=film), size=I(3.5), hjust=.5, vjust=0) + 
  theme(legend.position=c(.1,.9)) + ggtitle(name) +
  labs(y="Budget (Million 2014 US dollars)", x="", size="Gross / Budget") +
  geom_smooth(method="loess", fill=I(col), col=I(col),
              alpha=I(.1), linetype=2) +
  scale_size_continuous(range=c(2,10), limits=c(-.3, 1),
                        breaks=c(-.3, 0, .5, 1), oob=squish,
                        labels=c("Flop", "Broke even", "Success", "Commercial hit")) +
  coord_cartesian(ylim=c(-10, max(subset(imdb, actor == name)$adjusted.budget/1e6)*1.1))

alist <- unique(imdb$actor)
as.character(alist)
bp <- rep(bp, 2)

for(i in 1:length(alist)){
  # apply to list of actors in naive way
  fn <- paste0("figures/imdb_action_", gsub(" ", "_", alist[i]), ".svg")
  message(fn)
  svg(fn, 11, 6)
  print(plotActor(alist[i], col=bp[i]))
  dev.off()
}

imdb$success <- imdb$adjusted.gross / imdb$adjusted.budget

bdat <- group_by(imdb, actor) %.% summarise(overall = mean (adjusted.gross / adjusted.budget), 
                                            s.e =std.error(adjusted.gross / adjusted.budget))

bdat$actor <- as.character(bdat$actor
bdat <- arrange(bdat, overall)

pdf("figures/action_roi.pdf", 5, 4)
ggplot(bdat, aes(x=reorder(actor, overall), y=overall)) +
  geom_errorbar(aes(ymin=overall - (1.96 * s.e),
                ymax=overall + (1.96 * s.e)), width=.2) +
  geom_point(col=I("grey50"), size=I(3)) +
  labs(y="Expected ROI (multiple of budget)", x="") +
  coord_cartesian(ylim=c(0,35)) + coord_flip() + theme_minimal() +
  theme(plot.background = element_rect(fill = "transparent",colour = NA),
        panel.background = element_rect(fill = "transparent",colour = NA))
dev.off()


budget <- dPlot(adjusted.budget ~ actor, data=imdb, col="film", 
                type="bar", groups="film", height=600, width=800)
budget
budget$save("interactive/imdb_budget.html", cdn = T)

imdb.2 <- group_by(imdb, actor) %.% arrange(desc(release))
imdb.2$gross.mills <- imdb.2$adjusted.gross / 1e6
gross <- dPlot(gross.mills ~ actor, data=imdb.2, col="film",
               type="bar", groups=c("actor", "film"), height=600, width=700)
gross$yAxis(tickFormat = "#! function(d) {return '$' + d3.format(',.2f')(d)} !#",
            title="Total estimated gross (2014 USD)")
gross
gross$save('interactive/imdb_gross.html', cdn = T)

## need to cut a few for to improve visibility:
imdb.slim <- subset(imdb, !actor %in% c("Charles Bronson", "Steve McQueen",
                                        "Chuck Norris", "Clint Eastwood"))

svg("figures/imdb_bugetVgross.svg", 12, 10)
ggplot(imdb.slim, aes(y=adjusted.gross/1e6, x=adjusted.budget/1e6, col=actor)) +
  geom_point() + 
  geom_smooth(method="rlm", se=F, col=I("black"), show_guide=F) +
  theme_bw() + theme(legend.position=c(.85,.9)) +
  geom_text(aes(label=film, col=actor), show_guide=F, 
            size=I(4), hjust=0, vjust=0) +
  labs(x="Budget (million 2014 dollars)", col="",
       y="Estimated gross (million 2014 dollars)") +
  scale_color_brewer(type="qual", palette="Paired") +
  ## label abline on plot
  xlim(0, 320) + geom_abline(intercept=0, slope=1, linetype=2, show_guide=F) +
  guides(col=guide_legend(ncol=2))
dev.off()

di <- dPlot(adjusted.gross ~ adjusted.budget, data=imdb.slim,
      col="actor", type="bubble", groups=c("actor", "film"),
      height=600, width=600, legend=T)
di$yAxis(type = "addMeasureAxis")
di$xAxis(type = "addMeasureAxis")
di$save("interactive/imdb_budgetVgross.html", cdn=T)
