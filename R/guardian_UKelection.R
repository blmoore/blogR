library("ggplot2")
library("scales")

## Data: http://www.theguardian.com/news/datablog/2009/oct/21/icm-poll-data-labour-conservatives#data (State of the parties spreadsheet)
# ICM poll resultsof ~1000 people taken every month or so
# (more often before an election) as well as election
# results dating from June 1984.
sop <- read.csv("data/StateOfTheParties.csv", stringsAsFactors=F)

## Data cleanup
sop[,2:5] <- apply(sop[,2:5], 2, function(x) as.numeric(gsub("%", "", x)))
sop[,1] <- as.Date(sop[,1], format="%d-%m-%Y")
colnames(sop)[1] <- "Date"

# correct for some rounding errors leading to 101/99 %
sop$rsum <- apply(sop[,2:5], 1, sum)
sop[,2:5] <- sop[,2:5] / sop$rsum

# Melt for ggplot
melt.fun <- function(x) 
  melt(x, measure.vars=c("CON", "LAB", "LIB.DEM", "OTHER"), id.vars="Date")

props <- melt.fun(sop)

# Grab election data for additional layer
elections <- sop[which(grepl("ELECTION", sop$Sample)),]
elections <- melt.fun(elections)

# Party colours (ish)
cols <- brewer.pal(8, "Pastel1")[c(2,1,6,4)]

# Fig 1. Area plot overview of public support for main parties over 30 years
svg("figures/election_f1_areaplot.svg", 8, 6)
ggplot(props, aes(x=Date, y=value, fill=variable, group=variable)) +
  geom_area() + scale_fill_manual(values=cols) +
  geom_bar(data=elections, stat="identity", position=position_stack(.1), 
           width=I(100), col="grey40") +
  scale_y_continuous(expand=c(0,0), labels=percent_format()) + 
  scale_x_date(expand=c(0,0)) + 
  labs(list(y="Public support", fill="Party", x="")) #+
  ggtitle("UK general elections 1984-2014") 
dev.off()

## Look at the time between elections
e.dates <- c(unique(elections$Date), as.Date("2015-05-07"))
results <- c(rep("CON",2), rep("LAB", 3), "CON.LIB", "?")
  
sop.rows <- which(grepl("ELECTION", sop$Sample))
e.years <- c(sub(".*(\\d{4})$", "\\1", sop[sop.rows, "Sample"], perl=T), "2015")

sop$runup <- rep(c(1:7), c(sop.rows[1], diff(c(sop.rows, nrow(sop)))))
sop$win <- rep(results, c(sop.rows[1], diff(c(sop.rows, nrow(sop)))))
e <- sop

e <- melt(e, measure.vars=c("CON", "LAB", "LIB.DEM", "OTHER"), 
            id.vars=c("runup", "Date", "win"))

e$prior <- with(e, as.numeric(Date - e.dates[runup]))
e$header <- with(e, paste0(e.years[e$runup], " (", win, ")"))

# Fn to make colours legible on white bg
darken <- function(hex.col, amount=.15)
  return(rgb(t(col2rgb(hex.col)*(1-amount)), max=255))
darken <- Vectorize(darken)
dcols <- unname(darken(cols))

# Fig 2. Previous 6 election support history
svg("figures/election_f2_electionLines.svg", 8, 6)
ggplot(subset(e, header != "2015 (?)"), 
       aes(x=prior, y=value, col=variable, fill=variable)) +
  facet_wrap(~header, scales="free_x") + 
  scale_color_manual(values=dcols) +
  scale_fill_manual(values=dcols) +
  geom_smooth(method="loess") + 
  geom_line() + geom_point() + theme_bw() +
  scale_y_continuous(labels=percent_format()) +
  labs(list(y="Public support", x="Days relative to election", 
            col="Party", fill="Party"))
dev.off()

# boxlots of election runups
ggplot(e, aes(x=header, y=value, fill=variable)) +
  theme_bw() + geom_boxplot(position=position_dodge(.8)) +
  scale_fill_manual(values=cols) +
  labs(list(y="Public support", x="Days relative to election", 
            col="Party", fill="Party")) +
  ggtitle("UK general elections 1984-2014") +
  theme(legend.position=c(.9,.85), legend.background=element_blank())

# Fig 3. Loess smooths of Labour vs. Conservative per election
svg("figures/election_f3_conLabLoess.svg", 6, 7.5)
ggplot(subset(e, variable %in% c("LAB", "CON")), 
       aes(x=prior, y=value, col=variable, fill=variable)) +
  facet_grid(header~., scales="free_y") +
  scale_color_manual(values=dcols) +
  scale_fill_manual(values=dcols) +
  geom_smooth(method="loess") + 
  scale_x_continuous(expand=c(0.01,0.01)) +
  theme_bw() + scale_y_continuous(labels=percent_format()) +
  labs(list(y="Public support", x="Days relative to election", 
            col="Party", fill="Party")) 
dev.off()
