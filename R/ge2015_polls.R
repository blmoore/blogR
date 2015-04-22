## ge205 polling data
library("blmR")
library("ggplot2")
library("RColorBrewer")
library("reshape2")
library("XML")

# voting intention since 2010
int <- readHTMLTable("http://ukpollingreport.co.uk/voting-intention-2", 
                     header=T, stringsAsFactors=F)[[1]]

# remove rows where all are NA (first)
int <- int[rowSums(is.na(int)) == 0,]

colnames(int) <- c("pollster", "date", "CON", "LAB", "LD",
                   "UKIP", "Green", "CON lead")

int <- int[,-ncol(int)]
int$date <- as.Date(int$date)

int <- melt(int, id.var=c("pollster", "date"))
int$value <- as.numeric(int$value)

# theme_blm from blmR / else theme_bw
theme_set(theme_blm())

# generate colour palette for parties
palette <- brewer.pal(9, "Pastel1")[c(2,1,6,4,3)]

darken <- function(colname, by=.05){
  rgb_vals <- col2rgb(colname) * (1-by)
  rgb(t(rgb_vals), max=255)
}

pdf("figures/ge2015_votingIntention.pdf", 6, 4)
ggplot(int, aes(x=date, y=value, col=variable)) +
  geom_jitter(alpha=I(.3)) +
  geom_smooth(aes(fill=variable), show_guide=F) +
  scale_color_manual(values=palette) +
  scale_fill_manual(values=palette) +
  labs(col="Party", y="Voting intention (%)", x="") +
  guides(colour=guide_legend(override.aes = list(alpha = 1, size=3),
                        direction="horizontal", keywidth=.8)) +
  theme(legend.position="top")
dev.off()  
  

# classify pollster and commisioner from a previous post:
testing <- unique(int$pollster)

sapply(rbind, strsplit(testing, "\\/"))


# commision
newspapers <- c("Sunday Times", "Times", "Scotsman on Sunday",
                "Daily Mail", "Mail on Sunday", #"Sunday Express",
                "Sun", "Daily Record")#, "Sunday Post")

#company
company <- c("Panelbase", "YouGov", "Ipsos MORI", "Survation",
             "ICM", "TNS-?BMRB", "Progressive", "Ashcroft",
             "Angus Reid")

# Questions::
# 
# MORI: How would you vote if there were a General Election held tomorrow? Would you vote… Conservative, Labour, Liberal Democrat [rotate order] or for some other party
# ICM: If there were to be a general election tomorrow which party do you think you would vote for? Conservative/Labour/Liberal Democrat/Other?
# YouGov: If there were a general election tomorrow, which party would you vote for? Conservative, Labour, Liberal Democrat, Scottish Nationalist/Plaid cymru, some other party, would not vote, don’t know
# Populus: If the general election was tomorrow, which party would you vote for? Would it be [rotate order] Conservative, Labour, Liberal Democrat, or another party – or would you not vote at all?
# Communicate Research: If there were a general election tomorrow, would you vote Conservative, Labour, Liberal Democrat or some other party? 
# NOP: If you do vote in the next general election, which party will you vote for – Conservative, Labour, Liberal Democrat, or some other party?