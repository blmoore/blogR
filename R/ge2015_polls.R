## ge205 polling data
library("blmR")
library("dplyr")
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

# darken using blmR::darken_col
palette <- darken_col(palette, by=.1)

#pdf("figures/ge2015_votingIntention.pdf", 6, 4)
ggplot(int, aes(x=date, y=value, col=variable)) +
  geom_jitter(alpha=I(.3), size=I(1.4)) +
  geom_smooth(aes(fill=variable), col=I("grey65"), show_guide=F) +
  scale_color_manual(values=palette) +
  scale_fill_manual(values=palette) +
  labs(col="", y="Voting intention (%)", x="") +
  guides(colour=guide_legend(override.aes = list(alpha = 1, size=3),
                        direction="horizontal", keywidth=.8)) +
  theme(legend.position="top")
#dev.off()   

# commisioned by...
newspapers <- c("Sunday Times", "Sun", "Times", "Independent",
                "Independent on Sunday", "Evening Standard",
                "Sunday Times", "Observer", "Sunday Telegraph",
                "Mirror", "Mail on Sunday")

int$newspaper <- NA

for(n in newspapers){
  int$newspaper[which(grepl(paste0("/\\b", n, "\\b$"), int$pollster))]  <- n
}

int$newspaper <- as.factor(int$newspaper)
counts <- int %>% group_by(newspaper) %>% tally()
pick <- counts[which(counts$n > 150),]$newspaper

ggplot(subset(int, newspaper %in% pick),
       aes(x=date, y=value, col=newspaper, group=newspaper)) +
  geom_jitter(alpha=I(.5), size=I(1.4)) +
  facet_wrap(~variable, scales="free_y") +
  geom_smooth(method="loess", se=F, show_guide=F, size=1.2) +
  scale_color_brewer(type="qual", palette="Set2") +
  scale_fill_brewer(type="qual", palette="Set2") +
  labs(col="Newspaper", y="Voting intention (%)", x="") +
  guides(colour=guide_legend(override.aes = list(alpha = 1, size=3),
                             direction="vertical", keywidth=.8, ncol=1)) +
  theme(legend.position=c(.85, .15))


# aggregate into weekly average voting intention
int$week <- cut(int$date, "week")
weeks <- int %>% group_by(variable, week) %>% 
  summarise(av=mean(value))

pdf("figures/ge2015_votingIntentionWeekly.pdf", 6, 4)
ggplot(weeks, aes(x=as.Date(week), y=av, col=variable)) +
  geom_point(size=1.25) + geom_smooth(col=NA, aes(fill=variable)) +
  scale_color_manual(values=palette) +
  scale_fill_manual(values=palette) +
  labs(col="", fill="", y="Voting intention (%)", x="") +
  guides(colour=guide_legend(override.aes = list(alpha = 1, size=3),
                             direction="horizontal", keywidth=.8)) +
  theme(legend.position="top")
dev.off()

# most recent six months
pdf("figures/ge2015_votingIntention6mo.pdf", 5, 4)
ggplot(int, aes(x=date, y=value, col=variable)) +
  geom_jitter(alpha=I(1), size=I(1.4)) +
  geom_smooth(aes(fill=variable), col=I("grey65"), show_guide=F) +
  scale_color_manual(values=palette) +
  scale_fill_manual(values=palette) +
  labs(col="", y="Voting intention (%)", x="") +
  theme(legend.position="none") +
  coord_cartesian(xlim=c(Sys.Date()-(30*6), Sys.Date()))
dev.off()

## modelling stuff --- not used ::
library("nnet")
df <- dcast(int, date + newspaper  ~variable, value.var="value",
            fun.aggregate=mean, na.rm=T)
df <- df[complete.cases(df),]
net <- multinom(as.matrix(df[,3:7]) ~ as.numeric(date) + newspaper, data=df)

# netdf <- melt(int, id.vars=c("date", "newspaper", "week"), 
#                measure.vars="variable")
# head(netdf)
# net <- multinom(as.matrix(mnpdf[,-1]) ~ as.numeric(as.Date(mnpdf$week)))

probs <- as.data.frame(predict(net, df, "probs"))

probs$date <- df$date
probs$newspaper <- df$newspaper

probs <- melt(probs, id.vars=c("date", "newspaper"))

ggplot(probs, aes(x=date, y=value, col=newspaper)) +
  geom_point() + facet_wrap(~variable, scales="free_y") +
  scale_color_brewer(type="qual")
