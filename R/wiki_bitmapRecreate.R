require("zoo")
require("ggplot2")

# unemployment
u <- read.csv("Employment.csv", header=T)
u <- melt(u, "Year")

unemployment <- data.frame(date=as.yearmon(do.call(paste, u[,1:2]), "%Y %b"),
                           rate=u$value)
unemployment <- unemployment[unemployment$date > as.yearmon("2008-12"),]
u2 <- unemployment
colnames(u2) <- c("date", "rate")

n <- read.csv("netChange.csv", header=T)
n <- melt(n, "Year")

# Check (!) dates are the same in each input, else repeat parse 
all.equal(net.change$date, unemployment$date) # TRUE
net.change <- data.frame(date=unemployment$date,
                         change=n$value)
net.change <- net.change[net.change$date > as.yearmon("2008-12"),]
n2 <- net.change
colnames(n2) <- c("date", "rate")
n2$panel <- "Net change in number of jobs (000s)"
u2$panel <- "Unemployment rate (%)"
both <- rbind(u2, n2)

# First: hacky ggplot2 plot
p <- ggplot(both, aes(x=as.Date(date), y=rate)) +
  facet_grid(panel~., scale="free_y") +
  layer(data=u2, geom="smooth", method="loess") +
  layer(data=u2, geom="point") +
  layer(data=n2, geom="bar", stat="identity") +
  theme_bw() + labs(y="", x="") +
  ggtitle("United States employment statistics (2009 - 2013)")

ggsave(p, file="ggplot_svg.svg")

# Or separate plots, but mis-aligned
require("gridExtra")
grid.arrange(
  ggplot(unemployment, aes(x=as.Date(date), y=rate)) +
    geom_smooth(method="loess") + geom_point() +
    labs(x="", y="Unemployment rate (%)") +
    theme_classic(),
  ggplot(net.change, aes(x=as.Date(date), y=change)) + 
    geom_bar(stat="identity") + 
    labs(y="Net change per month (000s of employees)",
         x="") + theme_classic()
  , ncol=1)


## base R version
dev.off()
## This recreates the original figure as close as possible (well, ish)
pdf("recreated.pdf", 7, 5)
par(mar=c(3,4.5,5,4.2), mgp=c(1.8,.65,0))
# sort by date
net.change <- net.change[order(net.change$date),]
# original starts at 09
net.change <- net.change[net.change$date > as.yearmon("2008-12"),]

#par(yaxs="i")
bpos <- barplot(net.change$change, plot=F)
x <- as.Date(net.change$date)
# scale will be integer (== month)
plot(1:length(x), net.change$change, type="n", ylim=c(-1000, 600),
     frame=F, axes=F, xlab="", ylab="", xlim=c(2.7, length(x)-1.7))
rect(xleft=1:length(x)-.3, xright=1:length(x)+.3, 
     ybottom=0, ytop=net.change$change, col="#4F81BD", lend=2)
abline(h=0, lwd=1)

## Awful code, avert your eyes
labs <- format(as.Date(as.character(
  cut(as.Date(seq(x[1], x[length(x)], length.out=5), 
  format= "%m/%Y"), breaks="years"))), "%m/%d%/%y")
labs <- c(gsub("01","1", labs), "")

axis(1, at=seq(1, length(x), length.out=6), tick=F, 
     labels=labs, las = 1)
axis(2, at=seq(-1000, 600, by=200), las=1, tck=-0.015)
mtext("Number jobs lost/created", side=2, col="#1F497D", line=3)

## now unemployment
unemployment <- unemployment[order(unemployment$date),]
unemployment <- unemployment[unemployment$date > as.yearmon("2008-12"),]
par(new=T)
# set up same plot
plot(1:length(x), unemployment$rate, type="n", ylim=c(6.5, 10),
     frame=F, axes=F, xlab="", ylab="", xlim=c(2.7, length(x)-1.7))
lines(1:length(x), unemployment$rate, col="#C0504D", lwd=3.5)
axis(4, las=1, tck=-.015)
#mtext("Percent unemployed", side=4, col="#4F81BD", line=1, las=0, adj=0)
legend("bottom", legend=c("Unemployment rate", "1 month Net Change (000's)"),
       col=c("#C0504D", "#4F81BD"), lty=c(1, NA), lwd=c(3.5, NA),
       fill=c(0, "#4F81BD"), merge=T, border=NA, text.font=2)
text(length(x)*1.12, (par("usr")[4] + par("usr")[3])/2, "Percent unemployed", 
     srt = 270, xpd = TRUE, col="#1F497D")

mtext("United States Employment Statistics\n Jan 2009 - Dec 2013", side=3, 
      col="#1F497D", cex=1.4, line=2, font=2)
mtext("Monthly change, seasonally adjusted", side=3, cex=1.1, line=1)
dev.off()

## This is the same figure but with a few minor improvements
dev.off()
svg("new.svg", 7.5, 5)
par(mar=c(5.5,4.5,5,3.8), mgp=c(1.8,.7,0))
net.change <- net.change[order(net.change$date),]
net.change <- net.change[net.change$date > as.yearmon("2008-12"),]

bpos <- barplot(net.change$change, plot=F)
x <- as.Date(net.change$date)
# scale will be integer (== month)
plot(1:length(x), net.change$change, type="n", ylim=c(-1000, 600),
     frame=F, axes=F, xlab="", ylab="", xlim=c(2.7, length(x)-1.7))
rect(xleft=1:length(x)-.3, xright=1:length(x)+.3, 
     ybottom=0, ytop=net.change$change, col="#4F81BD", lend=2)
abline(h=0, lwd=1)

labs <- format(as.Date(as.character(cut(as.Date(seq(x[1], x[length(x)], length.out=10), 
              format= "%M %Y"), breaks="months"))), "%b %Y")

axis(1, at=seq(1, length(x), length.out=length(labs)), 
     labels=labs, las=3)
axis(2, at=seq(-1000, 600, by=200), las=1)
mtext("Net change in employment per month (1000s of jobs)", 
      side=2, col="#1F497D", line=3)

## now unemployment
unemployment <- unemployment[order(unemployment$date),]
unemployment <- unemployment[unemployment$date > as.yearmon("2008-12"),]
par(new=T)
# set up same plot
plot(1:length(x), unemployment$rate, type="n", ylim=c(6.5, 10),
     frame=F, axes=F, xlab="", ylab="", xlim=c(2.7, length(x)-1.7))
lines(1:length(x), unemployment$rate, col="#C0504D", lwd=3.5)
axis(4, las=1)

text(length(x)*1.1, (par("usr")[4] + par("usr")[3])/2, "Percent unemployed", 
     srt = 270, xpd = TRUE, col="#C0504D")
text(length(x)*.85, ((par("usr")[4] + par("usr")[3])/4) *1.65, "Unemployment rate", 
     xpd = TRUE, col="#C0504D")
text(length(x)*.215, ((par("usr")[4] + par("usr")[3])/4) *1.75, "Jobs created or lost", 
     xpd = TRUE, col="#4F81BD")

mtext("United States Employment Statistics\n Jan 2009 - Dec 2013", side=3, 
      col="#1F497D", cex=1.4, line=2, font=2)
mtext("Monthly change, seasonally adjusted", side=3, cex=1.1, line=1)
dev.off()

