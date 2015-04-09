library("dplyr")
library("ggplot2")
library("reshape2")

# download this file from Project Tycho:
#    https://www.tycho.pitt.edu
# Steps:
# Register -> Log in -> Level 1 data ->
# "Search and retreive data" -> 
# Options{ geographic level: state,
#           disease outcome: incidence } ->
# Add all states -> "Click here to download results to Excel."
# (open in excel, export to CSV and then...)
measles <- read.csv("incidence.csv", header=T, 
                    skip=2, stringsAsFactors=F)

measles[measles == "-"] <- 0
measles[,-(1:2)] <- apply(measles[,-(1:2)], 2, as.numeric)

measles <- melt(measles, id.var=c("YEAR", "WEEK"))
colnames(measles) <- c("year", "week", "state", "cases")

# aggregate to yearly totals
mdf <- measles %>% group_by(state, year) %>% summarise(c=sum(cases))
mdf$state <- factor(mdf$state, levels=rev(levels(mdf$state)))

# edited from R-manual: converts AnyTHInG to Title Case
.simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1, 1)), tolower(substring(s, 2)),
        sep = "", collapse = " ")
}

levels(mdf$state) <- sapply(as.character(levels(mdf$state)), 
       function(i) .simpleCap(gsub("\\.", " ", i)))                                                  

# Hack together a colourbar
cols <- c(colorRampPalette(c(rgb(224, 232, 247, max=255), # v light blue
                             rgb(10, 120, 200, max=255), # deeper blue
                             rgb(59, 159, 57, max=255) # green
                             ))(10),
          colorRampPalette(c("yellow", 
                             rep(rgb(219, 131, 0, max=255),2), #orange
                            "red", "red", "red", "red"))(40))
breaks <- c(0, 10, 20, 50, 75, 
            100, 150, 200, 400, seq(500, 5000, length.out=50))

# building labels:
labels <- rep("", length(breaks))
# all approx:
labels[1] <- "0k"
labels[15] <- "1k"
labels[26] <- "2k"
labels[37] <- "3k"
labels[48] <- "4k"

# plot to PDF device (might fail on some devices due to fonts)
pdf("hm.pdf", 8, 6)
ggplot(mdf, aes(y=state, x=year, fill=c)) + 
  geom_tile(colour="white") + theme_minimal() +
  scale_fill_gradientn(colours=cols,
                       limits=c(0, 4000),
                       breaks=breaks,
                       labels=labels,
                       guide=guide_colourbar(ticks=F,
                                             nbin=50,
                                             barheight=.5, label=T,
                                             barwidth=10)) +
  scale_x_continuous(expand=c(0,0), breaks=seq(1930, 2010, by=10)) +
  geom_segment(x=1963, xend=1963, y=0, yend=51.5, size=.9, lineend = "round") +
  labs(x="", y="", fill="") +
  ggtitle("Measles") +
  theme(legend.position=c(.5, -.13),
        legend.direction="horizontal",
        legend.text=element_text(colour="grey20"),
        plot.margin=grid::unit(c(.5,.5,1.5,.5), "cm"),
        axis.text.y=element_text(size=6, family="Helvetica"),
        axis.text.x=element_text(size=8),
        title=element_text(hjust=-.07, face="bold", vjust=1, family="Helvetica"),
        text=element_text(family="URWHelvetica")) +
  annotate("text", label="Vaccine introduced", x=1963, y=53, vjust=1, hjust=0,
           size=I(3), family="Helvetica")
dev.off()
