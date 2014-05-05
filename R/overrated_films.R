library("RCurl")
library("jsonlite")
library("ggplot2")
library("RColorBrewer")
library("scales")
library("gridExtra")

api.key <- "yourAPIkey"
rt <- getURI(paste0("http://api.rottentomatoes.com/api/public/v1.0/lists/dvds/top_rentals.json?apikey=", api.key, "&limit=50"))

rt <- fromJSON(rt)

title <- rt$movies$title
critics <- rt$movies$ratings$critics_score
audience <- rt$movies$ratings$audience_score
df <- data.frame(title=title, critic.score=critics,
                 audience.score=audience)

# Top 50 rentals, max returnable
ggplot(df, aes(x=critic.score, y=audience.score)) +
  geom_text(aes(label=title, col=1/(critic.score - audience.score)))

# how can we get more? similar chaining
# STILL at most 5 per film (sigh)
getRatings <- function(id){
  sim.1 <- getURI(paste0("http://api.rottentomatoes.com/api/public/v1.0/movies/",
                id, "/similar.json?apikey=",
                api.key, "&limit=5"))
  
  sim <- fromJSON(sim.1)
  #cat(sim$movies$title)
  d <- data.frame(id    = sim$movies$id, 
                  title = sim$movies$title,
                  crit  = sim$movies$ratings$critics_score,
                  aud   = sim$movies$ratings$audience_score)
  return(d)
}

rt.results <- function(idlist){
  r <- sapply(unique(as.character(idlist)), getRatings, simplify=F)
  r <- do.call(rbind, r)
  return(r)
}

r1 <- rt.results(rt$movies$id)
r2 <- rt.results(r1$id)
r3 <- rt.results(r2$id)
r4 <- rt.results(r3$id)
r5 <- rt.results(r4$id)
r6 <- rt.results(r5$id)
r7 <- rt.results(r6$id)

f <- function(x)
  (5**x)-1

# Fig. 1: Number of films gathered via recursive descent
# of 'similar films' lists. Depletion in realised numbers
dev.off()
options(scipen=99)
pdf(4, 4, file="../figures/rottenTomatoHits.pdf")
par(cex.axis=.7, pch=20, mar=c(4,3,1,1), mgp=c(1.5,.3,0), tck=-.02)
plot(1:7, f(1:7), type="b", xlab="Recursions", ylab="Number of hits",
     log="y", col=muted("blue"), lwd=2, ylim=c(4, 1e5))
lines(1:7, c(nrow(r1), nrow(r2), nrow(r3), nrow(r4), nrow(r5),
            nrow(r6), nrow(r7)), type="b", col=muted("red"), lwd=2)
legend("bottomright", col=c(muted("blue"), muted("red")), pch=20, lwd=2,
       legend=c(expression(Max~(5^x)), "Realised"), bty="n", lty="47")
dev.off()

r <- rbind(r1, r2, r3, r4, r5, r6, r7)

# 1279 unique films
ru <- r[!duplicated(as.character(r$id)),]

#saveRDS(ru, "../data/rt_ratings.rds")
#ru <- readRDS("../data/rt_ratings.rds")

# Films with insufficient critics reviews get -1 score
ru[which(ru$crit == -1),]
ru <- ru[ru$crit != -1,]

ru$diff <- ru$crit - ru$aud

pcc <- cor(ru$crit, ru$aud)

# Top 50 rentals, max returnable
svg(7, 6, file="../figures/RtFilmsOverview.svg")
ggplot(ru, aes(x=crit, y=aud, col=diff)) +
  geom_point() +
  coord_cartesian(xlim=c(-10,110), ylim=c(-10,110)) +
  scale_color_gradientn(colours=brewer.pal(11, "RdYlBu"),
                        breaks=seq(-60,40, length.out=11),
                        labels=c("Underrated", rep("", 4),
                                 "Agree", rep("", 4), 
                                 "Overrated")) +
  geom_text(aes(label=ifelse(diff < quantile(diff, .005) | diff > quantile(diff, .995), as.character(title), ""),
                size=abs(diff)), 
            hjust=0, vjust=0, angle=45) +
  scale_size_continuous(range=c(2,4), guide="none") +
  labs(list(x="Critic's score", y="Audience score",
            col="")) +
  annotate("text", 3, 3, 
           label=paste0("rho ==", format(pcc, digits=2)),
           parse=T)
dev.off()


tab <- ru
colnames(tab) <- c("id", "Title", "Critics", "Audience", "Difference")

# Most underrated films:
grid.newpage()
grid.draw(tableGrob(tab[order(tab$Difference),][1:15,-1], show.rownames=F))

# Most overrated:
grid.newpage()
grid.draw(tableGrob(tab[order(tab$Difference, decreasing=T),][1:15,-1], show.rownames=F))
