library("ggplot2")
library("dplyr")
require("grid")
require("MASS")
require("stats")
library("xtable")

helms <- read.table("../data/helmet_data.tsv", sep="\t")
colnames(helms) <- c("make", "model", "type", "price", "rating", "sizes")

## 1) Get price ranges per manufacturer
summary <- group_by(helms, make) %>% summarise(count=n(), middle=median(price))
summary <- summary[order(summary$middle),]
summary <- summary[summary$count > 5,]

h2 <- helms[helms$make %in% summary$make,]
h2$make <- factor(h2$make, levels=summary$make)

pdf("../figures/helmet_price_ranges.pdf", 5.5, 5.5)
ggplot(h2, aes(x=make, y=price)) +
  geom_boxplot(fill=I("grey80"), width=I(.75),
               colour=I("grey80"), outlier.size=0) + 
  theme_bw() + 
  coord_flip() + labs(x="", y="Price (GBP)") +
  stat_summary(geom = "crossbar", width=0.65, fatten=1.2, 
               color="white", fun.y=median, fun.ymin=median, fun.ymax=median) +
  stat_summary(geom="text", aes(label=make), hjust=1, size=I(4), colour=I("grey20"),
               fun.y=function(d){ 
                 #boxplot.stats(d)$stats[2]} 
                 # apparently done diff, see https://groups.google.com/forum/#!topic/ggplot2/_CJRfU0Cx6w
                 min(d[d > (quantile(d, .25) - 1.5*IQR(d))]) }) +
  theme(axis.text.y=element_blank(), panel.grid.major.y=element_blank(),
        axis.ticks.y=element_blank()) +
  ggtitle("Price ranges of motorcycle helmet brands") +
  scale_y_continuous(breaks=seq(0,600, by=100), limits=c(0,600))# + ylim(0, 600) 
dev.off()
  
## 2) Look at helmet type vs. safety rating
h3 <- group_by(helms, type, rating) %>% summarise(n=n())
n <- group_by(helms, type) %>% summarise(totals=n())
# Somehow can't figure out how to do this properly, oh well..
h3[h3$type == "System","n"] <- h3[h3$type == "System","n"] / n[n$type=="System",]$totals
h3[h3$type == "Full face","n"] <- h3[h3$type == "Full face","n"] / n[n$type=="Full face",]$totals

h3$type <- ifelse(h3$type == "System", "Flip-front", "Full face")
offset <- position_dodge(width=.6)

pdf("../figures/helmet_types.pdf", 6, 6)
ggplot(h3, aes(x=paste0(rating,  "*"), y=100*n, 
               col=type, fill=type, group=type)) +
  theme_bw() + coord_flip() +
  geom_bar(width=.5, stat="identity", col=NA,
           position=offset) +
  geom_text(aes(label=100*round(n, 3)), hjust=-0.2,
            position=offset, col=I("black")) +
  labs(x="SHARP rating", y="Percent of helmets tested", fill="Helmet") + ylim(0,50) +
  theme(legend.position=c(.8,.2), panel.grid.major.y=element_blank()) +
  scale_fill_manual(values=c("grey80", "grey60")) +
  ggtitle("Safety ratings by helmet type")
dev.off()  

## 3) Helmet brands -- does expensive == better?
many <- names(which(table(helms$make) > 8))
# simplify plot, rm those I don't know off-hand
many <- many[!many %in% c("Box", "KBC", "Lazer")]

pdf("../figures/helmets_acrossbrands.pdf", 8, 5)
ggplot(subset(helms, make %in% many), aes(x=price, y=rating, col=make)) +
  geom_jitter(aes(shape=type)) + 
  geom_smooth(method="rlm", se=F) +
  theme_bw() +
  labs(x="Price (GBP)", y="SHARP rating", col="Brand", shape="Type") +
  ggtitle("Trends across price ranges per brand") +
  theme_bw() +
  scale_color_brewer(type="qual", palette="Paired")
dev.off()

## Are more epensive helmets more protective overall?
lm_eqn = function(df){
  ## from: http://stackoverflow.com/a/7549819/1274516
  m = lm(rating ~ price, df);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(coef(m)[1], digits = 2), 
                        b = format(coef(m)[2], digits = 2), 
                        r2 = format(summary(m)$r.squared, digits = 2)))
  as.character(as.expression(eq));                 
}

pdf("../figures/helmets_overall_lm.pdf", 7, 6)
ggplot(helms, aes(x=price, y=rating)) +
  geom_jitter(aes(shape=type, col=type)) + 
  geom_smooth(method="lm", se=T, col="#ffa775",alpha=I(.2)) +
  labs(x="Price (GBP)", y="SHARP rating", col="Type", shape="Type") +
  ggtitle("Are more expensive helmets more protective?") +
  theme_bw() + theme(legend.position=c(.8,.2)) + xlim(0,600) +
  scale_color_manual(values=c("black", "grey40")) +
  annotate("text", x=400, y=.5, label=lm_eqn(helms), parse=T)
dev.off()
# (Should really be using ordered logit model or probit)

## 10 best / worst helmets in terms of value for money
top <- helms[helms$rating == 5,]
top <- head(top[order(top$price, decreasing=F),-6], 10)
rownames(top) <- NULL
print(xtable(top), type="html")

bottom <- helms[helms$rating %in% c(1,2),]
bottom <- head(bottom[order(bottom$price, decreasing=T),-6], 10)
rownames(bottom) <- NULL
print(xtable(bottom), type="html")

# ranked of median price vs. rank of median safety:
h5 <- group_by(helms, make) %>% 
  summarise(n=n(), median.price=median(price),
            median.rating=mean(rating)) %>%
  filter(n >5)

h5$median.price <- rank(h5$median.price)

pdf("../figures/helmets_brand_summary.pdf", 6, 6)
ggplot(h5, aes(x=median.rating, y=median.price,
               label=make)) +
  labs(x="Mean safety rating",
       y="Median price (rank / 22)") +
  annotate("rect", xmin=1, xmax=5, ymin=0, ymax=7.33,
           fill=I("grey80"), alpha=I(.4)) +
  annotate("rect", xmin=quantile(h5$median.rating, .66), 
           xmax=5, ymin=0, ymax=23,
           fill=I("grey80"), alpha=I(.4)) +
  geom_text() + theme_bw() +
  annotate("text", x=1.5, y=6, label="Cheapest", 
           col=I("#ffa775"), face="bold", size=6) +
  annotate("text", x=4.5, y=22, label="Safest", 
           col=I("#ffa775"), face="bold", size=6) +
  ggtitle("Brand summary over all tested helmets")
dev.off()
