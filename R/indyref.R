library("ggplot2")
library("grid")
library("reshape2")
library("scales")
library("XML")

# First row w/ headers breaks
polls <- readHTMLTable("http://ukpollingreport.co.uk/scottish-independence-referendum", skip.rows=1)[[1]]

# inspect
str(polls)
colnames(polls) <- c("pollster", "date", "yes", "no",
                     "non-voting", "dontknow", "yessplit")

f2n <- function(x)
  as.numeric(as.character(x))

polls$date <- as.Date(polls$date, format="%d/%m/%y")
polls$yes <- f2n(polls$yes)
polls$no <- f2n(polls$no)
polls$dontknow <- f2n(polls$dontknow)

polls <- melt(polls, id.vars=c("pollster", "date"), 
     measure.var=c("yes", "no", "dontknow"))
colnames(polls)[3] <- "response"
levels(polls$response) <- c("Yes", "No", "Undecided")

#pdf("~/other/blog/blogR/figures/indyref.pdf", 7, 6)
ggplot(polls, aes(x=date, y=value, col=response, fill=response)) + 
  geom_point() + geom_smooth(method="loess", alpha=I(.2)) +
  theme_blm() + 
  theme(legend.position=c(.5,.1), legend.direction="horizontal") +
  scale_color_brewer(type="qual", palette=3) +
  scale_fill_brewer(type="qual", palette=3) +
  scale_x_date(labels = date_format("%b '%y")) +
  scale_y_continuous(breaks=seq(0, 70, 10), limits=c(0,70)) +
  ggtitle("Should Scotland be an independent country?") +
  labs(x="", y="%", fill="Poll response:", col="Poll response:")
#dev.off()

## results per pollster

polls$pollster <- gsub(" ?\\(.*", "", polls$pollster)
polls$pollster <- gsub("-", " ", polls$pollster)

cols <- do.call(rbind, strsplit(as.character(polls$pollster), "/"))
colnames(cols) <- c("company", "newspaper")

# commision
newspapers <- c("Sunday Times", "Times", "Scotsman on Sunday",
                "Daily Mail", "Mail on Sunday", #"Sunday Express",
                "Sun", "Daily Record")#, "Sunday Post")

#company
company <- c("Panelbase", "YouGov", "Ipsos MORI", "Survation",
             "ICM", "TNS-?BMRB", "Progressive", "Ashcroft",
             "Angus Reid")

polls <- cbind(polls, cols)

library("dplyr")
sdf <- group_by(polls, newspaper, response) %>% 
## optional, exlucde don't know:
  filter(response != "Undecided") %>%
  summarise(median=median(value), count=n()) %>%
  arrange(median)

head(sdf)
sdf <- group_by(sdf, newspaper) %>% mutate(total=sum(median))

ggplot(subset(sdf, count > 3), 
       aes(x=newspaper, y=median/total, fill=response)) +
  geom_bar(stat="identity", position="stack") +
  coord_flip() + theme_blm()
  
  geom_boxplot(position=position_dodge(.8)) + theme_blm() + 
  geom_point(position=position_dodge(.8))

