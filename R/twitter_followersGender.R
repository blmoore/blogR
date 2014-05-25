require("dplyr")
require("gender")
library("rCharts")
require("RCurl")
require("scales")
require("twitteR")
set.seed(42)

# Get top 100 twitter users by followers (from twittercounter.com)
top.100 <- getURL("http://twittercounter.com/pages/100")
top.100 <- unlist(strsplit(top.100, "\n"))
top.100 <- top.100[sapply(top.100, grepl, pattern="@")]
top.100 <- gsub(".*>@(.+)<.*", "\\1", top.100)[2:101]

# Your twitter API keys here
cons.key <- scan("../consumerkey.twitter", what="character")
cons.sec <- scan("../consumersecret.twitter", what="character")

## Setup OAuth
# via http://davetang.org/muse/2013/04/06/using-the-r_twitter-package/
cred <- OAuthFactory$new(consumerKey    = cons.key,
                         consumerSecret = cons.sec,
                         requestURL     = 'https://api.twitter.com/oauth/request_token',
                         accessURL      = 'https://api.twitter.com/oauth/access_token',
                         authURL        = 'https://api.twitter.com/oauth/authorize')

cred$handshake()
getTwitterOAuth(cons.key, cons.sec)
registerTwitterOAuth(cred)

user <- lookupUsers(top.100)
# Check your rate limits:
twitteR::getCurRateLimitInfo()

getFollows <- function(userList, numFollowers){
  df <- data.frame(user = character(), 
                   followNames = character())
  for(u in 1:length(userList)){
    print(u)
    try({
      follows <- userList[[u]]$getFollowers(n=numFollowers)
      names <- unlist(lapply(follows, "[[", "name"))
      names <- tolower(gsub(" .*", "", names))
      rows <- data.frame(user        = userList[[u]]$screenName,
                         followNames = names)
      df <- rbind(df, rows)
      Sys.sleep(90)
    })
  }
  return(df)
}

### Try get 1k followers per user, with 2k >50% have some technical
### issue. Possible strange characters in usernames / other data 
### that break the R JSON import. Could be fixed w/ RCurl probably.
### WARNING: these take a _long_ time to run due to API rate limits.
### wait between requests alone is 2.5h (when using 90s, try 60s).
df <- getFollowers(user, 1000)

# How many have some gender prob? ~60%
length(df$proportion_male[!is.na(df$proportion_male)])

### These had technical issues with 1000 followers, try 500
u2 <- user[which(!top.100 %in% unique(df$user))]
getFollowers(u2, 500)

# Finally 2 still broke, try 200
u3 <- user[which(!top.100 %in% c(unique(as.character(df$user)), 
                                 unique(as.character(df2$user))))]
df3 <- getFollowers(u3, 200)

followers <- rbind(df, df2, df3)
followers$followNames <- as.character(followers$followNames)

## 2) Get gender probabilities, methods: 1) simple: threshold,
## 2) probablistic 3) bayesian, combine threshold and probs.
followers <- cbind(followers, gender(followers$followNames))
hist(followers$proportion_male)
followers$mf <- ifelse(runif(nrow(followers)) < followers$proportion_male, "M", "F")

# Plot barcharts with unknown genders included
summarys <- group_by(followers, user, mf) %.% 
  summarise(sum=n()) %.% mutate(tot=cumsum(sum))
summarys$tot <- rep(summarys$tot[seq(3, nrow(summarys), by =3)], each=3)
pdf("../plots/twitter_GenderWithMissing.pdf", 10, 14)
ggplot(summarys, aes(x=mf, y=sum/tot, fill=mf)) + 
  facet_wrap(~user, ncol=10) + #, scale="free_y") + 
  geom_bar(stat="identity") +
  theme(legend.position="none") + 
  scale_y_continuous(labels=percent) +
  labs(y="Percent of followers", x="Gender")
dev.off()

## Without unknowns:
plot <- followers
plot <- plot[!is.na(plot$mf),]
plot <- group_by(plot, user, mf) %.% summarise(sum=n()) %.% mutate(tot=cumsum(sum))

## Tidy up:
plot$user <- as.character(plot$user)
plot$tot <- rep(plot$tot[seq(2, nrow(plot), by=2)], each=2)
plot$perc <- with(plot, sum / tot)
plot$perc.m <- rep(plot$perc[seq(2, nrow(plot), by=2)], each=2)
plot$user <- factor(plot$user, levels=unique(plot$user[order(plot$perc.m)]))
plot$mf <- ifelse(plot$mf == "F", "Female", "Male")

pdf("../plots/twitter_genderDist.pdf", 6, 10)
ggplot(plot, aes(x=user, y=sum/tot, fill=mf)) + 
  geom_bar(stat="identity") +
  theme(legend.position="none") + 
  scale_y_continuous(labels=percent, expand = c(0,0)) +
  coord_flip() + scale_fill_brewer(palette="Paired") +
  labs(x="Twitter account", y="", fill="Gender of\n followers") + 
  geom_hline(y=.5, linetype=2) + theme_minimal() +
  theme(axis.text.y = element_text(size=8))
dev.off()

# order by top100:
#plot <- plot[order(match(plot$user, top.100)),]
# order by percent male:
plot$user <- as.character(plot$user)
plot <- plot[order(plot$perc.m),]

plot$perc <- plot$perc * 100
plot$sum <- NULL
plot$tot <- NULL
plot$perc.m <- NULL
n1 <- nPlot(perc ~ user, group="mf",
            data = plot, color="mf", 
            type = "multiBarHorizontalChart",
            height=1100, width=600)
n1

n1$publish("other")

# Find missing account (@TwitPic)
top.100[which(!top.100 %in% unique(plot$user))]
