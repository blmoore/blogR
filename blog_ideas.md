## TODO list

Here's a list of blog post ideas I've written down in the past, 
the vast majority of which I'll never get around to though I expect 
some of these (most?) have already been done somewhere. In fact
if it hasn't been done by someone it's probably not a good idea or the data 
doesn't exist. That said, feel free to develop and implement any
of these—I'd be interested to hear about the outcomes!

### Academic

* **Supplementary length over time**: after Lior Pachter's talk at GI2013 it would be cool to see a graph of
supplementary page length vs. article length over time (+per journal). Presumably the latter has remained
pretty constant while supplementaries are seemingly ever-increasing 
* **Self-citation**: Academics will often cite their own work but doing so unecessarily might be a sign of 
inflating one's own metrics. One way to test for this might be to count the number of in-text references
to a self citation: a self-citation referenced many times is probably closely related to the current work whereas 
a self-citation referenced just once might be more peripheral. If certain authors used this tactic a lot it 
could suggest foul play.
* **Dead links in abstracts**: quantify the number of dead links in paper abstracts per journal, by IF etc. What's the
half-life of a bioinformatics tool? (NB When I mentioned this on twitter a while ago I was pointed to existing studies which
have looked at this)
* **Journal review stats**: [scirev](https://scirev.sc/reviews/) has a scrapable table of journal reviews
* **Received to published times**: Most journals give received, accepted times per article. Who's fast, who's slow, what's
an acceptable time and is turnaround time decreasing? PLOS offers a full XML dump of all articles if you ask them nicely.

### R related

* **Prevalence of 2.2e-16**: this is commonly the value of `.Machine$double.eps` and hence widely reported as 
*p*-values. Interesting to see how many of these there are in full-text academic papers over time, also a 
proxy for R usage.


### Pop culture

* **Eurovision bloc voting**: Terry Wogan used to bemoan "bloc voting" or countries voting for their geographical
neighbours, it'd be interesting to look at to what extent this is true and if it's less important now than a few years ago.
* **Movie director diversity**: scrape films from e.g. IMDB, who's the most diverse director/actor etc. in terms of numbers
of genres spanned?
* **Character vs Actor age**: actors and actresses play characters with a variety of ages, what's the biggest gap between
character age and actor person age?
* **Companies on twitter**: which customer-facing twitter accounts reply to the highest proportion of their mentions? Most 
of their complaints? Response time?
* **Gadget uptake**: I wonder how the trajectories of various electronics to full market penetrance compare... TVs, PCs, mobiles
* **Marijuana use and paranoia**: there's a popular reddit community dedicated to recreational drug use and others relating to 
conspiracy theories, anxiety etc. A hypothesis might be that active members of drug use subreddits might be more likely to 
frequent these other boards. Normalisation and building proper subreddit relationship graphs would be key.
* **Spouse references**: Given an article's subject is female, how much more likely is it to contain "married to"  or 
otherwise reference the husband or spouse. Compare this to articles with male subjects.
* **Cyclical trends**: Various trends are thought to be cyclical: skateboarding, cuts of jeans... Would be interesting
to graph these, compare wavelengths etc. Search data could give modern results but probably doesn't go far enough back
to get multiple cycles...
* **Least translatable words**: It's interesting that some languages have words which have no equivalent or need a 
sentence to explain in English (e.g. schadenfreude, zeitgeist). Maybe this could be quantified with the Google translate API, 
though I guess it just balks at these for now.
* **Poker: skill vs. luck**: maybe by analysing WSOP placings over consecutive years one could try to tease apart the contribution of luck and skill in no limit texas hold'em, i.e. to what extent are placings in consecutive tournaments independent.
* **Sport seedings**: many tournaments predict the peformance of their best players by seeding them. Which sports are most accurate with their seedings? Normalise for tournament sizes etc.
* **Brand logos over time**: it seems brand logos have become simpler over time, e.g. microsoft's 3D wavy flag logo is now four coloured squares. This could be quantified by image analysis, clustering etc. is this a general trend? 

### Politics

* **Commons / PMQs transcripts**:  public opinion a while ago was generally that the UK parliament was too London-focused. By
scraping debate transcripts we could find out to what extent that's true. I wonder if it is discussed disproportionately  
more than expected given its size or GDP contribution.
* **Public schooling**: plot the proportion of publicly-educated politicans per party over time. Not sure there's a good 
source for this data.
