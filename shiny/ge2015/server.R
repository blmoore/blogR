
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library("ggplot2")
library("shiny")
library("RColorBrewer")
library("reshape2")
library("XML")
library("blmR")

# data prep
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

theme_set(theme_blm())

palette <- brewer.pal(9, "Pastel1")[c(2,1,6,4,3)]
palette <- darken_col(palette, by=.1)

shinyServer(function(input, output) {

  output$distPlot <- renderPlot({
    plist <- input$parties
    
    ggplot(subset(int, variable %in% plist),
           aes(x=date, y=value, col=variable, fill=variable)) +
      geom_jitter(alpha=I(.3), size=I(1.4)) +
      geom_smooth(show_guide=F, col=I("grey65")) +
      scale_color_manual(values=palette, drop=T, limits=levels(int$variable)) +
      scale_fill_manual(values=palette, drop=T, limits=levels(int$variable)) +
      labs(col="Party", fill="Party", y="Voting intention (%)", x="") +
      coord_cartesian(xlim=c(input$dates)) +
      guides(colour=guide_legend(override.aes = list(alpha = 1, size=3),
                                 direction="horizontal", keywidth=.8)) +
      theme(legend.position="top")
    
  })

})
