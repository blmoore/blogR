
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library("shiny")
library("ggplot2")

shinyUI(fluidPage(

  # Application title
  titlePanel("General election 2015 polling data"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      p("Explore voter intention data leading up to the UK general
        election on May 7. Data scraped live from the ",
      a(href="http://ukpollingreport.co.uk/voting-intention-2", "UK polling report.")
      ),
      checkboxGroupInput("parties", 
                         label = h4("Parties to show:"), 
                         choices = list("Conservatives" = "CON", 
                                        "Labour" = "LAB", 
                                        "Liberal Democrats" = "LD",
                                        "UKIP" = "UKIP", 
                                        "Green" = "Green"),
                         selected = c("CON", "LAB", "LD")),
            dateRangeInput('dates',
                           label = h4('Polls taken between:'),
                           start = as.Date("2010-05-13"), end = Sys.Date()),
      radioButtons("radio", label = h4("Aggregation:"),
                   choices = list("None (all polls)" = 1, "Weekly average" = 2), 
                   selected = 1, inline=T)
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
))
