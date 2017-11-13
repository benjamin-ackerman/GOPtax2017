
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("How will your taxes increase under the proposed GOP tax plan?"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput("status",
                  "Filing Status",
                  choices = list(`Single` = "single",
                                 `Married filing jointly` = "married joint",
                                 `Married filing separately` = "married separate",
                                 `Head of household` = "head of household",
                                 `Qualifying Widow(er) with dependent child` = "surviving spouse"
                                 )),
      selectInput("exempt","Exemptions",
                  choices = list(`Yourself` = 1,
                                 `Spouse` = 2
                                 )),
      numericInput("stipend","How much is your annual stipend?",0,min=0),
      numericInput("tuition","How much is your program's annual tuition?",0,min=0)
    ),

    # Print taxes
    mainPanel(
      h3("Current Estimated Federal Income Tax"),
      textOutput("current_tax"),
      h3("Amount of Stipend Lost to Federal Income Tax"),
      textOutput("current_tax_percent"),
      h3("Proposed Estimated Federal Income Tax"),
      textOutput("proposed_tax"),
      h3("Amount of Stipend Lost to Proposed Federal Income Tax Changes"),
      textOutput("proposed_tax_percent")
    )
  )
))
