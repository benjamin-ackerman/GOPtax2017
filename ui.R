
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("How will the House Tax Bill Impact Graduate Students?"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput("status",
                  "Filing Status",
                  choices = list(`Single` = "single",
                                 `Married filing jointly` = "married joint"#,
                                 #`Married filing separately` = "married separate",
                                 #`Head of household` = "head of household",
                                 #`Qualifying Widow(er) with dependent child` = "surviving spouse"
                                 )),
      #selectInput("exempt","Number of Dependents (including self)",
                  #choices = list(`1` = 1#,
                                 #`2` = 2,
                                 #`3` = 3,
                                 #`4` = 4
                                 #)),
      numericInput("stipend","How much is your annual stipend/salary?",30000,min=0),
      numericInput("tuition","How much is your program's annual tuition?",25000,min=0)
    ),

    # Print taxes
    mainPanel(
      tabsetPanel(type="tabs",
                  tabPanel("Tax Estimates",
                           p(),
                           HTML("Under the proposed House tax bill, tuition waivers would no longer be nontaxable.
                             This will <a href='https://www.vox.com/policy-and-politics/2017/11/7/16612288/gop-tax-bill-graduate-students'>potentially devastate about 145,000 graduate students around the country</a> by making them pay additional taxes on tuition dollars, and is believed to disproportionately impact students in STEM-related programs.
                                This app allows you to enter your annual stipend, your school's annual tuition costs, and your tax filing status, in order to see how the elimination of the tuition waiver could impact your graduate school finances."),
                           h3("Estimated 2018 Federal Income Tax"),
                           textOutput("current_tax"),
                           h4("Percent of Earnings"),
                           textOutput("current_tax_percent"),
                           h3("Proposed Estimated 2018 Federal Income Tax under H.R. 1"),
                           textOutput("proposed_tax"),
                           h4("Percent of Earnings"),
                           textOutput("proposed_tax_percent"),
                           
                           plotOutput("plotdiff")
                           ),
                  
                  tabPanel("Disclaimer",
                           p("The purpose of this app is to assess the potential impact of the proposed H.R. 1 bill on graduate students in the United States who receive nontaxable tuition waivers from their universities.
                                  This app should NOT be used to calculate personal estimated 2018 income taxes for official use.  
                                  These calculations only account for certain standard tax credits, like the Standard Deduction and Personal Exemption.
                                  As of now, calculations are only available for people filing taxes as individuals or jointly with a spouse (without kids).
                                  Additional tax credits (i.e. for property, other dependents, or additional sources of income/self employment) are not factored into these calculations."),
                                  tags$b("I am not a tax expert - taxes are complicated!"),
                           p(),
                           HTML("Information on 2018 tax brackets and credits were obtained from <a href='https://www.forbes.com/sites/kellyphillipserb/2017/10/19/irs-announces-2018-tax-brackets-standard-deduction-amounts-and-more/#586bdff0273b'>this Forbes article</a>.
                                 Information on proposed changes to the 2018 tax brackets and credits in H.R. 1 were obtained from <a href='http://www.businessinsider.com/tax-brackets-2018-trump-tax-plan-chart-house-senate-comparison-2017-11'> this Business Insider article</a>.")
                  )

    ))
  )
))
