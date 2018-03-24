##########
#
# UI for Payment vs Invest app
#

library(shiny)
library(plotly)
library(reshape2)

shinyUI(fluidPage(
      
      titlePanel(h1("Payment of Student Loans vs Investment")),
      p("We'll investigate the interplay between loan payments and investment. Naively, 
        one might think that it is best to pay off a loan as quickly as possible. But, if we consider
        investing the extra money that goes to paying off the loan more quickly, we might see that
        our assumptions may not be correct."),
      hr(),
      p("This topic is interesting to anyone that may be paying off a loan. But, we feel it is 
        especially pertinent to recent college graduates. Often saddled with debt from post-secondary 
        studies, graduates must pay their loans while considering the necessity to save for retirement. 
        And when considering retirement, the earlier ones starts saving and investing, the richer their 
        retirement rewards."),
      p("These young adults must choose just how quickly they should pay off their student loans (which 
        are accruing interest daily) and how much they should be setting aside for their later years. 
        The application that follows is an attempt, albeit a naive one, at answering this question. 
        The results from our analysis should be enlightening, ",strong("but should be considered a 
        general overview and not as gospel. "),"We make many assumptions in our anaylsis that would 
        most likenly fail in the real world, in particular when considering loans over a long period 
        of time when variables like ", em("rate of return, monthly loan payment,"), "and ",
        em("monthly investment")," would most probabiliy change."),
      h4("Good Luck, Have Fun, and Enjoy!"),
      hr(),
      
      h3("Gross Income Adjusted for Taxes - Resulting in Monthly Net Income"),
      fluidRow(
            column(4,
                   numericInput("grossIncome",label = h4("Gross Income (before Taxes)"),
                                min = 0, max = 1000000, value = 100000)
                   ),
            column(4,
                   sliderInput("taxesPercent", label = h4("Percent of income taken for taxes:"),
                               min = 25, max = 45, step = 0.5, value = 34)
                   ),
            column(4,
                   h3(textOutput("netPerMonth"))
                   )
      ),
      
      br(),hr(),
      
      h3("Budgetary Items - set values for each category (with suggested percentages in parenthesis)*"),
      p("Before we can start playing around with numbers, we need to see how you spend your money. Below is a ",
        em("very"),"rudimentary budget. Give your best guess for the bugdet items, and at the end we'll 
        see just how much is left over for savings/investment and student loan payments."),
      p(strong("Note: "),em("Savings/Investments & Student Loan Payments")," determines how much of your monthly 
        income will go towards these two items. This amount will be important for the rest of our analysis."),

      fluidRow(
            column(4,
                   numericInput("houseP", label = h5("Income going towards housing cost (25-35%):"),
                               min = 0, max = 1000000, step = 1, value = 1500),
                   numericInput("utilP", label = h5("Income going towards utilities (5-10%):"),
                               min = 0, max = 1000000, step = 1, value = 300),
                   numericInput("foodP", label = h5("Income going towards food (10-15%):"),
                               min = 0, max = 1000000, step = 1, value = 750)
            ),
            column(4,
                   numericInput("healthP", label = h5("Income going towards health (5-10%):"),
                               min = 0, max = 1000000, step = 1, value = 300),
                   numericInput("tranP", label = h5("Income going towards transportation (10-15%):"),
                               min = 0, max = 1000000, step = 1, value = 275),
                   numericInput("insureP", label = h5("Income going towards insurance (10-15%):"),
                               min = 0, max = 1000000, step = 1, value = 500)
                               ),
            column(4,
                   numericInput("personP", label = h5("Income going towards personal things (10-15%):"),
                               min = 0, max = 1000000, step = 1, value = 600),
                   numericInput("saveP", label = h4("Income going towards savings/
                                                         investments & student loan payments (5-15%):"),
                               min = 0, max = 1000000, step = 1, value = 800)
            )
      ),
      br(),
      
      fluidRow(
            column(8,
                   h3(textOutput("totalPercent")),
                   h4("If total percentage is ",strong("GREATER THAN 100%")," go back and 
                     adjust your budget to keep the total percentage 100% or less.")
                   ),
            column(4,
                   h4("Monthly Expentiture Breakdown"),
                   h4(textOutput("netPerMonth2")),
                   hr(),
                   strong(textOutput("HexBreakdown")),
                   strong(textOutput("UexBreakdown")),
                   strong(textOutput("FexBreakdown")),
                   strong(textOutput("HlthexBreakdown")),
                   strong(textOutput("TexBreakdown")),
                   strong(textOutput("IexBreakdown")),
                   strong(textOutput("PexBreakdown")),
                   strong(textOutput("SexBreakdown"))
            )
      ),
      
      br(),hr(),
      
      h2("Savings/Investment & Student Loan Payment"),
      p("Now that we know how much money we can put towards investment and loan payments, let's 
        set some of the other parameters that are important. What we need to know about the student 
        loan is the loan interest rate, loan term, and prinicpal. For the investment parameters, we 
        need to know any initial investment and the annual rate of return."),
      
      fluidRow(
            column(4,
                   h4("Student Loan Details"),
                   numericInput("studLoan",label = "Total Principal of Student Loans ($):",
                                min = 0, max = 1000000, step = 1, value = 25000),
                   numericInput("studRate",label = "Student Loan Interest Rate (%)",
                                min = 0, max = 20, step=0.05, value = 5.0),
                   numericInput("studTime",label = "Loan Term (yrs)",
                                min = 0, max = 30, step=0.5, value = 15),
                   h4(strong(textOutput("minStudPay")))
                   ),
            column(4,
                   h4("Loan Payments to Investment Payments Ratio"),
                   p("Determine how much (%) of the ",em("Savings/Investments & Student Loan Payment"),
                     " should go to Student Loans. "),
                   p(strong("Example:")," let's say that we set aside ",
                     strong("$100")," a month to go towards student loans as well as savings and ",
                     "investments. We set the percentage below to ",strong("60%."),
                     " That means that of the $100 a month, ",strong("$60"),
                     " will go towards ",em("paying student loans"), " and the remaining ",strong("$40"), 
                     " will go towards ",em("savings and investing.")),
                   sliderInput("payRatio","Student Loan Payment Percentage",
                               min = 0, max = 100, step = 0.5, value = 25)
                   ),
            
            column(4,
                   h4("Investment Details"),
                   numericInput("invA",label = "Initial, One-Time Investment ($)",
                                min = 0, max = 1000000, step = 1, value = 0),
                   numericInput("investRate",label = "Annual Rate of Return (%)",
                                min = 0, max = 20, step = 0.05, value = 6),
                   h4(strong(textOutput("investPay"))),
                   hr(),
                   h4(strong("Loan Details")),
                   h5(strong(textOutput("studLoanPay"))),
                   h5(strong(textOutput("extraPay")))
                   )
            ),
      
      br(),
      hr(),
      h2("Financial Analysis"),
      p("Great! The hard work is done. Sit back and investigate the tables (that show schedules of 
        loan and investment payments with accompanying details) and the plots (showing various 
        comparisons)."),
      
      navlistPanel("Student Loan",
            tabPanel("Min-Pay Schedule",dataTableOutput("tab1")),
            tabPanel("Var-Pay Schedule",dataTableOutput("tab2")),
            tabPanel("Payment Plot",plotlyOutput("plot_SL_pay")),
            tabPanel("Interest Paid",plotlyOutput("plot_SL_accint")),
            "Investment Analysis",
            tabPanel("Investment Schedule with Min-Pay",dataTableOutput("tab3")),
            tabPanel("Investment Schedule with Var-Pay",dataTableOutput("tab4")),
            tabPanel("Interest Paid vs Earned",plotlyOutput("plot_I_relative")),
            tabPanel("Investment Balance",plotlyOutput("plot_I_bal")),
            widths = c(2,10)
      ),
      
      hr(),
      p("* Budgetary guidlines inspired by", 
        a("http://www.collegeboard.com/prod_downloads/highered/res/cc_tips/BudgetGrad05.pdf")
        )
))
