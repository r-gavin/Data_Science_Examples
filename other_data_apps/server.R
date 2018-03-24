##########
#
# UI for Payment vs Invest app
#

library(shiny)
library(plotly)
library(reshape2)


B_n <- function(u,x,y,z) {
      
      # u = A, x = i, y = n, z = P
      x <- x/12
      u*(1+x)^y-z/x*((1+x)^y-1)      
}

P_n <- function(u,x,y,z) {

      p_pay <- (B_n(u,x,y-1,z) - B_n(u,x,y,z))
      i_pay <- z - p_pay
      
      c(p_pay,i_pay)
}

## FUNCTION FOR CALCULATING THE INVESTMENT MONTHLY BALANCE,
## IN PARTICULAR WITH VARYING MONTHLY PAYMENTS INTO INVESTMENT
invest_func <- function(A,i_rate,temp) {
      
      ##    A = initial ammount, i_rate = interest rate, temp = investment payments every month      
      temp_uniq <- unique(temp)
      temp_nums <- apply(array(temp_uniq,c(1,length(temp_uniq))), 
            2, function(x) sum(temp == x))
      temp_invest <- A
      
      for(i in 1:length(temp_uniq)) {
            temp_invest <- c(temp_invest,
                             B_n(temp_invest[length(temp_invest)],i_rate,1:
                                       temp_nums[i],-temp_uniq[i]))
            }
      
      temp_invest
}

## FUNCTION TO CALCULATE ACCRUED INTEREST
accum_interest <- function(temp) {
      
      acc_int <- temp[1]
      for(i in 2:length(temp)) {
            acc_int <- c(acc_int,sum(temp[1:i]))
      }
      acc_int
}

## FUNCTION FOR CALCULATING INVESTMENT INTEREST EARNED EACH MONTH
int_earn_pm <- function(inv_bal,inv_pm) {
      i_e_pm <- 0
      
      for(i in 2:length(inv_bal)) {
            i_e_pm <- c(i_e_pm,inv_bal[i]-inv_bal[i-1]-inv_pm[i-1])
      }
      i_e_pm
}

shinyServer(function(input, output) {
      
      # NET MONTHLY INCOME (AFTER TAXES)
      npm <- reactive({input$grossIncome/12*(1-input$taxesPercent/100)})
      # MONTHLY INCOME GOING TO HOUSING
      houP <- reactive({input$houseP})
      # MONTHLY INCOME GOING TO UTILITIES
      uP <- reactive({input$utilP})
      # MONTHLY INCOME GOING TO FOOD
      fP <- reactive({input$foodP})
      # MONTHLY INCOME GOING TO HEALTH
      heaP <- reactive({input$healthP})
      # MONTHLY INCOME GOING TO TRANSPORTATION
      tP <- reactive({input$tranP})
      # MONTHLY INCOME GOING TO INSURANCE
      iP <- reactive({input$insureP})
      # MONTHLY INCOME GOING TO PERSONAL
      pP <- reactive({input$personP})
      # MONTHLY INCOME GOING TO SAVINGS
      sP <- reactive({input$saveP})

      output$netPerMonth <- renderText({
            paste0("Net Monthly Income (after taxes): $", 
                   round(input$grossIncome/12*(1-input$taxesPercent/100),0))
      })

            # TOTAL PERCENTAGE OF MONTHLY INCOME GOING TO BUDGET ITEMS ABOVE
      totP <- reactive({(houP()+pP()+uP()+fP()+
                  heaP()+tP()+iP()+sP())/npm()})
      output$totalPercent <- renderText(
            paste0("Total Percentage of Monthly Net Income Allocated: ",round(totP()*100,1),"%")
            )
      
      # MONTHLY EXPENDITURE BREAKDOWN
      output$netPerMonth2 <- renderText({paste0("with Net Monthly Income of $", round(npm(),0),":")})
      output$HexBreakdown <- renderText({paste0("Housing = $",houP(),"  (",round(100*houP()/npm(),1),"%)")  })
      output$UexBreakdown <- renderText({paste0("Utilities = $",uP(),"  (",round(100*uP()/npm(),1),"%)")  })
      output$FexBreakdown <- renderText({paste0("Food = $",fP(),"  (",round(100*fP()/npm(),1),"%)")  })
      output$HlthexBreakdown <- renderText({paste0("Health = $",heaP(),"  (",round(100*heaP()/npm(),1),"%)")  })
      output$TexBreakdown <- renderText({paste0("Transportation = $",tP(),"  (",round(100*tP()/npm(),1),"%)")  })
      output$IexBreakdown <- renderText({paste0("Insurance = $",iP(),"  (",round(100*iP()/npm(),1),"%)")  })
      output$PexBreakdown <- renderText({paste0("Personal = $",pP(),"  (",round(100*pP()/npm(),1),"%)")  })
      output$SexBreakdown <- renderText({paste0("Sav/Invest & Loans = $",sP(),"  (",round(100*sP()/npm(),1),"%)")  })

      # STUDENT LOAN VARIABLES
      stud_A <- reactive({input$studLoan})
      stud_i <- reactive({input$studRate/100 * 1/12})
      stud_N <- reactive({input$studTime*12})

      # STUDENT LOAN MINIMUM MONTHLY PAYMENT
      stud_P <- reactive({stud_i()*stud_A()/(1-(1+stud_i())^-stud_N())})

      # LOAN TERM IN MONTHS
      mnths <- reactive({0:stud_N()})
      # SAVINGS RATION
      payR <- reactive({input$payRatio/100})
      # VARIABLE LOAN MONTHLY PAYMENT
      studLP <- reactive({payR()*sP()})
      # INVESTMENT INTEREST RATE / RATE OF RETURN
      invest_i <- reactive({input$investRate/100})
      # MONTHLY INVESTMENT PAYMENT WHEN LOAN PAYMENT IS VARIABLE
      investP <- reactive({(1-payR())*sP()})
      # EXTRA MONTHLY LOAN PAYMENT AMOUNT ABOVE MINIMUM PAYMENT
      extraP <- reactive({studLP()-stud_P()})
      # INITIAL INVESTMENT AMOUNT
      investA <- reactive({input$invA})
      
      # OUTPUT FOR SAVINGS/INVEST & LOAN PAYMENT
      output$minStudPay <- renderText(paste0("Minimum Monthly Payment: $",round(stud_P(),2)))
      output$studLoanPay <- renderText(paste0("Monthly Loan Payment: $",round(studLP(),2)))
      output$investPay <- renderText(paste0("Money for Monthly Investment: $",round(investP(),2)))
      output$extraPay <- renderText(paste0("Extra Monthly Payment on Loans: $",round(extraP(),2)))
      
      # MONTHLY LOAN BALANCE WITH MINIMUM MONTHLY PAYMENT
      loanPay_min <- reactive({B_n(stud_A(),stud_i()*12,0:stud_N(),stud_P())})
      # MINIMUM MONTHLY LOAN PAYMENT SPLIT INTO PRINCIPAL AND INTEREST PAID OVER LOAN TERM
      split_min <- reactive({P_n(stud_A(),stud_i()*12,0:stud_N(),stud_P())})
      split_min_P <- reactive({split_min()[1:I(length(split_min())/2)]})
      split_min_I <- reactive({split_min()[I(length(split_min())/2 + 1):length(split_min())]})
      
      # MONTHLY LOAN BALANCE WITH VARIABLE MONTHLY PAYMENT
      loanPay <- reactive({B_n(stud_A(),stud_i()*12,0:stud_N(),studLP())})
      # VARIABLE MONTHLY LOAN PAYMENT SPLIT INTO PRINCIPAL AND INTEREST PAID OVER LOAN TERM
      split <- reactive({P_n(stud_A(),stud_i()*12,0:stud_N(),studLP())})
      split_P <- reactive({split()[1:I(length(split())/2)]})
      split_I <- reactive({split()[I(length(split())/2 + 1):length(split())]})

      # LIST OF MONTHS WITH LOAN BALANCE ZERO WHEN PAYING WITH VARIABLE MONTHLY AMOUNT
      ltz <- reactive({loanPay() >= 0})

      # LIST OF EVERY MONTHS PAYMENT - MINIMUM
      stud_P_list <- reactive({c(0,rep(stud_P(),stud_N()))})
      # LIST OF EVERY MONTHS PAYMENT - VARIABLE
      studLP_list <- reactive({c(0,rep(studLP(),stud_N()))})
      # LIST OF EVERY MONTHS "TOTAL SAVINGS (LOAN + INVEST)" ALLOTMENT
      save_list <- reactive({c(0,rep(sP(),stud_N()))})
      # LIST OF EVERY MONTHS INVESTMENT ALLOTMENT
      invest_list <- reactive({c(investA(),(save_list()-studLP_list()*ltz())[2:I(stud_N()+1)])})
      # LIST OF EVERY MONTHS INVESTMENT ALLOTMENT WITH MINIMUM MONTHLY LOAN PAYMENT
      ex_invest_list <- reactive({c(investA(),rep(I(investP()+extraP()),stud_N()))})
      
      # LIST OF ACCRUED LOAN INTEREST - MINIMUM MONTHLY LOAN PAYMENT
      accu_P <- reactive({accum_interest(c(0,split_min_I()[2:length(split_min_I())]))})
      # LIST OF ACCRUED LOAN INTEREST - VARIABLE MONTHLY LOAN PAYMENT
      accu_lP <- reactive({accum_interest(I(c(0,split_I()[2:length(split_I())])*ltz()))})
      
      # MONTHLY INVESTMENT BALANCE WITH MINIMUM MONTHLY LOAN PAYMENT
      ex_tot_invest <- reactive({B_n(investA(),invest_i(),0:stud_N(),I(-investP()-extraP()))})
      # MONTHLY INVESTMENT BALANCE WITH VARIABLE MONTHLY LOAN PAYMENT
      orig_tot_invest <- reactive({invest_func(investA(),invest_i(),invest_list()[2:I(stud_N()+1)])})

      # LIST OF INVESTMENT INTEREST EARNED EACH MONTH WITH MINIMUM MONTHLY LOAN PAYMENT - NOT ACCRUED (NO SUM)
      ex_accu_I <- reactive({ex_tot_invest()-accum_interest(ex_invest_list())})
      # LIST OF INVESTMENT INTEREST EARNED EACH MONTH WITH VARIABLE MONTHLY LOAN PAYMENT - NOT ACCRUED (NO SUM)
      accu_I <- reactive({orig_tot_invest()-accum_interest(invest_list())})
      
      # SCHEDULE OF LOAN WITH MINIMUM MONTHLY LOAN PAYMENT
      tempTable1 <- reactive({data.frame(
            "Month" = 0:I(length(loanPay_min())-1), 
            "Loan Balance" = round(loanPay_min(),2),
            "Monthly Payment" = round(stud_P_list(),2),
            "Principal Paid" = round( c(0,split_min_P()[2:length(split_min_P())] ),2), 
            "Interest Paid" = round( c(0,split_min_I()[2:length(split_min_I())]) ,2),
            "Accumulated Interest" = round(accu_P(),2))
            })

      # SCHEDULE OF LOAN WITH VARIABLE MONTHLY LOAN PAYMENT
      tempTable2 <- reactive({data.frame(
            "Month" = 0:I(length(loanPay_min())-1), 
            "Loan Balance" = I(round(loanPay(),2)*ltz()),
            "Monthly Payment" = I(round(studLP_list(),2)*ltz()),
            "Principal Paid" = I(round( c(0,split_P()[2:length(split_P())]) ,2)*ltz()), 
            "Interest Paid" = I(round( c(0,split_I()[2:length(split_I())]) ,2)*ltz()),
            "Accumulated Interest" = round(accu_lP(),2))
      })

      # SCHEDULE OF INVESTMENT WITH MINIMUM MONTHLY LOAN PAYMENT
      tempTable3 <- reactive({data.frame(
            "Month" = 0:I(length(loanPay_min())-1),
            "Investment Balance" = round(ex_tot_invest(),2),
            "Investment Per Month" = round(ex_invest_list(),2),
            "Accumulated Interest" = round(ex_accu_I(),2)
      )})
      
      # SCHEDULE OF INVESTMENT WITH VARIABLE MONTHLY LOAN PAYMENT
      tempTable4 <- reactive({data.frame(
            "Month" = 0:I(length(loanPay_min())-1),
            "Investment Balance" = round(orig_tot_invest(),2),
            "Investment Per Month" = round(invest_list(),2),
            "Accumulated Interest" = round(accu_I(),2)
      )})
      
      output$tab1 <- renderDataTable(tempTable1())
      output$tab2 <- renderDataTable(tempTable2())
      output$tab3 <- renderDataTable(tempTable3())
      output$tab4 <- renderDataTable(tempTable4())
      
      ## STUDENT LOAN PAYMENT PLOT
      output$plot_SL_pay <- renderPlotly({
            df_plot_min <- reactive({
                  melt(data.frame(
                        "Month" = mnths(),
                        "Interest Paid" = round(c(0,split_min_I()[2:length(split_min_I())]),2),
                        "Principal Paid" = round(c(0,split_min_P()[2:length(split_min_P())]),2)
                  ),id.vars = "Month")
            })
            df_plot <- reactive({
                  melt(data.frame(
                        "Month" = mnths(),
                        "Interest Paid" = I(round(c(0,split_I()[2:length(split_I())]),2)*ltz()),
                        "Principal Paid" = I(round(c(0,split_P()[2:length(split_P())]),2)*ltz())
                  ),id.vars = "Month")
            })
            p1 <- reactive({
                  plot_ly(df_plot_min(),x=~Month,y=~value,color=~variable) %>%
                        add_bars(name = "Minimum Payments") %>% 
                        layout(xaxis = list(title = "Month"), 
                               yaxis = list(title="Dollars per Month"),
                               barmode = "stack")
            })
            p2 <- reactive({
                  plot_ly(df_plot(),x=~Month,y=~value,color=~variable) %>%
                        add_bars(name = "Variable Payments") %>% 
                        layout(title = "Student Loan Monthly Payment", 
                               xaxis = list(title = "Month"), 
                               yaxis = list(title="Dollars per Month"),
                               barmode = "stack")
            })
            subplot(p1(),p2(),shareY = TRUE,titleX = TRUE)
      })
      
      ## ACCRUED PAID INTEREST - STUDENT LOAN PLOT
      output$plot_SL_accint <- renderPlotly({
            plot_ly(
                  data = data.frame(Month = mnths(),
                                    accint_min = round(accu_P(),2),
                                    accint_var = round(accu_lP(),2)
                  )) %>%
                  add_lines(x=~Month,y=~accint_min,type="scatter",mode="lines",
                            name="Interest from \n Minimum Monthly Payment") %>%
                  add_lines(x=~Month,y=~accint_var,type="scatter",mode="lines",
                            name="Interest from \n Variable Monthly Payment") %>%
                  layout(title="Total Interest Paid Over the Term of Student Loan",
                         xaxis=list(title="Month"),yaxis=list(title="Dollars"))
      })

      ## INTEREST PAID VS ACCRUED EARNED
      output$plot_I_relative <- renderPlotly({
            df_I_rel_min <- reactive({
                  melt(data.frame(
                        "Month" = mnths(),
                        "Interest Paid" = -round(accu_P(),2),
                        "Interest Earned" = round(ex_accu_I(),2),
                        "Sum" = I(round(ex_accu_I()-accu_P(),2))
                  ),id.vars = c("Month","Sum"))
            })
            df_I_rel_var <- reactive({
                  melt(data.frame(
                        "Month" = mnths(),
                        "Interest Paid" = I(-round(accu_lP(),2)*ltz()),
                        "Interest Earned" = round(accu_I(),2),
                        "Sum" = I(round(accu_I()-accu_lP(),2))
                  ),id.vars = c("Month","Sum"))
            })
            p3 <- reactive({
                  plot_ly(df_I_rel_min(),x=~Month) %>%
                        add_bars(y=~value,color=~variable,name = "Minimum Pay \n Schedule") %>% 
                        add_lines(y=~Sum,mode="scatter",type="lines",name="Min Pay Balance") %>%
                        layout(xaxis = list(title = "Month"),
                               yaxis = list(title="Dollars"),
                               barmode = "relative")
            })
            p4 <- reactive({
                  plot_ly(df_I_rel_var(),x=~Month) %>%
                        add_bars(y=~value,color=~variable,name = "Variable Pay \n Schedule") %>% 
                        add_lines(y=~Sum,mode="scatter",type="lines",name="Var Pay Balance") %>%
                        layout(title="Investment Interest Accrued vs Loan Interest Paid",
                              xaxis = list(title = "Month"),
                              yaxis = list(title="Dollars"),
                              barmode = "relative")
            })
            subplot(p3(),p4(),shareY = TRUE,titleX = TRUE)
      })

      ## INVESTMENT BALANCE
      output$plot_I_bal <- renderPlotly({
            plot_ly(
                  data = data.frame(Month = mnths(),
                                    i_bal_min = round(ex_tot_invest(),2),
                                    i_bal_var = round(orig_tot_invest(),2)
                  )) %>%
                  add_lines(x=~Month,y=~i_bal_min,type="scatter",mode="lines",
                            name="Investment Balance with \n Minimum Monthly Stud. Loan Payment") %>%
                  add_lines(x=~Month,y=~i_bal_var,type="scatter",mode="lines",
                            name="Investment Balance with \n Variable Monthly Stud. Loan Payment") %>%
                  layout(title="Investment Balance Over the Term of Student Loan",
                         xaxis=list(title="Month"),yaxis=list(title="Dollars"))
      })
      
})
