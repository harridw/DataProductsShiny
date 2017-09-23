#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(plyr)
library(dplyr)
library(data.table)
library(tidyr)
library(ggplot2)
library(plotly)

# Mortality table information from Social Security (2014) [For current age 0 - 119]
Death.Male <- c(76.33, 76.81, 76.84, 76.86, 76.88, 76.89, 76.9,
                76.91, 76.92, 76.93, 76.94, 76.94, 76.95, 76.96,
                76.97, 76.99, 77.02, 77.05, 77.08, 77.13, 77.18,
                77.24, 77.3, 77.37, 77.44, 77.51, 77.58, 77.65,
                77.72, 77.79, 77.86, 77.93, 78, 78.07, 78.15,
                78.22, 78.29, 78.37, 78.44, 78.52, 78.6, 78.68,
                78.76, 78.85, 78.95, 79.04, 79.15, 79.26, 79.38,
                79.51, 79.64, 79.79, 79.94, 80.11, 80.29, 80.47,
                80.67, 80.87, 81.09, 81.32, 81.55, 81.79, 82.04,
                82.3, 82.57, 82.84, 83.12, 83.4, 83.7, 84.01,
                84.32, 84.66, 85, 85.36, 85.73, 86.11, 86.51,
                86.93, 87.36, 87.81, 88.28, 88.76, 89.27, 89.8,
                90.34, 90.91, 91.5, 92.11, 92.74, 93.4, 94.08,
                94.79, 95.52, 96.27, 97.05, 97.85, 98.68, 99.53,
                100.39, 101.27, 102.15, 103.04, 103.93, 104.83, 105.73,
                106.63, 107.54, 108.45, 109.36, 110.28, 111.2, 112.13,
                113.05, 113.98, 114.92, 115.86, 116.79, 117.74, 118.68,
                119.63)
Death.Female <- c(81.11, 81.54, 81.57, 81.59, 81.6, 81.61, 81.62,
                  81.63, 81.64, 81.64, 81.65, 81.66, 81.66, 81.67,
                  81.68, 81.69, 81.7, 81.72, 81.73, 81.75, 81.77,
                  81.8, 81.82, 81.85, 81.87, 81.9, 81.93, 81.96,
                  81.99, 82.03, 82.06, 82.1, 82.13, 82.17, 82.21,
                  82.25, 82.29, 82.34, 82.39, 82.44, 82.49, 82.55,
                  82.61, 82.67, 82.74, 82.81, 82.88, 82.96, 83.05,
                  83.14, 83.24, 83.34, 83.45, 83.57, 83.7, 83.83,
                  83.96, 84.1, 84.25, 84.4, 84.56, 84.72, 84.89,
                  85.07, 85.25, 85.44, 85.63, 85.84, 86.06, 86.29,
                  86.53, 86.78, 87.05, 87.34, 87.63, 87.94, 88.26,
                  88.6, 88.96, 89.33, 89.73, 90.14, 90.58, 91.04,
                  91.52, 92.01, 92.53, 93.08, 93.64, 94.23, 94.85,
                  95.5, 96.18, 96.88, 97.61, 98.37, 99.16, 99.96,
                  100.79, 101.63, 102.48, 103.33, 104.19, 105.06, 105.93,
                  106.81, 107.69, 108.58, 109.47, 110.37, 111.27, 112.18,
                  113.09, 114.01, 114.93, 115.86, 116.79, 117.74, 118.68,
                  119.63)


# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  output$barPlot <- renderPlot({
        
        # Develop age range of death -- expected plus +/- 5 years
        male.death.mid <- reactive({Death.Male[input$age+1]+0})
        male.death.lwr <- reactive({max(Death.Male[input$age+1]-5,input$retire + 1)})
        male.death.upr <- reactive({Death.Male[input$age+1]+5})
        
        female.death.mid <- reactive({Death.Female[input$age+1]+0})
        female.death.lwr <- reactive({max(Death.Female[input$age+1]-5, input$retire + 1)})
        female.death.upr <- reactive({Death.Female[input$age+1]+5})
        
        #      mortality <- reactive({
        #            paste("Death",input$gender,sep=".")[input$age1]})
        
        death.lwr <- reactive({ifelse(input$gender == "Male", male.death.lwr(), female.death.lwr())})
        death.mid <- reactive({ifelse(input$gender == "Male", male.death.mid(), female.death.mid())})
        death.upr <- reactive({ifelse(input$gender == "Male", male.death.upr(), female.death.upr())})
        
        Age.of.Death <- reactive({c(death.lwr(), death.mid(), death.upr())})
        
        
        # Calculate number of years until retirement & number of years in retirement
        yrs.to.retire <- reactive({input$retire - input$age})
        yrs.retired.lwr <- reactive({death.lwr() - input$retire})
        yrs.retired.mid <- reactive({death.mid() - input$retire})
        yrs.retired.upr <- reactive({death.upr() - input$retire})
        
        # Equivalent values at Retirement (income/spending & Social Security)
        income.adj <- reactive({input$income*(1 - input$fund/100)})
        net.income.adj <- reactive({income.adj() - (input$socsec*12)})
        retire.inc <- reactive({net.income.adj()*(1+input$inflation/100)^(yrs.to.retire())})
        
        
        # Defines rate to estimate present value of expenses during retirement
        ratediff <- reactive({(input$return-input$inflation)/100})
        
        
        # Present Value of annual expenses throughout retirement
        # Formula:  pmt x ((1 - (1/(1 + r)^n))/r)
        lvl1 <- reactive({retire.inc()*
                    ((1-(1/(1+ratediff())^yrs.retired.lwr()))/ratediff())})
        lvl2 <- reactive({retire.inc()*
                    ((1-(1/(1+ratediff())^yrs.retired.mid()))/ratediff())})
        lvl3 <- reactive({retire.inc()*
                    ((1-(1/(1+ratediff())^yrs.retired.upr()))/ratediff())})
        Retirement.Fund.Amount <- reactive({c(lvl1(), lvl2(), lvl3())})
        
        
       RetireReady <- reactive({data.frame(Age.of.Death(), Retirement.Fund.Amount())})
       par(mar=c(5,7,2,1)+0.01)
       barplot(round(RetireReady()[,2]/1000000,1), width = 1, space = 0.1,
                  names.arg = RetireReady()[,1], 
                  col = c("red", "blue", "green"),
                  ylim = c(0, 8),
                  axis(2, seq(0,8, by = .5), las = 1),
                  main = "Estimated Retirement Funds Required",
                  xlab = "Age of Death",
                  ylab = "Retirement Fund Amount (Millions)",
                  legend = RetireReady()[,1], beside = TRUE)
       }, height = 800, width = 600)


})