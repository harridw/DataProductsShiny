#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
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


# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Retirement Factors"),
  
  # Sidebar with a slider input for key elements to estimating Retirement Funding 
  sidebarLayout(
    sidebarPanel(
       radioButtons(inputId = "gender",
                    label = "Select your gender:",
                    choices = c("Male", "Female"),
                    inline = TRUE),
       sliderInput(inputId = "age",
                   label = "Your current age:",
                   min = 20,
                   max = 75,
                   value = 45,
                   step = 1,
                   round = TRUE),
       sliderInput(inputId ="retire",
                   label = "At what age do you plan to retire:",
                   min = 50,
                   max = 75,
                   value = 65,
                   step = 1,
                   round = TRUE),
       sliderInput(inputId = "income",
                   label = "Your current gross income:",
                   min = 50000,
                   max = 500000,
                   pre = "$",
                   sep = ",",
                   value = 100000,
                   step = 5000,
                   round = TRUE),
       sliderInput(inputId = "fund",
                   label = "Percent of income applied to fund retirement:",
                   min = 0,
                   max = 20,
                   post = "%",
                   value = 10,
                   step = 1),
       sliderInput(inputId = "socsec",
                   label = "Estimated Social Security benefit (monthly):",
                   min = 0,
                   max = 3750,
                   pre = "$",
                   sep = ",",
                   value = 2000,
                   step = 500,
                   round = TRUE),
       sliderInput(inputId = "inflation",
                   label = "Average annual inflation rate:",
                   min = 1,
                   max = 6,
                   post = "%",
                   value = 3.5,
                   step = 0.5),
       sliderInput(inputId = "return",
                   label = "Average annual return on investments:",
                   min = 2,
                   max = 10,
                   post = "%",
                   value = 6,
                   step = 0.5)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("barPlot")
    )
  )
))
