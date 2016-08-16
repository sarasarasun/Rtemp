#R Code for Shiny Tutorial for Boston Data Con

#Load the dataset
#install.packages('ElemStatLearn') #loaded during session
library(ElemStatLearn)
data("SAheart")
?SAheart
names(SAheart)
summary(SAheart)
SAheart$chd<-factor(SAheart$chd)

#First install and load the package
#install.packages("shiny") # loaded during session
library(shiny)

#Three parts of shiny
#1.Inputs
#2.Server
#3.Application

#Inputs
#Let's try a slider
?sliderInput
#ui<-fluidPage( sliderInput())
#We need a max
ui<-fluidPage(sliderInput(inputId ="num", max = 100, min = 2, value = 5, label= "Breaks"))
#We also need a min!
server <- function(input,output){}
shinyApp(ui = ui, server = server) #This launches our first shiny app!

#Try changing the values in our ui to see how it changes the output

#Outputs
ui<-fluidPage(sliderInput(inputId ="num", max = 100, min = 2, value = 5, label= "Breaks"),
                plotOutput("hist"))
server <- function(input,output){}
shinyApp(ui = ui, server = server) 
#No change!

#Server
?renderPlot
ui<-fluidPage(sliderInput(inputId ="num", max = 100, min = 2, value = 5, label= "Breaks"),
              plotOutput("hist"))
server <- function(input,output){
  output$hist<-renderPlot({hist(SAheart$sbp)})
}
shinyApp(ui = ui, server = server) #We have a histogram displayed!

#Let's add some interaction
ui<-fluidPage(sliderInput(inputId ="num", max = 100, min = 2, value = 5, label= "Breaks"),
              plotOutput("hist"))
server <- function(input,output){
  output$hist<-renderPlot({hist(SAheart$sbp, breaks = input$num)})
}
shinyApp(ui = ui, server = server)

#Let's clean it up
ui<-fluidPage(sliderInput(inputId ="num", max = 85, min = 1, value = 5, label= "Histogram Breaks"),
              plotOutput("hist"))
server <- function(input,output){
  output$hist<-renderPlot({hist(SAheart$sbp, breaks = input$num, main = "Histogram of Blood Pressure", xlab = "Systolic Blood Pressure (mm Hg)")})
}
shinyApp(ui = ui, server = server)


################################
#Let's change our slider to a text box
ui<-fluidPage(numericInput(inputId ="num", min = 1, value = 5, label= "Histogram Breaks"),
              plotOutput("hist"))
server <- function(input,output){
  output$hist<-renderPlot({hist(SAheart$sbp, breaks = input$num, main = "Histogram of Blood Pressure", xlab = "Systolic Blood Pressure (mm Hg)")})
}
shinyApp(ui = ui, server = server)

#Let's try out text input
?textInput
ui<-fluidPage(numericInput(inputId ="num", min = 1, value = 5, label= "Histogram Breaks"),
              textInput(inputId = "text", label = "Histogram Title"),
              plotOutput("hist"))
server <- function(input,output){
  output$hist<-renderPlot({hist(SAheart$sbp, breaks = input$num, main = input$text, xlab = "Systolic Blood Pressure (mm Hg)")})
}
shinyApp(ui = ui, server = server)


#Now for a more complex example
#We'll use the app to explore the relationship between the independent variables and the dependent variable Chronic Heart Disease (chd)
ui<-fluidPage(selectInput(inputId = "independent", label = "Independent Variable", choices = colnames(SAheart[,c(1:4,6:9)])),
              plotOutput("boxplot"))
server<-function(input,output){
  output$boxplot<-renderPlot({boxplot(SAheart[,input$independent]~SAheart$chd, xlab = "Chronic Heart Disease", ylab = input$independent)})
}
shinyApp(ui = ui, server = server)
