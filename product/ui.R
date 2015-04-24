#setwd("~/work/r/class/CourseraDataScienceCapstone/product")
#library(shinyapps)
#deployApp()
#less than 100 mb
library(shiny)

fluidPage(
	sidebarPanel(
		selectInput('ds', 'Data Source', c('Twitter','Blog','News'))
	),	
	mainPanel(
		#textInput("search", label = h3("Word Prediction Demo"), value = "Enter text..."),
		verbatimTextOutput("message"),
		br(),
		textInput("search", label = h3("Word Prediction Demo"), value = "Enter text..."),
		actionButton("predictButton", "Predict"),
		hr(),
		fluidRow(column(3, tableOutput("result")))
	)
)