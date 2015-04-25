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
				
		fluidRow(
			column(9,textInput("search", label = h3("Word Prediction Demo"), value = "Please wait....")),
			column(3,actionButton("predictButton", "Find Next Word"))
		),
		fluidRow(column(12,textOutput("message"))),	
		hr(),
		fluidRow(column(3, tableOutput("result")))
	)
)