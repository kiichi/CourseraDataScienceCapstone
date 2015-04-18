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
		textInput("search", label = h3("Word Prediction Demo"), value = "Enter text..."),
		hr(),
		fluidRow(column(3, tableOutput("result")))
	)
)