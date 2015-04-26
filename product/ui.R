#setwd("~/work/r/class/CourseraDataScienceCapstone/product")
#library(shinyapps)
#deployApp()
#less than 100 mb
library(shiny)

fluidPage(
	br(),
	fluidRow(
		br(),
		column(4,		
			selectInput('datasource', '1. Select Data Source', c('Blog','News','Twitter')),
			textOutput("message")
		),
		column(4,							 
					 textInput("search", label = "2. Enter Some Words"),
					 actionButton("predictButton", "Find Next Word"),
					 br(),
					 br(),
					 hr()
		),
		column(4,
					 h5('3. Review results'),
					 tableOutput("result")
		)
	)	
)