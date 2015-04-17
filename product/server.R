library(shiny)
library(ggplot2)

function(input, output) {
	
#	#dataset <- reactive({
#		diamonds[sample(nrow(diamonds), input$sampleSize),]
#	})
	
	output$result <- renderPrint({ paste(input$search,input$ds)})
	
	#output$result <- renderPrint({ input$search })
}