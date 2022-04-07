#
#

library(shiny)
library(dvir)
library(forrel)
library(glue)
source("helpers.R")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("DNA Based Identification"),

    # Sidebar 
    sidebarLayout(

        sidebarPanel(

        fileInput("file1", 
                  h5("User data. If provided, select `newRdata or 
                     `Familiasfile` accordingly` below")),
        
         selectInput("dat", 
                  label = "Data",
                  choices = list("None selected",
                                 "Tutorial example", 
                                 "grave",
                                 "planecrash",
                                 "newRdata",
                                 "FamiliasFile")
                  ),
         

         selectInput("analysis",
                  label = "Choose analysis",
                  choices = list("None selected",
                                 "IBD estimates",
                                 "Exclusion",
                                 "Pairwise",
                                 "Joint",
                                 "Posterior",
                                 "Power simulation")
                  ),
        
        numericInput("nMissing", 
                     h5("No of missing. Only for special cases, see doc"),
                     min = -1,
                     value = -1) 
        ),
        

        mainPanel( 
           tabsetPanel(
           tabPanel("Description",textOutput("selected_var")),
           tabPanel("Plot", plotOutput("distPlot1")),
           tabPanel("Analysis", tableOutput("table"))
          )
        
        )
    )
)

#
server <- function(input, output) {
    
    output$selected_var <- renderText({ 
        if(input$dat == "Tutorial example")
          describeData(data = "Tutorial example")
        else if (input$dat == "grave")
          describeData(data = "grave")
        else if (input$dat == "planecrash")
          describeData(data = "planecrash")
        else if (input$dat == "newRdata" )
            RData(file = input$file1, method = "DescribeData")
        else if (input$dat ==  "FamiliasFile")
            c(" Familias file")
        })

    output$table <- renderTable(rownames = T, {
      if(input$analysis == "IBD estimates"){
        if(input$dat == "Tutorial example")
          IBDestimates(example1$pm, nlines = 10, sorter = TRUE)
        else if (input$dat == "grave")
          IBDestimates(grave$pm, nlines = 10, sorter = TRUE)
        else if (input$dat == "planecrash")
          IBDestimates(planecrash$pm, nlines = 10, sorter = TRUE)
        else if (input$dat == "newRdata")
          RData(file = input$file1, method = 'IBDestimates',  nlines = 10, sorter = TRUE)
        else
          data.frame(ToDO = "Not implemented.")
      }
      else if(input$analysis == "Exclusion"){
                if(input$dat == "Tutorial example")
                  exclusionMatrix(example1$pm, example1$am , example1$missing)
                else if (input$dat == "grave")
                  data.frame(ToDO = "Return saved table to save time")
                else if (input$dat == "planecrash")
                  exclusionMatrix(planecrash$pm, planecrash$am , planecrash$missing)
                else if (input$dat == "newRdata") 
                  RData(file = input$file1, method = 'Exclusion')
                else if (input$dat == "FamiliasFile"){
                  if (input$nMissing < 0)
                    MPs = 'Missing person'
                  else
                    MPs = paste0("M", 1:input$nMissing)
                  familias(file = input$file1, method = 'Exclusion', 
                           relabel = TRUE, miss = MPs)
                }
        }
        else if(input$analysis == "Pairwise"){
                if (input$dat == "Tutorial example")
                    pairwiseLR(example1$pm, example1$am , example1$missing)$LRmatrix
                else if (input$dat == "grave")
                    pairwiseLR(grave$pm, grave$am , grave$missing)$LRmatrix
                else if (input$dat == "planecrash")
                    pairwiseLR(planecrash$pm, planecrash$am, planecrash$missing)$LRmatrix
                else if (input$dat == "newRdata")
                  RData(file = input$file1, method = 'Pairwise')
            }        
        else if(input$analysis == "Joint"){
            if(input$dat == "Tutorial example")
                jointDVI(example1$pm, example1$am , example1$missing)
            else if (input$dat == "grave")
                data.frame(ToDO = "Return saved table to save time")
            else if (input$dat == "planecrash")
                jointDVI(planecrash$pm, planecrash$am, planecrash$missing)
            else if (input$dat == "newRdata")
                  RData(file = input$file1, method = 'Joint')
        } 
        else if (input$analysis == "Posterior"){
            if (input$dat == "Tutorial example")
                 Bmarginal(jointDVI(example1$pm, example1$am, example1$missing), 
                                 example1$missing)
            else if (input$dat == "planecrash")
                 Bmarginal(jointDVI(planecrash$pm, planecrash$am, planecrash$missing), 
                                planecrash$missing)
            else if (input$dat == "newRdata")
                 RData(file = input$file1, method = 'Posterior')
            else
                 data.frame(ToDO = "Not implemented")
        }
 
        else if (input$analysis == "Power simulation")
                data.frame(ToDO = "Not implemented.")

    })  
    
        output$distPlot1 <- renderPlot({
            if(input$dat == "Tutorial example"){
                dat = example1
                plotPedList(list(dat$pm, dat$am), hatched = typedMembers, marker = 1,
                            titles = c("Post Mortem", "Ante Mortem"),
                            cex = 1.2, margins = c(1,1.2,1,1.2), frames = F,
                            col = list(red = dat$missing, blue = "R1"))
            }
            else if (input$dat == "grave"){
                dat = grave
                plot(grave$am, marker = 1, hatched = typedMembers, 
                     col = list(red = dat$missing) )  
            }
            else if (input$dat == "planecrash"){
                dat = planecrash
                plotPedList(dat$am, hatched = typedMembers, 
                            col = list(red = dat$missing))
            }
            else if (input$dat == 'newRdata')
                RData(file = input$file1, method = 'plot')
            
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
