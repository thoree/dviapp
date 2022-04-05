#
#

library(shiny)
library(dvir)
library(forrel)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("DNA based Identification"),

    # Sidebar 
    sidebarLayout(
        sidebarPanel(

         selectInput("dat", 
                  label = "Available data",
                  choices = list("None selected",
                                 "Tutorial example", 
                                 "grave",
                                 "planecrash",
                                 "newRdata",
                                 "FamiliasFile")
                  ),
         
         fileInput("file1", h5("User data")),
         
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

        ),

        # Output
        verticalLayout(
           textOutput("selected_var"),
           tableOutput("table"),
           plotOutput("distPlot1")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
    output$selected_var <- renderText({ 

        if(input$dat == "Tutorial example")
# Failed to display output from dvir::summariseDVI(pm, am, missing),hence:
            c(" Tutorial example. 3 victims: V1, V2, V3.  
                3 missing: M1, M2, M3. 1 family, 1 typed refs: R1. 1 marker")
        else if (input$dat == "grave")
            c(" grave example. 8 victims: V1, ..., V8.  
                8 missing: M1, ..., M8. 1 family, 5 typed refs: R1, ...,R5. 23 markers.")
        else if (input$dat == "planecrash")
            c(" planecrash example. 8 victims: V1, ..., V8.  
                Five families, each with one missing and one typed ref. 15 markers.")
        else if (input$dat == "newRdata" )
            c(" newRdata")
        else if (input$dat ==  "FamiliasFile")
            c(" Familias file")
        })

    output$table <- renderTable({
      
      if(input$analysis == "IBD estimates"){
        if(input$dat == "Tutorial example"){
          res = checkPairwise(example1$pm)
          head(data.frame(V = rownames(res), res))
        }
        else if (input$dat == "grave"){
          res = checkPairwise(grave$pm)
          head(data.frame(V = rownames(res), res))
        }
        else 
          data.frame(ToDO = "Not implemented.")
      }
      else if(input$analysis == "Exclusion"){
                if(input$dat == "Tutorial example"){
                    res = exclusionMatrix(example1$pm, example1$am , example1$missing)
                    head(data.frame(V = rownames(res), res))
                }
                else if (input$dat == "grave")
                    data.frame(ToDO = "Return saved table to save time")
                else if (input$dat == "planecrash"){
                    res = exclusionMatrix(planecrash$pm, planecrash$am, planecrash$missing)
                    head(data.frame(V = rownames(res), res))
                }
                else if (input$dat == "newRdata"){
                    res = exclusionMatrix(newdata$pm, newdata$am , newdata$missing)
                    head(data.frame(V = rownames(res), res))
                }
                else if (input$dat == "FamiliasFile"){
                    file = input$file1
                    x = readFam(file$datapath)
                    pm = x[[1]]
                    am = x[[2]]$`Family tree`[[1]]
                    missing = c("Missing person","Missing male 2", "Missing male 3")
                    res = exclusionMatrix(pm, am , missing)
                    head(data.frame(V = rownames(res), res))
                }
            }
        else if(input$analysis == "Pairwise"){
                if(input$dat == "Tutorial example"){
                    res = pairwiseLR(example1$pm, example1$am , example1$missing)$LRmatrix
                    head(data.frame(V = rownames(res), res))
                }
                else if (input$dat == "grave"){
                    res = pairwiseLR(grave$pm, grave$am , grave$missing)$LRmatrix
                    head(data.frame(V = rownames(res), res))
                }
                else if (input$dat == "planecrash"){
                    res = pairwiseLR(planecrash$pm, planecrash$am, planecrash$missing)$LRmatrix
                    head(data.frame(V = rownames(res), res))
                }
            }        
        else if(input$analysis == "Joint"){
            if(input$dat == "Tutorial example"){
                res = jointDVI(example1$pm, example1$am , example1$missing)
                head(res)
            }
            else if (input$dat == "grave")
                data.frame(ToDO = "Return saved table to save time")
            else if (input$dat == "planecrash"){
                res = jointDVI(planecrash$pm, planecrash$am, 
                               planecrash$missing, verbose = F)
                head(res)
            }
        } 
        else if (input$analysis == "Posterior"){
          if(input$dat == "Tutorial example"){
            res = jointDVI(example1$pm, example1$am , example1$missing)
            res2 = Bmarginal(res, example1$missing)
            head(data.frame(V = rownames(res2), res2))
          }
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
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
