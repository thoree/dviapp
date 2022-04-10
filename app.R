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
    sidebarLayout(position = "left",
                  sidebarPanel(
                  selectInput(
                      "simulation",
                      label = "Built in pedigree for power simulation",
                      choices = list("None selected",
                                     "Brother",
                                     "Avuncular")
                    ),
                  sliderInput(inputId = "markers",
                              label = "NorwegianFrequency markers:",
                              min = 1,
                              max = 35,
                              value = 22),
                  
                    
        fileInput("famFile", "Familias file for power simulation")
                  ,

        selectInput(
            "dat", 
            label = "Built in data for DVI",
            choices = list("No data selected",
            "Tutorial example", 
            "grave",
            "planecrash")
                  ),
        
        fileInput("file1", "User data for DVI")
        ,
        

        selectInput(
           "analysis",
            label = "Choose DVI analysis",
            choices = list("None selected",
                           "IBD estimates",
                            "Exclusion",
                            "Pairwise",
                            "Joint",
                          "Posterior")
                  ),
         
         numericInput(
           "refFam", 
           "Reference family to plot",
           min = 1,
           value = 1
         ),
         
         checkboxGroupInput("settings", label = "Settings", 
                            choices = list( "Relabel DVI" = "relabel",  "Mutation" = "mutation", 
                                            "Flat prior" = "flat"),
                            selected = c("relabel", "flat")),
        
        numericInput(
                     "nMissing", 
                     "No of missing. Only for special cases, see doc",
                     min = -1,
                     value = -1
                     ),
        downloadButton("downloadTable", "Download DVI table output")
        ),

        mainPanel( 
           tabsetPanel(
             tabPanel("Power pedigree", plotOutput("simPed")),
             tabPanel("Power plot", plotOutput("powerPlot")),
             tabPanel("DVI data summary",textOutput("DVISummary")),
             tabPanel("DVI plot", plotOutput("plot")),
             tabPanel("DVI table output", tableOutput("table"))
           )
   
        )
    )
)

#
server <- function(input, output) {
  
  #
  output$simPed <- renderPlot({
    if(input$simulation == "Brother")
      Brother()
    else if(input$simulation == "Avuncular")
      Avuncular()
    else {
      file = input$famFile
      ext = getExt(file = file)
      familias(file = file, method = "plotSimPed", DVI = FALSE)
    }
    
  })
  
  #
  output$powerPlot<- renderPlot({
    if(input$simulation == "Brother")
      Brother(plotPed = FALSE, nMark = input$markers)
    else if(input$simulation == "Avuncular")
      Avuncular(plotPed = FALSE, nMark = input$markers )
    else {
      file = input$famFile
      ext = getExt(file = file)
      familias(file = file, method = "powerFamilias", DVI = FALSE)
    }
  })
  
  # Data summary
  output$DVISummary <- renderText({
      if(input$dat == "Tutorial example")
        summariseDVIreturned(example1$pm, example1$am, example1$missing, header = "Tutorial data. ")
      else if (input$dat == "grave")
        summariseDVIreturned(grave$pm, grave$am, grave$missing, header = "grave data. ")
      else if (input$dat == "planecrash")
        summariseDVIreturned(planecrash$pm, planecrash$am, planecrash$missing, header = "planecrash data. ")
      else {
        file = input$file1
        ext = getExt(file = file)
        if (ext == "RData" )
          RData(file = file, method = "Describe data")
        else if (ext ==  "fam")
         relab = any(input$settings[1] %in% "relabel")
         familias(file = file, method = "Describe data", relabel = relab)
      }
})
  
  # Defines reactive functions for table output, i.e., for
  # IBD estimates, exclusion, pairwise, joint and posterior
  
    tableIBD = reactive({
    if(input$dat == "Tutorial example")
      IBDestimates(example1$pm, nlines = 10, sorter = TRUE)
    else if (input$dat == "grave")
      IBDestimates(grave$pm, nlines = 10, sorter = TRUE)
    else if(input$dat == "planecrash")
      IBDestimates(planecrash$pm, nlines = 10, sorter = TRUE)
    else{ 
      file = input$file1
      ext = getExt(file = file)
      if(ext == "RData")
        RData(file = file, method = "IBD estimates",  nlines = 10, sorter = TRUE)
      else if(ext == "fam")
        relab = any(input$settings[1] %in% "relabel")
        familias(file = file, method = "IBD estimates", relabel = relab)
    }
  })

    tableExclusion = reactive({
      if(input$dat == "Tutorial example")
        exclusionMatrix(example1$pm, example1$am , example1$missing)
      else if (input$dat == "grave")
        data.frame(ToDO = "Return saved table to save time")
      else if (input$dat == "planecrash")
        exclusionMatrix(planecrash$pm, planecrash$am , planecrash$missing)
      else{
        if(!any(input$settings %in% "relabel"))
          stop(safeError("Need to tick 'Relabel DVI' for Exclusion, Pairwise, Joint and Posterior"))
        file = input$file1
        ext = getExt(file = file)
        if (ext == "RData") 
          RData(file = file, method = 'Exclusion')
        else if (ext == "fam"){
        if (input$nMissing < 0)
          MPs = 'Missing person'
        else
          MPs = paste0("M", 1:input$nMissing)

        familias(file = file, method = 'Exclusion',  relabel = TRUE, miss = MPs)
        }
      }
  })
    
    tablePairwise = reactive({
      if (input$dat == "Tutorial example")
        pairwiseLR(example1$pm, example1$am , example1$missing)$LRmatrix
      else if (input$dat == "grave")
        pairwiseLR(grave$pm, grave$am , grave$missing)$LRmatrix
      else if (input$dat == "planecrash")
        pairwiseLR(planecrash$pm, planecrash$am, planecrash$missing)$LRmatrix
      else{
        if(!any(input$settings %in% "relabel"))
          stop(safeError("Need to tick 'Relabel DVI' for Exclusion, Pairwise, Joint and Posterior"))
        file = input$file1
        ext = getExt(file = file)
      if (ext == "RData")
        RData(file = file, method = 'Pairwise')
      else 
        if (input$nMissing < 0)
          MPs = 'Missing person'
        else
          MPs = paste0("M", 1:input$nMissing)

        familias(file = file, method = 'Pairwise',  relabel = TRUE, miss = MPs)
      }
    })
    
    tableJoint = reactive({
      if(input$dat == "Tutorial example")
        jointDVI(example1$pm, example1$am , example1$missing)
      else if (input$dat == "grave")
        data.frame(ToDO = "Return saved table to save time")
      else if (input$dat == "planecrash")
        jointDVI(planecrash$pm, planecrash$am, planecrash$missing)
      else{
        if(!any(input$settings %in% "relabel"))
          stop(safeError("Need to tick 'Relabel DVI' for Exclusion, Pairwise, Joint and Posterior"))
        
        file = input$file1
        ext = getExt(file = file)
        if (ext == "RData")
          RData(file = file, method = 'Joint')
        else 
          if (input$nMissing < 0)
            MPs = 'Missing person'
        else
          MPs = paste0("M", 1:input$nMissing)

        familias(file = file, method = 'Joint',  relabel = TRUE, miss = MPs)
      }
      
    })
    
    tablePosterior = reactive({
      if (input$dat == "Tutorial example")
        Bmarginal(jointDVI(example1$pm, example1$am, example1$missing), 
                  example1$missing)
      else if (input$dat == "planecrash")
        Bmarginal(jointDVI(planecrash$pm, planecrash$am, planecrash$missing), 
                  planecrash$missing)
      else{
        if(!any(input$settings %in% "flat"))
          stop(safeError("Need to tick 'Flat prior' for Posterior analysis"))
        if(!any(input$settings %in% "relabel"))
          stop(safeError("Need to tick 'Relabel DVI' for Exclusion, Pairwise, Joint and Posterior"))
        
        file = input$file1
        ext = getExt(file = file)
        if (ext == "RData")
          RData(file = file, method = 'Posterior')
        else 
          if (input$nMissing < 0)
            MPs = 'Missing person'
        else
          MPs = paste0("M", 1:input$nMissing)
        

        
        familias(file = file, method = 'Posterior',  relabel = TRUE, miss = MPs)
      }
      
    })
    
    
    output$table <- renderTable(rownames = T,{
    if(input$analysis == "IBD estimates")
      tableIBD()
    else if (input$analysis == "Exclusion")
      tableExclusion()
    else if (input$analysis == "Pairwise")
        tablePairwise()
    else if (input$analysis == "Joint")
        tableJoint()
    else if (input$analysis == "Posterior")
        tablePosterior()
     })
    

    output$downloadTable <- downloadHandler(
    filename = "DVITableOutput.csv",
    content = function(file = filename) {
      if(input$analysis == "IBD estimates")
        write.csv(tableIBD(), file, row.names = TRUE, quote = F)
      else if (input$analysis == "Exclusion")
        write.csv(tableExclusion(), file, row.names = TRUE, quote = F)
      else if (input$analysis == "Pairwise")
        write.csv(tablePairwise(), file, row.names = TRUE, quote = F)
      else if (input$analysis == "Joint")
        write.csv(tableJoint(), file, row.names = TRUE, quote = F)
      else if (input$analysis == "Posterior")
        write.csv(tablePosterior(), file, row.names = TRUE, quote = F)
    })
    
    output$plot <- renderPlot({
      
      file = input$file1
      ext = tools::file_ext(file$datapath)
      if(input$dat == "Tutorial example")
        plot(example1$am, hatched = typedMembers, title = "Reference family",
             cex = 1.2, col = list(red = planecrash$missing,
                                   blue = typedMembers(example1$am)))
      
      else if (input$dat == "grave")
        plot(grave$am, marker = 1, hatched = typedMembers, title = "Reference family",
             col = list(red = grave$missing, blue = typedMembers(grave$am), cex = 1.2))
      
      else if (input$dat == "planecrash")
        plot(planecrash$am[[input$refFam]], hatched = typedMembers, 
                    title = paste("Reference family ", input$refFam),
                    col = list(red = planecrash$missing,
                               blue = typedMembers(planecrash$am[[input$refFam]])))
      else{
        file = input$file1
        ext = getExt(file = file)
        if (ext == "RData") 
          RData(file = input$file1, method = 'plot', refFam = input$refFam)
        else if (ext == "fam")
          if (input$nMissing < 0)
            MPs = 'Missing person'
        else
          MPs = paste0("M", 1:input$nMissing)
        relab = input$settings[1] == "relabel"
        familias(file = file, method = 'plot', 
                   relabel = relab, refFam = input$refFam, miss = MPs)
        }

    })

}

# Run the application 
shinyApp(ui = ui, server = server)
