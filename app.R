library(shiny)
library(dvir)
library(forrel)
library(glue)
source("helpers.R")

ui <- fluidPage(
  titlePanel("DVI layout"),
    navbarPage("Introduction",
               tabPanel(icon("home"),
                        fluidRow(
                          column(tags$img(src = "bookKETP.png", 
                                                 width = "160px", height = "200px"), width = 2),
                          column(
                          br(),
                          p("Explain what this is all about bla bla bla bla bla bla bla bla bla bla"),
                          br(),
                          "bla bla bla bla bla bla bla bla bla bla",
                          br(), br(), br(), br(), br(),
                          p("More information on:",
                          a(href="https://www.elsevier.com/books/mass-identifications/kling/978-0-12-818423-3",  
                            "Mass identications,",target="_blank"), 
                          a(href="https://www.familias.no",  
                            "Familias software,",target="_blank"),
                          a(href="https://www.elsevier.com/books/pedigree-analysis-in-r/vigeland/978-0-12-824430-2",  
                            "Pedigree Analysis in R",target="_blank")),
                          width = 8)
                          ),
               ),
               
               tabPanel("Power",
                        p("Explain what this is all about bla bla bla bla bla bla bla bla bla bla"),
                        br(),
                        sidebarLayout(position = "left",
                                      sidebarPanel(
                                        selectInput(
                                          "pedigreePower", label = "Built in pedigree for power simulation",
                                          choices = list( "None selected", "brother", "halfBrother"),
                                        ),
                                        fileInput("famPower", "Familias file for power simulation"),
                                      ),
                                      mainPanel(
                                        fluidRow(
                                          column(plotOutput("powerPlotPremade"),  width = 4),
                                          column(plotOutput("powerPlotFam"),  width = 6)
                                        )
                                        
                                      )
                        ),
               ),              
 

               tabPanel("Prioritise",
                        p("Explain what this is all about bla bla bla bla bla bla bla bla bla bla"),
                        br(),
                        sidebarLayout(position = "left",
                                      sidebarPanel(
                                        selectInput(
                                          "pedigreePri", label = "Built in pedigree for priority simulation",
                                          choices = list( "None selected", "brother", "halfBrother"),
                                        ),
                                        fileInput("priPower", "Familias file for priority simulation"),
                                      ),
                                      mainPanel(
                                        fluidRow(
                                          column(plotOutput("priPlotPremade"),  width = 4),
                                          column(plotOutput("priPlotFam"),  width = 6)
                                        )
                                        
                                      )
                        ),
               ), 
           
                navbarMenu("DVI",
                     tabPanel("Select data",
                              p("Explain what this is all about bla bla bla bla bla bla bla bla bla bla"),
                              br(),
                              sidebarLayout(position = "left",
                                            sidebarPanel(
                                              fileInput("file1", "User data (optional) for DVI"),
                                              selectInput(
                                                "dat", 
                                                label = "Built in data for DVI",
                                                choices = list("No data selected",
                                                               "Tutorial example", 
                                                               "grave",
                                                               "planecrash")
                                              ),
                                            ),
                                            mainPanel(
                                              fluidRow(
                                                column(textOutput("DVISummary"),  width = 8)
                                              )
                                              
                                            )
                              ),
                     ),
                     tabPanel("Analysis",
                              
                              p("Explain what this is all about bla bla bla bla bla bla bla bla bla bla"),
                              br(),
                              sidebarLayout(position = "left",
                                            sidebarPanel(
                                              checkboxGroupInput("settings", label = "Settings", 
                                                                 choices = list( "Relabel DVI" = "relabel",  "Mutation" = "mutation", 
                                                                                 "Flat prior" = "flat"),
                                                                 selected = c("relabel", "flat")),
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
                                            ),
                                            mainPanel(
                                              fluidRow(
                                                column(tableOutput("table"),  width = 8)
                                              )
                                              
                                            )
                              ),
                     ),
                        
              ),              
                            
              
              
    )
)   


server <- function(input, output, session) {
  output$powerPlotPremade = renderImage( {
    if(input$pedigreePower == "brother")
      list(src = "hardCoded/brotherPow.png")
    else if(input$pedigreePower == "halfBrother")
      list(src = "hardCoded/halfBrotherPow.png")
    else if (TRUE)
      list(src = "hardCoded/empty.png")
  } 
  
  , deleteFile = FALSE)
  
  output$powerPlotFam = renderPlot({
      file = input$famPower
      ext = getExt(file = file)
      familias(file = file, method = "Power", DVI = FALSE)
  })
  
  
  output$priPlotPremade = renderImage( {
    if(input$pedigreePri == "brother")
       list(src = "hardCoded/brotherPri.png")
    else if(input$pedigreePri == "halfBrother")
          list(src = "hardCoded/halfBrotherPri.png")
    else if (TRUE)
      list(src = "hardCoded/empty.png")
  } 

    , deleteFile = FALSE)
  
  output$priPlotFam = renderPlot({
    file = input$priPower
    ext = getExt(file = file)
    familias(file = file, method = "Prioritise", DVI = FALSE)
  })
  
  
    output$powerPlotPremade3 = renderImage( {
    if(input$simulation3 == "brother")
       list(src = "hardCoded/brotherPow.png")
    else if(input$simulation3 == "halfBrother")
          list(src = "hardCoded/halfBrotherPow.png")
    else if (TRUE)
      list(src = "hardCoded/empty.png")
  } 

    , deleteFile = FALSE)
    
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
          if (TRUE) #if (input$nMissing < 0)
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
          if (TRUE) #if (input$nMissing < 0)
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
          if (TRUE) #if (input$nMissing < 0)
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
          if (TRUE) #if (input$nMissing < 0)
            MPs = 'Missing person'
        else
          MPs = paste0("M", 1:input$nMissing)
        
        
        
        familias(file = file, method = 'Posterior',  relabel = TRUE, miss = MPs)
      }
      
    })    

}



if (interactive()) {
  shinyApp(ui, server)
}