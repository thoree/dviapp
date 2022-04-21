library(shiny)
library(dvir)
library(forrel)
library(glue)


ui <- fluidPage(
  titlePanel("Disaster Victim Identification"),
  
             actionButton("reset", "Reset all", class = "btn btn-danger",
                         style = "position: absolute; bottom:30px; width: 170px"),
                        
    navbarPage("Introduction",
               tabPanel(icon("home"),

                        fluidRow(
                          column(tags$img(src = "bookKETP.png", 
                                                 width = "160px", height = "200px"), width = 2),
                          column(
                          h4("Purpose"),
                          p("Explain what this is all about bla bla bla bla bla bla bla bla bla bla"),
                          br(),
                          "bla bla bla bla bla bla bla bla bla bla",
                          br(), br(), br(), br(), br(),
                          p("More information:",
                          a(href="https://www.elsevier.com/books/mass-identifications/kling/978-0-12-818423-3",  
                            "Mass identications,",target="_blank"), 
                          a(href="https://www.familias.no",  
                            "Familias software,",target="_blank"),
                          a(href="https://www.elsevier.com/books/pedigree-analysis-in-r/vigeland/978-0-12-824430-2",  
                            "Pedigree Analysis in R",target="_blank")),
                          width = 8)
                          ),
               ),
               
               navbarMenu("Power",
                          
                 tabPanel("Demo",
                          p("The LR comparing H1: MP = POI, versus H2: MP and POI unrelated,
                             has been computed for 1000 unconditional simulations of MP and REF conditioned on H1.
                             The simulations use the 35 markers in the database `NorwegianFrequencies` 
                             documented in the R library forrel. The pedigree and the simulated LR distribution
                             can be obtained below for some cases, instantly, since this is precomputed. "),
                          br(),
                          sidebarLayout(position = "left",
                                        sidebarPanel(
                                          selectInput(
                                            "pedigreePower", label = "Built in pedigree for power simulation",
                                            choices = list( "None selected", "brother", "uncle"),
                                          ),
                                        ),
                                        mainPanel(
                                          fluidRow(
                                            column(plotOutput("powerPlotPremade"),  width = 9)
                                          )
                                          
                                        )
                          ),
                 ), 
                 
                tabPanel("Built in cases",
                          p("The LR comparing H1: MP = POI, versus H2: MP and POI unrelated,
                             will be computed for a specified number simulations of MP and REF conditioned on H1.
                             The simulations use the n (specified below) first of the 35 markers in the database 
                             `NorwegianFrequencies` documented in the R library forrel. The pedigree and the simulated LR distribution
                             can be obtained below for some cases. This may take some time."),
                          br(),
                          sidebarLayout(position = "left",
                                        sidebarPanel(
                                          selectInput(
                                            "pedigreePowerSimulated", label = "Built in pedigree for power simulation",
                                            choices = list( "None selected", "brother", "uncle"),
                                          ),
                                        ),
                                        mainPanel(
                                          fluidRow(
                                            column(plotOutput("powerPlotSimulated"),  width = 9)
                                          )
                                          
                                        )
                          ),
                 ),
               
                 tabPanel("fam file",
                          p("Explain what this is all about bla bla bla bla bla bla bla bla bla bla"),
                          br(),
                          sidebarLayout(position = "left",
                                        sidebarPanel(
                                          fileInput("famPower", "Familias file for power simulation"),
                                        ),
                                        mainPanel(
                                          fluidRow(
                                            column(plotOutput("powerPlotFam"),  width = 9)
                                          )
                                          
                                        )
                          ),
                 ),              
               ), 
 
               navbarMenu("Prioritise",
                          
                 tabPanel("Built in data",
                        p("Explain what this is all about bla bla bla bla bla bla bla bla bla bla"),
                        br(),
                        sidebarLayout(position = "left",
                                      sidebarPanel(
                                        selectInput(
                                          "pedigreePri", label = "Built in pedigree for priority simulation",
                                          choices = list( "None selected", "brother", "uncle"),
                                        ),
                                      ),
                                      mainPanel(
                                        fluidRow(
                                          column(plotOutput("priPlotPremade"),  width = 9)
                                        )
                                        
                                      )
                        ),
                 ), 

                 tabPanel("fam file",
                          p("Explain what this is all about bla bla bla bla bla bla bla bla bla bla"),
                          br(),
                          sidebarLayout(position = "left",
                                        sidebarPanel(
                                          fileInput("priPower", "Familias file for priority simulation"),
                                        ),
                                        mainPanel(
                                          fluidRow(
                                            column(plotOutput("priPlotFam"),  width = 9)
                                          )
                                          
                                        )
                          ),
                  ),
               ),
                          
                navbarMenu("DVI",
                     tabPanel("Built in data",
                              p("Select built in data below (or go to `Load data` if you would like to analyse 
                                 your own data)"),
                              br(),
                              sidebarLayout(position = "left",
                                            sidebarPanel(
                                            selectInput(
                                                "dat", 
                                                label = "Built in data for DVI",
                                                choices = list("None selected",
                                                               "Tutorial example", 
                                                               "grave",
                                                               "planecrash")
                                              ),

                                            ),
                                            mainPanel(
                                              fluidRow(
                                                column(textOutput("DVISummaryBuiltIn"),  width = 8)
                                              )
                                              
                                            )
                              ),
                     ),
                     
                      tabPanel("Load data",
                              p("No action required here unless you would like to load and analyse your own data.
                                 If `Relabel` is ticked, names are changed to V1, ..(for pm-samples); 
                                 M1,..., (for am-samples) and F1, .... (for reference families). Relabelling is
                                 required for some analyses as explained later"),
                              br(),
                              sidebarLayout(position = "left",
                                            sidebarPanel(
                                              checkboxInput("relabel", label = "Relabel", value =  FALSE),
                                              fileInput("file1", "User data (optional fam or RData file) for DVI"),
                                            ),
                                            mainPanel(
                                              fluidRow(
                                                column(textOutput("DVISummaryLoad"),  width = 8)
                                              )
                                              
                                            )
                              ),
                     ),                    
                     
                     tabPanel("Plot am data",
                              p("plot"),

                              sidebarLayout(position = "left",
                                            sidebarPanel(
                                                        numericInput(
                                                           "refFam", 
                                                           "Reference family to plot",
                                                           min = 1,
                                                           value = 1
                                                         ),
                                            ),
                                            mainPanel(
                                              fluidRow(
                                                column(plotOutput("plot"),  width = 8)
                                              )
                                              
                                            )
                              ),
                     ),
                     tabPanel("Analysis",
                              
                              p("The previous setting 'Relabel DVI' need to be ticked for fam file except for 
                                the IBD estimates below."),
                              br(),
                              p("The entry 'No of missing' should only be changed 
                                when a fam file that has multiple missing in on family is used."), 
                              p("In this case the total number of
                                missing shoulod be given and these should be named M1, M2, ... in the fam file"),
                              br(),
                              checkboxInput("mutation", label = "Mutation", value = FALSE),                              
                              sidebarLayout(position = "left",
                                            sidebarPanel(
                                              numericInput("nMissing", "No of missing", min = -1, value = -1),
                                            
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
                                              downloadButton("downloadTable", "Download DVI table output")
                                            ),
                                            mainPanel(
                                              fluidRow(
                                                column(tableOutput("table"),  width = 8)
                                              )
                                              
                                            )
                              ),
                     ),
                        
              ),              
###
               tabPanel("Settings",
                       p("Some default settings can be changed below"),
                       numericInput("nSimulation", "No of simulations", min = 1, max = 1001, step = 100, value = 100),
                                 ),                       


###
              
              
    )
)   


server <- function(input, output, session) {
  output$powerPlotPremade = renderImage( {
    if(input$pedigreePower == "brother")
      list(src = "figures/brotherPow.png")
    else if(input$pedigreePower == "uncle")
      list(src = "figures/unclePow.png")
    else if (TRUE)
      list(src = "figures/empty.png")
  } 
  , deleteFile = FALSE)
  
  output$powerPlotFam = renderPlot({
      file = input$famPower
      ext = getExt(file = file)
      familias(file = file, method = "Power", DVI = FALSE)
  })
  
  
  output$priPlotPremade = renderImage( {
    if(input$pedigreePri == "brother")
       list(src = "figures/brotherPri.png")
    else if(input$pedigreePri == "uncle")
          list(src = "figures/unclePri.png")
    else if (TRUE)
      list(src = "figures/empty.png")
  } 

    , deleteFile = FALSE)
  
  output$priPlotFam = renderPlot({
    file = input$priPower
    ext = getExt(file = file)
    familias(file = file, method = "Prioritise", DVI = FALSE)
  })
  

    output$DVISummaryBuiltIn <- renderText({
      if(input$dat == "Tutorial example")
        summariseDVIreturned(example1$pm, example1$am, example1$missing, header = "Tutorial data. ")
      else if (input$dat == "grave")
        summariseDVIreturned(grave$pm, grave$am, grave$missing, header = "grave data. ")
      else if (input$dat == "planecrash")
        summariseDVIreturned(planecrash$pm, planecrash$am, planecrash$missing, header = "planecrash data. ")
    })
    
    output$DVISummaryLoad <- renderText({
        file = input$file1
        ext = getExt(file = file)
        if (ext == "RData" |  ext == "rda" )
          RData(file = file, method = "Describe data")
        else if (ext ==  "fam")
         familias(file = file, method = "Describe data", relabel = input$relabel)
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
        if(ext == "RData" |  ext == "rda")
          RData(file = file, method = "IBD estimates",  nlines = 10, sorter = TRUE)
        else if(ext == "fam")
          familias(file = file, method = "IBD estimates", relabel = input$relabel)
        
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
        if(!input$relabel)
          stop(safeError("Need to tick 'Relabel DVI' for Exclusion, Pairwise, Joint and Posterior"))
        file = input$file1
        ext = getExt(file = file)
        if (ext == "RData" |  ext == "rda") 
          RData(file = file, method = 'Exclusion')
        else if (ext == "fam"){
        if (input$nMissing < 0)
            MPs = 'Missing person'
          else
            MPs = paste0("M", 1:input$nMissing)
          
          familias(file = file, method = 'Exclusion',  relabel = input$relabel, miss = MPs)
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
        if(!input$relabel)
          stop(safeError("Need to tick 'Relabel DVI' for Exclusion, Pairwise, Joint and Posterior"))
        file = input$file1
        ext = getExt(file = file)
        if (ext == "RData" |  ext == "rda")
          RData(file = file, method = 'Pairwise')
        else {
        if (input$nMissing < 0)
            MPs = 'Missing person'
        else
            MPs = paste0("M", 1:input$nMissing)
          familias(file = file, method = 'Pairwise',  relabel = input$relabel, miss = MPs)
        }
      }
    })
    
    tableJoint = reactive({
      if(input$dat == "Tutorial example")
        myjointDVI(example1$pm, example1$am , example1$missing, mutation = input$mutation)
      else if (input$dat == "grave")
        data.frame(ToDO = "Return saved table to save time")
      else if (input$dat == "planecrash")
        myjointDVI(planecrash$pm, planecrash$am, planecrash$missing, mutation = input$mutation)
      else{
        if(!input$relabel)
          stop(safeError("Need to tick 'Relabel DVI' for Exclusion, Pairwise, Joint and Posterior"))
        
        file = input$file1
        ext = getExt(file = file)
        if (ext == "RData" |  ext == "rda")
          RData(file = file, method = 'Joint', mutation = input$mutation)
        else {
        if (input$nMissing < 0)
            MPs = 'Missing person'
        else
            MPs = paste0("M", 1:input$nMissing)
          familias(file = file, method = 'Joint',  relabel = input$relabel, miss = MPs)
        }
        
      }
      
    })
    
    tablePosterior = reactive({
      if (input$dat == "Tutorial example")
        Bmarginal(myjointDVI(example1$pm, example1$am, example1$missing, 
                  mutation = input$mutation), example1$missing)
      else if (input$dat == "planecrash")
        Bmarginal(myjointDVI(planecrash$pm, planecrash$am, planecrash$missing, 
                  mutation = input$mutation), planecrash$missing)
      else{
        if(!input$relabel)
          stop(safeError("Need to tick 'Relabel DVI' for Exclusion, Pairwise, Joint and Posterior"))
        
        file = input$file1
        ext = getExt(file = file)
        if (ext == "RData" |  ext == "rda")
          RData(file = file, method = 'Posterior', mutation = input$mutation)
        else {
          if (input$nMissing < 0)
              MPs = 'Missing person'
          else
              MPs = paste0("M", 1:input$nMissing)
            familias(file = file, method = 'Posterior',  relabel = input$relabel, miss = MPs)
        }
          
      }
      
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
        if (ext == "RData" |  ext == "rda") 
          RData(file = input$file1, method = 'plot', refFam = input$refFam)
        else if (ext == "fam"){
            if (input$nMissing < 0)
              MPs = 'Missing person'
          else
            MPs = paste0("M", 1:input$nMissing)
          familias(file = file, method = 'plot', 
                     relabel = input$relabel, refFam = input$refFam, miss = MPs)
          }
        }

    })
    
  observe({


    if(!is.null(input$file1))
      updateSelectInput(session, "dat", label = "Built in data for DVI",
                        choices = list("None selected","Tutorial example", "grave", "planecrash"),
                        selected = "None selected")                                             
 
  })
 
   observeEvent(input$reset, {
    updateSelectInput(session, "pedigreePower", selected = "None selected")
    updateSelectInput(session, "pedigreePri", selected = "None selected")
    updateSelectInput(session, "dat", selected = "None selected")
    updateCheckboxInput(session, "relabel", value = FALSE)
    updateNumericInput(session, "refFam", value = 1)
    updateNumericInput(session, "nMissing", value = -1)
    updateSelectInput(session, "analysis", selected = "None selected")
  })
  
}



  shinyApp(ui, server)
