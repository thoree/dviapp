suppressPackageStartupMessages({
  library(shiny)
  library(dvir)
  library(forrel)
  library(glue)
})

version = 1.0

ui <- fluidPage(
  
  titlePanel("Disaster Victim Identification"),
  
    # Reset all input except file load ( don't know how)
    actionButton("reset", "Reset all", class = "btn btn-danger",
                  style = "position: absolute; bottom:30px; width: 170px"),
                        
    navbarPage("Introduction",
               
      # Button to return to introduction         
      tabPanel(icon("home"),
      
        # Button to reset                
        fluidRow(
          column(tags$img(src = "bookKETP.png", width = "160px", height = "200px"), width = 2), 
                                
          column(
            p("Purpose: Explain what this is all about."),
            br(), br(), br(), br(), br(),
            p("More information:",
            a(href="https://www.elsevier.com/books/mass-identifications/kling/978-0-12-818423-3",  
                   "Mass identications,",target="_blank"), 
            a(href="https://www.familias.no", "Familias software,",target="_blank"),
            a(href="https://www.elsevier.com/books/pedigree-analysis-in-r/vigeland/978-0-12-824430-2",  
                   "Pedigree Analysis in R",target="_blank")),
                   width = 8)
         ),
       ),
               
        navbarMenu("Power",
                          
          tabPanel("Demonstrations",
            p("The below explanation applies to the example obtained if 'brother' (default) is chosen in the pull down menu below.
               The LR comparing H1: MP and REF full brothers, versus H2: MP and REF unrelated,
               has been computed for 100 unconditional simulations of MP and REF conditioned on H1 below.
               The simulations use the 22 first markers in the database `NorwegianFrequencies` 
               documented in the R library forrel. At the bottom of the histogram, the probaility that LR exceeds the
               threshold 10000, is given, in this case 0.79. In `Power > Built in cases` parameters like the number of markers used, 
               can  be changed. In `Power > Load data`, similar output is obtained by loading a familias file, either one 
               that comes with this app or your own"),
            br(),
              sidebarLayout(position = "left",
                sidebarPanel(
                  selectInput(
                    "pedigreePower", label = "Built in pedigree for power simulation",
                     choices = list( "None selected", "brother", "uncle"), selected = "brother",
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
              p("The functionality is explained in `Power > Demonstrations`. The threshold for the LR can be reduced from the
                default of 10,000 below. Also, you can choose to include only markers 1,..., Last Marker of the database 
                `NorwegianFrequencies. The number of simulations and the seed can be changed in `Settings`."),
              br(),
                sidebarLayout(position = "left",
                  sidebarPanel(
                    sliderInput("threshold", "Threshold", min = 0, max = 10000, step = 100, value = 10000),
                    sliderInput("lastMarker", "Last Marker", min = 1, max = 35, step = 1, value = 35),
                    selectInput("pedigreePowerSimulated", label = "Built in pedigree for power simulation",
                      choices = list( "None selected", "Missing brother", 
                                      "Missing uncle", "Missing first cousin",
                                      "Missing GF, 2 grandchildren typed"),
                    ),
                  ),
                  mainPanel(
                    fluidRow(
                      column(plotOutput("powerPlotSimulated"),  width = 9)
                    )
                  )
                ),
              ),
               
              tabPanel("Load data",
                p("Power is calculated by uploading a Familias file below and checking `Simulate`. The missing person should be
                   named `MP` and the reference `REF`. The file `BrotherPower.fam` gives output similar to that
                   in `Power > Demonstrations` (but not identical, even for the same seed, since the simulation
                   implementation is not identical). Further information on the fam file loaded, and potential 
                   errors in the conversion of the fam file, are reported
                   to the console."),
                br(),
                  sidebarLayout(position = "left",
                    sidebarPanel(
                      sliderInput("thresholdFam", "Threshold", min = 0, max = 10000, step = 100, value = 10000),
                      fileInput("famPower", "Familias file for power simulation"), 
                      checkboxInput("simulateFam", "Simulate", value = FALSE),
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
                          
                 tabPanel("Demonstrations",
                        p("The below explanation applies to the example obtained if 'brother' (default) 
                        is chosen in the pull down menu below.
                        The LR comparing H1: MP and REF full brothers, versus H2: MP and REF unrelated,
                        has been computed for 100 unconditional simulations of MP and REF conditioned on H1 below.
                        This corresponds to `REF`case in the panel to the right. We see that we can expect no exclusions
                        (in fact exclusions are impossible with only two brothers) and log10(LR) slightly exceeding 10. If one brother, 
                        `E1` is genotyped we can expect more than 10 exclusions and a log10(LR) slightly exceeding 20. Finally,
                        if both brothers `E1`and `E2` are genotyped, the expected number exclusions and LR increase further.
                        10 profiles are simulated for the relatives, assuming H1,  of `MP`. For each of these 10 profiles,
                        corresponding to the smaller circles, 1000 simulations are performed for `MP` under H1 and H2.
                        In `Prioritise > Built in cases` simulations can be performed for various parameter choices. In
                        `Prioritise > load` simillar simulations can be performed from a fam file that comes with the app our your own.
                        "),
                        br(),
                        sidebarLayout(position = "left",
                          sidebarPanel(
                            selectInput("pedigreePri", label = "Built in pedigree for priority simulation",
                                        choices = list( "None selected", "brother"),
                            ),
                          ),
                           mainPanel(
                            fluidRow(
                              column(plotOutput("priPlotPremade"),  width = 9)
                            )
                          )
                        ),
                 ), 

                 tabPanel("Built in cases",
                          p("Simulations explained in `Prioritise > Demonstrations` can be performed"),
                          br(),
                          sidebarLayout(position = "left",
                            sidebarPanel(
                              checkboxInput("plotOnly", label = "Only plot pedigree", value = FALSE),
                              numericInput("nProfiles", "No of sims for references", min = 1, max = 10, value = 1),
                              sliderInput("lastMarkerPri", "Last Marker", min = 1, max = 35, step = 1, value = 13),
                              selectInput("pedigreePowerSimulatedPri", 
                                        label = "Built in pedigree for power simulation",
                                        choices = list( "None selected", "Missing brother", 
                                        "Missing uncle"),
                                        ),
                                      ),
                                        mainPanel(
                                          fluidRow(
                                            column(plotOutput("powerPlotSimulatedPri"),  width = 9)
                                          )
                                        )
                          ),
                 ),
                                  
                 tabPanel("Load data",
                          p("Explain what this is all about bla bla bla bla bla bla bla bla bla bla"),
                          br(),
                          sidebarLayout(position = "left",
                            sidebarPanel(
                              fileInput("priPower", "Familias file for priority simulation"), ),
                                mainPanel(
                                  fluidRow( column(plotOutput("priPlotFam"),  width = 9))
                                        )
                          ),
                  ),
               ),
                          
                navbarMenu("DVI",
                     tabPanel("Built in cases",
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
                       numericInput("seed", "Seed", min = 1, max = 100000, step = 1, value = 1729),
                       numericInput("nSimulations", "No of simulations", min = 0, max = 10000, step = 100, value = 100),
                       checkboxInput("mutation", label = "Mutation", value = FALSE),   
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

  output$powerPlotSimulated = renderPlot( {
    if(input$pedigreePowerSimulated == "Missing brother"){
      claim = nuclearPed(fa = "FA", mo = "MO", children = c("MP", "REF"))
      pedPower(claim, nsim = input$nSimulations, seed = input$seed, 
               thresh = input$threshold, lastMarker = input$lastMarker)
    }
    else if(input$pedigreePowerSimulated == "Missing uncle"){
      x = nuclearPed(2, father = "FA", mother ="MO1", children = c("MP", "BR"))
      x = addSon(x, parent = "BR",  id = "REF")
      claim = relabel(x, "MO2", "NN_1")
      pedPower(claim, nsim = input$nSimulations, seed = input$seed,  
               thresh = input$threshold, lastMarker = input$lastMarker)
    }
    else if(input$pedigreePowerSimulated == "Missing first cousin"){
      x = cousinPed(1)
      claim = relabel(x, c("MP","REF"), 7:8)
      pedPower(claim, nsim = input$nSimulations, seed = input$seed,  
               thresh = input$threshold, lastMarker = input$lastMarker)
    }
    else if(input$pedigreePowerSimulated == "Missing GF, 2 grandchildren typed"){
      IDS = c("MP","REF1", "REF2")
      x = cousinPed(1)
      claim = relabel(x, IDS, c(1,7, 8))
      pedPower(claim, ids = IDS, nsim = input$nSimulations, seed = input$seed,  
               thresh = input$threshold, lastMarker = input$lastMarker)
    }
  })  
  
  output$powerPlotSimulatedPri = renderPlot( {
    if(input$pedigreePowerSimulatedPri == "Missing brother"){
      ped = nuclearPed(2, father = "FA", mother ="MO", children = c("MP", "REF"))
      ped = addChildren(ped, father = "FA", mother = "MO", nch = 2, 
                        sex = 1, ids = c("E1", "E2"))
      priPower(ped, plotPed = input$plotOnly, nMark = input$lastMarkerPri, seed = input$seed,  
               nProfiles = input$nProfiles, lrSims = input$nSimulations)
    }
    else if(input$pedigreePowerSimulatedPri == "Missing uncle"){
      x = nuclearPed(2, father = "FA", mother ="MO1", children = c("MP", "E2"))
      x = addSon(x, parent = "E2",  id = "REF")
      ped = relabel(x, "E1", "NN_1")      

      priPower(ped, plotPed = input$plotOnly, nMark = input$lastMarkerPri, seed = input$seed,  
               nProfiles = input$nProfiles, lrSims = input$nSimulations)
    }

  })   
  
  
  output$powerPlotFam = renderPlot({
      file = input$famPower
      ext = getExt(file = file)
      if(input$simulateFam)
        familias(file = file, method = "Power", DVI = FALSE, threshold = input$thresholdFam,
               seed = input$seed, lrSims = input$nSimulations)
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
    updateSelectInput(session, "pedigreePower", selected = "brother")

    updateSliderInput(session, "threshold", value = 10000)
    updateSliderInput(session, "thresholdFam", value = 10000)
    updateSliderInput(session, "lastMarker", value = 35)
    updateSelectInput(session, "pedigreePowerSimulated", selected = "None selected")
    
    updateCheckboxInput(session, "plotOnly", value = FALSE)
    updateNumericInput(session, "nProfiles", value = 1)
    updateSliderInput(session, "lastMarkerPri", value = 13)    
    updateSelectInput(session, "pedigreePowerSimulatedPri", selected = "None selected")
    
    updateSelectInput(session, "pedigreePri", selected = "brother")
    updateSelectInput(session, "powerPlotFam", selected = "None selected")
    updateSelectInput(session, "dat", selected = "None selected")
    updateCheckboxInput(session, "simulateFam", value = FALSE)
    updateCheckboxInput(session, "relabel", value = FALSE)
    updateNumericInput(session, "refFam", value = 1)
    updateNumericInput(session, "nMissing", value = -1)
    updateNumericInput(session, "nSimulations", value = 100)
    updateNumericInput(session, "seed", value = 1729)
    updateSelectInput(session, "analysis", selected = "None selected")
    updateCheckboxInput(session, "mutation", label = "Mutation", value = FALSE) 
  })

   # Change 0 simulations to 1 always 
   observeEvent(input$nSimulations, {
     x = input$nSimulations
     if(x == 0 | is.na(x))
       updateNumericInput(session,"nSimulations", value = 10)
   })
  
}



  shinyApp(ui, server)
