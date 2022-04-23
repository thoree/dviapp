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
                          
          tabPanel("Explanations",
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
                 
            tabPanel("Analyses based on built in cases",
              p("The functionality is explained in `Power > Explanations`. The threshold for the LR can be reduced from the
                default of 10,000 below. Also, you can choose to include only markers 1,..., `Last Marker` of the database 
                `NorwegianFrequencies`. The number of simulations and the seed can be changed in `Settings`.
                To perform the simulation, uncheck `Only plot pedigree`."),
              br(),
                sidebarLayout(position = "left",
                                
                  sidebarPanel(
                    checkboxInput("plotOnlyBuiltPower", label = "Only plot pedigree", value = TRUE),                     
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
               
              tabPanel("Analyses based on user loaded data",
                p("Power is calculated by uploading a Familias file below and unchecking  `Only plot pedigree`. The missing person should be
                   named `MP` and the reference `REF`. The file `BrotherPower.fam` gives output similar to that
                   in `Power > Explanations` (but not identical, even for the same seed, since the simulation
                   implementation is not identical). Further information on the fam file loaded, and potential 
                   errors in the conversion of the fam file, are reported
                   to the console."),
                br(),
                  sidebarLayout(position = "left",
                    sidebarPanel(
                      checkboxInput("plotOnlyFamPower", "Only plot pedigree", value = TRUE),
                      sliderInput("thresholdFam", "Threshold", min = 0, max = 10000, step = 100, value = 10000),
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
                          
                 tabPanel("Explanations",
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

                 tabPanel("Analyses based on built in cases",
                          p("Simulations explained in `Prioritise > Explanations` can be performed"),
                          br(),
                          sidebarLayout(position = "left",
                            sidebarPanel(
                              checkboxInput("plotOnlyBuiltPri", label = "Only plot pedigree", value = TRUE),
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
                                  
                 tabPanel("Analyses based on user loaded data",
                          p("Priority power is calculated by uploading a Familias file below and unchecking  `Only plot pedigree`. 
                            The missing person should be named `MP`, the reference `REF` and the extra candidatesfor
                            genotyping `E1`and `E2`. The file `Brother.fam` gives output similar to that
                            in `Priority > Explanations` (but not identical, even for the same seed, since the simulation
                            implementation is not identical). Further information on the fam file loaded, and potential 
                            errors in the conversion of the fam file, are reported to the console."),
                          br(),
                          sidebarLayout(position = "left",
                            sidebarPanel(
                              checkboxInput("plotOnlyFamPri", label = "Only plot pedigree", value = TRUE),
                              fileInput("priPower", "Familias file for priority simulation"), ),
                                mainPanel(
                                  fluidRow( column(plotOutput("priPlotFam"),  width = 9))
                                        )
                          ),
                  ),
               ),
                          
                navbarMenu("DVI",
                           
                           tabPanel("Explanations",
                                    p("Analyses can be done in this module from built in cases, from Familias (`fam`)
                                      files or from R data. The below figure shows the planecrash data. When the
                                      data is loaded in `DVI > Built in cases`, the following summary is provided"),
                                    p("- DVI data. 8 victims: V1 ,..., V8 . 5 missing: M1 ,..., M5 . 
                                      5 typed refs: R1 ,..., R5 . 5 reference families."),
                                    p("See the documentation for the details on the five analyses implemented. Here
                                      we only provide brief explanations"),
                                    p("- IBD estimates.  The pairwise relationship between all pairs of victims is
                                         estimated"),
                                    p("- Exlusion. Each victim is tried as each missing person and the number 
                                         of exclusions is given"),
                                    p("- Pairwise LR. For each victim V and each missing person M, the LR comparing
                                      `V = M` to `V and M unrelated` is calculated"),
                                    p("- Joint.  All possible assignments of victims to missing,
                                      are evaluated and solutions ranked according to the likelihood."),
                                    p("- Posterior.  Computes posterior pairing and non-pairing probabilities, 
                                      based on a prior and the output from `Joint`."),
                                    mainPanel(
                                      fluidRow(
                                        column(plotOutput("planecrashPlot"),  width = 9)
                                      )
                                      
                                    ),
                           ),
                           
                     tabPanel("Analyses based on built in cases",
                              p("Select built in data below (or go to `Load data` if you would like to analyse 
                                 your own data)"),
                              br(),
                              sidebarLayout(position = "left",
                                            sidebarPanel(
                                            selectInput(
                                                "dat", 
                                                label = "Built in data for DVI",
                                                choices = list("None selected",
                                                               "Family with three missing", 
                                                               "grave",
                                                               "planecrash")
                                              ),
                                            numericInput(
                                              "refFam", 
                                              "Reference family to plot",
                                              min = 0,
                                              value = 0
                                            ),                                        
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
                                                column(textOutput("DVISummaryBuiltIn"),  width = 8),
                                                column(plotOutput("plot"),  width = 8),
                                                column(tableOutput("table"),  width = 8)
                                              )
                                              
                                            )
                              ),
                     ),
                     
                      tabPanel("Analyses based on user loaded data",
                              p("Analyses based on  your own data (Familias fam files or R RData files).
                                 If `Relabel` is ticked, names are changed to V1, ..(for pm-samples); 
                                 M1,..., (for am-samples) and F1, .... (for reference families). 
                                 Relabelling is required for some analyses."),
                              br(),
                              sidebarLayout(position = "left",
                                sidebarPanel(
                                  fileInput("file1", "User data (optional fam or RData file) for DVI"),
                                  checkboxInput("relabel", label = "Relabel", value =  FALSE),
                                  numericInput("nMissing", "Total no of missing. Only required for fam files with multiple missing in families", min = -1, value = -1),
                                  numericInput(
                                    "refFamLoad", 
                                    "Reference family to plot",
                                    min = 0,
                                    value = 0
                                  ),                                        
                                  selectInput(
                                    "analysisLoad",
                                    label = "Choose DVI analysis",
                                    choices = list("None selected",
                                                   "IBD estimates",
                                                   "Exclusion",
                                                   "Pairwise",
                                                   "Joint",
                                                   "Posterior")
                                  ),
                                  downloadButton("downloadTableLoad", "Download DVI table output")
                                ),
                                            mainPanel(
                                              fluidRow(
                                                column(textOutput("DVISummaryLoad"),  width = 8),
                                                column(plotOutput("plotLoad"),  width = 8),
                                                column(tableOutput("tableLoad"),  width = 8)                                                
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
  
  output$planecrashPlot = renderImage( {
      list(src = "figures/planecrash.png")
  } 
  , deleteFile = FALSE)

  output$powerPlotSimulated = renderPlot( {
    if(input$pedigreePowerSimulated == "Missing brother"){
      claim = nuclearPed(fa = "FA", mo = "MO", children = c("MP", "REF"))
      pedPower(claim, nsim = input$nSimulations, seed = input$seed, 
               thresh = input$threshold, lastMarker = input$lastMarker,
               plotOnly = input$plotOnlyBuiltPower)
    }
    else if(input$pedigreePowerSimulated == "Missing uncle"){
      x = nuclearPed(2, father = "FA", mother ="MO1", children = c("MP", "BR"))
      x = addSon(x, parent = "BR",  id = "REF")
      claim = relabel(x, "MO2", "NN_1")
      pedPower(claim, nsim = input$nSimulations, seed = input$seed,  
               thresh = input$threshold, lastMarker = input$lastMarker,
               plotOnly = input$plotOnlyBuiltPower)
    }
    else if(input$pedigreePowerSimulated == "Missing first cousin"){
      x = cousinPed(1)
      claim = relabel(x, c("MP","REF"), 7:8)
      pedPower(claim, nsim = input$nSimulations, seed = input$seed,  
               thresh = input$threshold, lastMarker = input$lastMarker,
               plotOnly = input$plotOnlyBuiltPower)
    }
    else if(input$pedigreePowerSimulated == "Missing GF, 2 grandchildren typed"){
      IDS = c("MP","REF1", "REF2")
      x = cousinPed(1)
      claim = relabel(x, IDS, c(1,7, 8))
      pedPower(claim, ids = IDS, nsim = input$nSimulations, seed = input$seed,  
               thresh = input$threshold, lastMarker = input$lastMarker,
               plotOnly = input$plotOnlyBuiltPower)
    }
  })  
  
  output$powerPlotSimulatedPri = renderPlot( {
    if(input$pedigreePowerSimulatedPri == "Missing brother"){
      ped = nuclearPed(2, father = "FA", mother ="MO", children = c("MP", "REF"))
      ped = addChildren(ped, father = "FA", mother = "MO", nch = 2, 
                        sex = 1, ids = c("E1", "E2"))
      priPower(ped, plotPed = input$plotOnlyBuiltPri, nMark = input$lastMarkerPri, seed = input$seed,  
               nProfiles = input$nProfiles, lrSims = input$nSimulations)
    }
    else if(input$pedigreePowerSimulatedPri == "Missing uncle"){
      x = nuclearPed(2, father = "FA", mother ="MO1", children = c("MP", "E2"))
      x = addSon(x, parent = "E2",  id = "REF")
      ped = relabel(x, "E1", "NN_1")      

      priPower(ped, plotPed = input$plotOnlyBuiltPri, nMark = input$lastMarkerPri, seed = input$seed,  
               nProfiles = input$nProfiles, lrSims = input$nSimulations)
    }

  })   
  
  
  output$powerPlotFam = renderPlot({
      file = input$famPower
      ext = getExt(file = file)
      familias(file = file, method = "Power", DVI = FALSE, threshold = input$thresholdFam,
               seed = input$seed, lrSims = input$nSimulations, 
               plotOnly = input$plotOnlyFamPower)
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
    familias(file = file, method = "Prioritise", DVI = FALSE,
             plotOnly = input$plotOnlyFamPri)
  })
  

    output$DVISummaryBuiltIn <- renderText({
      if(input$dat == "Family with three missing")
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
    
    output$tableLoad <- renderTable(rownames = T,{
      if(input$analysisLoad == "IBD estimates")
        tableIBD()
      else if (input$analysisLoad == "Exclusion")
        tableExclusion()
      else if (input$analysisLoad == "Pairwise")
        tablePairwise()
      else if (input$analysisLoad == "Joint")
        tableJoint()
      else if (input$analysisLoad == "Posterior")
        tablePosterior()
    })
    
    # Defines reactive functions for table output, i.e., for
    # IBD estimates, exclusion, pairwise, joint and posterior
    
    tableIBD = reactive({
      if(input$dat == "Family with three missing")
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
      if(input$dat == "Family with three missing")
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
      if (input$dat == "Family with three missing")
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
      if(input$dat == "Family with three missing")
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
      if (input$dat == "Family with three missing")
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

    output$downloadTableLoad <- downloadHandler(
      filename = "DVITableOutput.csv",
      content = function(file = filename) {
        if(input$analysisLoad == "IBD estimates")
          write.csv(tableIBD(), file, row.names = TRUE, quote = F)
        else if (input$analysisLoad == "Exclusion")
          write.csv(tableExclusion(), file, row.names = TRUE, quote = F)
        else if (input$analysisLoad == "Pairwise")
          write.csv(tablePairwise(), file, row.names = TRUE, quote = F)
        else if (input$analysisLoad == "Joint")
          write.csv(tableJoint(), file, row.names = TRUE, quote = F)
        else if (input$analysisLoad == "Posterior")
          write.csv(tablePosterior(), file, row.names = TRUE, quote = F)
      })
    
    output$plot <- renderPlot({
      if(input$refFam > 0) {
        file = input$file1
        ext = tools::file_ext(file$datapath)
        if(input$dat == "Family with three missing"){
          if(input$refFam > 1)
             stop(safeError("Impossible value for `Reference family to plot`, only 1 reference family"))
          plot(example1$am, hatched = typedMembers, title = "Reference family",
               cex = 1.2, col = list(red = planecrash$missing,
                                     blue = typedMembers(example1$am)))
        }
        
        else if (input$dat == "grave"){
          if(input$refFam > 1)
            stop(safeError("Impossible value for `Reference family to plot`, only 1 reference family"))
          plot(grave$am, marker = 1, hatched = typedMembers, title = "Reference family",
               col = list(red = grave$missing, blue = typedMembers(grave$am), cex = 1.2))
        }
        
        else if (input$dat == "planecrash"){
          if(input$refFam > 5)
            stop(safeError("Impossible value for `Reference family to plot`, only 5 reference families"))
          plot(planecrash$am[[input$refFam]], hatched = typedMembers, 
                      title = paste("Reference family ", input$refFam),
                      col = list(red = planecrash$missing,
                                 blue = typedMembers(planecrash$am[[input$refFam]])))
        }
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
      }

    })
    
    output$plotLoad <- renderPlot({
      if(input$refFamLoad > 0) {
          file = input$file1
          ext = getExt(file = file)
          if (ext == "RData" |  ext == "rda") 
            RData(file = input$file1, method = 'plot', refFam = input$refFamLoad)
          else if (ext == "fam"){
            if (input$nMissing < 0)
              MPs = 'Missing person'
            else
              MPs = paste0("M", 1:input$nMissing)
            familias(file = file, method = 'plot', 
                     relabel = input$relabel, refFam = input$refFamLoad, miss = MPs)
          }
        }
    })
    
  observe({
    if(!is.null(input$file1)){
      updateSelectInput(session, "dat", label = "Built in data for DVI",
                        choices = list("None selected","Family with three missing", "grave", "planecrash"),
                        selected = "None selected") 
      updateNumericInput(session, "refFam", value = 0) 
      updateSelectInput(session, "analysis", selected = "None selected")
    }
 
  })
 
   observeEvent(input$reset, {
    updateSelectInput(session, "pedigreePower", selected = "brother")

    updateSliderInput(session, "threshold", value = 10000)
    updateSliderInput(session, "thresholdFam", value = 10000)
    updateSliderInput(session, "lastMarker", value = 35)
    updateSelectInput(session, "pedigreePowerSimulated", selected = "None selected")

    updateCheckboxInput(session, "plotOnlyBuiltPower", value = TRUE)
    updateCheckboxInput(session, "plotOnlyBuiltPri", value = TRUE)
    updateCheckboxInput(session, "plotOnlyFamPri", value = TRUE)

    updateCheckboxInput(session, "input$plotOnlyFamPower", value = TRUE)

    updateNumericInput(session, "nProfiles", value = 1)
    updateSliderInput(session, "lastMarkerPri", value = 13)    
    updateSelectInput(session, "pedigreePowerSimulatedPri", selected = "None selected")
    
    updateSelectInput(session, "pedigreePri", selected = "brother")
    updateSelectInput(session, "powerPlotFam", selected = "None selected")
    updateSelectInput(session, "dat", selected = "None selected")
    updateCheckboxInput(session, "plotOnlyFamPower", value = TRUE)
    updateCheckboxInput(session, "relabel", value = FALSE)
    updateNumericInput(session, "refFam", value = 0)
    updateNumericInput(session, "refFamLoad", value = 0)
    updateNumericInput(session, "nMissing", value = -1)
    updateNumericInput(session, "nSimulations", value = 100)
    updateNumericInput(session, "seed", value = 1729)
    updateSelectInput(session, "analysis", selected = "None selected")
    updateSelectInput(session, "analysisLoad", selected = "None selected")
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
