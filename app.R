suppressPackageStartupMessages({
  library(shiny)
  library(dvir)
  library(forrel)
  library(glue)
  library(ggplot2)
  library(patchwork)
  library(magrittr)
  library(shiny.i18n)
  library(yaml)
})


i18n <- Translator$new(translation_json_path = paste0(getwd(), '/final-translation.json'),
                       translation_csv_config = "data/config.yaml")

# Set language, English "en", Spanish "es"
# i18n$set_translation_language("es")

VERSION = 1.0

ui <- fluidPage(
  
  shiny.i18n::usei18n(i18n),

  titlePanel(i18n$t("Disaster Victim Identification")),
  
    navbarPage(i18n$t("Introduction"),
               
      # Button to return to introduction         
       tabPanel(icon("home"),
               
            mainPanel(
              fluidRow(
              column(tags$img(src = "bookKETP.png", width = "176px", height = "220px"), width = 4),
              column(
                
                markdown(i18n$t("Introduction_long_1")),
                
                "REMOVE: This app deals with Disaster Victim Identification (DVI) problems and power 
                calculation for kinship problems. Our goal has been to make  available functionality
                in the `pedsuite` of R libraries and also the `dvir` library. We also expand on functionality 
                in the", 
                a(href="https://www.familias.no", "Familias software.",target="_blank"),
                "There are tree modules, all based on built in cases or user data (Familias or R files):",
                br(),
                strong("- Power:"), "Simulations can be done to determine if goals are likely to be achieved.",
                br(),
                strong("- Priority:"), "The aim is to find the optimal extra persons to genotype.",
                br(),
                strong("- DVI:"), "Methods to include or exclude missing persons are provided.",
                br(), 
                "For more information, check the books:",
                
                a(href="https://www.elsevier.com/books/mass-identifications/kling/978-0-12-818423-3",  
                "Mass identications,",target="_blank"), 
                
                "(Kling et al., 2021),",
                
                a(href="https://www.elsevier.com/books/pedigree-analysis-in-r/vigeland/978-0-12-824430-2",
                "Pedigree Analysis in R",target="_blank")," 
                
                (Vigeland, 2021), and the",
                
                a(href="https://www.nature.com/articles/s41598-021-93071-5", "dvir paper",target="_blank"),
                
                "(Vigeland and Egeland, 2021). 
                
                For further documentation and bug reporting, please go ",
                
                a(href="https://github.com/thoree/dviapp", "here.",target="_blank"),
                
                width = 8)
               ),
            ),
       ),
               
        navbarMenu(i18n$t("Power"),
          tabPanel(i18n$t("Explanations"),
                   
               markdown(i18n$t("Power_long_1")),
               
               "REMOVE: LR comparing H1: `MP and REF full brothers`, versus H2: `MP and REF` unrelated,
               has been computed for 1000 simulations of MP and REF conditioned on H1 below.
               The simulations use the 35 markers in the database `NorwegianFrequencies` 
               documented in the R library forrel. In `Power > Analyses based on built in cases` some prepared
               cases can be run and parameters like the number of markers, 
               can  be changed. In `Power > Analyses based on user loaded data`, 
               similar output is obtained by loading a familias file
               prepared by the user. The simulations will be conditioned on genotyped individuals, if any.",
               fluidRow(
                 column(tags$img(src = "brotherPow.png", width = "792px", height = "240px"), width = 12),
                 ), 
          ),
                 
          tabPanel(i18n$t("Analyses based on built in cases"),
                     
            actionButton("resetPowerBuilt", label = i18n$t("Reset window"), class = "btn btn-danger",
              style = "position: absolute; bottom:30px; width: 170px"),
                  
            br(),
              sidebarLayout(position = "left",
                sidebarPanel(
                  selectInput("pedigreePowerSimulated", label = i18n$t("Pedigree"),
                    choices = c( 
                      "None selected", 
                      "Missing father",
                      "Missing brother", 
                      "Missing uncle", 
                      "Missing first cousin",
                      "Missing GF, 2 grandchildren typed"),
                    selected = "None selected",
                    ),
                    sliderInput("lastMarker", label = i18n$t("No of markers"), 
                                min = 1, max = 35, step = 1, value = 22),
                    checkboxInput("log10Power", label = "log10(LR)", value = TRUE),
                  
                    actionButton("goPowerBuilt", label = i18n$t("Simulate!"), class = "btn-success"),
                ),
                mainPanel(
                  fluidRow(
                    column(plotOutput("powerPlotPedigree"),  width = 3),
                    column(plotOutput("powerPlotSimulated"),  width = 9)
                  )
                  )
                ),
              ),
               
              tabPanel(i18n$t("Analyses based on user loaded data"),
                actionButton("resetPowerFam", i18n$t("Reset window"), class = "btn btn-danger",
                  style = "position: absolute; bottom:30px; width: 170px"),
                
                markdown(i18n$t("Power_long_2")),
                
                "REMOVE: The missing person should be named `MP` and the reference `REF` in the file. The file",
                a(href = "https://familias.name/dviapp/BrotherPower.fam", "BrotherPower.fam", target="_blank"),
                "gives output similar to that in `Power > Explanations` (but not identical, even for the 
                same seed, since the simulation implementation is not identical).  Genotyped individuals 
                (if any) are hatched and first marker displayed in the plot and these individuals
                will be conditioned on. Here's an example file", 
                a(href = "https://familias.name/dviapp/BrotherPowerConditioned.fam", 
                  "BrotherPowerConditioned.fam", target="_blank"),
                br(),
                  sidebarLayout(position = "left",
                    sidebarPanel(width = 3,
                      fileInput("famPower", "Familias file"),
                      checkboxInput("log10PowerFam", label = "log10(LR)", value = TRUE), 
                       actionButton("goPowerLoad", label = i18n$t("Simulate!"), class = "btn-success"),
                    ),
                        mainPanel(width = 9,
                          fluidRow(
                            column(plotOutput("powerPlotFamPedigree"),  width = 3),
                            column(plotOutput("powerPlotFam"),  width = 9)
                          )
                        )
                    ),
                 ),              
               ), 
 
               navbarMenu(i18n$t("Prioritise"), 
                          
                 tabPanel(i18n$t("Explanations"),
                          
                 markdown(i18n$t("Prioritise_long_1")),
                 
                   "REMOVE: The below explanation applies to the example obtained if 'brother' (default) 
                    is chosen in the pull down menu below. The LR comparing H1: `MP and REF full brothers`, 
                    to H2: `MP and REF unrelated`, has been computed for 100 unconditional simulations 
                    of MP and REF conditioned on H1 below. This corresponds to the `REF` case in the panel 
                    to the right. We see that we can expect no exclusions
                    (in fact exclusions are impossible with only two brothers) and log10(LR) slightly exceeding 10. 
                    If one brother, 
                    `E1` is genotyped we can expect more than 10 exclusions and a log10(LR) slightly exceeding 20.
                    Finally, if both brothers `E1`and `E2` are genotyped, the expected number of exclusions and 
                    LR increase further.  10 profiles are simulated for the relatives ('REF', `E1` and `E2`), assuming H1. 
                    For each of these 10 profiles, corresponding to the smaller circles, 1000 
                    simulations are performed for `MP` under H1 and H2.
                    In `Prioritise > Analyses based on built in cases` simulations can be performed 
                    for various parameter choices. In
                    `Prioritise > Analyses based on user loaded data` similar simulations can be performed from a
                    fam file.",
                   
                    br(),
                    sidebarLayout(position = "left",
                      sidebarPanel(
                        selectInput("pedigreePri", label = i18n$t("Built in pedigree for priority simulation"),
                          choices = list( "brother", "None selected" ),
                          ),
                        ),
                        mainPanel(
                          fluidRow(
                            column(plotOutput("priPlotPremade"),  width = 12)
                            )
                          )
                      ),
                 ), 

                 tabPanel(i18n$t("Analyses based on built in cases"),
                          
                   actionButton("resetPriBuilt", label = i18n$t("Reset window"), class = "btn btn-danger",
                     style = "position: absolute; bottom:30px; width: 170px"),
                          
                     sidebarLayout(position = "left",
                       sidebarPanel(
                         selectInput("pedigreePowerSimulatedPri", 
                           label = i18n$t("Pedigree"),
                             choices = list( 
                               "None selected", 
                               "Missing brother",
                               "Missing uncle"),
                           ),
                         sliderInput("lastMarkerPri", label = i18n$t("No of markers"), min = 1, max = 35, 
                                     step = 1, value = 22),
                         actionButton("goPriBuilt", label = i18n$t("Simulate!"), class = "btn-success"),
                       ),
                       mainPanel(
                         fluidRow(
                           column(plotOutput("powerPlotSimulatedPriPedigree"), width = 3),
                           column(plotOutput("powerPlotSimulatedPri"), width = 9)
                           )
                         )
                       ),
                 ),
                                  
                 tabPanel(i18n$t("Analyses based on user loaded data"),
                 
                 markdown(i18n$t("Prioritise_long_2")),

                  "REMOVE :Priority power is calculated by uploading a Familias file. Here's an example:",
                  
                  a(href="https://familias.name/dviapp/BrotherPriority.fam", "BrotherPriority.fam", target="_blank"),
                  
                  ". The missing person should be named `MP`, the reference `REF`, and the extra candidates
                  for genotyping `E1`and `E2`. The mentioned file gives output similar to that
                  in `Priority > Explanations` (but not identical, even for the same seed, since this is simulation).",
                  
                  br(),
                    sidebarLayout(position = "left",
                      sidebarPanel(width = 3,
                        fileInput("priPower", label = i18n$t("Familias file")),
                        actionButton("goPriLoad", label = i18n$t("Simulate!"), class = "btn-success"),
                        ),
                      mainPanel(width = 9,
                        fluidRow( column(plotOutput("priPlotFamPedigree"),  width = 3),
                                  column(plotOutput("priPlotFam"),  width = 9))
                       )
                      ),
                   ),
               ),
                          
                navbarMenu(i18n$t("DVI"),
                  
                  tabPanel(i18n$t("Explanations"),
                           
                    markdown(i18n$t("DVI_long_1")), 
                    
                    "REMOVE: Analyses can be done in this module from built in cases, from Familias (`fam`)
                    files or from R data. The below figure shows the planecrash data. When the
                    data is loaded in `DVI > Analyses based on built in cases`, 
                    the following summary is provided:,
                    `DVI data. 8 victims: V1 ,..., V8 . 5 missing. 
                    5 typed refs: R1 ,..., R5 . 5 reference families.`
                    The data is also available as a fam file:",
                   
                     a(href="https://familias.name/dviapp/planecrash.fam", "planecrash.fam", target="_blank"),
                                    
                    ", and can also be downloaded as RData: ",
                                    
                    a(href="https://familias.name/dviapp/planecrash.RData", "planecrash.RData", target="_blank"),
                                      
                    ". See the documentation for the details on the five analyses implemented. Here
                    we only provide brief explanations:", br(), br(), 
                    strong("IBD estimates:"),"The pairwise relationship between all pairs of victims is estimated.",
                    
                    br(),
                    strong("Exclusion:"),"Each victim is tried as each missing person and the number 
                    of exclusions is given.",
                    br(),
                    strong("Pairwise LR:"), "For each victim V and each missing person M, the LR comparing
                    `V = M` to `V and M unrelated` is calculated.",
                    br(),
                    strong("Joint:"),  "All possible assignments of victims to missing persons
                    are evaluated and solutions ranked according to the likelihood.",
                    br(),
                    strong("Posterior:"),  "Computes posterior pairing probabilities, i.e.,
                    the probability that a victim V is the missing person M.", 
                    br(),
                    
                    mainPanel(
                      fluidRow(
                        column(plotOutput("planecrashPlot"),  width = 9)
                        )
                      ),
                    ),
                           
                    tabPanel(i18n$t("Analyses based on built in cases"),
                              
                      actionButton("resetDVIBuilt", label = i18n$t("Reset window"), class = "btn btn-danger",
                        style = "position: absolute; bottom:30px; width: 170px"),
                      
                          sidebarLayout(position = "left",
                            sidebarPanel(width = 4,
                              fluidRow(
                                column(width = 9,           
                                  selectInput("datDVIBuilt",  label = i18n$t("Case"), 
                                               choices = list("None selected", 
                                               "planecrash", 
                                               "ExclusionData" = "exclusionExample",
                                               "DVIbook-Example-4.8.1",
                                               "DVIbook-Example-4.8.4",
                                               "DVIbook-Exercise-4.9.7",
                                               "DVIbook-Exercise-4.9.8",
                                               "DVIbook-Exercise-6.2.7",
                                               "Three missing"))
                                   ),
                                column(width = 3, numericInput("refFam", 
                                  i18n$t("Plot"), min = 1, value = 1)),
                              ),
                              selectInput("analysis",
                                label = i18n$t("Choose DVI analysis"),
                                choices = list("None selected", "IBD estimates", "Exclusion","Pairwise",
                                  "Joint", "Posterior")
                                ),
                              fluidRow(
                             column(6, actionButton("goDVIBuilt", label = i18n$t("Analyze!"), class = "btn-success")),
                            column(6, downloadButton("downloadTable", label = i18n$t("Download"))),
                              ),
                            ),
                              mainPanel(width = 8,
                                fluidRow(
                                  column(textOutput("summaryDVIBuilt"),  width = 12), 
                                  column(plotOutput("plotDVIBuilt"),  width = 3),
                                  column(tableOutput("tableDVIBuilt"),  width = 9)
                                  )
                                )
                            ),
                      ),

                      tabPanel(i18n$t("Analyses based on user loaded data"),
                               
                      actionButton("resetDVILoad", i18n$t("Reset window"), class = "btn btn-danger",
                          style = "position: absolute; bottom:30px; width: 170px"), 
                      
                      markdown(i18n$t("DVI_long_2")),
                      
                      "REMOVE: If there are multiple missing persons in a family, like in the case based on
                      the ", a(href = "https://familias.name/dviapp/FamilyWith3Missing.fam", 
                              "FamilyWith3Missing.fam" ,target="_blank"), 
                      ", which is similar to a built-in-case,
                      the number of missing must be specified in `Settings`, (`No missing :3`),
                      and the missing persons must be named
                      `M1`, `M2`,...(this is the case in the linked fam-file).",
                      

                       
                          sidebarLayout(position = "left",
                            sidebarPanel(width = 4,
                              fluidRow(         
                              column(9, fileInput("fileDVI", label = i18n$t("fam - or RData file"))),
                              column(3, checkboxInput("relabel", label = i18n$t("Relabel"), value =  FALSE))
                              ),
                              numericInput("refFamLoad", label =i18n$t("Reference family to plot"), 
                                           min = 0, value = 1),    
                              selectInput("analysisLoad",
                                label = i18n$t("Choose DVI analysis"),
                                choices = list("None selected", "IBD estimates", "Exclusion", "Pairwise",
                                  "Joint", "Posterior")
                                ),
                              
                             actionButton("goDVILoad", label = i18n$t("Analyze!"), class = "btn-success"),
                              downloadButton("downloadTableLoad", i18n$t("Download DVI table output"))
                            ),
                              mainPanel(width = 8,
                                fluidRow(
                                  column(textOutput("DVISummaryLoad"),  width = 12),
                                  column(plotOutput("plotLoad"),  width = 3),
                                  column(tableOutput("tableLoad"),  width = 9)
                                  )
                                )
                            ),
                        ),
                     ),              

                     tabPanel(i18n$t("Settings"),
                              
                       actionButton("reset", label = i18n$t("Reset all"), class = "btn btn-danger",
                       style = "position: absolute; bottom:30px; width: 170px"),
    
                       "Some default settings can be changed below, see:",
                       a(href="https://github.com/thoree/dviapp", "manual.",target="_blank"),
                       br(),
                       
                       fluidRow(
                         column(2, numericInput("seed", label = i18n$t("Seed"), min = 1, 
                                                max = 100000, step = 1, value = 1729)),
                         column(2, numericInput("nSimulations", label =i18n$t("No simulations"), 
                                                min = 0, max = 10000, step = 100, value = 100),),
                         column(3, numericInput("nProfiles", label = i18n$t("No reference simulations"), 
                                                min = 1, max = 10, value = 1)),
                         column(2, numericInput("nMissing", label = i18n$t("No missing"), min = -1, value = -1)),
                         column(3, numericInput("nExcl", label = i18n$t("Exclusion threshold"), min = -0, value = 0)),
                       ),
                       fluidRow(
                         column(4, checkboxInput("truePedH1", label = i18n$t("True ped H1"), value = TRUE)),
                         column(4, checkboxInput("mutation", label = i18n$t("Mutation"), value = FALSE)),
                         column(4, checkboxInput("ignoreSex", label = i18n$t("Ignore sex"), value = TRUE)),
                         ),
                       
                       fluidRow(
                         column(6, sliderInput("thresholdIP", label = i18n$t("LR threshold inclusion power"), 
                                               min = 0, max = 10000, step = 100, value = 10000)),
                         column(6, sliderInput("thresholdLRDisplay", label = i18n$t("Show LR above"), 
                                               min = 0, max = 1000, step = 10, value = 0)),
                         ),
                       fluidRow(
                         div(style = "float: bottom;",
                             selectInput('selected_language',
                                         label = i18n$t("Change language"),
                                         choices = list("en", "es"),
                                         selected = "en")
                         ),
                       )
                       
                    )
              
        )
)   


server <- function(input, output, session) {
  
  observeEvent(input$selected_language, {
    # This print is just for demonstration
    print(paste("Language change!", input$selected_language))
    # Here is where we update language in session
    shiny.i18n::update_lang(session, input$selected_language)
  })

  ### Fix names of victims for dataExercise498 
  names(dataExercise498$pm) = paste0("V", 1:3)
  
  # Power > Analysis based on built in cases. Plot pedigree
  output$powerPlotPedigree = renderPlot( {
    if(input$pedigreePowerSimulated == "Missing brother"){
      claim = nuclearPed(fa = "FA", mo = "MO", children = c("MP", "REF"))
      plot(claim, hatched = typedMembers, col = list(red = "MP", blue = "REF"))
    }
    else if(input$pedigreePowerSimulated == "Missing father"){
      claim = nuclearPed(fa = "MP", mo = "MO", children = "REF")
      plot(claim, hatched = typedMembers, col = list(red = "MP", blue = "REF"))
    }
    else if(input$pedigreePowerSimulated == "Missing uncle"){
      x = nuclearPed(2, father = "FA", mother ="MO1", children = c("MP", "BR"))
      x = addSon(x, parent = "BR",  id = "REF")
      claim = relabel(x, "MO2", "NN_1")
      plot(claim, hatched = typedMembers, col = list(red = "MP", blue = "REF"))
    }
    else if(input$pedigreePowerSimulated == "Missing first cousin"){
      x = cousinPed(1)
      claim = relabel(x, c("MP","REF"), 7:8)
      plot(claim, hatched = typedMembers, col = list(red = "MP", blue = "REF"))
    }
    else if(input$pedigreePowerSimulated == "Missing GF, 2 grandchildren typed"){
      x = cousinPed(1)
      claim = relabel(x, c("MP", "REF1", "REF2"), c(1,7, 8))
      plot(claim, hatched = typedMembers, col = list(red = "MP", blue = c("REF1", "REF2")))
    }
  }) 
  
  # Power > Analysis based on built in cases. Simulation plot
  output$powerPlotSimulated = renderPlot( {
    
    withProgress(message = "Calculation in progress", value = 0, {
    
    if(input$pedigreePowerSimulated == "Missing brother"){
      claim = nuclearPed(fa = "FA", mo = "MO", children = c("MP", "REF"))
      pedPower(claim, nsim = input$nSimulations, seed = input$seed, 
               lastMarker = input$lastMarker, Log10 = input$log10Power,
               truePedH1 = input$truePedH1)
    }
    else if(input$pedigreePowerSimulated == "Missing father"){
      claim = nuclearPed(2, father = "MP", mother ="MO", children = "REF")
      pedPower(claim, nsim = input$nSimulations, seed = input$seed,  
               lastMarker = input$lastMarker, truePedH1 = input$truePedH1)
    }
    else if(input$pedigreePowerSimulated == "Missing uncle"){
      x = nuclearPed(2, father = "FA", mother ="MO1", children = c("MP", "BR"))
      x = addSon(x, parent = "BR",  id = "REF")
      claim = relabel(x, "MO2", "NN_1")
      pedPower(claim, nsim = input$nSimulations, seed = input$seed,  
               lastMarker = input$lastMarker, truePedH1 = input$truePedH1)
    }
    else if(input$pedigreePowerSimulated == "Missing first cousin"){
      x = cousinPed(1)
      claim = relabel(x, c("MP","REF"), 7:8)
      pedPower(claim, nsim = input$nSimulations, seed = input$seed,  
               lastMarker = input$lastMarker, truePedH1 = input$truePedH1)
    }
    else if(input$pedigreePowerSimulated == "Missing GF, 2 grandchildren typed"){
      IDS = c("MP", "REF1", "REF2")
      x = cousinPed(1)
      claim = relabel(x, IDS, c(1,7, 8))
      pedPower(claim, ids = IDS, nsim = input$nSimulations, seed = input$seed,  
               lastMarker = input$lastMarker, truePedH1 = input$truePedH1)
    }
   })
  }) %>%
    bindEvent(input$goPowerBuilt)


  # Power > Analyses based on user loaded data. Plot pedigree
  output$powerPlotFamPedigree = renderPlot({
    file = input$famPower
    ext = getExt(file = file)
    familias(file = file, method = "Power", DVI = FALSE, seed = input$seed, 
             lrSims = input$nSimulations, Log10 = input$log10PowerFam, plotOnly = TRUE)
  })  
  
  
  # Power > Analyses based on user loaded data. Simulation
  output$powerPlotFam = renderPlot({
    
    withProgress(message = 'Calculation in progress',
                 detail = NULL, value = 0, {
    
      file = input$famPower
      ext = getExt(file = file)
      familias(file = file, method = "Power", DVI = FALSE, seed = input$seed, plotOnly = FALSE,
               lrSims = input$nSimulations, Log10 = input$log10PowerFam, 
               truePedH1 = input$truePedH1)
    })
  }) %>% 
   bindEvent(input$goPowerLoad)
  # 
  
  ### Prioritise
  
  # Prioritise > Explanations: Picture
  output$priPlotPremade = renderImage( {
    if(input$pedigreePri == "brother")
       list(src = "figures/brotherPri.png")
    else if(input$pedigreePri == "uncle")
          list(src = "figures/unclePri.png")
    else if (TRUE)
      list(src = "figures/empty.png")
  } 
    , deleteFile = FALSE)
  
  # Prioritise > Analyses based on built in cases. Pedigree plot
  output$powerPlotSimulatedPriPedigree = renderPlot( {
    if(input$pedigreePowerSimulatedPri == "Missing brother"){
      ped = nuclearPed(2, father = "FA", mother ="MO", children = c("MP", "REF"))
      ped = addChildren(ped, father = "FA", mother = "MO", nch = 2, 
                        sex = 1, ids = c("E1", "E2"))
      plot(ped, col = list(red = "MP", blue = c("REF", "E1", "E2")))
    }
    else if(input$pedigreePowerSimulatedPri == "Missing uncle"){
      x = nuclearPed(2, father = "FA", mother ="MO1", children = c("MP", "E2"))
      x = addSon(x, parent = "E2",  id = "REF")
      ped = relabel(x, "E1", "NN_1") 
      plot(ped, col = list(red = "MP", blue = c("REF", "E1", "E2")))
    }

  })  
    # %>%
    # bindEvent(input$goPriBuilt)

  
  # Prioritise > Analyses based on built in cases. Simulation
  output$powerPlotSimulatedPri = renderPlot({
    
    withProgress(message = 'Calculation in progress',
                 detail = NULL, value = 0, {
                   
    if(input$pedigreePowerSimulatedPri == "Missing brother"){

        ped = nuclearPed(2, father = "FA", mother ="MO", children = c("MP", "REF"))
        ped = addChildren(ped, father = "FA", mother = "MO", nch = 2, 
                        sex = 1, ids = c("E1", "E2"))
        priPower(ped,  lastMarker = input$lastMarkerPri, 
               seed = input$seed,  nProfiles = input$nProfiles, 
               lrSims = input$nSimulations, thresholdIP = input$thresholdIP)
        }
    
    else if(input$pedigreePowerSimulatedPri == "Missing uncle"){
                     
      x = nuclearPed(2, father = "FA", mother ="MO1", children = c("MP", "E2"))
      x = addSon(x, parent = "E2",  id = "REF")
      ped = relabel(x, "E1", "NN_1")     
      priPower(ped, lastMarker = input$lastMarkerPri, seed = input$seed,  
               nProfiles = input$nProfiles, lrSims = input$nSimulations,  thresholdIP = input$thresholdIP)
      }
    })
  })  %>%
  bindEvent(input$goPriBuilt)
  

  # Prioritise > Analyses based on user loaded data. Pedigree plot
  output$priPlotFamPedigree = renderPlot({
    file = input$priPower
    ext = getExt(file = file)
    
    familias(file = file, method = "Prioritise", DVI = FALSE,
             plotOnly = TRUE, nProfiles = input$nProfiles,
             thresholdIP = input$thresholdIP)
                   
  }) 
  
  # Prioritise > Analyses based on user loaded data. Simulation
  output$priPlotFam = renderPlot({
    
    withProgress(message = 'Calculation in progress',
                 detail = NULL, value = 0, {
    
    file = input$priPower
    ext = getExt(file = file)
    familias(file = file, method = "Prioritise", DVI = FALSE,
             plotOnly = F, nProfiles = input$nProfiles,
             thresholdIP = input$thresholdIP)
    
                 })
  }) %>%
  bindEvent(input$goPriLoad)

  ### DVI
    
  # DVI > Explanations: picture
  output$planecrashPlot = renderImage( {
    list(src = "figures/planecrash.png")
  } 
  , deleteFile = FALSE)
  

   # DVI > Analysis based on built in data: Summary
  output$summaryDVIBuilt <- renderPrint({
    if(input$datDVIBuilt == "Three missing")
      summariseDVIreturned(example1$pm, example1$am, example1$missing, header = "Tutorial data. ")
    else if (input$datDVIBuilt == "DVIbook-Exercise-6.2.7")
      summariseDVIreturned(grave$pm, grave$am, grave$missing, header = "grave data. ")
    else if (input$datDVIBuilt == "planecrash")
      summariseDVIreturned(planecrash$pm, planecrash$am, planecrash$missing, 
                           header = "planecrash data. ")
    else if (input$datDVIBuilt == "exclusionExample")
      summariseDVIreturned(exclusionExample$pm, exclusionExample$am, exclusionExample$missing, 
                           header = "exclusionExample data. ")
    else if (input$datDVIBuilt == "DVIbook-Example-4.8.1")
      summariseDVIreturned(dataExample481$pm, dataExample481$am, dataExample481$missing, 
                           header = "DVI book pp 112-112. ")
    else if (input$datDVIBuilt == "DVIbook-Example-4.8.4")
      summariseDVIreturned(dataCh4$pm, dataCh4$am, dataCh4$missing, 
                           header = "DVI book pp 115-116. ")
    else if (input$datDVIBuilt == "DVIbook-Exercise-4.9.7")
      summariseDVIreturned(dataExercise497$pm, dataExercise497$am, dataExercise497$missing, 
                           header = "DVI book pp 124-125. ")
    else if (input$datDVIBuilt == "DVIbook-Exercise-4.9.8")
      summariseDVIreturned(dataExercise498$pm, dataExercise498$am, dataExercise498$missing, 
                           header = "DVI book pp 125-126. ")
      
    }) 

    
    # DVI > Analysis based on user loaded data: Summary
    output$DVISummaryLoad <- renderText({
        file = input$fileDVI
        ext = getExt(file = file)
        if (ext == "RData" |  ext == "rda" )
          RData(file = file, method = "Describe data", nMissingSpecified = input$nMissing)
        else if (ext ==  "fam")
         familias(file = file, method = "Describe data", relabel = input$relabel, DVI = TRUE,
                  nMissingSpecified = input$nMissing)
     })      
    
    # DVI > Analysis based on built in data. First, fix digits
    dig = reactiveVal(-4)
    
    observeEvent(input$analysis, {
      if(input$analysis == "IBD estimates")
        dig(-2)
      else if(input$analysis == "Exclusion") 
        dig(0)
      else if(input$analysis == "Pairwise" | input$analysis == "Joint" )
        dig(-2)
      else if(input$analysis == "Posterior")
        dig(4)
    })
    
    output$tableDVIBuilt <- renderTable(rownames = T,  digits = dig,{
      
        withProgress(message = 'Calculation in progress',
                   detail = NULL, value = 0, {
      
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
     }) %>%
    bindEvent(input$goDVIBuilt)
    


    # DVI > Analysis based on loaded data. First, fix digits
    digLoad = reactiveVal(-4)
    
    observeEvent(input$analysisLoad, {
      if(input$analysisLoad == "IBD estimates")
        digLoad(-2)
      else if(input$analysisLoad == "Exclusion") 
        digLoad(0)
      else if(input$analysisLoad == "Pairwise" | input$analysis == "Joint" )
        digLoad(-2)
      else if(input$analysisLoad == "Posterior")
        digLoad(4)
    })
    output$tableLoad <- renderTable(rownames = T, digit = digLoad,{
      
      withProgress(message = 'Calculation in progress',
                   detail = NULL, value = 0, {
      
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
    }) %>%
    bindEvent(input$goDVILoad)
    
    ### Reactive functions
    
    #IBD, for built in and loaded data, fam or RData
    tableIBD = reactive({
      if(input$datDVIBuilt == "Three missing")
        IBDestimates(example1$pm, thresholdLR = input$thresholdLRDisplay)
      else if (input$datDVIBuilt == "DVIbook-Exercise-6.2.7")
        IBDestimates(grave$pm, thresholdLR = input$thresholdLRDisplay)
      else if(input$datDVIBuilt == "planecrash")
        IBDestimates(planecrash$pm,  thresholdLR = input$thresholdLRDisplay)
      else if(input$datDVIBuilt == "exclusionExample")
        IBDestimates(exclusionExample$pm,  thresholdLR = input$thresholdLRDisplay)
      else if(input$datDVIBuilt == "DVIbook-Example-4.8.1")
        IBDestimates(dataExample481$pm,  thresholdLR = input$thresholdLRDisplay)
      else if(input$datDVIBuilt == "DVIbook-Example-4.8.4")
        IBDestimates(dataCh4$pm,  thresholdLR = input$thresholdLRDisplay)
      else if(input$datDVIBuilt == "DVIbook-Exercise-4.9.7")
        IBDestimates(dataExercise497$pm,  thresholdLR = input$thresholdLRDisplay)
      else if(input$datDVIBuilt == "DVIbook-Exercise-4.9.8")
        IBDestimates(dataExercise498$pm,  thresholdLR = input$thresholdLRDisplay)
      
      else{ 
        file = input$fileDVI
        ext = getExt(file = file)
        if(ext == "RData" |  ext == "rda")
          RData(file = file, method = "IBD estimates",  sorter = TRUE,
                thresholdLR = input$thresholdLRDisplay)
        else if(ext == "fam")
          familias(file = file, method = "IBD estimates", relabel = input$relabel,
                   thresholdLR = input$thresholdLRDisplay)
      }
    })
    
    #Exclusion, for built in and loaded data, fam or RData
    tableExclusion = reactive({
      if(input$datDVIBuilt == "Three missing")
        exclusionMatrix(example1$pm, example1$am , example1$missing)
      else if (input$datDVIBuilt == "DVIbook-Exercise-6.2.7")
        exclusionMatrix(grave$pm, grave$am , grave$missing)
      else if (input$datDVIBuilt == "planecrash")
        exclusionMatrix(planecrash$pm, planecrash$am , planecrash$missing)
      else if (input$datDVIBuilt == "exclusionExample")
        exclusionMatrix(exclusionExample$pm, exclusionExample$am , exclusionExample$missing)
      else if(input$datDVIBuilt == "DVIbook-Example-4.8.1")
        exclusionMatrix(dataExample481$pm, dataExample481$am , dataExample481$missing)
      else if(input$datDVIBuilt == "DVIbook-Example-4.8.4")
        exclusionMatrix(dataCh4$pm, dataCh4$am , dataCh4$missing)
      else if(input$datDVIBuilt == "DVIbook-Exercise-4.9.7")
        exclusionMatrix(dataExercise497$pm, dataExercise497$am , dataExercise497$missing)
      else if(input$datDVIBuilt == "DVIbook-Exercise-4.9.8")
        exclusionMatrix(dataExercise498$pm, dataExercise498$am , dataExercise498$missing)

      else{
        if(!input$relabel)
          stop(safeError("Need to select, load data and tick 'Relabel DVI' for loaded data for Exclusion, Pairwise, Joint and Posterior"))
        file = input$fileDVI
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
    
    #Pairwise, for built in and loaded data, fam or RData
    tablePairwise = reactive({

      if (input$datDVIBuilt == "Three missing")
        myPairwiseLR(example1$pm, example1$am , example1$missing, input$mutation, 
                     ignoreSex = input$ignoreSex)$LRmatrix
      else if (input$datDVIBuilt == "DVIbook-Exercise-6.2.7")
        myPairwiseLR(grave$pm, grave$am , grave$missing, input$mutation, 
                     ignoreSex = input$ignoreSex)$LRmatrix
      else if (input$datDVIBuilt == "planecrash")
        myPairwiseLR(planecrash$pm, planecrash$am, planecrash$missing, input$mutation, 
                     ignoreSex =input$ignoreSex)$LRmatrix
      else if (input$datDVIBuilt == "exclusionExample")
        myPairwiseLR(exclusionExample$pm, exclusionExample$am, exclusionExample$missing, mutation = TRUE, 
                     ignoreSex = input$ignoreSex)$LRmatrix
      else if(input$datDVIBuilt == "DVIbook-Example-4.8.1")
        myPairwiseLR(dataExample481$pm, dataExample481$am, dataExample481$missing, 
                     input$mutation, ignoreSex = input$ignoreSex)$LRmatrix
      else if(input$datDVIBuilt == "DVIbook-Example-4.8.4")
        myPairwiseLR(dataCh4$pm, dataCh4$am, dataCh4$missing, input$mutation, 
                     ignoreSex = input$ignoreSex)$LRmatrix
      else if(input$datDVIBuilt == "DVIbook-Exercise-4.9.7")
        myPairwiseLR(dataExercise497$pm, dataExercise497$am, dataExercise497$missing, 
                     input$mutation, ignoreSex = input$ignoreSex)$LRmatrix
      else if(input$datDVIBuilt == "DVIbook-Exercise-4.9.8")
        myPairwiseLR(dataExercise498$pm, dataExercise498$am, dataExercise498$missing, 
                     input$mutation, ignoreSex = input$ignoreSex)$LRmatrix 
      
      else{
        if(!input$relabel)
          stop(safeError("Need to tick 'Relabel DVI' for Exclusion, Pairwise, Joint and Posterior"))
        file = input$fileDVI
        ext = getExt(file = file)
        if (ext == "RData" |  ext == "rda")
          RData(file = file, method = 'Pairwise', mutation = input$mutation, ignoreSex = input$ignoreSex)
        else {
        if (input$nMissing < 0)
            MPs = 'Missing person'
        else
            MPs = paste0("M", 1:input$nMissing)
          familias(file = file, method = 'Pairwise',  relabel = input$relabel, 
                   miss = MPs, mutation = input$mutation,ignoreSex = input$ignoreSex)
        }
      }
    })
    
    #Joint, for built in and loaded data, fam or RData
    tableJoint = reactive({
      

      if(input$datDVIBuilt == "Three missing")
        myjointDVI(example1$pm, example1$am , example1$missing, mutation = input$mutation,
                   thresholdLR = input$thresholdLRDisplay, ignoreSex = input$ignoreSex)
      else if (input$datDVIBuilt == "DVIbook-Exercise-6.2.7")
        myjointDVI(grave$pm, grave$am , grave$missing, mutation = input$mutation,
                   thresholdLR = input$thresholdLRDisplay,  ignoreSex = input$ignoreSex)
      else if (input$datDVIBuilt == "planecrash")
        myjointDVI(planecrash$pm, planecrash$am, planecrash$missing, mutation = input$mutation,
                   thresholdLR = input$thresholdLRDisplay, ignoreSex = input$ignoreSex)
      else if (input$datDVIBuilt == "exclusionExample")
        myjointDVI(exclusionExample$pm, exclusionExample$am, exclusionExample$missing, mutation = input$mutation,
                   thresholdLR = input$thresholdLRDisplay, ignoreSex = input$ignoreSex,
                   nExcl = input$nExcl)
      else if(input$datDVIBuilt == "DVIbook-Example-4.8.1")
        myjointDVI(dataExample481$pm, dataExample481$am , dataExample481$missing, mutation = input$mutation,
                   thresholdLR = input$thresholdLRDisplay, ignoreSex = input$ignoreSex)
      else if(input$datDVIBuilt == "DVIbook-Example-4.8.4")
        myjointDVI(dataCh4$pm, dataCh4$am , dataCh4$missing, mutation = input$mutation,
                   thresholdLR = input$thresholdLRDisplay)
      else if(input$datDVIBuilt == "DVIbook-Exercise-4.9.7")
        myjointDVI(dataExercise497$pm, dataExercise497$am , dataExercise497$missing, 
                   mutation = input$mutation, thresholdLR = input$thresholdLRDisplay, 
                   ignoreSex = input$ignoreSex)
      else if(input$datDVIBuilt == "DVIbook-Exercise-4.9.8")
        myjointDVI(dataExercise498$pm, dataExercise498$am , dataExercise498$missing, 
                   mutation = input$mutation,
                   thresholdLR = input$thresholdLRDisplay, ignoreSex = input$ignoreSex)
      
      else{
        if(!input$relabel)
          stop(safeError("Need to tick 'Relabel DVI' for Exclusion, Pairwise, Joint and Posterior"))
        
        file = input$fileDVI
        ext = getExt(file = file)
        if (ext == "RData" |  ext == "rda")
          RData(file = file, method = 'Joint', mutation = input$mutation,
                thresholdLR = input$thresholdLRDisplay, ignoreSex = input$ignoreSex, nExcl = input$nExcl)
        else {
        if (input$nMissing < 0)
            MPs = 'Missing person'
        else
            MPs = paste0("M", 1:input$nMissing)
        
          familias(file = file, method = 'Joint',  relabel = input$relabel, miss = MPs,
                   mutation = input$mutation, thresholdLR = input$thresholdLRDisplay,
                   ignoreSex = input$ignoreSex,  nExcl = input$nExcl)
        }
      }
    })
 
    #Posterior, for built in and loaded data, fam or RData   
    tablePosterior = reactive({
      
      if (input$datDVIBuilt == "Three missing")
        Bmarginal(myjointDVI(example1$pm, example1$am, example1$missing, 
                  mutation = input$mutation, ignoreSex = input$ignoreSex), example1$missing)
      else if (input$datDVIBuilt == "DVIbook-Exercise-6.2.7")
        Bmarginal(myjointDVI(grave$pm, grave$am, grave$missing, 
                             mutation = input$mutation, ignoreSex = input$ignoreSex), grave$missing)
      else if (input$datDVIBuilt == "planecrash")
        Bmarginal(myjointDVI(planecrash$pm, planecrsah$am, planecrash$missing, 
                             mutation = input$mutation, ignoreSex = input$ignoreSex), planecrash$missing)
      else if (input$datDVIBuilt == "exclusionExample")
        myBmarginal(exclusionExample$pm, exclusionExample$am, exclusionExample$missing, mutation = input$mutation, 
                    ignoreSex = input$ignoreSex, prior = NULL, nExcl = input$nExcl)
      else if(input$datDVIBuilt == "DVIbook-Example-4.8.4")
        Bmarginal(myjointDVI(dataCh4$pm, dataCh4$am, dataCh4$missing, 
                             mutation = input$mutation, ignoreSex = input$ignoreSex), dataCh4$missing)
      else if(input$datDVIBuilt == "DVIbook-Example-4.8.1")
        Bmarginal(myjointDVI(dataExample481$pm, dataExample481$am, dataExample481$missing, 
                             mutation = input$mutation, ignoreSex = input$ignoreSex), dataCh4$missing)  
      else if(input$datDVIBuilt == "DVIbook-Exercise-4.9.7")
        Bmarginal(myjointDVI(dataExercise497$pm, dataExercise497$am, dataExercise497$missing, 
                             mutation = input$mutation, ignoreSex = input$ignoreSex), dataExercise497$missing) 
      else if(input$datDVIBuilt == "DVIbook-Exercise-4.9.8")
        Bmarginal(myjointDVI(dataExercise498$pm, dataExercise498$am, dataExercise498$missing, 
                             mutation = input$mutation, ignoreSex = input$ignoreSex), dataExercise498$missing)
      
      else{
        if(!input$relabel)
          stop(safeError("Need to tick 'Relabel DVI' for Exclusion, Pairwise, Joint and Posterior"))
        
        file = input$fileDVI
        ext = getExt(file = file)
        if (ext == "RData" |  ext == "rda")
          RData(file = file, method = 'Posterior', mutation = input$mutation, ignoreSex = input$ignoreSex,
                nExcl = input$nExcl)
        else {
          if (input$nMissing < 0)
              MPs = 'Missing person'
          else
              MPs = paste0("M", 1:input$nMissing)
            familias(file = file, method = 'Posterior',  relabel = input$relabel, miss = MPs,
                     ignoreSex = input$ignoreSex, nExcl = input$nExcl)
        }
      }
    })
    
    #DVI > Analyses based on built in data, download
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

    #DVI > Analyses based on user loaded data, download
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
    
    #DVI > Built in cases: plots reference families
    output$plotDVIBuilt <- renderPlot({
      if(input$refFam > 0) {
        file = input$fileDVI
        ext = tools::file_ext(file$datapath)
        if(input$datDVIBuilt == "Three missing"){
          if(input$refFam > 1)
             stop(safeError("Impossible value for `Reference family to plot`, only 1 reference family."))
          plot(example1$am, hatched = typedMembers, title = "Reference family",
               cex = 1.2, col = list(red = example1$missing,
                                     blue = typedMembers(example1$am)))
        }
        
        else if (input$datDVIBuilt == "DVIbook-Exercise-6.2.7"){
          if(input$refFam > 1)
            stop(safeError("Impossible value for `Reference family to plot`, only 1 reference family."))
            labs = labels(grave$am)
            index = c(1:5,7:8, 16, 20:21)
            am0 = relabel(grave$am, 1:length(index), labs[index] )
            plot(am0,  hatched = typedMembers, title = "Reference family",
               col = list(red = grave$missing, blue = typedMembers(grave$am)), cex = 0.7)
        }
        
        else if (input$datDVIBuilt == "planecrash"){
          if(input$refFam > 5)
            stop(safeError("Impossible value for `Reference family to plot`, only 5 reference families."))
          plot(planecrash$am[[input$refFam]], hatched = typedMembers, 
                      title = paste("Reference family ", input$refFam),
                      col = list(red = planecrash$missing,
                                 blue = typedMembers(planecrash$am[[input$refFam]])))
        }
        
        else if (input$datDVIBuilt == "exclusionExample"){
          if(input$refFam > 15)
            stop(safeError("Impossible value for `Reference family to plot`, only 15 reference families."))
          plot(exclusionExample$am[[input$refFam]], hatched = typedMembers, 
               title = paste("Reference family ", input$refFam),
               col = list(red = exclusionExample$missing,
                          blue = typedMembers(exclusionExample$am[[input$refFam]])))
        }
        
        else if (input$datDVIBuilt == "DVIbook-Example-4.8.1"){
          if(input$refFam > 1)
            stop(safeError("Impossible value for `Reference family to plot`, only 1 reference family."))
          plot(dataExample481$am, hatched = typedMembers, 
               title = paste("Reference family ", input$refFam),
               col = list(red = dataExample481$missing,
                          blue = typedMembers(dataExample481$am)))
        }
        
        else if (input$datDVIBuilt == "DVIbook-Example-4.8.4"){
          if(input$refFam > 3)
            stop(safeError("Impossible value for `Reference family to plot`, only 3 reference families."))
          plot(dataCh4$am[[input$refFam]], hatched = typedMembers, 
               title = paste("Reference family ", input$refFam),
               col = list(red = dataCh4$missing,
                          blue = typedMembers(dataCh4$am[[input$refFam]])))
        }
        
        else if (input$datDVIBuilt == "DVIbook-Exercise-4.9.7"){
          if(input$refFam > 3)
            stop(safeError("Impossible value for `Reference family to plot`, only 3 reference families."))
          plot(dataExercise497$am[[input$refFam]], hatched = typedMembers, 
               title = paste("Reference family ", input$refFam),
               col = list(red = dataExercise497$missing,
                          blue = typedMembers(dataExercise497$am[[input$refFam]])))
        }
        
        else if (input$datDVIBuilt == "DVIbook-Exercise-4.9.8"){
          if(input$refFam > 1)
            stop(safeError("Impossible value for `Reference family to plot`, only 1 reference family."))
          plot(dataExercise498$am, hatched = typedMembers, 
               title = paste("Reference family ", input$refFam),
               col = list(red = dataExercise498$missing,
                          blue = typedMembers(dataExercise498$am)))
        }
      }

    })
    
    #DVI > Analyses based on built in data: plots reference families
    output$plotLoad <- renderPlot({
      if(input$refFamLoad > 0) {
          file = input$fileDVI
          ext = getExt(file = file)
          if (ext == "RData" |  ext == "rda") 
            RData(file = input$fileDVI, method = 'plot', refFam = input$refFamLoad)
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
    
    ### Set analysis to `None selected` if new data 
    observeEvent(input$datDVIBuilt, {
        updateSelectInput(session,"analysis", selected = "None selected")
    })  
    
    ### Set analysis to `None selected` if new file read 
    observeEvent(input$fileDVI, {
      updateSelectInput(session,"analysisLoad", selected = "None selected")
    })
    
   ### Change 0 simulations to 1 always 
    observeEvent(input$nSimulations, {
      x = input$nSimulations
      if(x == 0 | is.na(x))
        updateNumericInput(session,"nSimulations", value = 10)
    })
    
    ### Change 0 simulations to 1 always 
    observeEvent(input$nMissing, {
      if(input$nMissing > -1)
        updateNumericInput(session,"nExcl", value = 0)
    })

   ### Reset functions below
   observeEvent(input$resetPowerBuilt, {
    updateCheckboxInput(session, "log10Power", value = TRUE)
    updateSliderInput(session, "lastMarker", value = 22) 
    updateSelectInput(session, "pedigreePowerSimulated", selected = "None selected")
   })
 
   observeEvent(input$resetPowerFam, {
    updateCheckboxInput(session, "log10PowerFam", value = TRUE)
    updateSelectInput(session, "powerPlotFam", selected = "None selected")
   })
   
  observeEvent(input$resetPriBuilt, {
    updateNumericInput(session, "nProfiles", value = 1)
    updateSliderInput(session, "lastMarkerPri", value = 22) 
    updateSelectInput(session, "pedigreePowerSimulatedPri", selected = "None selected")
   })
  


  observeEvent(input$resetDVIBuilt, {
    updateSelectInput(session, "datDVIBuilt", selected = "None selected")
    updateNumericInput(session, "refFam", value = 1)
    updateSelectInput(session, "analysis", selected = "None selected")
   })
  
  observeEvent(input$resetDVILoad, {
    updateCheckboxInput(session, "relabel", value = FALSE)
    updateNumericInput(session, "refFamLoad", value = 1)
    updateSelectInput(session, "analysisLoad", selected = "None selected")
   })
      
   observeEvent(input$reset, {
    updateSliderInput(session, "lastMarker", value = 35)
    updateSelectInput(session, "pedigreePowerSimulated", selected = "None selected")
    updateCheckboxInput(session, "log10Power", value = TRUE)
    updateCheckboxInput(session, "log10PowerFam", value = TRUE)
    updateNumericInput(session, "nProfiles", value = 1)
    updateSliderInput(session, "lastMarkerPri", value = 13)    
    updateSelectInput(session, "pedigreePowerSimulatedPri", selected = "None selected")
    updateSelectInput(session, "pedigreePri", selected = "brother")
    updateSelectInput(session, "powerPlotFam", selected = "None selected")
    updateSelectInput(session, "datDVIBuilt", selected = "None selected")
    updateCheckboxInput(session, "relabel", value = FALSE)
    updateNumericInput(session, "refFam", value = 1)
    updateNumericInput(session, "refFamLoad", value = 1)
    updateNumericInput(session, "nMissing", value = -1)
    updateNumericInput(session, "nSimulations", value = 100)
    updateNumericInput(session, "seed", value = 1729)
    updateSelectInput(session, "analysis", selected = "None selected")
    updateSelectInput(session, "analysisLoad", selected = "None selected")
    updateCheckboxInput(session, "mutation", label = "Mutation", value = FALSE) 
    updateCheckboxInput(session, "ignoreSex", label = "Ignore sex", value = TRUE) 
    updateCheckboxInput(session, "truePedH1", label = "True ped H1", value = TRUE) 
    updateSliderInput(session, "thresholdIP", value = 10000) 
    updateSliderInput(session,"thresholdLRDisplay", value = 0)
    updateSelectInput(session, "selected_language", selected = "en")
    updateNumericInput(session, "nExcl", value = 0)
  })

}


  shinyApp(ui, server)
