# Power plot

pedPower = function(claim, nsim = 10, thresh = NULL, seed = 1729, lastMarker = 35,
                    ids = c("MP", "REF"), plotOnly = TRUE, Log10 = TRUE){
  claim = setMarkers(claim, locusAttributes = NorwegianFrequencies[1:lastMarker])
  unrel = list()
  for(i in 1:length(ids))
    unrel[[i]] = singleton(ids[i])
  unrel = transferMarkers(claim, unrel)
  if(plotOnly){
    plot(claim, hatched = ids, col = list(red = ids[1], blue = ids[-1]))
  }
  else{
    pow1 = LRpower(claim, unrel,  ids = ids, nsim = nsim,
                   threshold = thresh, seed = seed, plot = F, verbose = F)
    par(mfcol = c(1,2), oma = c(0,0, 2, 0))
    if(Log10){
      y = log10(pow1$LRperSim)
      xl = "log10(LR)"
    } else{
      y = pow1$LRperSim
      xl = "LR"
    }
      
    hist(y, xlab = xl, main = "", prob = TRUE)
    f <- ecdf(y)
    r = range(y)
    curve(1-f(x), from = r[1], to = r[2],  xlim = r,
          ylab = "Excedance probability", xlab = "x", lty = 1)
    tittel = paste("No of simulations: ", nsim,". Markers: 1 - ", lastMarker)
    title(tittel, outer = TRUE)
    par(mfcol = c(1,1))
  }
}



# Wrapper for jointDVI

myjointDVI = function(pm, am, missing, mutation = FALSE){
  if(mutation)
    jointDVI(pm, am, missing, disableMutation = FALSE)
   else
    jointDVI(pm, am, missing)
}

#' Help functions for dviapp
#' 
#' The  function priPower
#' deals with plotting and priority calculations
#' 
#' For each selection, i.e., for one or two brothers,
#' the genotypes are generated nProfiles times. For each of
#' these simulations, lrSims simulations of the missing person are generated under HP
#' (for LR calculations) and under HD (for exclusion calculations)
#' 

priPower = function(ped, plotPed = T, nMark = 22, seed = 17, nProfiles = 1, lrSims = 10,
                    sel = list( "REF", c("REF", "E1"), c("REF", "E1", "E2"))) {
  
  if(plotPed)
    plot(ped, col = list(red = "MP", blue = c("REF", "E1", "E2")))
  else{
    ped = setMarkers(ped, locusAttributes = NorwegianFrequencies[1:nMark])
    
    simData = MPPsims(ped, missing = "MP", nProfiles = nProfiles,
                      lrSims = lrSims, seed = seed,
                      selections = sel, addBaseline = FALSE, numCores = 1)
    powerPlot(simData, type = 3)

  }
}


#' pairwise
#' 
#' Runs  dvir::checkPairwise.
#' Victims are given as a list of singletons, and references as a list of
#' pedigrees. All possible assignments are evaluated and solutions ranked
#' according to the likelihood.
#'
#' @param pm A list of singletons.
#' @param nlines Integer. No of lines to output.
#' @param sorter Logical. If true, descending according to LR.
#'
#' @return Outputs table
#'
#' @seealso [pairwiseLR()], [findUndisputed()]
#'
#' @examples
#'
#' pairwise(example1$pm)
#'



IBDestimates = function(pm, nlines = 10, sorter = FALSE){
  res = checkPairwise(pm, plot = F)
  if(sorter)
    res = res[order(res$LR, decreasing = TRUE),]
  head(res, n = nlines)
}

#' RData
#' Function to process RData input to DVI
#' 

RData = function(file = input$file1, nlines = 10, sorter = TRUE,
                 method = NULL, refFam = 1, mutation = FALSE){
  load(file$datapath)
  if(method == 'Exclusion')
    exclusionMatrix(pm, am, missing)
  else if (method == 'IBD estimates')
     checkPairwise(pm, plot = F)
  else if (method == 'Pairwise')
    pairwiseLR(pm, am, missing)$LRmatrix
  else if (method == 'Joint')
    myjointDVI(pm, am, missing, mutation)
  else if (method == 'Posterior')
    Bmarginal(myjointDVI(pm, am, missing, mutation), missing)
  else if (method == 'plot'){
    if (is.ped(am))
      plot(am,  hatched = typedMembers, title = "Reference family",
           col = list(red = missing, blue = typedMembers(am), cex = 1.2))
    else
      plot(am[[refFam]], hatched = typedMembers, 
           title = paste("Reference family ", refFam),
           col = list(red = missing, blue = typedMembers(am[[refFam]]), cex = 1.2))
    }

  else if (method == 'Describe data')
    summariseDVIreturned(pm, am, missing)
}


#' summariseDVIreturned
#' modification of summariseDVI to return output
#'

summariseDVIreturned = function (pm, am, missing, header = "DVI data."){
  
  vics = unlist(labels(pm))
  nvics = length(vics)
  nmissing = length(missing)
  refs = typedMembers(am)
  nrefs = length(refs)
  nam = if (is.ped(am)) 
    1
  else length(am)
  t1 = paste(nvics, "victims: ")
  if(nvics == 1) t2 = paste(vics[1],". ")
  if(nvics > 1) t2 = paste(vics[1],",...,",vics[nvics], ". ")

  t3 = paste(nmissing, "missing: ")
  if(nmissing == 1) t4 = paste(missing[1], ". ")
  if(nmissing > 1) t4 = paste(missing[1],",...,",missing[nmissing], ". ")
  

  if(nrefs == 1){
    t5 = paste(nrefs, "typed ref: ")
    t6 = paste(refs[1], " ", ". ")
  } 
  
  if(nrefs >  1){
    t5 = paste(nrefs, "typed refs: ")
    t6 = paste(refs[1],",...,",refs[nrefs], ". ")
  }
  
  if(nam == 1 ) t7 = paste(nam, "reference family. ")
  if(nam > 1 ) t7 = paste(nam, "reference families. ")

  text = glue(header, t1, t2, t3, t4, t5, t6, t7)
  
  text
  
}

#' familias
#' Function to process fam file input
#'

familias =  function(file = NULL, method = NULL, 
                     relabel = TRUE, miss = 'Missing person', refFam = 1, DVI = TRUE,
                     nProfiles = 1, lrSims = 100, seed = 17, threshold = 10000,
                     plotOnly = TRUE, Log10 = TRUE){
  x = readFam(file$datapath)
  
  #Relabel if DVI and not power
  if(DVI) {
      pm = x$`Unidentified persons`
    if (length(x) < 3)
      am = x[[2]][[2]]
    else{
      am = lapply(x[-1], function(dat) {
      ref = dat$`Reference pedigree`     
      ref[[which(pedsize(ref) > 1)]]     # remove redundant singleton
      })
    }
    
    # Rename pm samples to V1, V2 ...; reference families to F1, F2, ...
    # and missing to M1, M2, ... 
    # We need to rename missing
    
    z = relabelDVI(pm, am, missing = miss)
    miss = z$missing
    
    # To make plots nice and to anonymise, rename also pm and am
    if(relabel){ 
      pm = z$pm
      am = z$am
    }
  }

  if(method == "Describe data")
    summariseDVIreturned(pm, am, miss, header = "Familias data. ")
  
  else if (method == "IBD estimates")
    checkPairwise(pm, plot = F)
  
  else if (method == "Exclusion")
    exclusionMatrix(pm, am, miss)
  
  else if (method == "Pairwise")
    pairwiseLR(pm, am, miss)$LRmatrix
  
  else if (method == "Joint")
    jointDVI(pm, am, miss)
  
  else if (method == "Posterior")
    Bmarginal(jointDVI(pm, am, miss), miss)
  
  else if (method == "plot")
    if (is.ped(am))
      plot(am,  hatched = typedMembers, title = "Reference family",
           col = list(red = miss, blue = typedMembers(am), cex = 1.2))
    else
       plot(am[[refFam]], hatched = typedMembers, 
         title = paste("Reference family ", refFam),
         col = list(red = miss, blue = typedMembers(am[[refFam]]), cex = 1.2))
  else if (method == "plotSimPed")
    plot(x[[1]], hatched = "REF", col = list(red = "Missing", blue = c("REF", "E1","E2")))
  else if (method == "Prioritise"){
    
    if(plotOnly)
      plot(x[[1]],  col = list(red = "MP", blue = c("REF", "E1", "E2")))
    else{    
      sel = list( "REF", c("REF", "E1"), c("REF", "E1", "E2"))
      simData = MPPsims(x[[1]], missing = "MP", nProfiles = nProfiles, lrSims = lrSims, seed = seed,
                        selections = sel, thresholdIP = NULL, addBaseline = FALSE, 
                        numCores = 1)
      powerPlot(simData, type = 3)
    }
  }
  else if (method == "Power"){
    if(plotOnly)
      plot(x[[1]], hatched = c("MP", "REF"), col = list(red = "MP", blue = "REF"))
    else{
      simData = MPPsims(x[[1]], missing = "MP", nProfiles = nProfiles, lrSims = lrSims, seed = seed,
                        selections = list("REF"), thresholdIP = NULL, addBaseline = FALSE,
                        numCores = 1)

      par(mfcol = c(1,2), oma = c(0, 0, 2, 0))
      if(Log10){
        y = log10(simData$REF$ip[[1]]$LRperSim)
        xl ="log10(LR)"
      }
      else {
        y = simData$REF$ip[[1]]$LRperSim
        xl = "LR"
      }
      hist(y, xlab = xl, main = "", prob = TRUE)
      f = ecdf(y)
      r = range(y)
      curve(1-f(x), from = r[1], to = r[2],  xlim = r,
            ylab = "Excedance probability", xlab = "x", lty = 1)
      tittel = paste("No of simulations: ", lrSims, ". Markers: 1 - ", nMarkers(x[[1]]) )
      title(tittel, outer = TRUE)
      par(mfcol = c(1,1))      
      
    }
  }
}

#' getExt
#' Function to check if there is an input file and if so get extension

getExt = function(file){
  req(file)
  tools::file_ext(file$datapath)
}

  
