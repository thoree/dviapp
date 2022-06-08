#' Help functions for dviapp
#' Function
#' pedPower
#'
#' Plots pedigree, histogram and P(LR>x)
#'
#' @param am H1 pedigree
#' @param nsim Integer
#' @param seed Integer
#' @param lastMarker Integer, last marker in NorwegianFrequencies
#' @param ids character, MP followed by ref(s)
#' @param log10 logical

pedPower = function(claim, nsim = 10, seed = NULL, lastMarker = 35,
                    ids = c("MP", "REF"), Log10 = TRUE, truePedH1 = TRUE){
  # Set markers and define H2
  claim = setMarkers(claim, locusAttributes = NorwegianFrequencies[1:lastMarker])
  unrel = list()
  for(i in 1:length(ids))
    unrel[[i]] = pedtools::singleton(ids[i])
  unrel = pedtools::transferMarkers(claim, unrel)
  
  if(truePedH1){
    hyp = "H1"
    pow1 = LRpower(claim, unrel,  ids = ids, nsim = nsim,
                   seed = seed, plot = F, verbose = F)
  }
  else {
    hyp = "H2"
    pow1 = LRpower(claim, unrel, truePed = unrel,  ids = ids, nsim = nsim,
                   seed = seed, plot = F, verbose = F)
  }
  if(all(pow1$LRperSim == 0))  
    stop(safeError("All simulated values are 0"))
  
  par(mfcol = c(1,2), oma = c(0, 0, 1, 0))
  meanLR = mean(pow1$LRperSim)
    
  # Fix labels, titles, and subtitles
  if(Log10){
      y = log10(pow1$LRperSim)
      xl = "log10(LR)"
      sub1 = "mean(log10(LR))) = "
      sub2 = paste("P(log10(LR)) > ")
  } else{
      y = pow1$LRperSim
      xl = "LR"
      sub1 = "mean(LR) = "
      sub2 = paste("P(LR >")
    }
  m = mean(y)  
  q20 = quantile(y, 0.2)
  hist(y, xlab = xl, main = "", prob = TRUE,
      sub = paste(sub1, formatC(m, format = "e", digits = 2)))
  f = ecdf(y)
  r = range(y)
  curve(1-f(x), from = r[1], to = r[2],  xlim = r,
      ylab = "Excedance probability", xlab = "x", lty = 1,
      sub = paste(sub2, formatC(q20, format = "e", digits = 2), ") = 0.8"))
  
  tittel = paste("No of simulations: ", nsim,". Markers: 1 - ", lastMarker,
                 " True: ", hyp)
  title(tittel, outer = TRUE)
  par(mfcol = c(1,1))
}

#' Function
#' myjointDVI
#' 
#' Wrapper for jointDVI. Accomodates mutation and possibility to show
#' only LR above specified thresholdLR
#' 
#' @param pm, am, missing: see jointDVI
#' @param mutation logical
#' @param thresholdLR double
#' @param ignoreSex logical
#' @param nExcl integer

myjointDVI = function(pm, am, missing, mutation = FALSE, thresholdLR = 0, 
                      ignoreSex = TRUE, nExcl = 0){
  if (nExcl > 0){
    reduced = reduceDVI(pm, am, missing, nExcl)
    pm = reduced$pm
    am = reduced$am
    missing = reduced$missing
   }
    
  if(mutation)
    res = jointDVI(pm, am, missing, disableMutation = FALSE, ignoreSex = ignoreSex)
   else
    res = jointDVI(pm, am, missing, ignoreSex = ignoreSex)
   res[res$LR >= thresholdLR, ]
}


#' Function
#' priPower
#' 
#' Deals with plotting and priority calculations
#' 
#' For each selection, i.e., for one or two brothers,
#' the genotypes are generated nProfiles times. For each of
#' these simulations, lrSims simulations of the missing person(s) are generated under HP
#' (for LR calculations) and under HD (for exclusion calculations)
#' 
#' @param ped pedigree
#' @param plotPed Logical, only plot
#' @param lastMarker Integer, last marker in NorwegianFrequencies
#' @param seed integer
#' @param nProfiles integer, see forrel::MPPsims
#' @param lrSims integer, see forrel::MPPsims
#' @param thresholdIP double, see forrel::MPPsims


priPower = function(ped, lastMarker = 22, seed = NULL, nProfiles = 1, lrSims = 10,
                    thresholdIP = NULL, sel = list( "REF", c("REF", "E1"), c("REF", "E1", "E2"))) {
  
    ped = setMarkers(ped, locusAttributes = NorwegianFrequencies[1:lastMarker])
    
    simData = MPPsims(ped, missing = "MP", nProfiles = nProfiles,
                      lrSims = lrSims, seed = seed, thresholdIP = thresholdIP,
                      selections = sel, addBaseline = FALSE, numCores = 1)
    p1 = powerPlot(simData, type = 1)
    p3 = powerPlot(simData, type = 3)
    p1 + p3 + plot_layout(guides = 'collect')

}

#' Function
#' IBDestimates
#' 
#' Wrapper for forrel::checkPairWise
#' 
#' @param pm See forrel::jointDVI
#' @param sorter Logical
#' @param thresholdLR double, display onlyt above

IBDestimates = function(pm,  sorter = TRUE, thresholdLR = 0){
  res = checkPairwise(pm, plot = F)
  if(sorter)
    res = res[order(res$LR, decreasing = TRUE),]
  res[res$LR >= thresholdLR, -c(7:9)]
}

#' Function
#' RData
#' 
#' Function to process RData input
#' 
#' @param file *.RData file
#' @param sorter Logical
#' @param method character IBDestimates, Exclusion, Pairwise, Joint,or Posterior
#' @param refFam integer Reference family to plot
#' @param mutation logical
#' @param thresholdLR double, see IBDestimates
#' @param nMissingSpecified integer. No of missing if multiple missing in at least one family
#' @param ignoreSex logical
#' @param nExcl integer

RData = function(file = input$file1, sorter = TRUE,  method = NULL, refFam = 1, 
                 mutation = FALSE, thresholdLR = NULL,  nMissingSpecified = -1,
                 ignoreSex = TRUE, nExcl = 0){
  # data pm, am, missing loaded next:
  load(file$datapath)
  errorText = "Must have the same markers for all pm and am data"
  if (method == 'IBD estimates')
    IBDestimates(pm, thresholdLR = thresholdLR)
  else if(method == 'Exclusion'){
    if(differentMarkers(pm, am)$ulike)
      stop(safeError(errorText))
    exclusionMatrix(pm, am, missing)
  }
  else if (method == 'Pairwise'){
    if(differentMarkers(pm, am)$ulike)
      stop(safeError(errorText))
    myPairwiseLR(pm, am, missing, mutation,ignoreSex = ignoreSex)$LRmatrix
  }
  else if (method == 'Joint'){
    if(differentMarkers(pm, am)$ulike)
      stop(safeError(errorText))
    myjointDVI(pm, am, missing, mutation, thresholdLR = thresholdLR, ignoreSex = ignoreSex, nExcl = nExcl)
  }
  else if (method == 'Posterior'){
    if(differentMarkers(pm, am)$ulike)
      stop(safeError(errorText))
    myBmarginal(pm, am, missing, mutation = mutation,  ignoreSex = ignoreSex,  nExcl = nExcl)
  }
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
    summariseDVIreturned(pm, am, missing, nMissingSpecified = nMissingSpecified)
}

#' Function
#' summariseDVIreturned
#' 
#' Modification of dvi::summariseDVI to return description of DVI data
#'
#'@param pm, am, missing, see forrel::jointDVI
#'@param header character, first text displayed
#'@param nMissingSpecified integer. No of missing if multiple missing in at least one family

summariseDVIreturned = function (pm, am, missing, header = "DVI data.", nMissingSpecified = -1){

  vics = unlist(labels(pm))
  nvics = length(vics)
  if(nMissingSpecified > 0)
    nmissing = nMissingSpecified
  else
    nmissing = length(missing)
  refs = typedMembers(am)
  nrefs = length(refs)
  nam = if (is.ped(am)) 
    1
  else length(am)
  t1 = paste(nvics, "victims: ")
  if(nvics == 1) t2 = paste(vics[1],". ")
  if(nvics > 1) t2 = paste(vics[1],",...,",vics[nvics], ". ")

  t3 = paste(nmissing, "missing. ")
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
  t8 = differentMarkers(pm, am)$tekst
  t9 =" PS: The number of missing need to be given in Settings if there are families with
       multiple missing."
  glue(t1, t2, t3, t5, t6, t7, t8)
}

#' Function
#' familias
#' 
#' Function to process fam file input
#'
#' @param file fam file
#' @param method character IBDestimates, Exclusion, Pairwise, Joint,or Posterior
#' @param relabel logical If TRUE, run dvi::relabelDVI
#' @param miss character
#' @param refFam integer Reference family to plot
#' @param DVI logical If TRUE, DVI analysis
#' @param nProfiles, lrSims, seed, thresholdIP: see forrel::MPPsims
#' @param plotOnly logical
#' @param Log10 logical
#' @param mutation logical
#' @param thresholdLR double, see IBDestimates
#' @param nMissingSpecified integer. No of missing if multiple missing in at least one family
#' @param ignoreSex
#' @param truePedH1 logical
#' @param nExcl integer
#' 
#' 
familias =  function(file = NULL, method = NULL, relabel = TRUE, miss = 'Missing person', 
                     refFam = 1, DVI = TRUE, nProfiles = 1, lrSims = 100, seed = NULL, 
                     thresholdIP = 10000, plotOnly = TRUE, Log10 = TRUE, mutation = FALSE, 
                     thresholdLR = 0, nMissingSpecified = -1, ignoreSex = TRUE, truePedH1 = TRUE,
                     nExcl = 0){
  if(!truePedH1)
    stop(safeError("Simulation under H2 not implemented"))
  x = readFam(file$datapath)
  
  #Relabel if DVI and not power. Convert fam-file
  if(DVI) {
      pm = x$`Unidentified persons`
    if (length(x) < 3){
      am = x[[2]][[2]]
      if(!is.ped(am)) am = am[[which(pedsize(am) > 1)]] 
    }
    else{
      am = lapply(x[-1], function(dat) {
        ref = dat$`Reference pedigree`
        if(!is.ped(ref))
          ref = ref[[which(pedsize(ref) > 1)]]     # remove redundant singleton
        ref
      })
    }

    errorText = "Typed members of reference families must be named differently"
    if(any(duplicated(typedMembers(am))))
      stop(safeError(errorText))
    
    # Always rename missing
    z = relabelDVI(pm, am, missing = miss)
    miss = z$missing
    
    # Rename pm samples to V1, V2 ...; reference families to F1, F2, ...
    # and missing to M1, M2
    if(relabel){ 
      pm = z$pm
      am = z$am
    }
  }
  errorTextSame = "Must have the same markers for all pm and am data"
  if(method == "Describe data")
    summariseDVIreturned(pm, am, missing = miss, header = "Familias data. ", 
                         nMissingSpecified = nMissingSpecified)
  
  else if (method == "IBD estimates")
    IBDestimates(pm, thresholdLR = thresholdLR)
  
  else if (method == "Exclusion"){
    if(differentMarkers(pm, am)$ulike)
      stop(safeError(errorTextSame))
    exclusionMatrix(pm, am, miss)
  }
  
  else if (method == "Pairwise"){
    if(differentMarkers(pm, am)$ulike)
      stop(safeError(errorTextSame))
    myPairwiseLR(pm, am, miss, mutation, ignoreSex = ignoreSex)$LRmatrix
  }
  
  else if (method == "Joint"){
    if(differentMarkers(pm, am)$ulike)
      stop(safeError(errorTextSame))
    myjointDVI(pm, am, miss, mutation, thresholdLR = thresholdLR, ignoreSex = ignoreSex, nExcl = nExcl)
  }
  
  else if (method == "Posterior"){
    if(differentMarkers(pm, am)$ulike)
      stop(safeError(errorTextSame))
    myBmarginal(pm, am, missing, mutation = mutation,  ignoreSex = ignoreSex,  nExcl = nExcl)
    #Bmarginal(myjointDVI(pm, am, miss, mutation, ignoreSex = ignoreSex), miss)
  }
  
  else if (method == "plot"){
    
    nAM = length(am)
    if (is.ped(am) & refFam == 1)
      plot(am,  hatched = typedMembers, title = "Reference family",
           col = list(red = miss, blue = typedMembers(am), cex = 1.2))
    else if(is.ped(am) & refFam > 1)
      stop(safeError(paste("There is only 1 reference family")))
  
    else if(!is.ped(am) & refFam > nAM)
        stop(safeError(paste("Only ", nAM, "reference familie(s)")))
    else
       plot(am[[refFam]], hatched = typedMembers,  title = paste("Reference family ", refFam),
         col = list(red = miss, blue = typedMembers(am[[refFam]]), cex = 1.2))
    }

  else if (method == "plotSimPed")
    plot(x[[1]], hatched = "REF", col = list(red = "Missing", blue = c("REF", "E1","E2")))
  else if (method == "Prioritise"){
    
    if(plotOnly)
      plot(x[[1]],  col = list(red = "MP", blue = c("REF", "E1", "E2")))
    else{    
      sel = list( "REF", c("REF", "E1"), c("REF", "E1", "E2"))
      simData = MPPsims(x[[1]], missing = "MP", nProfiles = nProfiles, lrSims = lrSims, seed = seed,
                        selections = sel, thresholdIP = thresholdIP, addBaseline = FALSE, 
                        numCores = 1)
      p1 = powerPlot(simData, type = 1)
      p3 = powerPlot(simData, type = 3)
      p1 + p3 + plot_layout(guides = 'collect')
    }
  }
  else if (method == "Power"){
    if(plotOnly)
      plot(x[[1]], hatched = typedMembers, col = list(red = "MP", blue = "REF"), marker = 1)
    else{
      simData = MPPsims(x[[1]], missing = "MP", nProfiles = nProfiles, lrSims = lrSims, seed = seed,
                        selections = list("REF"), thresholdIP = NULL, addBaseline = FALSE,
                        numCores = 1)

      par(mfcol = c(1,2), oma = c(0, 0, 2, 0))
      if(Log10){
        y = log10(simData$REF$ip[[1]]$LRperSim)
        xl ="log10(LR)"
        sub1 = "mean(log10(LR))) = "
        sub2 = paste("P(log10(LR)) > ")
      }
      else {
        y = simData$REF$ip[[1]]$LRperSim
        xl = "LR"
        sub1 = "mean(LR) = "
        sub2 = paste("P(LR >")
      }
      m = mean(y)  
      q20 = quantile(y, 0.2)
      hist(y, xlab = xl, main = "", prob = TRUE,
         sub = paste(sub1, formatC(m, format = "e", digits = 2)))
      f = ecdf(y)
      r = range(y)
      curve(1-f(x), from = r[1], to = r[2],  xlim = r,
            ylab = "Excedance probability", xlab = "x", lty = 1,
          sub = paste(sub2, formatC(q20, format = "e", digits = 2), ") = 0.8"))
      tittel = paste("No of simulations: ", lrSims, ". Markers: 1 - ", nMarkers(x[[1]]), 
                     "True ped H1" )
      title(tittel, outer = TRUE)
      par(mfcol = c(1,1))      
      
    }
  }
}

#' Function
#' getExt
#' 
#' Function to check if there is an input file and if so get extension
#' 
#' @param file Input file
#' 

getExt = function(file){
  req(file)
  tools::file_ext(file$datapath)
}

#' Function
#' differentMarkers
#' 
#' Function to check if the number of markers differ
#' 
#' @param pm, am see forrel::jointDVI
#'

differentMarkers = function(pm, am){
  if(length(pm) == 1)
    nPM = nMarkers(pm)
  else
    nPM = unlist(lapply(pm, function(x) nMarkers(x)))
  if(is.ped(am))
    nAM = nMarkers(am)
  else
   nAM = unlist(lapply(am, function(x) nMarkers(x)))
  nM = unique(c(nPM,nAM))
  ulike = (length(nM) != 1)
  if(ulike){
    ra = range(nM)
    tekst = paste("No of markers:", ra[1],"-", ra[2],".")
  }
  else{
    if(nM == 1)
       tekst = "1 marker."
    else
      tekst = paste( nM, "markers.")
  }
  list(ulike = ulike, tekst = tekst)
}
  
#'
#' myPairwiseLR
#' 
#' @param pm, am, missing: se dvir::pairwiseLR
#' @param mutation logical
#' @param ignoreSex logical
#' 
myPairwiseLR = function(pm, am, miss, mutation, ignoreSex = TRUE){
  if(!mutation){
     pm = setMutationModel(pm, 'trivial')
     am = setMutationModel(am, 'trivial')
  }
  pairwiseLR(pm, am, miss, ignoreSex = ignoreSex)  
}

#'
#' reduceDVI
#' 
#' Remove victims or reference families if the number of exclusion
#' exceeds (>=) a specified value, nExcl
#' @param pm, am, missing : as before
#' nExcl integer
#' 

reduceDVI = function(pm, am, missing, nExcl = 3){
  # Find exclusion matrix
  em = exclusionMatrix(pm, am, missing)
  # Find potential victims to drop and drop
  minExclusionVictims = apply(em,1, function(x) min(x))
  dropVictims = (1:dim(em)[1])[minExclusionVictims >= nExcl]
  if(length(dropVictims > 0))
    em = em[-dropVictims,]
  
  # Drop potential families
  minExclusionFamilies = apply(em,2, function(x) min(x))
  dropFamilies = (1:dim(em)[2])[minExclusionFamilies >= nExcl]
  
  #Update pm, am and missing
  if(length(dropVictims) > 0 )
    pm = pm[-dropVictims]
  if(length(dropFamilies) > 0 )
    am = am[-dropFamilies]
  keepMissing = missing %in% unlist(labels(am))
  if(length(keepMissing) == 0)
    stop("None identified")
  else
    missing = missing[keepMissing]
  list(pm = pm, am = am, missing = missing)
}

#'
#' myBmarginal
#' 
#' Wrapper for Bmarginal to allow for removing based on exclusion
#' 
#' @param as before
#' @param prior not yet used
myBmarginal = function(pm, am, missing, mutation = FALSE,  ignoreSex = TRUE, prior = NULL, nExcl = 0){
  if (nExcl > 0){
    reduced = reduceDVI(pm, am, missing, nExcl)
    pm = reduced$pm
    am = reduced$am
    missing = reduced$missing
  }
  jointRes = myjointDVI(pm, am, missing, mutation = mutation,
                        ignoreSex = ignoreSex, nExcl = 0)
  Bmarginal(jointRes, missing)
}