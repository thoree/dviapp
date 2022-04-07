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



IBDestimates = function(pm, nlines = 10, sorter = TRUE){
  res = checkPairwise(pm, plot = F)
  if(sorter)
    res = res[order(res$LR, decreasing = TRUE),]
  head(res, n = nlines)
}

#' RData
#' 

RData = function(file = input$file1, nlines = 10, sorter = TRUE, method = NULL){
  load(file$datapath)
  if(method == 'Exclusion')
    exclusionMatrix(pm, am, missing)
  else if (method == 'IBDestimates')
    IBDestimates(pm, nlines = nlines, sorter = sorter)
  else if (method == 'Pairwise')
    pairwiseLR(pm, am, missing)$LRmatrix
  else if (method == 'Joint')
    jointDVI(pm, am, missing)
  else if (method == 'Posterior')
    Bmarginal(jointDVI(pm, am, missing), missing)
  else if (method == 'plot')
    plotPedList(list(pm, am), hatched = typedMembers, marker = 1,
                titles = c("Post Mortem", "Ante Mortem"),
                cex = 1.2, margins = c(1,1.2,1,1.2), frames = FALSE,
                col = list(red = missing))
  else if (method == 'DescribeData'){

    vics = as.character(unlist(labels(pm)))
    refs = typedMembers(am)
    text1 = "User R data. "
    text2 = paste(length(vics), "victims, ")
    text3 = paste(length(pm), "missing, ")
    text4 = paste(length(refs), "references, ")
    text5 = paste(length(am), "families. ")
    text6 = paste(nMarkers(pm), "markers in pm. ")
    text7 = paste(nMarkers(am), "markers in am.")
    glue(text1, text2, text3, text4, text5, text6, text7)
  }
  
}

describeData = function(data = NULL){
        if(data == "Tutorial example")
            c(" Tutorial example. 3 victims: V1, V2, V3.  
                3 missing: M1, M2, M3. 1 family, 1 typed refs: R1. 1 marker")
        else if (data == "grave")
            c(" grave example. 8 victims: V1, ..., V8.  
                8 missing: M1, ..., M8. 1 family, 5 typed refs: R1, ...,R5. 23 markers.")
        else if (data == "planecrash")
            c(" planecrash example. 8 victims: V1, ..., V8.  
                Five families, each with one missing and one typed ref. 15 markers.")
         else if (data ==  "FamiliasFile")
            c(" Familias file")
        }

