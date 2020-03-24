RNAcirPlot <- function(ctFile,cex = 1,posNum = 2.5,ifNum = FALSE,
                       ifBulge = FALSE,ifExternalLoops = FALSE,ifHairpin = FALSE,
                       ifInternalLoops = FALSE,ifMultiBranchLoops = FALSE){

  baseNum <- nrow(ctFile)
  factors <- as.factor(ctFile$V6)

  circos.initialize(letters[1], xlim = c(1, baseNum))

  circos.trackPlotRegion("a", ylim = c(0, 1),
                         bg.border = "white",track.height = 0.05)
  for (i in 1:baseNum) {
    circos.text(i,0.5, ctFile[i,2], facing = "clockwise", cex = cex)
    if(ifBulge){
      arrBulge <- unlist(bulge_loop(ctFile))
      if(i %in% arrBulge){
        circos.text(i,0.5, ctFile[i,2], facing = "clockwise", cex = cex,col = 4)
      }
    }
    if(ifExternalLoops){
      arrExternalLoops <- unlist(external_loop(ctFile))
      if(i %in% arrExternalLoops){
        circos.text(i,0.5, ctFile[i,2], facing = "clockwise", cex = cex,col = 6)
      }
    }
    if(ifHairpin){
      arrHairpin <- unlist(hairpin_loop(ctFile))
      if(i %in% arrHairpin){
        circos.text(i,0.5, ctFile[i,2], facing = "clockwise", cex = cex,col = 13)
      }
    }
    if(ifInternalLoops){
      arrInternalLoops <- unlist(internal_loop(ctFile))
      if(i %in% arrInternalLoops){
        circos.text(i,0.5, ctFile[i,2], facing = "clockwise", cex = cex,col = 7)
      }
    }
    if(ifMultiBranchLoops){
      arrMultiBranchLoops <- unlist(multi_branch_loop(ctFile))
      if(i %in% arrMultiBranchLoops){
        circos.text(i,0.5, ctFile[i,2], facing = "clockwise", cex = cex,col = 10)
      }
    }

    if(ifNum){
      circos.text(i,0.5 + posNum, ctFile[i,6], facing = "clockwise", cex = cex)
    }
  }
  for(i in 1:baseNum){
    if(ctFile[i,5] != 0){
      circos.link("a",ctFile[i,5],"a",ctFile[i,6],directional = 0)
    }
  }
  circos.clear()
}





