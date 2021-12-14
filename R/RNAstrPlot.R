RNAstrPlot <- function(ctFile){
  ###
  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(oldpar))
  bulge <- bulge_loop(ctFile)
  hairpin <- hairpin_loop(ctFile)
  internal <- internal_loop(ctFile)
  multi_branch <- multi_branch_loop(ctFile)
  stems <- stem(ctFile)
  external <- external_loop(ctFile)
  ###
  bulge_arr <- unlist(bulge)
  hairpin_arr <- unlist(hairpin)
  internal_arr <- unlist(internal)
  multi_branch_arr <- unlist(multi_branch)
  stems_arr <- unlist(stems)
  external_arr <- unlist(external)
  ###
  dot <- ct2dot(ctFile)
  ct <- makeCt(dot[[1]][1],dot[[2]][1])
  co <- ct2coord(ct)
  graphics::layout(matrix(c(1,1,2,2),byrow = F,2,2)
         ,widths = c(3,1),heights = c(1,1))
  RNAPlot(co,modspec=TRUE,labTF = T
          ,main = "RNA secondary structure",
          modp=c(bulge_arr,hairpin_arr
                 ,internal_arr,multi_branch_arr
                 ,stems_arr,external_arr)
          ,mod=c(rep(16,(length(bulge_arr)+length(hairpin_arr)
                         +length(internal_arr)
                         +length(multi_branch_arr)
                         +length(stems_arr)
                         +length(external_arr))
                         ))
          ,modcol=c(rep(4,length(bulge_arr))
                    ,rep(13,length(hairpin_arr))
                    ,rep(7,length(internal_arr))
                    ,rep(10,length(multi_branch_arr))
                    ,rep(19,length(stems_arr))
                    ,rep(6,length(external_arr)))
)
  ###
  graphics::par(mar = c(0,0,3,0))
  graphics::plot(c(1:4),type = "n",xaxt = "n",yaxt="n"
       ,xlab = "",ylab = "",bty = "n")
  graphics::legend(1, 4,
         c("bulge loop","external loop","hairpin loop","internal loop"
            ,"multiBranch loop","stem"), col = c(4,6,13,7,10,19)
         ,text.col = c(4,6,13,7,10,19),pch = c(19,19,19,19,19,19),
         bg = "gray75")

  ##############
  print("------------------------------------------------------")
  print("*                                                    *")
  print("*   what print below is the information of result    *")
  print("*                                                    *")
  print("------------------------------------------------------")

  ###
  #
  mat <- matrix(rep(0,6*5),nrow = 6)
  rownames(mat) <- c("bulge loops","external loop","hairpin loop","internal loop"
                     ,"multi branch loop","stem")
  colnames(mat) <- c("number of bases","number of loops"
                     ,"Maximum length","Minimum length"
                     ,"Average length"
                     )
  #bulge
  if(length(bulge) != 0){
    bulge_length <- length(bulge_arr)
    bulge_number <- length(bulge)
    bulge_max <- length(bulge[[1]])
    bulge_min <- length(bulge[[1]])
    for (i in 1:length(bulge)) {
      if(length(bulge[[i]]) > bulge_max){
        bulge_max <- length(bulge[[i]])
      }
      if(length(bulge[[i]]) < bulge_min){
        bulge_min <- length(bulge[[i]])
      }
    }
    bulge_mean <- bulge_length/bulge_number
    #
    mat[1,1] <- bulge_length
    mat[1,2] <- bulge_number
    mat[1,3] <- bulge_max
    mat[1,4] <- bulge_min
    mat[1,5] <- bulge_mean
  }
  ###external loop
  if(length(external) != 0){
    external_length <- length(external_arr)
    external_number <- length(external)
    external_max <- length(external[[1]])
    external_min <- length(external[[1]])
    for (i in 1:length(external)) {
      if(length(external[[i]]) > external_max){
        external_max <- length(external[[i]])
      }
      if(length(external[[i]]) < external_min){
        external_min <- length(external[[i]])
      }
    }
    external_mean <- external_length/external_number
    #
    mat[2,1] <- external_length
    mat[2,2] <- external_number
    mat[2,3] <- external_max
    mat[2,4] <- external_min
    mat[2,5] <- external_mean
  }
  #hairpin
  if(length(hairpin) != 0){
    hairpin_length <- length(hairpin_arr)
    hairpin_number <- length(hairpin)
    hairpin_max <- length(hairpin[[1]])
    hairpin_min <- length(hairpin[[1]])
    for (i in 1:length(hairpin)) {
      if(length(hairpin[[i]]) > hairpin_max){
        hairpin_max <- length(hairpin[[i]])
      }
      if(length(hairpin[[i]]) < hairpin_min){
        hairpin_min <- length(hairpin[[i]])
      }
    }
    hairpin_mean <- hairpin_length/hairpin_number
    #
    mat[3,1] <- hairpin_length
    mat[3,2] <- hairpin_number
    mat[3,3] <- hairpin_max
    mat[3,4] <- hairpin_min
    mat[3,5] <- hairpin_mean
  }
  #internal
  if(length(internal) != 0){
    internal_length <- length(internal_arr)
    internal_number <- length(internal)
    internal_max <- length(internal[[1]])
    internal_min <- length(internal[[1]])
    for (i in 1:length(internal)) {
      if(length(internal[[i]]) > internal_max){
        internal_max <- length(internal[[i]])
      }
      if(length(internal[[i]]) < internal_min){
        internal_min <- length(internal[[i]])
      }
    }
    internal_mean <- internal_length/internal_number
    #
    mat[4,1] <- internal_length
    mat[4,2] <- internal_number
    mat[4,3] <- internal_max
    mat[4,4] <- internal_min
    mat[4,5] <- internal_mean
  }
  #multi-branch
  if(length(multi_branch) != 0){
    multi_branch_length <- length(multi_branch_arr)
    multi_branch_number <- length(multi_branch)
    multi_branch_max <- length(multi_branch[[1]])
    multi_branch_min <- length(multi_branch[[1]])
    for (i in 1:length(multi_branch)) {
      if(length(multi_branch[[i]]) > multi_branch_max){
        multi_branch_max <- length(multi_branch[[i]])
      }
      if(length(multi_branch[[i]]) < multi_branch_min){
        multi_branch_min <- length(multi_branch[[i]])
      }
    }
    multi_branch_mean <- multi_branch_length/multi_branch_number
    #
    mat[5,1] <- multi_branch_length
    mat[5,2] <- multi_branch_number
    mat[5,3] <- multi_branch_max
    mat[5,4] <- multi_branch_min
    mat[5,5] <- multi_branch_mean

  }
  #stems
  if(length(stems) != 0){
    stems_length <- length(stems_arr)
    stems_number <- length(stems)
    stems_max <- length(stems[[1]])
    stems_min <- length(stems[[1]])
    for (i in 1:length(stems)) {
      if(length(stems[[i]]) > stems_max){
        stems_max <- length(stems[[i]])
      }
      if(length(stems[[i]]) < stems_min){
        stems_min <- length(stems[[i]])
      }
    }
    stems_mean <- stems_length/stems_number
    #
    mat[6,1] <- stems_length
    mat[6,2] <- stems_number
    mat[6,3] <- stems_max
    mat[6,4] <- stems_min
    mat[6,5] <- stems_mean
  }
  #
  result <- list()
  result[["sumery of result"]] <- mat
  if(length(bulge) != 0){
    result[["bases in bulge loops"]] <- sort(bulge_arr)
  }
  if(length(external) != 0){
    result[["bases in external loops"]] <- sort(external_arr)
  }
  if(length(hairpin) != 0){
    result[["bases in hairpin loops"]] <- sort(hairpin_arr)
  }
  if(length(internal) != 0){
    result[["bases in internal loops"]] <- sort(internal_arr)
  }
  if(length(multi_branch) != 0){
    result[["bases in multi-branch loops"]] <- sort(multi_branch_arr)
  }
  if(length(stems) != 0){
    result[["bases in stems"]] <- sort(stems_arr)
  }
  return(result)
}


