strComparePlot <- function(ctFile1,ctFile2){
  if(dim(ctFile1)[1] >= dim(ctFile2)[1]){
    RNAstructure1 <- ctFile1
    RNAstructure2 <- ctFile2
  }else{
    RNAstructure1 <- ctFile2
    RNAstructure2 <- ctFile1
  }
  blastResult <- strCompare(RNAstructure1,RNAstructure2,randomTime = 1)
  codeBlast1 <- blastResult$longSeqCode
  codeBlast2 <- blastResult$shortSeqCode
  codeList1 <- strsplit(codeBlast1,split = "")[[1]]
  codeList2 <- strsplit(codeBlast2,split = "")[[1]]
  count1 <- 0
  arrIndex1 <- c()
  for(index1 in 1:length(codeList1)){
    count1 <- count1 + 1
    if(codeList1[index1] != "-"){
      arrIndex1 <- c(arrIndex1,count1)
    }
  }
  count2 <- 0
  arrIndex2 <- c()
  for(index2 in 1:length(codeList2)){
    count2 <- count2 + 1
    if(codeList2[index2] != "-"){
      arrIndex2 <- c(arrIndex2,count2)
    }
  }
  pairCol15 <- RNAstructure1[,5]
  pairCol16 <- arrIndex1
  for(index15 in 1:length(pairCol15)){
    if(pairCol15[index15] != 0){
      pairCol15[index15] <- arrIndex1[pairCol15[index15]]
    }
  }

  pairCol25 <- RNAstructure2[,5]
  pairCol26 <- arrIndex2
  for(index25 in 1:length(pairCol25)){
    if(pairCol25[index25] != 0){
      pairCol25[index25] <- arrIndex2[pairCol25[index25]]
    }
  }
  RNAlength <- length(codeList1)
  plot(0,0,xlim = c(0,RNAlength*1.25),ylim = c(0,RNAlength*1.25),type = "n",xaxt="n",yaxt="n",bty = "n",xlab = "",ylab = "")
  for(index1 in 1:RNAlength){
    if(codeList1[index1] == "-"){
      points(index1,RNAlength*0.75,pch = 20)
    }else if(codeList1[index1] == "H"){
      points(index1,RNAlength*0.75,pch = 20,col = 13)
    }else if(codeList1[index1] == "B"){
      points(index1,RNAlength*0.75,pch = 20,col = 4)
    }else if(codeList1[index1] == "I"){
      points(index1,RNAlength*0.75,pch = 20,col = 7)
    }else if(codeList1[index1] == "M"){
      points(index1,RNAlength*0.75,pch = 20,col = 10)
    }else if((codeList1[index1] == "S") || (codeList1[index1] == "s")){
      points(index1,RNAlength*0.75,pch = 20,col = 19)
    }else{
      points(index1,RNAlength*0.75,pch = 20,col = "hotpink")
    }

  }



  for(index2 in 1:RNAlength){
    if(codeList2[index2] == "-"){
      #lines(c(0,RNAlength),c(RNAlength/2,RNAlength/2),lty = 3)
      points(index2,RNAlength*0.7,pch = 20)
    }else if(codeList2[index2] == "H"){
      points(index2,RNAlength*0.7,pch = 20,col = 13)
    }else if(codeList2[index2] == "B"){
      points(index2,RNAlength*0.7,pch = 20,col = 4)
    }else if(codeList2[index2] == "I"){
      points(index2,RNAlength*0.7,pch = 20,col = 7)
    }else if(codeList2[index2] == "M"){
      points(index2,RNAlength*0.7,pch = 20,col = 10)
    }else if((codeList2[index2] == "S") || (codeList2[index2] == "s")){
      points(index2,RNAlength*0.7,pch = 20,col = 19)
    }else{
      points(index2,RNAlength*0.7,pch = 20,col = "hotpink")
    }

  }

  for(index1 in 1:length(pairCol15)){
    if(pairCol15[index1] != 0 && pairCol15[index1] > pairCol16[index1]){
      k = (1:180*10)/10 - 90
      r <- (pairCol15[index1] - pairCol16[index1])/2
      x=r*sin(k/180*pi) + (pairCol15[index1] + pairCol16[index1])/2
      y=r*cos(k/180*pi) + RNAlength*0.75
      lines(x,y,col="red")
    }
  }
  for(index2 in 1:length(pairCol25)){
   if(pairCol25[index2] != 0 && pairCol25[index2] > pairCol26[index2]){
      k = (1:180*10)/10 + 90
      r <- (pairCol25[index2] - pairCol26[index2])/2
      x=r*sin(k/180*pi) + (pairCol25[index2] + pairCol26[index2])/2
      y=r*cos(k/180*pi) + RNAlength*0.7
      lines(x,y,col="red")
    }
  }
print(paste0("similarity:",blastResult$Similarity))
}
