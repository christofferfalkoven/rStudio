#' randomWC
#'
#' Control function for Where's Croc where moves are random.
#' @param moveInfo See runWheresCroc for details
#' @param readings See runWheresCroc for details
#' @param positions See runWheresCroc for details
#' @param edges See runWheresCroc for details
#' @param probs See runWheresCroc for details
#' @return See runWheresCroc for details
#' @export
randomWC=function(moveInfo,readings,positions,edges,probs) {
  moveInfo$moves=c(sample(getOptions(positions[3],edges),1),0)
  print("MOVE INFO")
  print(moveInfo)
  print("READINGS")
  print(readings)
  print("POSITIONS")
  print(positions)
  print("EDGES")
  print(edges)
  print("PROBS")
  print(probs)
  return(moveInfo)
}
myFunction =function(moveInfo,readings,positions,edges,probs){
  if(length(moveInfo$mem) == 1){
    initialVector = makeInitialVector()
    init = list("init" = initialVector)
    moveInfo$mem <- append(moveInfo$mem, init)
    moveMatrix <- list("moveMatrix" = makeMoveMatrix())
    moveInfo$mem <- append(moveInfo$mem, moveMatrix)
  }
  if(is.null(moveInfo$moves)){
    moveInfo$mem$init = makeInitialVector()
  }
  
  transmissionMatrix = makeTransmissionMatrix(edges)
  emissionVector = makeEmissionVector(readings,probs)
  forwardVector = makeDistributionVector(transmissionMatrix, moveInfo$mem$init, emissionVector, moveInfo)
  
  moveInfo$mem$init<-forwardVector
  
  rangerPos = positions[3]

  initCopy <- moveInfo$mem$init
  
  firstChoice <- which.max(moveInfo$mem$init)
  n <- length(initCopy)
  secondChoice <- which(initCopy ==sort(initCopy, partial = n-1)[n-1])
  moveMatrix <- moveInfo$mem$moveMatrix
  # print("FIRST CHOICE")
  # print(firstChoice)
  # print("SECOND CHOICE")
  # print(secondChoice)  
  if (rangerPos == firstChoice) {
    moveOne <- 0
    moveTwo <- moveMatrix[rangerPos, secondChoice]
    moveInfo$moves=c(moveOne,moveTwo)
    return(moveInfo)
  }else{
    moveOne <- moveMatrix[rangerPos, firstChoice]
    if(moveOne == firstChoice){
      moveTwo <- 0
    }else{
      moveTwo <- moveMatrix[moveOne, firstChoice]
    }
    moveInfo$moves=c(moveOne,moveTwo)
    return(moveInfo)
  }
}
#' manualWC
#'
#' Control function for Where's Croc that allows manual play using keyboard.
#' @param moveInfo See runWheresCroc for details
#' @param readings See runWheresCroc for details
#' @param positions See runWheresCroc for details
#' @param edges See runWheresCroc for details
#' @param probs See runWheresCroc for details
#' @return See runWheresCroc for details
#' @export


makeMoveMatrix <- function(){
  moveCol1  = c(1,1,5,1,2,1,5,4,8,8,9,6,11,10,14,12,15,17,16,19,23,16,20,22,24,25,22,31,21,27,27,33,31,31,33,33,33,34,36,35)
  moveCol2  = c(2,2,5,2,2,4,5,4,5,8,9,6,7,10,11,12,15,13,16,19,18,16,20,22,24,25,22,25,21,27,27,29,31,31,29,33,33,34,36,35)
  moveCol3  = c(2,5,3,2,3,4,3,9,5,8,7,10,7,15,11,14,15,13,17,17,18,19,21,23,23,29,24,25,21,27,28,29,32,31,29,33,33,34,36,35)
  moveCol4  = c(4,4,5,4,2,4,5,4,8,8,9,6,11,10,14,12,15,13,16,19,18,16,20,22,24,25,22,25,21,27,27,28,31,31,33,33,33,34,36,35)
  moveCol5  = c(2,5,5,2,5,4,5,9,5,8,9,10,7,10,11,14,15,13,17,17,18,16,21,23,23,29,24,25,21,27,28,29,32,31,29,33,33,34,36,35)
  moveCol6  = c(6,4,5,6,2,6,5,4,8,12,9,6,11,10,14,12,15,17,16,19,23,16,20,22,24,25,22,25,26,27,27,28,31,30,33,33,33,34,36,35)
  moveCol7  = c(2,5,7,2,7,4,7,9,11,8,7,10,7,15,11,14,15,13,17,17,18,19,21,23,23,29,24,25,21,27,28,29,32,31,29,33,33,34,36,35)
  moveCol8  = c(4,4,5,8,9,4,11,8,8,8,9,10,11,10,14,14,15,13,16,17,18,16,20,22,24,25,22,25,21,27,27,29,31,31,29,33,33,34,36,35)
  moveCol9  = c(4,5,5,8,9,4,11,9,9,8,9,10,11,10,11,14,15,13,17,17,18,16,21,23,23,29,22,25,21,27,27,29,32,30,29,33,33,34,36,35)
  moveCol10 = c(4,4,5,8,9,12,11,10,8,10,15,10,11,10,14,14,15,17,16,19,18,16,20,22,24,25,22,25,21,27,27,28,31,31,33,33,33,34,36,37)
  moveCol11 = c(4,5,7,8,9,4,11,9,11,14,11,10,11,15,11,14,15,13,17,17,18,19,20,23,23,29,22,25,21,27,27,29,32,31,29,33,33,34,36,35)
  moveCol12 = c(6,4,5,6,9,12,11,10,8,12,15,12,11,10,14,12,19,17,16,19,23,16,20,22,24,25,22,25,26,27,27,28,31,31,33,33,33,34,36,35)
  moveCol13 = c(2,5,7,8,7,4,13,9,11,14,13,10,13,15,11,14,18,13,17,17,18,19,21,23,23,29,24,25,21,27,28,29,32,31,29,33,33,34,36,35)
  moveCol14 = c(4,4,7,8,9,12,11,10,11,14,15,10,11,14,14,14,15,17,16,17,18,16,20,22,24,25,22,25,21,27,27,28,31,31,33,33,33,34,36,35)
  moveCol15 = c(4,5,7,8,9,12,11,9,11,14,15,10,11,15,15,14,15,17,17,17,18,19,20,23,23,25,22,25,21,27,27,29,31,31,29,33,33,34,36,35)
  moveCol16 = c(6,4,7,6,9,12,11,10,8,14,15,16,11,16,14,16,19,17,16,19,23,16,20,22,24,25,22,25,26,27,27,28,31,31,33,33,33,34,36,35)
  moveCol17 = c(4,5,7,8,9,12,11,9,11,14,15,16,18,15,17,19,17,17,17,17,18,19,20,23,23,25,22,25,21,27,27,29,31,31,29,33,33,34,36,35)
  moveCol18 = c(2,5,7,8,7,12,13,9,11,14,13,16,18,15,17,19,18,18,17,17,18,19,21,23,23,29,24,25,21,27,28,29,32,31,29,33,33,34,36,35)
  moveCol19 = c(6,4,7,6,9,12,11,10,11,14,15,16,18,16,17,19,19,17,19,19,23,19,20,22,23,25,22,25,21,27,27,28,31,31,33,33,33,34,36,35)
  moveCol20 = c(6,4,7,6,9,12,13,10,11,14,15,16,18,16,17,19,20,17,20,20,23,19,20,23,23,25,24,25,21,27,27,28,31,31,32,33,33,34,36,35)
  moveCol21 = c(2,5,7,8,7,12,13,9,11,14,13,16,18,15,17,19,18,21,20,23,21,24,21,23,23,29,25,25,21,27,28,29,32,31,29,33,33,34,36,35)
  moveCol22= c(6,4,5,6,9,12,11,10,8,14,15,16,18,16,14,22,19,17,22,19,23,22,24,22,24,25,22,25,26,27,27,28,31,31,33,33,33,34,36,35)
  moveCol23 = c(6,5,7,6,7,12,13,10,11,14,15,16,18,15,17,19,20,21,20,23,23,24,23,23,23,25,25,25,21,27,27,28,31,31,32,33,33,34,36,35)
  moveCol24 = c(6,4,7,6,7,12,13,10,8,14,15,16,18,16,17,22,20,21,22,23,23,24,24,24,24,25,24,25,26,27,27,28,31,31,33,33,33,34,36,35)
  moveCol25 = c(6,4,7,6,7,12,13,10,11,14,15,16,18,16,17,22,20,21,20,23,23,24,25,25,25,25,25,25,26,27,28,28,31,31,32,33,33,34,36,35)
  moveCol26 = c(6,5,7,6,7,12,13,10,11,14,13,16,18,16,17,22,20,21,20,23,29,24,25,25,26,26,25,26,26,31,28,28,32,31,32,33,33,34,36,35)
  moveCol27 = c(6,4,7,6,9,12,13,10,8,14,15,16,18,16,14,22,19,21,22,23,23,27,24,27,27,25,27,31,32,27,27,28,31,31,33,34,33,34,36,37)
  moveCol28 = c(6,4,7,6,7,12,13,10,11,14,13,16,18,15,17,19,20,21,20,23,23,27,25,25,28,28,31,28,26,31,28,28,31,31,32,33,33,34,36,35)
  moveCol29 = c(2,5,7,2,7,12,13,9,11,14,13,16,18,15,17,19,18,21,17,23,29,24,21,23,26,29,25,32,29,31,33,29,32,31,29,33,33,39,36,35)
  moveCol30 = c(6,4,7,6,7,12,13,10,8,14,15,16,18,16,14,22,19,17,22,23,23,27,24,27,27,25,30,31,26,30,30,28,31,30,33,34,36,34,36,37)
  moveCol31 = c(6,4,7,6,9,12,13,10,8,14,15,16,18,16,14,22,19,21,22,23,23,27,24,27,28,28,31,31,32,31,31,33,31,31,33,33,33,34,36,35)
  moveCol32 = c(6,5,7,6,7,12,13,9,11,12,13,16,18,16,17,22,18,21,22,23,29,27,21,25,28,28,31,32,32,31,33,32,32,31,32,33,33,34,36,35)
  moveCol33 = c(6,5,7,6,7,12,13,10,11,12,13,16,18,16,17,22,18,21,22,23,29,27,21,27,28,28,31,32,32,31,33,33,33,31,33,33,33,34,36,37)
  moveCol34 = c(6,4,7,6,9,12,13,10,8,14,15,16,18,16,14,22,19,17,22,23,23,27,24,27,28,28,31,31,32,34,34,33,31,34,33,34,36,34,36,37)
  moveCol35 = c(6,5,7,6,7,12,13,9,11,12,13,16,18,16,17,22,18,21,17,23,29,27,21,27,28,29,31,32,35,31,33,35,35,36,35,33,33,39,36,35)
  moveCol36 = c(6,4,7,6,7,12,13,10,11,12,13,16,18,16,15,22,18,21,22,23,29,27,25,27,28,28,31,31,32,34,33,33,36,36,33,36,36,39,36,37)
  moveCol37 = c(6,4,7,6,7,12,13,10,11,14,13,16,18,16,17,22,18,21,22,23,29,27,25,27,28,28,31,31,32,31,33,33,37,36,33,37,37,39,37,37)
  moveCol38 = c(6,4,7,6,9,12,11,10,8,14,15,16,18,16,14,22,19,17,22,23,23,27,25,27,28,28,31,31,32,34,34,33,36,38,33,39,39,38,38,37)
  moveCol39 = c(6,4,7,6,7,12,13,10,11,14,13,16,18,16,17,22,18,21,22,23,29,27,25,27,28,28,31,31,32,34,33,33,36,36,33,39,39,39,39,37)
  moveCol40 = c(6,5,7,6,7,12,13,9,11,14,13,16,18,15,17,22,18,21,20,23,29,27,21,27,28,29,31,32,35,31,33,35,37,36,40,37,40,39,37,40)
  
  moveMatrix = matrix(c(moveCol1 , moveCol2 , moveCol3 , moveCol4 , moveCol5 , moveCol6,  moveCol7,  moveCol8,  moveCol9,  moveCol10, 
                  moveCol11, moveCol12, moveCol13, moveCol14, moveCol15, moveCol16, moveCol17, moveCol18, moveCol19, moveCol20, 
                  moveCol21, moveCol22, moveCol23, moveCol24, moveCol25, moveCol26, moveCol27, moveCol28, moveCol29, moveCol30, 
                  moveCol31, moveCol32, moveCol33, moveCol34, moveCol35, moveCol36, moveCol37, moveCol38, moveCol39, moveCol40),40)
  return(moveMatrix)
}

makeTransmissionMatrix <- function(edges){
  transmissionMatrix <- matrix(0,ncol = 40, nrow = 40)
  for (j in 1:40) {
    transmissionMatrix[j,j] = transmissionMatrix[j,j] +1 
  }
  for (i in 1:nrow(edges)) {
    transmissionMatrix[edges[i,1], edges[i,2]] = transmissionMatrix[edges[i,1], edges[i,2]] +1
    transmissionMatrix[edges[i,2], edges[i,1]] = transmissionMatrix[edges[i,2], edges[i,1]] +1
  }
  for (i in 1:40) {
    devide1 = which(transmissionMatrix[i,] == 1)
    devide = length(devide1)
    transmissionMatrix[i,] = (transmissionMatrix[i,]/devide)
  }
  return(transmissionMatrix)
}
makeInitialVector <- function(){
  initialVector <- c(rep(1/40, 40))
  return(initialVector)
}
makeEmissionVector <- function(readings, probs){
  dvaluesS <- dnorm(readings[[1]], probs$salinity[,1], probs$salinity[,2])
  dvaluesP <- dnorm(readings[[2]], probs$phosphate[,1], probs$phosphate[,2])
  dvaluesN <- dnorm(readings[[3]], probs$nitrogen[,1], probs$nitrogen[,2])
  emissionVector <- (dvaluesS*dvaluesP*dvaluesN)
  return(emissionVector)
}
# makeDistributionVectorHC()<-function(transmissionMatrix, initialVector, emissionVector, moveInfo){
#   
# }
makeDistributionVector <- function(transmissionMatrix, initialVector, emissionVector, moveInfo){

  otherNewVect <- c()
  for (j in 1:40) {
    newVect <- c()
    for (i in 1:40) {
      newVect[i] <- initialVector[i]*transmissionMatrix[i,j]
      #temp[length(temp)+1] <- newVect
    }
    otherNewVect[length(otherNewVect)+1] <- sum(newVect)
  }
  otherOtherNewVector <- otherNewVect*emissionVector
  otherOtherNewVector <- otherOtherNewVector/sum(otherNewVect)
  return(otherOtherNewVector)
}

forwardAlgoritm <- function(){
  
}
checkIfEaten <- function(positions, initialVector){
  if(positions[1]<0){
    whereHeGotEaten <- abs(positions[1])
    initialVector <- c(rep(0, 40))
    initialVector[whereHeGotEaten] = initialVector[whereHeGotEaten] + 1
    print("The backpacker at ")
    print(whereHeGotEaten)
    print(" got eaten")
       print(initialVector)
    return(initialVector)
  }
  if(positions[2]<0){
    whereHeGotEaten <- abs(positions[2])
    initialVector <- c(rep(0, 40))
    initialVector[whereHeGotEaten] = initialVector[whereHeGotEaten] +1
    print("The backpacker at ")
    print(whereHeGotEaten)
    print(" got eaten")
    print(initialVector)
    return(initialVector)
  }
  return(initialVector)
}
manualWC=function(moveInfo,readings,positions,edges,probs) {
  if(length(moveInfo$mem) == 1){
    initialVector = makeInitialVector()
    init = list("init" = initialVector)
    moveInfo$mem <- append(moveInfo$mem, init)
    moveMatrix <- list("moveMatrix" = makeMoveMatrix())
    print(initialVector)
    moveInfo$mem <- append(moveInfo$mem, moveMatrix)
    print("REEEE")
  }  
  transmissionMatrix = makeTransmissionMatrix(edges)
  #initialVector = checkIfEaten(positions, initialVector)
  emissionVector = makeEmissionVector(readings,probs)
  forwardVector = makeDistributionVector(transmissionMatrix, moveInfo$mem$init, emissionVector, moveInfo)
  
  print("emissionVector")
  print(emissionVector)
  print("forwardvector")
  print(forwardVector)
  moveInfo$mem$init<-forwardVector
  print(which.max(forwardVector))
  

  options=getOptions(positions[3],edges)
  print("Move 1 options (plus 0 for search):")
  print(options)
  mv1=readline("Move 1: ")
  if (mv1=="q") {stop()}
  if (!mv1 %in% options && mv1 != 0) {
    warning ("Invalid move. Search ('0') specified.")
    mv1=0
  }
  if (mv1!=0) {
    options=getOptions(mv1,edges)
  }
  print("Move 2 options (plus 0 for search):")
  print(options)
  mv2=readline("Move 2: ")
  if (mv2=="q") {stop()}
  if (!mv1 %in% options && mv1 != 0) {
    warning ("Invalid move. Search ('0') specified.")
    mv2=0
  }

  moveInfo$moves=c(mv1,mv2)
  return(moveInfo)
}

#' testWC
#'
#' Use this to debug under multiple circumstances and to see how your function compares with the par function
#' With the default seed of 21, the mean for the par function on this is 5.444, and the sd is approximately 3.853.
#'
#' Your final result will be based on how your function performs on a similar run of 500 games, though with
#' a different seed used to select them.
#'
#' While this is dependent on the machine used, we expect your function to be able to run the 500 evaluation games on
#' the evaluation machine in under 30 seconds. Note that you will need to reuse objects that do not change
#' from game to game (such as the transition matrix and routing information) in order to achieve this sort
#' of speed.
#'
#' The par function takes approximately 3.85 seconds on my laptop. If it takes longer than 30 seconds on the
#' evaluation machine, the time limit will be increased so as to be 25% slower than the par function.
#'
#' @param myFunction Your function to be passed to runWheresCroc. See runWheresCroc documentation for details.
#' @param verbose Set to 0 for no output, 1 for a summary of the results of the games played (mean,
#' standard deviation and time taken), and 2 for the above plus written output detailing seeds used and the
#' runWheresCroc output of the result of each game.
#' @param returnVec See return value.
#' @param seed The random seed to use. Pass NA to not set a random seed.
#' @param timeLimit The time limit. If this is breached, a NA is returned.
#' @return If your function is too slow, NA is returned. Otherwise if returnVec is TRUE, a vector containing
#' all results is returned. If returnVec is FALSE, the average performance is returned.
#' @export
testWC=function(myFunction,verbose=0,returnVec=FALSE,seed=21,timeLimit=300){
  set.seed(21)
  seeds=sample(1:25000,500)
  startTime=Sys.time()
  mem=NA
  hmm=c()
  for (s in seeds) {
    midTime=Sys.time()
    if (as.numeric(midTime)-as.numeric(startTime)>timeLimit) {
      cat("\nRun terminated due to slowness.")
      return (NA)
    }
    set.seed(s)
    if (verbose==2)
      cat("\nNew game, seed",s)
    res=runWheresCroc(myFunction,doPlot=F,pause=0,verbose=verbose==2,returnMem=T,mem=mem)
    mem=res$mem
    hmm=c(hmm,res$move)
  }
  if (verbose>=1) {
    endTime=Sys.time()
    cat("\nMean moves:", mean(hmm))
    cat("\nSD moves:", sd(hmm))
    cat("\nTime taken:",as.numeric(endTime)-as.numeric(startTime),"seconds.")
  }
  if (returnVec)
    return (hmm)
  else
    return (mean(hmm))
}

#' Run Where's Croc
#'
#' Runs the Where's Croc game. In this game, you are a ranger in an Australian national park.
#' This park consists of a number of waterholes, some of which are connected to each other.
#' There is a crocodile in the park called 'Croc'. Croc has been fitted with sensors that record
#' the salinity, phosphate and nitrogen levels in the water where he is swimming. He was also
#' fitted with a sensor that records his position, but that has broken.
#' Your task is to find Croc using the available information. To aid in this you have information
#' about the probability distributions for different salinity, phosphate and nitrogen levels in
#' different waterholes.
#' There are also two tourists in the park. Both the tourists and Croc walk randomly, each turn
#' moving to one of the neighboring waterholes from where they are or staying still. All moves
#' are equally likely.
#' If Croc and a tourist end up on the same waterhole, Croc will eat the tourist. If you search
#' the waterhole you are on when Croc is there, you have found Croc and win the game.
#' Your score is the number of turns it takes to find Croc.
#' To play manually pass manualWC
#' as the makeMoves function and enter the appropriate numbers to make moves.
#' Note that the croc will move randomly, with a uniform distribution over moving to any adjancent waterholes
#' or staying still.
#' @param makeMoves Your function that takes five arguments: (1) A list of information for the move.
#' This has two fields. The first is a vector of numbers called 'moves', where you will enter
#' the moves you want to make. You should
#' enter two moves (so you can move to a neighboring waterhole and search). Valid moves are the
#' numbers of a neighboring or current waterhole or '0' which means you will search your current
#' waterhole for Croc. The second field is a list called
#' 'mem' that you can use to store information you want to remember from turn to turn. (2) A
#' vector giving the salinity, phosphate and nitrogen reading from Croc sensors at his current
#' location. (3) A vector giving the positions of the two tourists
#' (elements 1 and 2) and yourself (element 3). If a tourist
#' has just been eaten by Croc that turn, the position will be multiplied by -1. If a tourist
#' was eaten by Croc in a previous turn, then the position will be NA. (4) a two column matrix giving the
#' edges paths between waterholes (edges) present (the numbers are from and to numbers for
#' the waterholes). All edges can be crossed both ways, so are only given once.
#' (5) a list of three matrices giving the mean
#' and standard deviation of readings for salinity, phosphate and nitrogen respectively
#' at each waterhole.
#' Your function should return the first argument passed with an updated moves vector
#' and any changes to the 'mem' field you wish to access later on.
#' @param doPlot A Boolean stating if you want the gameboard to be plotted each turn
#' @param showCroc A Boolean value specifying whether you want Croc to be shown on the gameboard.
#' Note that you are not permitted to use this visual information when you are scored.
#' @param pause The pause period between moves. Designed to give time to plot game.
#' @param verbose Set to FALSE to stop any print output
#' @param returnMem Should the info$mem field be returned? If so, the output is a list consisting of
#' the move field, giving the number of moves in the game, and the mem field consisting of the mem
#' object
#' @param mem If you returned a mem object from a previous run, it can be passed here. It's status
#' will be set to 1. Otherwise a new mem list will be created with status set to 0. The idea is
#' to speed up multiple runs, such as the evaluation run of 500 games, by avoiding redoing
#' expensive initial setups of the transition matrix and routing informing.
#' @return A string describing the outcome of the game.
#' @export
runWheresCroc=function(makeMoves,doPlot=T,showCroc=F,pause=1,verbose=T,returnMem=F,mem=NA) {
  positions=sample(1:40,4) # Croc, BP1, BP2, Player
  points=getPoints()
  edges=getEdges()
  probs=getProbs()
  move=0
  moveInfo=list(moves=c(),mem=list(status=0))
  if (!all(is.na(mem)))
    moveInfo$mem=mem
  first=T
  while (!is.na(positions[1])) {
    move=move+1
    if (!first) {
      positions[1]=sample(getOptions(positions[1],edges),1)
      if (!is.na(positions[2])&&positions[2]>0) {
        positions[2]=sample(getOptions(positions[2],edges),1)
      } else if (!is.na(positions[2]) && positions[2]<0) {
        positions[2]=NA
      }
      if (!is.na(positions[3])&&positions[3]>0) {
        positions[3]=sample(getOptions(positions[3],edges),1)
      } else if (!is.na(positions[3]) && positions[3]<0) {
        positions[3]=NA
      }
      if (!is.na(positions[2]) && positions[2]==positions[1]) {
        positions[2]=-positions[2]
      }
      if (!is.na(positions[3]) && positions[3]==positions[1]) {
        positions[3]=-positions[3]
      }
    }
    else
      first=F

    if (doPlot)
      plotGameboard(points,edges,move,positions,showCroc)

    Sys.sleep(pause)

    readings=getReadings(positions[1],probs)
    moveInfo=makeMoves(moveInfo,readings,positions[2:4],edges,probs)
    if (length(moveInfo$moves)!=2) {
      stop("Error! Passed makeMoves function should return a vector of two elements.")
    }
    for (m in moveInfo$moves) {
      if (m==0) {
        if (positions[1]==positions[4]) {
          if (verbose)
            cat("\nCongratualations - You got croc at move ",move)
          if (returnMem) {
            mem=moveInfo$mem
            mem$status=1
            return (list(move=move,mem=mem))
          }
          return (move)
        }
      } else {
        if (m%in%getOptions(positions[4],edges)) {
          positions[4]=m
        } else {
          warning("Invalid move.")
        }
      }
    }
  }
}
#' @keywords internal
getPoints=function() {
  points=matrix(c(1,1),ncol=2)
  points=rbind(points,c(1,7))
  points=rbind(points,c(1,17))
  points=rbind(points,c(2,3))
  points=rbind(points,c(2,12))
  points=rbind(points,c(3,2))
  points=rbind(points,c(3,19))
  points=rbind(points,c(4,7))
  points=rbind(points,c(4,11))
  points=rbind(points,c(5,5))
  points=rbind(points,c(5,15))
  points=rbind(points,c(6,1))
  points=rbind(points,c(6,20))
  points=rbind(points,c(7,6))
  points=rbind(points,c(7,11))
  points=rbind(points,c(8,2))
  points=rbind(points,c(8,14))
  points=rbind(points,c(8,18))
  points=rbind(points,c(9,6))
  points=rbind(points,c(10,10))
  points=rbind(points,c(10,18))
  points=rbind(points,c(11,1))
  points=rbind(points,c(11,12))
  points=rbind(points,c(12,6))
  points=rbind(points,c(12,12))
  points=rbind(points,c(13,16))
  points=rbind(points,c(14,4))
  points=rbind(points,c(14,12))
  points=rbind(points,c(14,20))
  points=rbind(points,c(15,3))
  points=rbind(points,c(15,8))
  points=rbind(points,c(15,17))
  points=rbind(points,c(16,14))
  points=rbind(points,c(17,3))
  points=rbind(points,c(17,18))
  points=rbind(points,c(18,10))
  points=rbind(points,c(19,13))
  points=rbind(points,c(20,2))
  points=rbind(points,c(20,6))
  points=rbind(points,c(20,19))
  return (points)
}

#' @keywords internal
getEdges=function() {
  edges=matrix(c(1,2),ncol=2)
  edges=rbind(edges,c(1,4))
  edges=rbind(edges,c(1,6))
  edges=rbind(edges,c(2,4))
  edges=rbind(edges,c(2,5))
  edges=rbind(edges,c(3,5))
  edges=rbind(edges,c(3,7))
  edges=rbind(edges,c(4,6))
  edges=rbind(edges,c(4,8))
  edges=rbind(edges,c(5,7))
  edges=rbind(edges,c(5,9))
  edges=rbind(edges,c(6,12))
  edges=rbind(edges,c(7,11))
  edges=rbind(edges,c(7,13))
  edges=rbind(edges,c(8,9))
  edges=rbind(edges,c(8,10))
  edges=rbind(edges,c(9,11))
  edges=rbind(edges,c(10,12))
  edges=rbind(edges,c(10,14))
  edges=rbind(edges,c(11,13))
  edges=rbind(edges,c(11,15))
  edges=rbind(edges,c(12,16))
  edges=rbind(edges,c(13,18))
  edges=rbind(edges,c(14,15))
  edges=rbind(edges,c(14,16))
  edges=rbind(edges,c(15,17))
  edges=rbind(edges,c(16,19))
  edges=rbind(edges,c(16,22))
  edges=rbind(edges,c(17,18))
  edges=rbind(edges,c(17,19))
  edges=rbind(edges,c(17,20))
  edges=rbind(edges,c(18,21))
  edges=rbind(edges,c(19,20))
  edges=rbind(edges,c(19,22))
  edges=rbind(edges,c(20,23))
  edges=rbind(edges,c(21,23))
  edges=rbind(edges,c(21,29))
  edges=rbind(edges,c(22,24))
  edges=rbind(edges,c(22,27))
  edges=rbind(edges,c(23,24))
  edges=rbind(edges,c(23,25))
  edges=rbind(edges,c(24,25))
  edges=rbind(edges,c(24,27))
  edges=rbind(edges,c(25,26))
  edges=rbind(edges,c(25,27))
  edges=rbind(edges,c(25,28))
  edges=rbind(edges,c(26,28))
  edges=rbind(edges,c(26,29))
  edges=rbind(edges,c(27,30))
  edges=rbind(edges,c(27,31))
  edges=rbind(edges,c(28,31))
  edges=rbind(edges,c(28,32))
  edges=rbind(edges,c(29,32))
  edges=rbind(edges,c(29,35))
  edges=rbind(edges,c(30,31))
  edges=rbind(edges,c(30,34))
  edges=rbind(edges,c(31,33))
  edges=rbind(edges,c(31,34))
  edges=rbind(edges,c(32,33))
  edges=rbind(edges,c(32,35))
  edges=rbind(edges,c(33,35))
  edges=rbind(edges,c(33,36))
  edges=rbind(edges,c(33,37))
  edges=rbind(edges,c(34,36))
  edges=rbind(edges,c(34,38))
  edges=rbind(edges,c(35,40))
  edges=rbind(edges,c(36,37))
  edges=rbind(edges,c(36,39))
  edges=rbind(edges,c(37,39))
  edges=rbind(edges,c(37,40))
  edges=rbind(edges,c(38,39))

  return (edges)
}

#' @keywords internal
getProbs=function(){
  salinity=cbind(runif(40,100,200),runif(40,5,30))
  phosphate=cbind(runif(40,100,200),runif(40,5,30))
  nitrogen=cbind(runif(40,100,200),runif(40,5,30))
  list(salinity=salinity,phosphate=phosphate,nitrogen=nitrogen)
}

#' @keywords internal
getReadings=function(point,probs){
  c(
    rnorm(1,probs$salinity[as.numeric(point),1],probs$salinity[as.numeric(point),2]),
    rnorm(1,probs$phosphate[as.numeric(point),1],probs$phosphate[as.numeric(point),2]),
    rnorm(1,probs$nitrogen[as.numeric(point),1],probs$nitrogen[as.numeric(point),2])
  )
}


#' @keywords internal
plotGameboard=function(points,edges,move,positions,showCroc) {
  plot(points,pch=18,col="blue",cex=2,xlab="X",ylab="Y",main=paste("Where's Croc - Move",move))
  xFrom=points[edges[,1],1]
  yFrom=points[edges[,1],2]
  xTo=points[edges[,2],1]
  yTo=points[edges[,2],2]
  segments(xFrom,yFrom,xTo,yTo)
  for (bp in 2:3)
    if (!is.na(positions[bp])) {
      if (positions[bp]>0) {
        points(points[as.numeric(positions[bp]),1],points[as.numeric(positions[bp]),2],col="orange",pch=17,cex=4)
      } else {
        points(points[-as.numeric(positions[bp]),1],points[-as.numeric(positions[bp]),2],col="red",pch=17,cex=4)
      }
    }
  points(points[as.numeric(positions[4]),1],points[as.numeric(positions[4]),2],col="green",pch=15,cex=4)
  if (showCroc) {
    points(points[as.numeric(positions[1]),1],points[as.numeric(positions[1]),2],col="red",pch=15,cex=4)
  }
  text(points[,1]+.4, points[,2], labels=as.character(1:40))
}

#' @keywords internal
getOptions=function(point,edges) {
  c(edges[which(edges[,1]==point),2],edges[which(edges[,2]==point),1],point)
}
