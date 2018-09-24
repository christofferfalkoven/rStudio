#' dumbDM
#'
#' This control function just moves randomly, until all packages are picked up and delivered by accident!
#' @param roads See help documentation for the runDeliveryMan function
#' @param cars See help documentation for the runDeliveryMan function
#' @param packages See help documentation for the runDeliveryMan function
#' @return See help documentation for the runDeliveryMan function
#' @export
dumbDM=function(roads,car,packages){
  car$nextMove=sample(c(2,4,6,8),1)
  return (car)
}
#' basicDM
#'
#' This control function will pick up the closest package (using distance and ignoring traffic).
#' As a first step, you should make sure you do better than this.
#' @param roads See help documentation for the runDeliveryMan function
#' @param cars See help documentation for the runDeliveryMan function
#' @param packages See help documentation for the runDeliveryMan function
#' @return See help documentation for the runDeliveryMan function
#' @export
basicDM=function(roads,car,packages) {
  nextMove=0
  toGo=0
  offset=0
  if (car$load==0) {
    toGo=which(packages[,5]==0)[1]
  } else {
    toGo=car$load
    offset=2
  }
  if (car$x<packages[toGo,1+offset]) {nextMove=6}
  else if (car$x>packages[toGo,1+offset]) {nextMove=4}
  else if (car$y<packages[toGo,2+offset]) {nextMove=8}
  else if (car$y>packages[toGo,2+offset]) {nextMove=2}
  else {nextMove=5}
  car$nextMove=nextMove
  car$mem=list()
  return (car)
}
#' manualDM
#'
#' If you have the urge to play the game manually (giving moves 2, 4, 5, 6, or 8 using the keyboard) you
#' can pass this control function to runDeliveryMan
#' @param roads See help documentation for the runDeliveryMan function
#' @param cars See help documentation for the runDeliveryMan function
#' @param packages See help documentation for the runDeliveryMan function
#' @return See help documentation for the runDeliveryMan function
#' @export
manualDM=function(roads,car,packages) {
  nextMove=0
  if (car$load==0) {
    listOfSteps = findClosestPackage(car$x, car$y, packages[,1], packages[,2], packages)
    packages <- cbind(packages, findShortestSteps(car$x, car$y, packages[,1], packages[,2], packages))
    packages <- packages[order(packages[,6], decreasing = FALSE),]
    print(packages)
    print("VÄGAR I HORIZONTELLT")
    #print(roads)
    #print(roads)
    #print(roads$vroads[9])
    print("SUCC THE SUCCERBERG")
    print(roads$hroads[1])
    print(roads$vroads[1])
    #c(north, south, west, east)
    vectorOfMoves <- c(1000, 1000, 1000, 1000)
    
    print(vectorOfMoves)
    if(roads$vroads[(car$y)+1]>0){
      closeNorth = min(findShortestSteps(car$x, (car$y)+1, packages[,1], packages[,2], packages)) + roads$vroads[((car$y)+1)]
      print("CLOSEST NORTH")
      print(min(findShortestSteps(car$x, (car$y)+1, packages[,1], packages[,2], packages)))
      vectorOfMoves[1]<-closeNorth
    }
    if(car$y-1 > 0 && roads$vroads[(car$y-1)]>=0){
      closeSouth = min(findShortestSteps(car$x, (car$y-1), packages[,1], packages[,2], packages)) + roads$vroads[((car$y)-1)]
      print("CLOSEST SOUTH")
      print(closeSouth)
      vectorOfMoves[2]<-closeSouth
      
    }
    if((car$x > 1) && roads$hroads[(car$x-1)]>=0){
      closeWest = min(findShortestSteps((car$x-1), car$y, packages[,1], packages[,2], packages)) + roads$hroads[((car$x)-1)]
      print("CLOSEST WEST")
      print(closeWest)
      vectorOfMoves[3]<-closeWest
    }
    if(roads$hroads[(car$y)+1]>0){
      closeEast = min(findShortestSteps((car$x+1), car$y, packages[,1], packages[,2], packages)) + roads$hroads[((car$x)+1)]
      print("CLOSEST EAST")
      print(closeEast)
      vectorOfMoves[4]<-closeEast
      
    }
    print("Vector of moves after everything")
    print(vectorOfMoves)
    toGo=which(packages[,5]==0)[1]
    
  } else {
    #listOfSteps = findClosestPackage(car$x, car$y, packages[,3], packages[,4], packages)
    #packages <- cbind(packages, findShortestSteps(car$x, car$y, packages[,3], packages[,4], packages))
    #packages <- packages[order(packages[,6], decreasing = FALSE),]
    print(packages)
    print("VÄGAR I HORIZONTELLT")
    print(roads)
    #print(roads)
    #print(roads$vroads[9])
    print("SUCC THE SUCCERBERG")
    print(roads$hroads[1])
    print(roads$vroads[1])
    #c(north, south, west, east)
    vectorOfMoves <- c(1000, 1000, 1000, 1000)
    
    print(vectorOfMoves)
    if(roads$vroads[(car$y)+1]>0){
      closeNorth = min(findShortestSteps(car$x, (car$y)+1, packages[,3], packages[,4], packages)) + roads$vroads[((car$y)+1)]
      print("CLOSEST NORTH")
      print(min(findShortestSteps(car$x, (car$y)+1, packages[,3], packages[,4], packages)))
      vectorOfMoves[1]<-closeNorth
    }
    if(car$y-1 > 0 && roads$vroads[(car$y-1)]>=0){
      closeSouth = min(findShortestSteps(car$x, (car$y-1), packages[,3], packages[,4], packages)) + roads$vroads[((car$y)-1)]
      print("CLOSEST SOUTH")
      print(closeSouth)
      vectorOfMoves[2]<-closeSouth
      
    }
    if((car$x > 1) && roads$hroads[(car$x-1)]>=0){
      closeWest = min(findShortestSteps((car$x-1), car$y, packages[,3], packages[,4], packages)) + roads$hroads[((car$x)-1)]
      print("CLOSEST WEST")
      print(closeWest)
      vectorOfMoves[3]<-closeWest
    }
    if(roads$hroads[(car$y)+1]>0){
      closeEast = min(findShortestSteps((car$x+1), car$y, packages[,3], packages[,4], packages)) + roads$hroads[((car$x)+1)]
      print("CLOSEST EAST")
      print(closeEast)
      vectorOfMoves[4]<-closeEast
      
    }
    print("Vector of moves after everything")
    print(vectorOfMoves)
  }
  car$nextMove=readline("Enter next move. Valid moves are 2,4,6,8,0 (directions as on keypad) or q for quit.")
  if (car$nextMove=="q") {stop("Game terminated on user request.")}
  return (car)
}

#' testDM
#'
#' Use this to debug under multiple circumstances and to see how your function compares with the par function
#' The mean for the par function (with n=500) on this is 172.734, and the sd is approximately 39.065.
#'
#' Your final result will be based on how your function performs on a similar run of 500 games, though with
#' a different seed used to select them.
#'
#' This set of seeds is chosen so as to include a tricky game that has pick ups and deliveries on the same
#' spot. This will occur in the actual games you are evaluated on too.
#'
#' While this is dependent on the machine used, we expect your function to be able to run the 500 evaluation games on
#' the evaluation machine in under 4 minutes (250 seconds). If the evaluation machine is slower than expected,
#' this will be altered so that the required time is 25% slower than the par function.
#'
#' The par function takes approximately 96 seconds on my laptop (with n=500 and verbose=0).
#'
#' @param myFunction The function you have created to control the Delivery Man game.
#' @param verbose Set to 0 for no output, 1 for a summary of the results of the games played (mean,
#' standard deviation and time taken), and 2 for the above plus written output detailing seeds used and the
#' runDeliveryMan output of the result of each game.
#' @param returnVec Set to TRUE if you want the results of the games played returned as a vector.
#' @param n The number of games played. You will be evaluated on a set of 500 games, which is also the default here.
#' @param timeLimit The time limit. If this is breached, a NA is returned.
#' @return If returnVec is false, a scalar giving the mean of the results of the games played. If returnVec is TRUE
#' a vector giving the result of each game played. If the time limit is breached, a NA is returned.
#' @export
testDM=function(myFunction,verbose=0,returnVec=FALSE,n=500,seed=21,timeLimit=250){
  if (!is.na(seed))
    set.seed(seed)
  seeds=sample(1:25000,n)
  startTime=Sys.time()
  aStar=sapply(seeds,function(s){
    midTime=Sys.time()
    if (as.numeric(midTime)-as.numeric(startTime)>timeLimit) {
      cat("\nRun terminated due to slowness.")
      return (NA)
    }
    set.seed(s)
    if (verbose==2)
      cat("\nNew game, seed",s)
    runDeliveryMan(myFunction,doPlot=F,pause=0,verbose=verbose==2)
  })
  endTime=Sys.time()
  if (verbose>=1){
    cat("\nMean:",mean(aStar))
    cat("\nStd Dev:",sd(aStar))
    cat("\nTime taken:",as.numeric(endTime)-as.numeric(startTime),"seconds.")
  }
  if (returnVec)
    return(aStar)
  else
    return (mean(aStar))
}

#' Run Delivery Man
#'
#' Runs the delivery man game. In this game, deliveries are randomly placed on a city grid. You
#' must pick up and deliver the deliveries as fast as possible under changing traffic conditions.
#' Your score is the time it takes for you to complete this task. To play manually pass manualDM
#' as the carReady function and enter the number pad direction numbers to make moves.
#' @param carReady Your function that takes three arguments: (1) a list of two matrices giving the
#' traffice conditions. The first matrix is named 'hroads' and gives a matrix of traffice conditions
#' on the horizontal roads. The second matrix is named 'vroads' and gives a matrix of traffic
#' conditional on the vertical roads. <1,1> is the bottom left, and <dim,dim> is the top right.
#'(2) a list providing information about your car. This
#' list includes the x and y coordinates of the car with names 'x' and 'y', the package the car
#' is carrying, with name 'load' (this is 0 if no package is being carried), a list called
#' 'mem' that you can use to store information you want to remember from turn to turn, and
#' a field called nextMove where you will write what you want the car to do. Moves are
#' specified as on the number-pad (2 down, 4 left, 6 right, 8 up, 5 stay still). (3) A
#' matrix containing information about the packages. This contains five columns and a row for each
#' package. The first two columns give x and y coordinates about where the package should be picked
#' up from. The next two columns give x and y coordinates about where the package should be
#' delivered to. The final column specifies the package status (0 is not picked up, 1 is picked up but not
#' delivered, 2 is delivered).
#' Your function should return the car object with the nextMove specified.
#' @param dim The dimension of the board. You will be scored on a board of dimension 10. Note that
#' this means you will have to remove duplicated nodes from your frontier to keep your AStar
#' computationally reasonable! There is a time limit for how long an average game can be run in, and
#' if your program takes too long, you will penalized or even fail.
#' @param turns The number of turns the game should go for if deliveries are not made. Ignore this
#' except for noting that the default is 2000 so if you have not made deliveries after 2000 turns
#' you fail.
#' @param doPlot Specifies if you want the game state to be plotted each turn.
#' @param pause The pause period between moves. Ignore this.
#' @param del The number of deliveries. You will be scored on a board with 5 deliveries.
#' @return A string describing the outcome of the game.
#' @export
runDeliveryMan <- function (carReady=manualDM,dim=10,turns=2000,
                            doPlot=T,pause=0.1,del=5,verbose=T) {
  roads=makeRoadMatrices(dim)
  car=list(x=1,y=1,wait=0,load=0,nextMove=NA,mem=list())
  packages=matrix(sample(1:dim,replace=T,5*del),ncol=5)
  packages[,5]=rep(0,del)
  for (i in 1:turns) {
    roads=updateRoads(roads$hroads,roads$vroads)
    if (doPlot) {
      makeDotGrid(dim,i)
      plotRoads(roads$hroads,roads$vroads)
      points(car$x,car$y,pch=16,col="blue",cex=3)
      plotPackages(packages)
    }
    if (car$wait==0) {
      if (car$load==0) {
        on=packageOn(car$x,car$y,packages)
        if (on!=0) {
          packages[on,5]=1
          car$load=on
        }
      } else if (packages[car$load,3]==car$x && packages[car$load,4]==car$y) {
        packages[car$load,5]=2
        car$load=0
        if (sum(packages[,5])==2*nrow(packages)) {
          if (verbose)
            cat("\nCongratulations! You suceeded in",i,"turns!")
          return (i)
        }
      }
      car=carReady(roads,car,packages)
      car=processNextMove(car,roads,dim)
    } else {
      car$wait=car$wait-1
    }
    if (pause>0) Sys.sleep(pause)
  }
  cat("\nYou failed to complete the task. Try again.")
  return (NA)
}
#' @keywords internal
packageOn<-function(x,y,packages){
  notpickedup=which(packages[,5]==0)
  onX=which(packages[,1]==x)
  onY=which(packages[,2]==y)
  available=intersect(notpickedup,intersect(onX,onY))
  if (length(available)!=0) {
    return (available[1])
  }
  return (0)
}

findClosestPackage<-function(xStart, yStart, xPack, yPack, packages){
  shortestSteps = abs(xStart-xPack) + abs(yStart-yPack)

  listOfSteps <- list()
  listOfSteps = append(listOfSteps, shortestSteps)
  listOfSteps = append(listOfSteps, xPack)
  listOfSteps = append(listOfSteps, yPack)
  list = list()
  list2 = list()
  list3 = list()
  for(i in 1:5){
    list  = append(list, listOfSteps[[i]])
    list2 = append(list2, listOfSteps[[5+i]])
    list3 = append(list3, listOfSteps[[10+i]])
  }
  vector = unlist(list, use.names=FALSE)
  vector2 = unlist(list2, use.names=FALSE)
  vector3 = unlist(list3, use.names=FALSE)

  finalList = list(c(vector), c(vector2), c(vector3))
  return (finalList)
}



memoryListFunc <- function(x, y, list, nextMove){
  elementVector <- c(x, y, nextMove)
  list = replace(list, 1, elementVector)
  #list = list[1] <-elementVector
  print("DETTA ÄR LISTAN")
  print(list)
  return(list)
}

saveValue <- function(list){
  savedList = list;
  return(savedList)  
}

#l <- list(c(100, 100, 100, 100))


ourDM<-function(roads, car, packages){
  nextMove=0
  #l <- list(c(100, 100, 100, 100))"
  print("LENGTH IF MEM")
  print(length(car$mem))
  if(length(car$mem)==0){
    memoryList = list(c(100, 100, 100, 100))
  }
  #memoryList = saveValue(l)
  #memoryListCopy = memoryList
  
  if (car$load==0) {
    listOfSteps = findClosestPackage(car$x, car$y, packages[,1], packages[,2], packages)
    packages <- cbind(packages, findShortestSteps(car$x, car$y, packages[,1], packages[,2], packages))
    packages <- packages[order(packages[,6], decreasing = FALSE),]
    print(packages)
    #c(north, south, west, east)
    vectorOfMoves <- c(1000, 1000, 1000, 1000)
    vectorOfStreets <- c(roads$vroads[((car$y)+1)], roads$vroads[((car$y)-1)], roads$hroads[((car$x)-1)], roads$hroads[((car$x)+1)])
    minVectorOfStreets <- min(vectorOfStreets)
    
    print(vectorOfMoves)
    if(roads$vroads[(car$y)+1]>0){
      closeNorth = min((findShortestSteps(car$x, (car$y)+1, packages[,1], packages[,2], packages)) + (roads$vroads[((car$y)+1)])/10)
      closeNorth = closeNorth*minVectorOfStreets
      print("CLOSEST NORTH")
      print(min(findShortestSteps(car$x, (car$y)+1, packages[,1], packages[,2], packages)))
      vectorOfMoves[1]<-closeNorth
    }
    if(car$y-1 > 0 && roads$vroads[(car$y-1)]>=0){
      closeSouth = min(findShortestSteps(car$x, (car$y-1), packages[,1], packages[,2], packages) + (roads$vroads[((car$y)-1)])/10)
      closeSouth = closeSouth*minVectorOfStreets
      print("CLOSEST SOUTH")
      print(closeSouth)
      vectorOfMoves[2]<-closeSouth
      
    }
    if((car$x > 1) && roads$hroads[(car$x-1)]>=0){
      closeWest = min(findShortestSteps((car$x-1), car$y, packages[,1], packages[,2], packages) + (roads$hroads[((car$x)-1)])/10)
      closeWest = closeWest*minVectorOfStreets
      print("CLOSEST WEST")
      print(closeWest)
      vectorOfMoves[3]<-closeWest
    }
    if(roads$hroads[(car$y)+1]>0){
      closeEast = min(findShortestSteps((car$x+1), car$y, packages[,1], packages[,2], packages)+ (roads$hroads[((car$x)+1)])/10)
      closeEast = closeEast*minVectorOfStreets
      print("CLOSEST EAST")
      print(closeEast)
      vectorOfMoves[4]<-closeEast
      
    }
    print("Vector of moves after everything")
    print(vectorOfMoves)
    #toGo=which(packages[,5]==0)[1]
    
  } else {
    print(packages)
    #c(north, south, west, east)
    vectorOfMoves <- c(1000, 1000, 1000, 1000)
    vectorOfStreets <- c(roads$vroads[((car$y)+1)], roads$vroads[((car$y)-1)], roads$hroads[((car$x)-1)], roads$hroads[((car$x)+1)])
    minVectorOfStreets <- min(vectorOfStreets)
    
    print(vectorOfMoves)
    if(roads$vroads[(car$y)+1]>0){
      closeNorth = min(findShortestSteps(car$x, (car$y)+1, packages[,3], packages[,4], packages) + (roads$vroads[((car$y)+1)])/10)
      closeNorth = closeNorth*minVectorOfStreets
      print("CLOSEST NORTH")
      print(min(findShortestSteps(car$x, (car$y)+1, packages[,3], packages[,4], packages)))
      vectorOfMoves[1]<-closeNorth
    }
    if(car$y-1 > 0 && roads$vroads[(car$y-1)]>=0){
      closeSouth = min(findShortestSteps(car$x, (car$y-1), packages[,3], packages[,4], packages) + (roads$vroads[((car$y)-1)])/10)
      closeSouth = closeSouth*minVectorOfStreets
      print("CLOSEST SOUTH")
      print(closeSouth)
      vectorOfMoves[2]<-closeSouth
      
    }
    if((car$x > 1) && roads$hroads[(car$x-1)]>=0){
      closeWest = min(findShortestSteps((car$x-1), car$y, packages[,3], packages[,4], packages) + (roads$hroads[((car$x)-1)])/10)
      closeWest = closeWest*minVectorOfStreets
      print("CLOSEST WEST")
      print(closeWest)
      vectorOfMoves[3]<-closeWest
    }
    if(roads$hroads[(car$y)+1]>0){
      closeEast = min(findShortestSteps((car$x+1), car$y, packages[,3], packages[,4], packages) + (roads$hroads[((car$x)+1)])/10)
      closeEast = closeEast*minVectorOfStreets
      print("CLOSEST EAST")
      print(closeEast)
      vectorOfMoves[4]<-closeEast
      
    }
    print("VECTOR OF MOVES IN ELSE STATEMENT")
    print(vectorOfMoves)
  }
  checkDirection = which.min(vectorOfMoves)[1]
  # 1 north
  # 2 syd
  # 3 east
  # 4 west
  print("LEGNBT OF PACKAGE")
  print(length(packages[1,]))
  if(length(packages[1,]) == 7){
    print("YOEGAY")
    print(packages[,7])
    if(packages[,7]==1){
      vectorOfMoves[2]<-1000
    }else if(packages[,7]==2){
      vectorOfMoves[1]<-1000
    }else if(packages[,7]==3){
      vectorOfMoves[4]<-1000
    }else if(packages[,7]==4){
      vectorOfMoves[3]<-1000
    }
  }
  checkDirection = which.min(vectorOfMoves)[1]

  if (checkDirection == 4) {nextMove=6}
  else if (checkDirection == 3) {nextMove=4}
  else if (checkDirection == 1) {nextMove=8}
  else if (checkDirection == 2) {nextMove=2}
  else {nextMove=5}    
  
  memoryList = memoryListFunc(car$x, car$y, memoryList, checkDirection)
  packages <- cbind(packages, checkDirection)
  print("LEGNBT OF PACKAGE TWO")
  print(length(packages[1,]))
  print(packages)
  print("MEMORY LIST")
  print(memoryList)
  car$nextMove=nextMove
  car$mem=list()
  return(car)
  
  # if (car$x<packages[toGo,1+offset]) {nextMove=6}
  # else if (car$x>packages[toGo,1+offset]) {nextMove=4}
  # else if (car$y<packages[toGo,2+offset]) {nextMove=8}
  # else if (car$y>packages[toGo,2+offset]) {nextMove=2}
  # else {nextMove=5}
  # car$nextMove=nextMove
  # car$mem=list()
  # return (car)
}  

findCloseRoadsCost <- function(carX, carY, roads){
  vectorOfMoves <- c(1000, 1000, 1000, 1000)
  if(carY+1<11 && roads$vroads[(carY)+1]>0){
    closeNorth <- roads$vroads[(carY)+1]
    vectorOfMoves[1] <- closeNorth
  }
  if(carY-1>0 && roads$vroads[(carY)-1]>=0){
    closeSouth <- roads$vroads[(carY)-1]
    vectorOfMoves[2] <- closeSouth
  }
  if(carX+1<11 && roads$hroads[(carX+1)]>0){
    closeWest <- roads$hroads[(carX+1)]
    vectorOfMoves[3] <- closeWest
  }
  if(carX-1>0 && roads$hroads[(carX-1)]>=0){
    closeEast <- roads$hroads[(carX-1)]
    vectorOfMoves <- closeEast
  }
  return(min(vectorOfMoves))
}
#Contains only the starting node at start. 
getOpenList<-function(carX, carY, openList){
  openList = append(openList, c(carX, carY))
  return(openList)
}

#Starts empty, adds the current x any y value of your position, adds the parent nodes where you came from.
getClosedList<-function(carX, carY, closedList, parentX, parentY){
  closedList = append(closedList, c(carX, carY, parentX, parentY))
  return(closedList)
}
findShortestSteps = function (xStart, yStart, xPack, yPack) {
  shortestSteps = abs(xStart - xPack) + abs(yStart - yPack)
  return(shortestSteps)
}


checkForMatch = function (openList, x, y) {
  xs = sapply(openList, function(elem) elem$x)
  ys = sapply(openList, function(elem) elem$y)
  
  any(xs == x & ys == y)
}

reconstructPath <- function(cameFrom, current){
  totalPath <- list()
  print("CURRENTO")
  print(cameFrom)
  totalPath = append(totalPath, current$prevMove)
  for (i in 1:length(cameFrom)) {
    current <- cameFrom[[i]]
    print("current!!!")
    print(current)
    totalPath = append(totalPath, current$prevMove)    
    
  }
  return (totalPath)
}
getNeighbors <- function(currentX, currentY, packages, roads, car){
  listOfNeighbors = list()
  if(currentY<10){
    if(car$load == 0){
      hScore= findShortestSteps(currentX, currentY+1, packages[,1], packages[,2])[1]
    }else if(car$load == 1){
      hScore= findShortestSteps(currentX, currentY+1, packages[,3], packages[,4])[1]
    }
    if(currentY == 10){
      currentY = 9
    }
    fScore = hScore + roads$vroads[currentX, currentY]
    listOfNeighbors = append(listOfNeighbors, list("NorthNeighbor" = list("x" = currentX, "y" = currentY+1, "gScore" = roads$vroads[currentX, currentY], "prevMove" = 8, "fScore" = fScore, "hScore" = hScore, "direction"= "NorthNeighbor")))  
  }
  if(currentY>1){
    if(car$load == 0){
      hScore = findShortestSteps(currentX, currentY-1, packages[,1], packages[,2])[1]
    }else if(car$load == 1){
      hScore = findShortestSteps(currentX, currentY-1, packages[,3], packages[,4])[1]
    }
    if(currentX == 10){
      currentX = 9
    }
    fScore = hScore + roads$vroads[currentX, currentY-1]
    listOfNeighbors = append(listOfNeighbors, list("SouthNeighbor" = list("x" = currentX, "y" = currentY-1, "gScore" = roads$vroads[currentX, currentY-1], "prevMove" = 2, "fScore" = fScore, "hScore" = hScore, "direction" = "SouthNeighbor")))  
  }
  if(currentX<10){
    if(car$load == 0){
      hScore = findShortestSteps(car$x+1, currentY, packages[,1], packages[,2])[1]
    }else if(car$load == 1){
      hScore= findShortestSteps(car$x+1, currentY, packages[,3], packages[,4])[1]
    }
    if(currentY == 10){
      currentY = 9
    }
    fScore = hScore + roads$hroads[currentX, currentY]
    listOfNeighbors = append(listOfNeighbors, list("EastNeighbor" = list("x" = currentX+1, "y" = currentY, "gScore" = roads$hroads[currentX, currentY], "prevMove" = 6, "fScore" = fScore, "hScore" = hScore, "direction" = "EastNeighbor")))  
  }
  if(currentX>1){
    if(car$load == 0){
      hScore= findShortestSteps(currentX-1, currentY, packages[,1], packages[,2])[1]
    }else if(car$load == 1){
      hScore= findShortestSteps(currentX-1, currentY, packages[,3], packages[,4])[1]
    }
    if(currentY == 10){
      currentY = 9
    }
    fScore = hScore + roads$hroads[currentX-1, currentY]
    listOfNeighbors = append(listOfNeighbors, list("WestNeighbor" = list("x" = currentX-1, "y" = currentY, "gScore" = roads$hroads[currentX-1, currentY], "prevMove" = 4, "fScore" = fScore, "hScore" = hScore , "direction" = "WestNeighbor"))) 
   }
  return(listOfNeighbors)
}




aStar = function (roads, car, packages) {
  packages <- cbind(packages, findShortestSteps(car$x, car$y, packages[,1], packages[,2]))
  packages <- packages[order(packages[,6], decreasing = FALSE),]

  #if (car$load == 0) {
  if(packages[,5][1]==0) {
    gScore = 0
    hScore = findShortestSteps(car$x, car$y, packages[,1], packages[,2])[1]
    start <- list("x" = car$x, "y" = car$y, "gScore" = 0, "prevMove" = 5, "fScore" = gScore + hScore, "hScore" = hScore)
    }
  else {
    gScore = 0
    hScore = findShortestSteps(car$x, car$y, packages[,3], packages[,4])[1]
    start <- list("x" = car$x, "y" = car$y, "gScore" = 0, "prevMove" = 5, "fScore" = gScore + hScore, "hScore" = hScore)
  }
  print("CARLOAD")
  print(car$load)
  print(packages)
  if (car$load == 0) {
    end <- list("x" = packages[,1][1], "y" = packages[,2][1])
  } else if (car$load == 1) {
    end <- list("x" = packages[,3][1], "y" = packages[,4][1])
    print("FIS")
    print(end)
  }
  closedList <- list()
  openList <- list() 
  openList[[length(openList)+1]] <- (start)
  cameFrom <- list()
  
  while (length(openList) > 0) {
    openList = openList[order(sapply(openList, `[[`, 5))]
    current = openList[1]
    if (checkForMatch(closedList, current$x, current$y)) {
      next
    }
    print("GOTHERE")
    print("END MY BOI")
    print(end$x)
    if ((current[[1]]$x == end$x) & (current[[1]]$y == end$y)) {
      print("Done")
      print(current)
      print("PRINTING LIST")
      cameFrom <- append(cameFrom, list(current[[1]]$prevMove))
      print(cameFrom)
      return(cameFrom)
    }
    openList = openList[-1]
    closedList <- append(closedList, current)
    closedList[length(closedList)+1] = current
    currentNeighbors = getNeighbors(current[[1]]$x, current[[1]]$y, packages, roads, car)
    for (i in 1:length(currentNeighbors)) {

      neighbor = currentNeighbors[[i]]
      tempG <- current[[1]]$gScore + neighbor$gScore + neighbor$fScore

      if(!(checkForMatch(closedList, neighbor$x, neighbor$y))){
        if (!(checkForMatch(openList, neighbor$x, neighbor$y))) {
            openList = append(openList, list(neighbor))
        }
        if (tempG >= neighbor$gScore) {
            closedList = append(closedList, current)
           # openList[[i]] <- NULL
            next
        }
      }
        print("FART")
        cameFrom <- append(cameFrom, list(current[[1]]$prevMove))
        neighbor$gScore <- tempG
        neighbor$fScore <- neighbor$gScore + neighbor$fScore
      
    } 
  }
}
myFunction <- function(roads, car, packages){
  nextMove = 0
  bestMoveList = aStar(roads, car, packages)
  print("BEST MOVE LIST")
  print(bestMoveList)
  nextMove = bestMoveList[[1]]
  car$nextMove=nextMove
  car$mem=list()

  return(car)
}
# findShortestSteps <- function(xStart, yStart, xPack, yPack, packages, roads){
#   shortestSteps = abs(xStart-xPack) + abs(yStart-yPack)
#   #roadCost = findCloseRoadsCost(xStart, yStart, roads)
#   #return(roadCost * shortestSteps)
#   return(shortestSteps)
# }
# removeFromOpenList <- function(openList, currentX, currentY){
#   xs = sapply(openList, function(elem) elem$x)
#   ys = sapply(openList, function(elem) elem$y)
#   if(any(xs == x & ys == y)){
#     
#   }
#   for (n in 1:length(openList)) {
#     if(currentX == (lapply(openList, '[[', 1))[n] && currentY == (lapply(openList, '[[', 2))[n]){
#       openList[[n]] <- NULL
#     }
#   }
# }
# getNeighbors <- function(car, packages, roads){
#   listOfNeighbors = list()
#   if(car$y+1<11){
#     if(car$load == 0){
#        fScore= findShortestSteps(car$x, car$y+1, packages[,1], packages[,2], packages, roads)[1]
#     }else if(car$load == 1){
#       fScore= findShortestSteps(car$x, car$y+1, packages[,3], packages[,4], packages, roads)[1]
#     }
#     listOfNeighbors = append(listOfNeighbors, list("NorthNeighbor" = list("x" = car$x, "y" = car$y+1, "gScore" = roads$vroads[((car$y)+1)], "prevMove" = 8, "fScore" = fScore)))  
#     }
#   if(car$y-1>0){
#     if(car$load == 0){
#       fScore= findShortestSteps(car$x, car$y-1, packages[,1], packages[,2], packages, roads)[1]
#     }else if(car$load == 1){
#       fScore= findShortestSteps(car$x, car$y-1, packages[,3], packages[,4], packages, roads)[1]
#     }
#     listOfNeighbors = append(listOfNeighbors, list("SouthNeighbor" = list("x" = car$x, "y" = car$y-1, "gScore" = roads$vroads[((car$y)-1)], "prevMove" = 2, "fScore" = fScore)))  
#   }
#   if(car$x+1<11){
#     if(car$load == 0){
#       fScore= findShortestSteps(car$x+1, car$y, packages[,1], packages[,2], packages, roads)[1]
#     }else if(car$load == 1){
#       fScore= findShortestSteps(car$x+1, car$y, packages[,3], packages[,4], packages, roads)[1]
#     }
#     listOfNeighbors = append(listOfNeighbors, list("EastNeighbor" = list("x" = car$x+1, "y" = car$y, "gScore" = roads$hroads[((car$x)-1)], "prevMove" = 6, "fScore" = fScore)))  
#     
#     
#     }
#   if(car$x-1>0){
#     if(car$load == 0){
#       fScore= findShortestSteps(car$x-1, car$y, packages[,1], packages[,2], packages, roads)[1]
#     }else if(car$load == 1){
#       fScore= findShortestSteps(car$x-1, car$y, packages[,3], packages[,4], packages, roads)[1]
#     }
#     listOfNeighbors = append(listOfNeighbors, list("WestNeighbor" = list("x" = car$x-1, "y" = car$y, "gScore" = roads$hroads[((car$x)+1)], "prevMove" = 4, "fScore" = fScore))) 
#     
#     }
#   return(listOfNeighbors)
# }
# getPath <- function(cameFrom, current){
#   path <- list()
#   while (h) {
#     
#   }
#   print("DONE")
#   return(path)
# }
# # if(length(closedList)>0 && checkForMatch(closedList, current$x, current$y)){
# checkForMatch <- function(openList, x, y){
#   print("PRINTING X AND Y IN CHECKFORMATCH")
#   print(x)
#   print(y)
#   print("PRINTING THE OPENLIST")
#   print(openList)
#   if(length(openList)== 0){
#     return(FALSE)
#   }
#   xs = sapply(openList, function(elem) elem$x)
#   ys = sapply(openList, function(elem) elem$y)
#   (any(xs == x & ys == y))
# }
# aStar<-function(roads, car, packages){
#   #bind the heurisic cost from current node to end node to [,6]
#   packages <- cbind(packages, findShortestSteps(car$x, car$y, packages[,1], packages[,2], packages, roads))
#   packages <- packages[order(packages[,6], decreasing = FALSE),]
#   #makes the current position the start position, car$x, car$y, g-score, list containing move to get there
#   start <- list("x" = car$x, "y" = car$y, "gScore" = 0, "prevMove" = 5, "fScore" = findShortestSteps(car$x, car$y, packages[,1], packages[,2], packages, roads)[1])
#   #makes the node with lowert manhattan distance x and y delivery the 
#   if(car$load == 0){
#     end <- list("x" = packages[,1][1], "y" = packages[,2][1])
#   }else if (car$load == 1){
#     end <- list("x" = packages[,3][1], "y" = packages[,4][1])
#   }
#   #Stores all the nodes that has finished being evaluated, everything done that you never again have to visit
#   closedList <- list()
#   # closedList[[1]] <- list("x" = 1000, "y" = 1002)
#   # closedList[[2]] <- list("x" = 2000, "y" = 2002)
#   # closedList2 <- list()
#   # closedList2 <- append(closedList2, list("x" = 1000, "y" = 1002))
#   # closedList2 <- append(closedList2, list("x" = 2000, "y" = 2002))
#   # print("CLOSED LIST MYWAY")
#   # print(closedList)
#   # print("CLOSED LIST APPEND")
#   # print(closedList2)
#   #Stores nodes that still need to be checked. Algoritm is finished when Open is empty
#   openList <- list()
#   print("START")
#   print(start)
#   #openList = append(openList, list(start))
#   
#   openList[[length(openList)+1]] <- start
#   print("OPENLIST")
#   print(openList)
#   #ManhattanDistance
#   if(car$load == 0){
#     fCost <- findShortestSteps(car$x, car$y, packages[,1], packages[,2], packages, roads)[1]
#   }else if (car$load == 1){
#     fCost <- findShortestSteps(car$x, car$y, packages[,3], packages[,4], packages, roads)[1]
#   }
#   #Stores all the nodes that has finished 
#   cameFrom <- list()
#   #Cost of roads to that node. 
#   gCost <- list(0)
#   while(length(openList)>0){
#     print("OOOOOOPEN LIST")
#     print(openList)
#     #openList <- as.data.frame(lapply(openList, unlist))
#     print("AS UNLIST")
#     print(openList)
#     #openList = openList[order(sapply(openList, `[[`, openList$fScore))]
#     #openList[order(openList$fScore), ] 
#     
#     openList[order(sapply(openList, function(x) x[5], simplify=TRUE), decreasing=TRUE)]
#     #openList = openList[order(sapply(openList, `[[`, 5))]
#     
#     
#    # openList[order(sapply(openList, function(x) x$fScore, simplify = TRUE), decreasing = FALSE)]
#     print("BAJSBAJS")
#     print(openList)
#     #The node in the openlist with lowest fscore
#     current = openList[[1]]
#     print("CURRENT IS THIS:")
#     print(current)
#     print("THIS IS closedList")
#     print(closedList)
#     #tjo = list("x"= 5, "y" = 5)
#    # closedList = append(closedList, list(tjo))
#     if(length(closedList)>0 && checkForMatch(closedList, current$x, current$y)){
#       next
#     }
#     #print("Open list")
#     #print(openList[[1]])
#     print("THIS IS END")
#     print(end)
#     print("THIS IS OPENLIST")
#     print(openList)
#     print("THIS IS CURRENT")
#     print(current$x)
#     print(current$y)
#     if((current$x == end$x) & (current$y == end$y)){
#       print("DONE HO")
# 
#     }
#     #maybe dont remove this
#     #removeFromOpenList(openList,current$x,current$y)
#     closedList = append(closedList,  list(current))
#     #closedList[[length(closedList)+1]] <- list(current)
#     currentNeighbors = getNeighbors(car, packages, roads)
#   
#     for (i in 1:length(currentNeighbors)) {
#       neighbor = currentNeighbors[i]
#       #unsure about this one
#       tempG <-current$gScore + neighbor$gScore + neighbor$fScore
#       if(!(checkForMatch(openList,neighbor$x,neighbor$y))){
#         openList = append(openList, list(neighbor))
#         #openList[[length(openList)+1]] <- list(neighbor)
#         
#       }else if(tempG >= neighbor$gScore) {
#         closedList = append(closedList, list(current))
#         #closedList[[length(closedList)+1]] <- list(current)
#         openList[[i]] <- NULL
#         next
#       }
#       cameFrom = append(cameFrom, list(current))
#       #cameFrom[[length(cameFrom)+1]] <- list(current)
#       
#       neighbor$gScore <- tempG
#       if(car$load == 0){
#         fCostNeighbor <- findShortestSteps(current$x, current$y, packages[,1], packages[,2], packages, roads)[1]
#       }else if (car$load == 1){
#         fCostNeighbor <- findShortestSteps(current$x, current$y, packages[,3], packages[,4], packages, roads)[1]
#       }
#       neighbor$fScore <- neighbor$gScore + fCostNeighbor
#       }
#     }
# }
# starDM<-function(roads, car, packages){
#   openList <- list()
#   closedList <- list()
#   packages <- cbind(packages, findShortestSteps(car$x, car$y, packages[,1], packages[,2], packages, roads))
#   packages <- packages[order(packages[,6], decreasing = FALSE),]
#   smallestRoad = findCloseRoadsCost(car$x, car$y, roads)
#   print(packages)
#   print(smallestRoad)
#   openList <- getOpenList(car$x, car$y, openList)
#   print(length(openList))
#   if(length(closedList) != 0){
#     closedList <- getClosedList(car$x, car$y, closedList, lapply(openList, `[[`, 1), lapply(openList, `[[`, 2))
#   }else{
#     closedList <- getClosedList(car$x, car$y, closedList, "NA", "NA")
#   }
#   print("OPEN LIST")
#   print(openList)
#   print("CLOSED LIST")
#   print(closedList)
#   print("DELIVERY X")
#   print(packages[,3][1])
#   print("DELIVERY S")
#   print(packages[,4][1])
#   print("lapply(openList, `[[`, 1)")
#   #print(lapply(openList, `[[`, 1))
#   print("lapply(openList, `[[`, 2)")
#   #print(lapply(openList, `[[`, 2))
#   while(lapply(openList, `[[`, 1) == packages[,3][1] && lapply(openList, `[[`, 2) == packages[,4][1]){
#     
#   }
#   car$nextMove=readline("Enter next move. Valid moves are 2,4,6,8,0 (directions as on keypad) or q for quit.")
#   if (car$nextMove=="q") {stop("Game terminated on user request.")}
#   return(car)
# }
# if (car$x<packages[toGo,1+offset]) {nextMove=6}
# else if (car$x>packages[toGo,1+offset]) {nextMove=4}
# else if (car$y<packages[toGo,2+offset]) {nextMove=8}
# else if (car$y>packages[toGo,2+offset]) {nextMove=2}
# else {nextMove=5}
# car$nextMove=nextMove
# car$mem=list()

#' @keywords internal
processNextMove<-function(car,roads,dim) {
  nextMove=car$nextMove
  if (nextMove==8) {
    if (car$y!=dim) {
      car$wait=roads$vroads[car$x,car$y]
      car$y=car$y+1
    } else {
      warning(paste("Cannot move up from y-position",car$y))
    }
  } else if (nextMove==2) {
    if (car$y!=1) {
      car$y=car$y-1
      car$wait=roads$vroads[car$x,car$y]
    } else {
      warning(paste("Cannot move down from y-position",car$y))
    }
  }  else if (nextMove==4) {
    if (car$x!=1) {
      car$x=car$x-1
      car$wait=roads$hroads[car$x,car$y]
    } else {
      warning(paste("Cannot move left from x-position",car$x))
    }
  }  else if (nextMove==6) {
    if (car$x!=dim) {
      car$wait=roads$hroads[car$x,car$y]
      car$x=car$x+1
    } else {
      warning(paste("Cannot move right from x-position",car$x))
    }
  } else if (nextMove!=5) {
    warning("Invalid move. No move made. Use 5 for deliberate no move.")
  }
  car$nextMove=NA
  return (car)
}

#' @keywords internal
plotPackages=function(packages) {
  notpickedup=which(packages[,5]==0)
  notdelivered=which(packages[,5]!=2)
  points(packages[notpickedup,1],packages[notpickedup,2],col="green",pch=18,cex=3)
  points(packages[notdelivered,3],packages[notdelivered,4],col="red",pch=18,cex=3)
}

#' @keywords internal
makeDotGrid<-function(n,i) {
  plot(rep(seq(1,n),each=n),rep(seq(1,n),n),xlab="X",ylab="Y",main=paste("Delivery Man. Turn ", i,".",sep=""))
}

#' @keywords internal
makeRoadMatrices<-function(n){
  hroads=matrix(rep(1,n*(n-1)),nrow=n-1)
  vroads=matrix(rep(1,(n-1)*n),nrow=n)
  list(hroads=hroads,vroads=vroads)
}

#' @keywords internal
plotRoads<- function (hroads,vroads) {
  for (row in 1:nrow(hroads)) {
    for (col in 1:ncol(hroads)) {
      lines(c(row,row+1),c(col,col),col=hroads[row,col])
    }
  }
  for (row in 1:nrow(vroads)) {
    for (col in 1:ncol(vroads)) {
      lines(c(row,row),c(col,col+1),col=vroads[row,col])
    }
  }
}
#' @keywords internal
updateRoads<-function(hroads,vroads) {
  r1=runif(length(hroads))
  r2=runif(length(hroads))
  for (i in 1:length(hroads)) {
    h=hroads[i]
    if (h==1) {
      if (r1[i]<.05) {
        hroads[i]=2
      }
    }
    else {
      if (r1[i]<.05) {
        hroads[i]=h-1
      } else if (r1[i]<.1) {
        hroads[i]=h+1
      }
    }
    v=vroads[i]
    if (v==1) {
      if (r2[i]<.05) {
        vroads[i]=2
      }
    }
    else {
      if (r2[i]<.05) {
        vroads[i]=v-1
      } else if (r2[i]<.1) {
        vroads[i]=v+1
      }
    }
  }
  list (hroads=hroads,vroads=vroads)
}
