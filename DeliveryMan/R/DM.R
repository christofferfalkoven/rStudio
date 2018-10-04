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
  start <- list("x" = car$x, "y" = car$y, "gScore" = 0, "prevMove" = 5, "fScore" =0, "hScore" = 0)
  toGo=0
  offset=0
  if(car$load == 0){
    toGo=which(packages[,5]==0)[1]
    end <- list("x" = packages[toGo,1], "y" = packages[toGo,2])
  }

  
  bestMoveList = aStar(roads, car, packages, start, end)
 hey =  getNeighbors(car$x, car$y, packages, roads, car)
    print(hey)
    print(packages)
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

getRightHvalues <- function(load, carX, carY, packages){
  hScore = 0
  if(load == 0){
    toGo=which(packages[,5]==0)[1]
    hScore= findShortestSteps(carX, carY, packages[toGo,1], packages[toGo,2])
  }else if(load == 1){
    if(packages[load, 5] == 0){
      hScore= findShortestSteps(carX, carY, packages[load,1], packages[load,2])
    }else if(packages[load, 5] == 1){
      hScore= findShortestSteps(carX, carY, packages[load,3], packages[load,4])
    }
  }
  return(hScore)
}

getNeighbors <- function(currentX, currentY, packages, roads, car, path, end){
  listOfNeighbors = list()
  load = car$load
  # temp <- c()
  # if(currentY<10){
  #   temp[length(temp+1)] <- roads$vroads[currentX, currentY]
  # }
  # if(currentY>1){
  #   temp[length(temp+1)]<-roads$vroads[currentX, currentY-1]
  # }
  # if(currentX<10){
  #   temp[length(temp+1)]<-roads$hroads[currentX, currentY]
  # }
  # if(currentX>1){
  #   temp <-roads$hroads[currentX-1, currentY]
  # }
  # if(length(temp)>= 1){
  #   smallestGscore = min(temp)
  # }else{
   smallestGscore = 1
  
  if(currentY<10){
   # hScore = getRightHvalues(load, currentX, currentY+1, packages)
    hScore = smallestGscore*(findShortestSteps(currentX, currentY+1, end$x, end$y))
    fScore = hScore + roads$vroads[currentX, currentY]
    #ev bugg i reads 10x9
    listOfNeighbors = append(listOfNeighbors, list("NorthNeighbor" = list("x" = currentX, "y" = currentY+1, "gScore" = roads$vroads[currentX, currentY], "prevMove" = 8, "fScore" = fScore, "hScore" = hScore, "direction"= "NorthNeighbor", "path"= append(path, 8))))  
  }
  if(currentY>1){
    #hScore = getRightHvalues(load, currentX, currentY-1, packages)
    hScore = smallestGscore*(findShortestSteps(currentX, currentY-1, end$x, end$y))
    fScore = hScore + roads$vroads[currentX, currentY-1]
    listOfNeighbors = append(listOfNeighbors, list("SouthNeighbor" = list("x" = currentX, "y" = currentY-1, "gScore" = roads$vroads[currentX, currentY-1], "prevMove" = 2, "fScore" = fScore, "hScore" = hScore, "direction" = "SouthNeighbor", "path"= append(path, 2))))  
  }
  if(currentX<10){
    #hScore = getRightHvalues(load, currentX+1, currentY, packages)
    hScore = smallestGscore*(findShortestSteps(currentX+1, currentY, end$x, end$y))
    fScore = hScore + roads$hroads[currentX, currentY]
    listOfNeighbors = append(listOfNeighbors, list("EastNeighbor" = list("x" = currentX+1, "y" = currentY, "gScore" = roads$hroads[currentX, currentY], "prevMove" = 6, "fScore" = fScore, "hScore" = hScore, "direction" = "EastNeighbor", "path"= append(path, 6))))  
  }
  if(currentX>1){
    #hScore = getRightHvalues(load, currentX-1, currentY, packages)
    hScore = smallestGscore*(findShortestSteps(currentX-1, currentY, end$x, end$y))
    fScore = hScore + roads$hroads[currentX-1, currentY]
    listOfNeighbors = append(listOfNeighbors, list("WestNeighbor" = list("x" = currentX-1, "y" = currentY, "gScore" = roads$hroads[currentX-1, currentY], "prevMove" = 4, "fScore" = fScore, "hScore" = hScore , "direction" = "WestNeighbor", "path"= append(path, 4)))) 
   }
  return(listOfNeighbors)
}

permutations = function(n){
  if(n==1){
    return(matrix(1))
  } else {
    recursive = permutations(n-1)
    nr = nrow(recursive)
    A = matrix(nrow=n*nr,ncol=n)
    for(i in 1:n){
      A[(i-1)*nr+1:nr,] = cbind(i,recursive+(recursive>=i))
    }
    return(A)
  }
}
routeCalc = function(vector, packages, car){
  total = abs(car$x - packages[vector[1],1]) + abs(car$y - packages[vector[1], 2])
  for(i in 1:length(vector)){
    if (i == 5){
      total = total + (abs(packages[vector[i],1]- packages[vector[i],3])+abs(packages[vector[i],2]- packages[vector[i],4]))
    }else{
      A = (abs(packages[vector[i],1]- packages[vector[i],3])+abs(packages[vector[i],2]- packages[vector[i],4]))
      B = (abs(packages[vector[i],3]- packages[vector[i+1],1]))+abs(packages[vector[i],4]- packages[vector[i+1],2])
      C = A + B
      total = total + C
    }
  }
  return(total)
}

routeCalculator= function(packages,car){
  allPermutations = permutations(nrow(packages))
  routeValues = apply(allPermutations, 1, function(x) routeCalc(x, packages, car))
  shortestRoute = which.min(routeValues)
  ourRoute = allPermutations[shortestRoute,]
  return(ourRoute)
}

aStar = function (roads, car, packages, start, end) {
  closedList <- list()
  openList <- list() 
  openList[[length(openList)+1]] <- (start)
  while (length(openList) > 0) {
    openList = openList[order(sapply(openList, `[[`, 5))]
    current = openList[1]
    if ((current[[1]]$x == end$x) & (current[[1]]$y == end$y)) {
      return(current[[1]]$path)
    }
    openList = openList[-1]
    closedList[length(closedList)+1] = current
    currentNeighbors = getNeighbors(current[[1]]$x, current[[1]]$y, packages, roads, car, current[[1]]$path, end)
    # print(end)
    # print("CURERNT NEIGHTBORS")
    # print(currentNeighbors)
    
    for (neighbor in currentNeighbors) {
      # temp = currentNeighbors[order(sapply(currentNeighbors, `[[`, 3))]
      # temp = temp[[1]]$gScore
      # neighbor$hScore <- neighbor$hScore*temp
      
      if(checkForMatch(closedList, neighbor$x, neighbor$y)){
        next
      }
      tempG <- current[[1]]$gScore + findShortestSteps(current[[1]]$x, current[[1]]$x, neighbor$x, neighbor$y)
      
      if (!(checkForMatch(openList, neighbor$x, neighbor$y))) {
          openList = append(openList, list(neighbor))
      }
      else if (tempG >= neighbor$gScore) {
          next
      }
      current[[1]]$path <- append(current[[1]]$path, neighbor$prevMove)
      neighbor$gScore <- tempG
      neighbor$fScore <- neighbor$gScore + neighbor$hScore
      }
  }
  stop("ERROR: No solution found", call. = TRUE, domain = NULL)
}

findClosestZero <- function(carX, carY, packages){
  thing <- c()
  for (i in 1:nrow(packages)) {
    if(packages[i, 5] == 0)
    thing[i] <- findShortestSteps(carX, carY, packages[i,1], packages[i,2])
  }
  return(which.min(thing))
}

myFunction <- function(roads, car, packages){
  toGo=0
  offset=0
  if(length(car$mem) == 0){
    car$mem = routeCalculator(packages, car)
  }
  offset=0
  if(car$load != 0) {
    toGo = car$load
    #offset=2
    end <- list("x" = packages[toGo, 3], "y" = packages[toGo,4])
  }else{
    availablePackages = which(packages[,5] == 0)
    car$mem = intersect(car$mem, availablePackages)
    #print(car$mem)
    toGo = car$mem[1]
    end <- list("x" = packages[toGo, 1], "y" = packages[toGo,2])
  }
   # print(packages)
   # print(end)
  #stop("TJO", call = TRUE, domain = NULL)
  # indexClose <- findClosestZero(car$x, car$y, packages)
  # if(car$load == 0){
  #   #toGo=findClosestZero(car$x, car$y, packages)
  #   end <- list("x" = packages[toGo,1], "y" = packages[toGo,2])
  # }else {
  #   if(packages[car$load, 5] == 0){
  #     end <- list("x" = packages[car$load,1], "y" = packages[car$load,2])
  #   }else if(packages[car$load, 5] == 1){
  #     end <- list("x" = packages[car$load,3], "y" = packages[car$load,4])
  #   }
  # }
  hScore = findShortestSteps(car$x, car$y, end$x, end$y) 
  start <- list("x" = car$x, "y" = car$y, "gScore" = 0, "prevMove" = 5, "fScore" = hScore + 0, "hScore" = hScore, "path" = list())

  bestMoveList = aStar(roads, car, packages, start, end)
  # print("ASTAR SUGGESTED MOVES")
  # print(bestMoveList)
  # print(roads)
  # print(packages)
  #stop("BAJS, call=TRUE, domain =NULL")
  if(length(bestMoveList) == 0 ){
    nextMove = 5
  }else{
  nextMove = bestMoveList[[1]]
  }
  car$nextMove=nextMove
  car$mem=list()
  return(car)
}

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
