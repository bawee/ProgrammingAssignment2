#Thanks for marking this assignment!

#The two functions below together calculates the inverse of a given matrix. 
#The inverse is only calculated if a new matrix is given. 
#If a new matrix is not given, the inverse matrix stored is retrieved from memory and not cacluated again.

#the first function, makeCacheMatrix, produces an "Object" containing the following four functions.

## i.   set - stores a new matrix, and deletes the previously stored inverse, if any
## ii.  get - reads in the matrix
## iii. setInverse - sets the variable "inverse", where i is the inverse variable that is fed to it
## iv.  getInverse - retrieves the inverse from the variable "inverse"

makeCacheMatrix <- function(x) {
  
  inverse <<- NULL #defines the variable "inverse", which stores the inverse matrix
  
  #i. set
  set <- function(y) { 
    x <<- y
    inverse <<- NULL
  }
  
  #ii. get
  get <- function() x
  
  #iii. setInverse
  setInverse <- function(i) inverse <<- i
  
  #iv. getInverse
  getInverse <- function() inverse
  
  
  #returns a list of functions
  returnedList <- list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}

# This 2nd function returns the inverse of a given matrix (not given directly but via makeCacheMatrix)

cacheSolve <- function(mCMObject) { #takes in the list/"object", here called mCMObject from makeCacheMatrix

#Part1: if inverse is already stored  
  
  inverse <- mCMObject$getInverse() #retrieve the inverse from mCMObject, if already stored

  if(!is.null(inverse)) { #makes sure the inverse variable is not empty, and returns the previously stored inverse, instead of caclulating again
    message("getting cached data")
    return(inverse)
  }

#Part 2: if Inverse is not already stored  
  
  matrix <- mCMObject$get() #gets the matrix that was given to makeCacheMatrix

  inverse <- solve(matrix, ...) # if inverse is empty, calculate the inverse using solve()
  
  mcMObject$setInverse(inverse) # set the newly calculated inverse
  
  inverse #return the newly calculated inverse

}
