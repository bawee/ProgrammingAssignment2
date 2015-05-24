#Thanks for marking this assignment

#The two functions below together calculates an inverse of a matrix.

#


# makeCacheMatrix produces a list of four functions:
## i.   set -
## ii.  get - 
## iii. setInverse -
## iv.  getInverse -

makeCacheMatrix <- function(x) {
  
  inverse <<- NULL
  
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }

  get <- function() x
  
  setInverse <- function(i) inverse <<- i
  
  getInverse <- function() inverse
  
  returnedList <- list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

  returnedList
}

# This function checks if inverse is stored in "inverse". If not it calculates the inverse matrix using solve()

cacheSolve <- function(leest) {
  
  inverse <- leest$getInverse()
  
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  matrix <- leest$get()

  inverse <- solve(matrix)
  
  leest$setInverse(inverse)
  
  inverse


}
