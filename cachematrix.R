## The function makeCacheMatrix takes in input a matrix and return a list of functions. There are 4 of them:
# - set: creates a Matrix with the input parameter
# - get: returns the the Matrix created 
# - setInverse: sets the (matrix)^(-1()
# - getInverse: gets the (matrix)^(-1)
# The set functions use the <<- operator to assign the value in the parent environment

makeCacheMatrix <- function(x = matrix()) {
      
      inverse <- NULL
      set <- function(matrix) {
            x <<- matrix
            inverse <<- NULL
      }
      get <- function(){
            x    
      } 
      setInverse <- function(inverse){
            inverse <<- inverse    
      } 
      getInverse <- function(){
            inverse      
      } 
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)

}


## The function cacheSolve takes in input list of function like the one created with makeCacheMatrix and #return the inverse of the matrix obtained through the get function. If the inverse has been previously #calculated, the funtion does not calculate it again but it takes the cached value through the getInverse #function

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'      
      inverse <- x$getInverse()
      if(!is.null(inverse)) { #Inverse has been already calculated
            message("getting cached data")
            return(inverse)
      }
      data <- x$get()
      inverse <- solve(data, ...)
      x$setInverse(inverse)
      inverse
}
