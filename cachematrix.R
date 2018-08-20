## These functions cache the inverse of a matrix

## This function creates a special "matrix" object 
makeCacheMatrix <- function(x = matrix()){
  #initialize the inverse matrix
      i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    
    #calculate the inverse of the matrix by using the solve function
    get <- function() x
    setinverse <- function(solve) i <<- solve
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  }

## This function gets the cache of the matrix
cacheSolve <- function(x, ...) {
  
  i <- x$getinverse()
  
  #Check if inverse has been calculated, if yes, then return value
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  
  #If the inverse has not been calculated, then calculate it and return the value
  data <- x$get()
  i <- solve(data,...)
  x$setinverse(i)
  i
}