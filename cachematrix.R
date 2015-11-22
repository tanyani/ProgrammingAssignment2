## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## this function is to initialize two cached matrix as well as four functions
## initialize cached matrix with the inverse matrix as null and the matrix cached
## create four functions:
## get() to get the cached matrix
## set() to update the cached matrix
## getInv() to get the cached inverse matrix
## setInv() to update the cached inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInv <- function(inv) m <<- inv
  getInv <- function() m
  list(set=set, get=get, setInv=setInv, getInv=getInv)
}


## Write a short comment describing this function
## the function inputs the cached matrix as well as the input matrix
## if the cached inverse exists (not null) and the data is the same as before,
## return the cached inverse matrix
## else, update the new cached matrix and recalculate the inverse and put it into the cached inverse
## return new inverse matrix
cacheSolve <- function(x, data) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInv()
  if(!is.null(m) & identical(data,x$get())){
    message("getting cached data")
    return(m)
  } else {
    message("updating cache with new data")
    x$set(data)
    m <- solve(x$get())
    x$setInv(m)
    return(m)  
  }
}
