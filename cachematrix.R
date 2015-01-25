## 'makeCacheMatrix' creates a list containing four functions to
## 1. set the matrix: set()
## 2. get the matrix: get()
## 3. set the inverse of the matrix: setinverse(TheInverseMatrix)
## 4. get the inverse of the matrix: getinverse()
##
## 'cacheSolve' checks if the inverse of the matrix returned by 'makeCacheMatrix' 
## has been calculated. If not, 'cacheSolve' should compute the inverse and put
## the result into 'makeCacheMatrix' as the cache and retrive it for repeated
## computation (if the matrix has not changed).


## 'makeCacheMatrix' creates a list containing functions to set and get
## a matrix(TheMatrix) as well as its inverse. 
## Use: x <- makeCacheMatrix(theMatrix) or 
##      x <- makeCacheMatrix() ; x$set(TheMatrix)

makeCacheMatrix <- function(x = matrix()) {
  
  inv_x <- NULL
  set <- function(y) {
    x <<- y
    inv_x <<- NULL
  }
  get <- function() x
  setinverse <- function(ix) inv_x <<- ix 
  getinverse <- function() inv_x
  
  list(set=set,  get=get, setinverse=setinverse, getinverse=getinverse)
  
}



## 'cacheSolve' computes the inverse of the matrix returned by 'makeCacheMatrix' 
## then puts the result into 'makeCacheMatrix' as the cache. If the computation 
## (with the same matrix) has to be performed repeatedly, 'cacheSolve' would 
## just retrieve the inverse from the cache. Use: cacheSolve(x)

cacheSolve <- function(x, ...) {
  
  inv_x <- x$getinverse()
  if(!is.null(inv_x)) {
    message("getteing cached data")
    return(inv_x)
  }
  
  dat <- x$get()
  inv_x <- solve(dat)
  x$setinverse(inv_x)
  inv_x  ## Return a matrix that is the inverse of 'x'
}
