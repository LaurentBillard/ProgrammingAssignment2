#Matrix inversion is usually a costly computation and their may be some benefit to caching 
#the inverse of a matrix rather than compute it repeatedly (there are also alternatives 
#to matrix inversion that we will not discuss here). 
#Here below the functions that cache the inverse of a matrix.
#Please note that the matrix supplied is always invertible.

makeCacheMatrix <- function(x = matrix()) {
  #This function creates a special "matrix" object that can cache its inverse.
  #A list of 4 functions :
  #set the value of the matrix
  #get the value of the matrix
  #set the value of the inverse of the matrix
  #get the value of the inverse of the matrix
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

cacheSolve <- function(x, ...) {
  #This function "cacheSolve"computes the inverse of the special "matrix" returned by 
  #the function "makeCacheMatrix". 
  #If the inverse has already been calculated (and the matrix has not changed), then 
  #this function "cachesolve" should retrieve the inverse from the cache.
  
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
