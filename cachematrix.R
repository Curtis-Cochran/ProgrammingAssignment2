##  This process atttempts to optimize the processing time 
##  of resource intensive calculations by caching computations 
##  that have already been calculated. This is particularly useful 
##  when working with large matrices where taking the inverse 
##  could result in less than optimal performance.


##  The makeCacheMatrix function creates a matrix object that 
##  can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

  ##  Initialize the inversed matrix variable (iX) to NULL;
  
  iX <- NULL
  
  ##  the "set" function takes the matrix passed into makeCacheMatrix
  ##  and assigns it to value X in the function environment. The 
  ##    inverse matrix variable "iX" is set to NULL.
  
  set<- function(Y){
    
    X <<- Y
    iX <<- NULL
  }
  
  ##  the "get" function creates a function in the function environment
  ##  and assigns X to it.
  
  get <- function() X
  
  ##  the "setiX" function takes the inverse of X and assigns it
  ##  to iX in the function environment.
  
  setiX <- function(solve) iX <<- solve
  
  ##  the "getiX" returns the inverse matrix (iX) from the function environment
  
  getiX <- function() iX
  
  ## returns a list of the values in the function environment
  
  list(set = set, get = get, setiX = setiX, getiX = getiX)
  
}


##  The cacheSolve function computes the inverse of the matrix 
##  returned by the makeCacheMatrix function.  If the inverse has
##  already been calculated, then the cacheSolve function will
##  retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  ##  Goes to the function environment and retrieves the iX value and assigns
  ##  it to the local variable iX.
  
  iX <- X$getiX()
  
  ##  If the variable iX in the function environment is not NULL then the
  ##  the function will return the message "getting cached data" and 
  ##  subsequently return the cached value stored for iX.
  
  if(!is.null(iX)) {
    
    message("getting cached data")
    return(iX)
    
  }
  
  ##  If the variable iX is NULL; get the matrix passed to
  ##  makeCacheMatrix and assign it to the local variable "data"
  
  data <- X$get()
  
  ##  Next the inverse of the passed matrix is calculated and set to the
  ##  the local variable iX.
  
  iX <- solve(data,...)
  
  ##  Once the inverse of the matrix has been calculated, we set it to
  ##  the iX variable so that it is cached in the function environment.
  
  X$setiX(iX)
  
  ##  Returns the inverse matrix of X. Currently assigned to the local
  ##  variable iX.
  
  iX 
  
}
