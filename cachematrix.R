## Put comments here that give an overall description of what your
## functions do:
## There are two main functions here. One to return a list setting and
## getting the matrix and its inverse

#############################################
## 
## This function returns a list containing functions to:set the matrix,
## get the matrix, set the inverse, get the inverse. This list is used
## as an input to cacheSolve() function.
#############################################
makeCacheMatrix <- function(x = matrix()) {
  inver = NULL
  set = function(y){
    x<<- y
    inver <<-NULL
  }
  get = function() x
  setinver = function(inverse) inver <<- inverse
  getinver = function() inver
  list(set=set, get=get, setinver=setinver, getinver=getinver)
}

########################################
## 
## This function receive an array and returns its inverse taking care to find out
## if the inverse has already been calculated, getting it from the cache.
########################################

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inver = x$getinver()
  if(!is.null(inver)){
    message("getting cached data")
    return(inver)
  }
  mat.data = x$get()
  inver = solve(mat.data, ...)
  x$setinver(inver)
  return(inver)
}
