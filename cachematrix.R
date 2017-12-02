## creates a special "matrix" that cache its inverse
## set the value of the vector
## get the value of the vector

## set the value of the inverse of matrix for x
## get the value of the inverse of matrix for x

makeCacheMatrix <- function(x = matrix()) {
  
  
  cin = NULL
  set = function(y) {
    x <<- y
    cin <<- NULL
  }
  get = function() x
  setcin = function(inverse) cin <<- inverse 
  getcin = function() cin
  list(set=set, get=get, setcin=setcin, getcin=getcin)
  
}


## This function will return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  cin = x$getcin()
  ## checking if inverse has already been calculated
  if (!is.null(cin)){
    message("getting cached data")
    ## if true, gets inverse from cache
    return(cin)
  }
  ## set the value of the inverse of matrix via setcin function
  m = x$get()
  cin = solve(m, ...)
  x$setcin(cin)
  ##retrieves inverse from cache
  return(cin)
}
