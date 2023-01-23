## Function to compute & cache matrix inverse computation


## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL # sets m to NULL
  set <- function(y) { # first function
    x <<- y     # sets x in parent environment to y
    s <<- NULL  # sets m in the parent environment to null  
  }
  get <- function() x # second function
  setsolve <- function(solve) s <<- solve # third function
  getsolve <- function() s # fourth function
  list(set = set, get = get, # output = list of functions
       setsolve = setsolve,
       getsolve = getsolve)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
  s <- x$getsolve() #searching cache for inverse of x and sets it to s 
  if(!is.null(s)) { # if s is not null returns true &
    message("getting cached data") # prints message "getting cached data" &
    return(s) # return saved inverse
  } #if s in "! is not null" returns false then
  data <- x$get() # sets data to x$get()
  s <- solve(data, ...) # sets s to inverse of data
  x$setsolve(s) # prints inverse (saved inverse to cache)
  s # prints s (inverse)
  
}
