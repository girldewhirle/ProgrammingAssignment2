## functions to create a matrix object and to give the inverse of that matrix
## functions create the matrix object in the cache so it can be recalled at a later
## point saving on processing time

## function that sets a variable to a matrix value

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## function that checks for values of the variable already in the cache
## and returns the inverse of that value
## if there is nothing already in the cache then it calculates the inverse itself
## and returns it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
