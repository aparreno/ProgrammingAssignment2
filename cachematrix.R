## Put comments here that give an overall description of what your
## functions do

## this function returns a list with the functions to set, get, set inverse and get inverse from
## tha matrix passed in the argument

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  ## set the matrix from the argument
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## get the matrix from the list
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## this function takes a list in the argument and search if inverse is already set
## if not it calculates inverse and set it, otherwise it returns cached data

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  ## chech if inverse is cached
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## if not cached calculate inverse and set it in the list
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
