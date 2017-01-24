## Data Science - R Programming
## Week3: Programming with R
## Programming Assignment2: Lexical Scoping
## Caching the inverse of a matrix


## Create a special "matrix" object that can cache its inverse
## Set and get the value of the matrix
## Set and get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Compute the inverse of the special "matrix" returned by makeCacheMatrix
## If the inverse has already been calculated, then the cacheSolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  
        ## Return a matrix that is the inverse of 'x'
  inv
}
