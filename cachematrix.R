##The first function stores a matrix and its inverse in cache
##(also able to get that matrix and its inverse from cache) and the second function
##returns inverse from cache, if not present, then calculates inverse and stores it
##in cache.

##"makeCacheMatrix" contains a list with 4 other functions:
##set - (globally) assign matrix to "x" from argument "y" for assigment to cache
##get - get value of matrix "x" from cache
##setinverse - (globally) assign inverse of matrix to "i" for assigment to cache
##getinverse - get inverse matrix "i" from cache

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


##"cacheSolve" does three things with the first function as an argument:
##1 - checks if inverse matrix "i" is in data cache
##2 - if "i" is in cache, returns "i"
##3 - if "i" is not in cache, calculates "i" as inverse matrix of "x" and
##    stores it in cache

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  return(i)
}
