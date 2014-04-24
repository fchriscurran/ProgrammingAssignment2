## The two functions below allow for the calculating and caching of
#matrix inverses.  Broadly, the cacheSolve function checks for a 
#cached matrix and returns one if found.  Otherwise, it calls
#functions in the makeCacheMatrix to solve for the inverse

## The makeCacheMatrix function has subfunctions that can calculate
#the inverse of the matrix, return a cached inverse, or save a calculated
#inverse into the cache

makeCacheMatrix <- function(x=matrix()) {

m <- NULL
set <- function(y) {
  x <<- y
  m <<- NULL
}
get <- function() x
setinverse <- function(solve) m <<- solve
getinverse <- function() m
list(set = set, get = get,
     setinverse = setinverse,
     getinverse = getinverse)
}

## The cacheSolve function checks to see if the inverse is
# cached.  If so, it returns the cached inverse.  If not, it
#calls functions from above to calculate the inverse

cacheSolve <- function(x = matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
  #Pull the inverse from the getinverse() function above
  m <- x$getinverse()
  
  #if the inverse is not null, then pull the cached version
  if(!is.null(m)) {
    message("getting cached inverse")
    return(m)
  }
  
  #if not cached, pull in the matrix and calculate the inverse
  data <- x$get()
  m <- solve(data)
  #set the inverse in the cache and return the inverse
  x$setinverse(m)
  m
  

}


