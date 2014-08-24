## file cachematrix.R
## J. Dinius 8-24-2014
## Definition of two functions: "makeCacheMatrix" and "cacheSolve" for caching
## the inverse of a matrix.  This eliminates redundant computations.
## Usage Example (from R console)
## > a <- matrix(rnorm(16),4,4) #create new 4x4 matrix
## > A<-makeCacheMatrix(a) #cache that matrix
## > anew<-A$get() # make sure that anew=a (matrix was stored properly)
## > A$getinverse() # inverse hasn't been computed yet, so this should be NULL
## NULL
## > ainv<-cacheSolve(A) #inverse has not been computed yet, so compute it now using solve
## > I<-a%*%ainv #if matrix is scaled properly (not singular), this should return the 4x4 identity matrix
## > View(I) # quick check to see how close to the identity I is
## > ainv_cache<-cacheSolve(A) #check to make sure that cacheSolve function returns cached value (avoid computing again)
## getting cached data #cached value is used to set ainv_cache


## function makeCacheMatrix(numeric matrix)
## creates cached matrix object that stores the matrix and the cached
## inverse (if it has been computed, otherwise the inverse is NULL)
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## function cacheSolve(cached numeric matrix)
## Pulls the inverse of the input cached matrix, if the inverse is not NULL (i.e. it
## exists), then pull the inverse from the cache.  Otherwise, compute the inverse using
## the "solve" built-in function.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
