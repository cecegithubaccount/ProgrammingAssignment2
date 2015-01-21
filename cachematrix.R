##makecachematrix create an object that can store a matrix and its inverse
##cacheSolve return the inverse of a matrix object created by makecachematrix

##exemple
##cm<-makeCacheMatrix(array(c(2,0,0,2),c(2,2)))
##> cacheSolve(cm)

##makeCacheMatrix returns an object with 4 functions, set,get,setinverse,getinverse
##for storing a matrix and its inverse when needed
makeCacheMatrix <- function(x = matrix()) {
  ##inversecache is the cache for inverse of the matrix
  inversecache <- NULL
  ##the set function is used to set the numrical value of the matrix
  ##null in the cache
  set <- function(y) {
    x <<- y
    inversecache <<- NULL
  }
  ##get function return the matrix
  get <- function() x
  ##set the cache with the computed inverse
  setinverse <- function(inverse) inversecache <<- inverse
  ##get the inverse from the cache
  getinverse <- function() inversecache
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
##cacheSolve return the inverse of a matrix from an matrixobject created by
## the makeCacheMatrix function
cacheSolve <- function(x, ...) {
  ##try to get inverse from cache
  m <- x$getinverse()
  ##if cache contain calculated inverse return it
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ##calculate inverse
  data <- x$get()
  m <- solve(data, ...)
  ##save inverse to the cache
  x$setinverse(m)
  ## return inverse
  m
}
