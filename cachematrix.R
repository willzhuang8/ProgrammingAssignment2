# The following functions can be used in conjunction to calculate the inverse of a matrix
# As this calculation can be complex, it caches the inverse if it has already been calculated

# makeCacheMatrix creates a special vector with the following four functions relating to matrices:
# 1) set the value of the matrix
# 2) get the value of the matrix
# 3) set the value of the inverse
# 4) get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv<<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function() x
  setinv<-function(inverse) inv<<-inverse
  getinv<-function() inv
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}

# cacheSolve returns the inverse of the cacheMatrix
# if the inverse has already been calculated, it retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
