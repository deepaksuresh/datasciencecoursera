## Matrix inversion is usually a costly computation and their may be some benefit to 
##caching the inverse of a matrix rather than compute it repeatedly.
##Following  pair of functions calculates the inverse, caches it, returns inverse from cache
##if it is present, else calculates it and stores in cache

## Function to initialize inverse, returns a function containing the matrix whose inverse
##is to be calculated and a set of helper functions

makeCacheMatrix <- function(x = matrix()) {
  inverse<-NULL
  set<-function(y){
    x<<-y
    inverse<<-NULL
  }
  get<-function() x
  setinverse<-function(inv) inverse <<-inv
  getinverse<-function() inverse
  list(set = set,get = get,setinverse=setinverse,getinverse=getinverse)
}


##function returns inverse of a matrix; if the inverse had been calculated, cached matrix is 
##returned, else, inverse is calculated and then returned

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<-x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)}
  data <-x$get()
  inv<-solve(data)
  x$setinverse(inv)
  inv
}
