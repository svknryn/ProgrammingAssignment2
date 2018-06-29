## makeCacheMatrix, cacheSolve are functions caching the inverse of a matrix.

## The function creates a "matrix" object that can cache its inverse.
makeCacheMatrix <- function(z = matrix()) {
  m<-NULL
  set<-function(y) {
    z<<-y
    m<<-NULL
  }
  get<-function() z
  setimx<-function(solve) m <<- solve
  getimx<-function () m
  list(set=set, get=get,setimx=setimx, getimx=getimx)
}
##The function computes the inverse of a special "matrix" 
##It is returned by makeCacheMatrix above. If the inverse has already been calculated 
##(and the matrix has not changed), then the cachesolve will retrieve
## the inverse from the cache
cacheSolve <- function(z, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- z$getimx()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- z$get()
  m <- solve(data, ...)
  z$setimx(m)
  m
}
z<-matrix(c(1,2,3,4),nrow=2,ncol=2)

cacheSolve(makeCacheMatrix(z))

