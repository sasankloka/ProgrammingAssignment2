## Put comments here that give an overall description of what your
## functions do

# This custom Matrix function to implments the cached inverse matrix to improve performance.

## Write a short comment describing this function
# makeCacheMatrix will take matrix as input and provides get,set methods to access matrix
# methods getsolve,setsolve to access inverse matrix.


makeCacheMatrix <- function(x = matrix()) {
#y<-matrix(data=x,nrow,ncol,byrow,dimnames)
  y<-x
  sm<-NULL
  
  get<- function() y
  
  set<- function(x){ y<-x
                      sm<<-NULL} 
  
  getsolve<-function() sm
    
  setsolve<-function(m) sm<<-m
  
  list(get = get,set=set,setsolve=setsolve,getsolve = getsolve)

}


## Write a short comment describing this function
# This function will take a matrix as input and build a cache version of inverse matrix. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'i
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
