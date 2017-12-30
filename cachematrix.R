## makeCacheMatrix is a function that creates a matrix object that gets the inverse of a matrix and can cache it.
## cacheSolve retrieves the cached inverse from makeCacheMatrix if it is there, or calculates the inverse if it is not.

##This function creates a matrix object, setting and getting the values, and setting and getting the inverse. 

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  set<-function(y){
    x<<-y
    i<--NULL
  }
  get<-function()x
  setInverse<-function(inverse)i<<-inverse
  getInverse<-function()i
  list(set=set, get=get, 
       setInverse=setInverse, 
       getInverse=getInverse)

}

## Gets the object from makeCacheMatrix and retrieves the cached inverse if it is there, or calculates the inverse if it is not. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i<-x$getInverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data<-x$get()
  i<-solve(data,...)
  x$setInverse(i)
  i
}
