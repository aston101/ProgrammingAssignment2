##makeCacheMatrix creates a matrix object, setting and getting the values, and setting and getting the inverse. 

makeCacheMatrix <- function(x = matrix()) {
 #Create a variable
   i<-NULL
   #set the function of the matrix
  set<-function(y){
    x<<-y
    i<--NULL
  }
  #get the function of the matrix
  get<-function()x
  #set the inverse
  setInverse<-function(inverse)i<<-inverse
  #get the inverse
  getInverse<-function()i
  #create a list of the functions
  list(set=set, get=get, 
       setInverse=setInverse, 
       getInverse=getInverse)

}

##cacheSolve gets the object from makeCacheMatrix and retrieves the cached inverse if it is there, or calculates the inverse if it is not. 

cacheSolve <- function(x, ...) {
  #gets the inverse from makeCacheMatrix
  i<-x$getInverse()
  #if inverse is chached, returns it
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  #otherwise, it calculates the inverse and returns it
  data<-x$get()
  i<-solve(data,...)
  x$setInverse(i)
  i
}
