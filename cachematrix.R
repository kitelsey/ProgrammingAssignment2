## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  invrt<-NULL
  set<-function(y){
   x<<-y
   invrt<<-NULL
 }
 get<-function() x
 setinvrt<-function(mat) invrt<<-mat
 getinvrt<-function() invrt
   
 list(set=set,get=get,
      getinvrt=getinvrt,
      setinvrt=setinvrt)
 }

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
 
    invrt<-x$getinvrt()
    mtrx<-x$get()
    
    if (is.null(invrt)){
      
      invrt<-x$setinvrt(solve(mtrx))
      return (invrt)
      }
    else {
      print("existed")
      return (x$getinvrt())

    }
}
