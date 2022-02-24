## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inverted.matrx<-NULL
  set<-function(y){
   x<<-y
   inverted.matrx<<-NULL
 }
 get<-function() x
 set.inverted.mtrx<-function(mat) inverted.matrx<<-mat
 get.inverted.mtrx<-function() inverted.matrx
   
 list(set=set,get=get,
      get.inverted.mtrx=get.inverted.mtrx,
      set.inverted.mtrx=set.inverted.mtrx)
 }

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
 
    invrt<-x$get.inverted.mtrx()
    mtrx.now<-x$get()

    if (is.null(invrt) || !identical(invrt, mtrx.now)){
      a<-mtrx.now[1]
      b<--mtrx.now[3]
      c<--mtrx.now[2]
      d<-mtrx.now[4]
      invrt<-x$set.inverted.mtrx(matrix(1/(a*d-b*c)*c(d,c,b,a),nrow=2,ncol=2))
      return (invrt)
      }
      else {
        print("existed")
        return (x$get.inverted.mtrx())

      }
}
