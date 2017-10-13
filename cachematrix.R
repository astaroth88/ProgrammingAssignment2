## The makeCacheMatrix caches the inverse of the Matrix 
## The cacheSolve computes the inverse of the Matrix

##The following function caches the inverse of the Matrix
##Contains 4 functions within the main function which are set(),get(),setinv(),getinv()

makeCacheMatrix <- function(x = matrix())
{
inv<-NULL
set<-function(y)
{
  x<<-y
  inv<<-NULL
}
get<-function()x
setinv<-function(inverse0)inv<<-inverse0
getinv<-function()inv
list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## Checks if the inverse matrix is Not Null; then it returns it
##If the inverse matrix is NUll, then it ges the inverse and displays it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<-x$getinv()
        if(!is.null(inv))
        {
          message("getting cached data")
          return(inv)
        }
        data<-x$get()
        inv<-solve(data,...)
        x$setinv(inv)
        inv
}
