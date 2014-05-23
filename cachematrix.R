## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	mInverseCache<- NULL
  	set<-function(y){
    x<<-y
    minverseCache <<-NULL
  	}
  	get<-function() x
  	setInverse<-function(inverse) mInverseCache<<-inverse
  	getInverse<-function() {mInverseCache}
  	list(set=set, get=get, 
        setInverse=setInverse,
        getInverse=getInverse)

}


## Write a short comment describing this function
## cacheSolve computes the inverse os a matrix stored in a "special matrix" returned by
## the function makeCacheMatrix. If the inverse of the matrix has already been calculated
## this matrix is retrieved from the cache

cacheSolve <- function(x, ...) {
	## Get the inverse (if there is any) stored in x
        mInverseCache<-x$getInverse()
    ## Check if the inverse has been calculated. If this case return the inverse
  if(!is.null(mInverseCache)){
    message("getting cached inverse matrix")
    return(mInverseCache)
  }
    ## If the inverse has not been calculated we retrieve the data matrix "stored" in x
  matIn<-x$get()
  mInverseCache<-solve(matIn,...)
  x$setInverse(mInverseCache)
  ## Return a matrix that is the inverse of 'x'
  mInverseCache
}
