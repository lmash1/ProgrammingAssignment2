## Lisa Mash
## Assignment 2
## 7/23/2014

## makeCacheMatrix creates a matrix with 
##functionality to set and cache original and inverse values.

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	setMat <- function(y){
		x<<- y
		inverse<<- NULL
	}
	getMat<- function() x
	setInv<- function(solve) inverse<<- solve
	getInv<- function() inverse
	list(setMat=setMat,getMat=getMat,setInv=setInv,getInv=getInv)
}


## cacheSolve accepts a makeCacheMatrix object and uses its functions
## to find the inverse of a matrix either by pulling from the cache
## or using the setInv function if the cache is empty.

cacheSolve <- function(x) {
	inv <- x$getInv()
	if(!is.null(inv)){
		message("getting cached inverse")
		return(inv)
	}
	matrix <- x$getMat()
	inv <- solve(matrix)
	x$setInv(inv)
	inv
}
