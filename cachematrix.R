## The function makeCacheMatrix creates a special "matrix" of functions

## The function makeCacheMatrix creates a special "matrix" of functions that caches the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {	#create function makeCacheMatrix with input matrix
	i <- NULL 						#assigns NULL value to i
	set <- function(y) {				#creates a function that...
		x <<- y					#assigns input value (y) to x
		i <<- NULL					#assigns NULL value to inverse of matrix
	}
	get <- function() x				#creates function to get the value of the matrix
	setinv <- function(solve) i <<- solve	#creates function to set the inverse of the matrix
	getinv <- function() i				#creates function to get the inverse of the matrix
	list(set = set, get = get, setinv = setinv, getinv =  getinv) #prints out list of functions
}

## cacheMatrix returns the inverse of a cached matrix that has already been calculated.
## If the inverse has not already been calculated, it calculates the inverse and then caches it.

cacheMatrix <- function(y, ...) {			#create function cacheMatrix using list of functions from makeCacheMatrix
      i <- y$getinv()					#gets inverse of matrix and assigns to i
	if(!is.null(i)) {					#if i exists (non-NULL value)...
		message("getting cached data")	#prints message
		return(i)					#prints inverse matrix
	}  
	data <- y$get()					#gets data from makeCacheMatrix
	i <- solve(data,...)				#calculates the inverse of data
	y$setinv(i)						#calculates inverse of data and assigns to i
	i 							#Prints i (the inverse of x)
}
