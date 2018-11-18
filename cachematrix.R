## This contains pair of functions that cache the inverse of a matrix
## functions that cache the inverse of a matrix
## Programming Assignment 2: Lexical Scoping

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
       m <- NULL					#begins by setting the inverse to NULL as a placeholder for a future value
        set <- function(y) {				#defines a function to set the matrix, x, to a new matrix, y, and resets the inverse, m, to NULL
                x <<- y
                m <<- NULL
        }
	  get<-function()x				#returns the matrix, x
	  setinv<-function(inverse)m<<-inverse		#sets the inverse, m, to inverse
	  getinv<-function() m				#returns the inverse, m
	  list(set=set,get=get,				#returns the 'special matrix' containing all the functions just defined
		setinv=setinv,
		getinv=getinv)

}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {			#without cacheSolve, the makeCacheMatrix function is incomplete
	  m<-x$getinv()
	  if(!is.null(m)) {				#check if the inverse is null
		message("getting cached data")
		return(m)

	  }
	  data<-x$get()					#gets the matrix and assign to data
	  m<-solve(data,...)				#gets the inverse using the solve function
	  x$setinv(m)
	  m

        ## Return a matrix that is the inverse of 'x'
