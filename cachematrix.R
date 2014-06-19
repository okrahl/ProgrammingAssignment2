## Matrix inversion normally is a costly operation, especially for large 
## matrices. To avoid recalculation of this inversion for recurring calls, here
## the result will be cached in a function object. No repetition of calculation 
## is needed at all, as long as the original matrix doesn't change

## makeCacheMatrix is a function object, that caches a matrix and its inverse
## in two variables. makeCacheMatrix supplies appropriate set- and 
## get-functions to manipulate these cached variables.
## Setting the matrix will discard the inverse, if there is any. Then, if the 
## inverse of the matrix is requested, a new calculation is required.

makeCacheMatrix <- function(x = matrix()) {
  ## 
  ## Returns a list of the four functions to operate on the makeCacheMatrix 
  ## object
  m_inverse <- NULL  # declare member variable m_inverse to make sure it exists
  set <- function(p_matrix){
    # as soon as a new matrix is set, any previous solution (solve, inverse)
    # is discarded 
    x <<- p_matrix  # bind parameter p_matrix to member x
    m_inverse <<- NULL  # bind member variable: "clear" m_inverse     
  }
  get <- function() x  # return the matrix data to be solved
  # set function to bind the solution to member variable
  setinverse <- function(p_inverse) m_inverse <<- p_inverse
  # get function to return the value of the solution (inverse) 
  getinverse <- function() m_inverse
  # return the functions in a list
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## cacheSolve calculates the inverse of a matrix and caches it in a function
## object. In succeeding calls cacheSolve returns the cached inverse instead
## of solving the matrix again.

cacheSolve <- function(x, ...) {
  ## parameter 'x' is a "special" matrix of type makeCacheMatrix
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()  # get the inverse of the matrix (maybe NULL)
  if(!is.null(inverse)){
    # inverse found: print message and return inverse
    message("getting cached data")
    return(inverse)
  }
  # inverse not found
  standardmatrix <- x$get()  # get the standard matrix data
  inverse <- solve(standardmatrix)  # calculate the inverse from standard matrix
  x$setinverse(inverse)  # store inverse for later reuse
  return(inverse)  # return result to caller  
}
