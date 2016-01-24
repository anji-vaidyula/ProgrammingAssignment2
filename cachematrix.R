## Put comments here that give an overall description of what your
## functions do 

################################################################################  
#   The first function makeCacheMatrix creates a special vector which
#   has functions as its elements. These functions set, get and setinv, getinv
#   This function can be thought of as a class in c++. It enapsulates the data
#   and has functions set and get the data. This enables caching the results of 
#   computationally intensive operations.
#   
#   The second function cacheSolve gets the inverse of the matrix. It returns the 
#   if the cached value. If no cache is found, it creates the inverse of the matrix 
#   and call the function to cache it.
#   
################################################################################

##  Write a short comment describing this function

################################################################################
# Function: makeCacheMatrix
#   This function implments the functions that make the data persistent.
#   It uses the special assigment operator <<- to achieve this
#   Input: a matrix that can be inverted. It doesn't check whether the matrix is invertable
#           It assumes it is invertable
#   Output: A list of functions that provide set and get the data
################################################################################

makeCacheMatrix <- function(x = matrix()) {

  # Initialize the invertex matrix to null
  inv <- NULL
  # Create set function that takes the matrix as input and stores it for
  # persistence using <<- operator. Whenver, this function is called, it signifies
  # that the matrix has changed and the Inverse variable needs to be recomputed
  # So, this fucntion resets the stored inverted matrix to NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # This get function returns the stored matrix
  get <- function()x
  
  # This function sets the inverted matrxi
  setinv <- function (i) inv <<- i
  
  # This function gets the inverted matrxi
  getinv <- function () inv
  
  # Return list of functions for settging and getting data
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

################################################################################
# Function: cacheSolve. This function calculates the inverse of the special matrix
#           created using makeCacheMatrix. This function before calculating checks 
#           whether inverse is already computed for the given matrix. 
# Input: a special matrix created using makeCacheMatrix function. 
# Output: Inverted matrix from cache if exists; otherwise create it by calling solve
#         function and caches it for future use.
################################################################################

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  # Get the inverese matrix from the matrix object
  i <- x$getinv()
  # Check whether it is null, if not then return the cached value
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  # This following code is executed when there is no inverse matrix in the cache
  
  # get the matrix
  mat <- x$get()
  i <- solve(mat, ...)
  x$setinv(i)
  i
}
