## This function was created to help understand how lexical scoping operates
## and how caching data can help make speed up calculations.
## The first function will create and store the Matrices.As well as act as 
## pointers. The second function will calculate inverse matrices by making use
## of the cached data.



## The first is makeCacheMatrix, this will create and store matrices.
## The first half will create and cache the variables within the parent 
## environment. While the second half will act as pointers to these variables
## and change them if needed.
## This function will not do any calculations.

makeCacheMatrix <- function(x = matrix()) {  ## initialize x as a matrix func.
  inmat = NULL  ## this variable is used to store the inverse matrix of x

  ## This creates the two variables that will act as the cache.
  ## They are given default values, to prevent "missing" errors down the line. 
  
  ##This function is used to change the cached matrix when a new matrix inputted 
  setmat = function(m){
    x <<- m  ## Assigns the new matrix to the parent variable (in the cache)
    inmat <<- NULL ## resets the inverse cache (parent variable) to null when a   
                   ## new matrix is inputted 
  }
  
  ## This is an anonymous function used to retrieve the matrix in the cache
  getmat = function() x
  
  ##This function assigns the solved inverse matrix to the cached inverse matrix
  setinmat = function(inversemat){
    inmat <<- inversemat
  }
  
  ## This anonymous function retrieves the cached inverse matrix
  getinmat = function() inmat
  
  ## The list function is used to create a list of names for the functions to
  ## to make it easier to call the functions
  list(setmat = setmat, getmat = getmat, setinmat = setinmat, getinmat = getinmat)
  
}


## The cacheSolve function searches the cache to see whether or not there is an
## inverse matrix. If there is an inverse matrix, it will retrieve its value
## display it. If not, it will calculate the inverse of the x matrix and store
## it in the cache, so it does not need to be calculated again.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inmat = x$getinmat() ## retrieves the inverse matrix stored in the cache
  
  ## If function is used to determine if the inverse matrix in the cache is NULL
  ## or not. If it is not NULL it will retrieve the inverse matrix and end the
  ## function as well as return the value.
  if(!is.null(inmat)){
    print("Retrieving cached inverse matrix")
    return(inmat)
  }
  
  ## If the inverse matrix is NULL, the rest of the function will calculate
  ## it using the solve() function
  
  m = x$getmat() ## retrieves the matrix in the cache 
  
  im = solve(m, ...) ## Calculates the inverse matrix 
  
  x$setinmat(im) ## Stores the new inverse matrix in the cache, for later use
  
  im ## Returns and displays the inverse matrix
  
}
