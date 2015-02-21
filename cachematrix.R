## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
### This function takes a matrix as input and and returns a list of getter/setter functions 
### that can be used to retrieve/store the inverse of the matrix.
makeCacheMatrix <- function(x = matrix()) {
  
      Invx <- NULL
      
      ### set function to store a new matrix and also reset the inverse
      set <- function(y) {
          x <<- y
          Invx <<- NULL
      }
      
      ### get function to return the matrix
      get <- function() x
      
      ### setInv function to store the inverse in parent environment
      setInv <- function(InputInvx) Invx <<- InputInvx
      
      ### getInv function to return the stored inverse
      getInv <- function() Invx
      
      ### return a list (of functions) back to caller.
      list(set = set, 
           get = get,
           setInv = setInv,
           getInv = getInv)
          

}


## Write a short comment describing this function
### This function returns the inverse of a matrix that is passed to it.
### It first checks whether inverse matrix value exists in the environment, then returns the same. 
### Else it calculate the inverse using solve() function, stores it in the parent environemnt and returns this value to caller
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
        Invx <- x$getInv()  ## get stored value of inverse matrix by calling getInv() function
        
        ## In case stored inverse matrix is not null, return this value
        if(!is.null(Invx)) {
            message("getting cached data")
            return(Invx)
        }
        
        data <- x$get()  ### get the matrix data by calling the 'get' function
        
        Invx <- solve(data)  ### calculate inverse matrix using 'solve' function.
        
        x$setInv(Invx)  ### call 'setInv' function to store this inverse matrix in parent environment.
        
        Invx     ### return the InvX value back to caller.
              
}
