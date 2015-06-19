## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
      #Inverse Matrix, initially undefined
      M1 <- NULL
      
      #To set matrix
      set <- function(y) {
    
            x <<- y
    
            M1 <<- NULL
            
      }
      
      #To get matrix
      get <- function() x
      
      #To set inverse matrix
      setinvmat <- function(inverse) M1 <<- inverse
      
      #To get inverse matrix
      getinvmat <- function() M1
      
      #To show the functions in form of lists
      list(set = set, get = get,
      setinvmat = setinvmat,
      getinvmat = getinvmat)
            
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        M1 <- x$getinvmat()
        
        ## If Inverse Matrix is not null, look for M1
        if(!is.null(M1)) {
                
                message("getting cached data")
                
                return(M1)
        }
        
        # If Inv. Matrix is null, get the new matrix
        M1stored <- x$get()
        
        # calculate the inverse of the new matrix
        M1 <- solve(M1stored, ...)
        
        # set the new inverse
        x$setinvmat(M1)
        
        #returns inverse matrix
        M1  
}

# Prueba

x <- matrix(rnorm(n = 9),nrow = 3, ncol = 3)
x
matriz <- makeCacheMatrix(x)
matriz$get()
matriz$getinvmat()
cacheSolve(matriz)
matriz$get() %*% matriz$getinvmat()
solve(x)
