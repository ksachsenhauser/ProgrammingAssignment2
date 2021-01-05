## cacheSolve generates the inversion of a squared matrix
## either by computing the inversion or using a cached
## value if available

## set up the matrix cache and provide functions 
## to use the cache

makeCacheMatrix <- function(x = matrix()) {
        ## initialize inverse matrix with NULL
        inv_mat <- NULL
        
        ## set function to introduce a new matrix
        set <- function(y){
                x <<- y
                inv_mat <<- NULL
        }
        
        ## get function to retrieve matrices
        get <- function() x
        
        ## setinv function to store a computed inverse
        setinv <- function(solve) inv_mat <<- solve
        
        
        ## getinv function to get inverted matrix
        getinv <- function() inv_mat
        
        ##list with defined functions as elements
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## generate an inverse matrix 
## try to solve from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## check if inverse matrix is available
        ## if so return it and done
        inv_mat <-x$getinv()
        if(!is.null(inv_mat)){
                message("getting data from cache")
                return(inv_mat)
        }
        
        ## get matrix
        matr_data <- x$get()
        
        ## invert matrix and store it
        inv_mat <- solve(matr_data,...)
        x$setinv(inv_mat)
        ## provide result
        inv_mat
}
