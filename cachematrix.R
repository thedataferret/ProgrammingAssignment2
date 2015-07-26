## These two functions help you avoid expensive calculation of the inverse of a (square) matrix 
## if you have already done that calculation before.
## The first time, the full calculation will go ahead (this may take a lot of time)
## The next time, R will just quickly retrieve the cached inverse matrix.

#----------------------------------------

## This function creates a "box" which contains four different useful functions
## These four functions are defined here so that the NEXT function can refer to them


makeCacheMatrix <- function(x = matrix()) {
  inverse_matrix <- NULL
  
  set_my_matrix_func <- function(new_matrix) {    ## set x (or whatever) as your matrix of interest
    x <<- new_matrix                              
    inverse_matrix <<- NULL                       ## set inverse matrix to null (because you don't have anything yet)
  }
  
  get_my_matrix_func <- function() {              ## retrieve the matrix of interest
    x
  }
  
  set_inverse_matrix_func <- function(inv) {      ## store whatever inv value is in cache as inverse_matrix
    inverse_matrix <<- inv
  }
  
  get_inverse_matrix_func <- function() {         ## retrieve inverse_matrix
    inverse_matrix
  }
  
  list(set_x = set_my_matrix_func,                    ## create a list of these 4 functions so that they can be called from the next function
       get_x = get_my_matrix_func,
       set_inverse_matrix = set_inverse_matrix_func,
       get_inverse_matrix = get_inverse_matrix_func)
}

#----------------------------------------
## This function allows you to check first if the inverse of your matrix has already been calculated and cached.
## If it has been calculated and cached, then the cached value will be returned (nice and quickly)
## If it has not been calculated and cached before, it will be calculated and cached (and then returned) now
## you need to run this function on the OUTPUT of makeCacheMatrix (you need to run this on a "box")


cacheSolve <- function(x, ...) {
  inv <- x$get_inverse_matrix()
  
  if(!is.null(inv)) {                          # if there is a value sitting in inv, tell me that you're retrieving cached data
    message("getting cached inverse data")
    
  } else {                                     # otherwise, (if inv is just null)
    matrix <- x$get_x()                        # use the function defined in makeCacheMatrix
    inv <- solve(matrix, ...)                  # go ahead and calculate the inverse (may take ages)
    x$set_inverse_matrix(inv)                  # and load the result into the cache
  }
  
  inv                                          # return inv
}

#----------------------------------------
# just some testing 

# x <- matrix(rnorm(100), 10, 10)
# box <- makeCacheMatrix(x)
# cacheSolve(box)
