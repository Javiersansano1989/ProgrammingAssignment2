makeCacheMatrix <- function(mx = matrix()){ #as per instructions, and defining the matrix as such
  
  inv_mx <- NULL # following the example of makeVector, this line creates an empty object to store the solution into.
  
  set <- function(ext_mx) { # this 'set' function assigns the a dummy object into matrix
    
    mx <<- ext_mx # the dummy object is called 'external matrix', as the <<- is used to assign values in a diferrent environment, or 'external'
    inv_mx <<- NULL
    
  }
  
  get <- function() mx # whereas set assigns the dummy object into the 'main' object, get 'grab' the object
  set_inv_mx <- function(ext_inv_mx) inv_mx <<- ext_inv_mx # and just like with the mx and ext_mx, the function set_inv_mx assgigns an external or dummy inverted matrix into the solution
  get_inv_mx <- function() inv_mx # and just like with the mx and ext_mx, the function set_inv_mx assgigns an external or dummy inverted matrix into the solution
  list(set = set, get = get, set_inv_mx = set_inv_mx, get_inv_mx = get_inv_mx) #finally, just like with makeVector, the BIG function is defined as a list of functions to 'create' and 'grab' both the 'main objects' (original and inverted matrixes) and and the 'dummies'
  
}

cacheSolve <- function(mx, ...) {
  
  inv_mx <- mx$get_inv_mx() # following the example of cachemean, the function for grabbing the 'dummy' inverted matrix is called
  
  if(!is.null(inv_mx)){ # if the code has already run, then it's better to go get the inverted matrix within the cache instead of "solving" the problem each and every time
    
    message("getting cached data")
    return(inv_mx)
    
  }
  
  data <- mx$get() # the matrix is "deposited" into 'data', effectively going into the cache
  inv_mx <- solve(data) # whereas now data is "solved"; as per cran.r-project.org: "... the standard R function for matrix inverse is solve()"
  mx$set_inv_mx(inv_mx)
  inv_mx # prints the inverted matrix, wheter it was just calculated, or already existed within the cache
  
}