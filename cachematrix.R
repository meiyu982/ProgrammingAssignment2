## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


makeCacheMatrix <- function(x = matrix()) { 
 	my_v <- NULL ## inverse matrix 
  
 	if (!is.matrix(x))  
 		x <- as.matrix(x) 
  
 	set <- function(newX) { 
 		x <<- newX 
 		my_v <<- NULL 
 		print("set new") 
 	} 
  
 	get <- function() x 
 	setMy_v <- function(i) my_v <<- i 
 	getMy_v <- function() my_v 
  
 	list(set=set,  
 	     get=get, 
 	     setMy_v=setMy_v, 
 	     getMy_v=getMy_v) 
 } 
  
  
## Write a short comment describing this function 
  
 cacheSolve <- function(x, ...) { 
 	my_v <- x$getMy_v() 
 	if (is.null(my_v)) { 
 		m <- x$get() 
 		my_v <- solve(m, ...) 
 	} 
 	x$setMy_v(my_v) 
 	my_v 
 } 
