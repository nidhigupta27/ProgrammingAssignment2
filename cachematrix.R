
## The functions makeCacheMatrix and cacheSolve are used to compute the inverse of an intervible
## matrix and save the result in cache for all future inverse computations over the same 
## matrix

## makeCacheMatrix returns a list of object methods(set,get,setinverse,getinverse).
## it takes input a square matrix that needs to be inversed by the cacheSolve function

makeCacheMatrix <- function(my_matrix=matrix()){
  i <- NULL
  set<-function(new_matrix){
    my_matrix <<- new_matrix
    i <<- NULL
  }
  get<-function() my_matrix
  setinverse<-function(inverse) i <<- inverse
  getinverse<-function() i
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}
#cacheSolve matrix checks to see if the inverse of the matrix already exists.
#If the inverse is already cached , its returned by cacheSolve function.
#If the inverse of the matrix does not exist(was not cached), its calculated 
# first,cached and returned by the function

cacheSolve <- function(matrix_to_be_inversed){
  i <- matrix_to_be_inversed$getinverse()
  if(!is.null(i)){
    print("Inverse is already cached")
    return(i)
  }
  data <- matrix_to_be_inversed$get()
  i<-solve(data)
  matrix_to_be_inversed$setinverse(i)
  i

}
