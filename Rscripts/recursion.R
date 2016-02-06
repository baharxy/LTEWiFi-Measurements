 recfunction <- function (input_list)
 i=0
 new_row=1
 df <- matrix(nrow = 10, ncol =10)
while (length(input_list)!=1) {
 for (el in input_list) {
   i <- i+1
   temp <- recfunction(input_list[input_list!=el ])
   df[1:dim(temp)[1],1] <- el
   df[new_row:dim(temp)[1],2:dim(temp)[2]]
   new_row <- new_row + dim(temp)[1]
 }
 
}
 if ((length(input_list)==1)) {
   df <- input_list
   break
 } 
 return (df)
 
 
 