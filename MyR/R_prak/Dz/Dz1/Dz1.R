#1____________________________________________________________2
mat0 <- matrix(seq(1,9), nrow=3, ncol=3, byrow=TRUE)
rownames(mat0) <- c("I", "II", "III")
colnames(mat0) <- c("A","B","C")
print(mat0)
cat("\n")
t(mat0) 
print(mat0)
print("___________________________________________")
#2____________________________________________________________
a <- c(1, 2, 3)
b <- c(4, 5, 6)
c <- c(7, 8, 9)
A <- cbind(a,b,c)
B <- rbind(a,b,c)
rownames(A) <- c("I", "II", "III")
colnames(A) <- c("I", "II", "III")
print(A)
cat("\n")
print(B)
print('___________________________________________')
#3_____________________________________________________________
dim(mat0)
mat0[2,2]
print(is.numeric(mat0[2,2]))
  print(is.logical(mat0[2,2]))
      print(is.na(mat0[2,2]))
            print(is.nan(mat0[2,2]))

