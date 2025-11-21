getwd()
setwd("C:/Users/Alika Kayla Martiza/OneDrive/Documents/KULIAH!!/SEMESTER 2/ANREG/")
ifls <- read.csv("dummy ifls.csv")
head(ifls)

#Menyusun matriks model regresi dari data
y <- as.matrix(ifls[,1])
head(y)
n <- nrow(y);n

#Membentuk matriks penjelas
X0 <- as.matrix(ifls[,-1])
head(X0)
u <- matrix(1, nrow=n, ncol=1)
X <- cbind(u,X0)
head(X)
p <- ncol(X);p
H <- X%*%solve(t(X)%*%X)%*%t(X)
I	<- diag(1, nrow=n, ncol=n)
e	<- (I-H)%*%y	
head(e)

#Taksiran parameter model
b <- solve(t(X)%*%X)%*%t(X)%*%y ;b

#Hitung standar error
SSE <- t(e)%*%e ; SSE
MSE	<- SSE/(n-p) ; MSE	
cov.b	<- solve(t(X)%*%X)%x%MSE	
se.b	<- sqrt(diag(cov.b));se.b

#R Squared dan Adjusted R Squared
J	<- matrix(1,nrow=n,ncol=n)	
SSR	<- t(y)%*%(H-J/n)%*%y ; SSR
MSR	<- SSR/(p-1) ; MSR
SST	<- t(y)%*%(I-J/n)%*%y	; SST	
SSE+SSR
MST	<- SST/(n-1) ; MST
SSR/SST #R squared
1-MSE/MST #Adjusted R squared

#MULTIKOLINEARITAS
##Menghitung matriks korelasi
R <- cor(X0);R
##Menghitung determinan
detR <- det(R);detR
##Menghitung VIF
VIF <- diag(solve(R));VIF
VIF>=10
###Ketentuan : >=10
###Karena hasil multikol ada 7 yang >= 10, maka terdapat multikol

#STEPWISE FORWARD
#Cycle 1
##Step 1
### X1 - X26
X <- cbind(u,X0)
p <- ncol(X)
bh <- solve(t(X)%*%X)%*%t(X)%*%y
H <- X%*%(solve(t(X)%*%X))%*%t(X)
e <- (I-H)%*%y
MSE <- (t(e)%*%e)/(n-p)
SE <- sqrt(diag((solve(t(X)%*%X)%x%MSE)))
t <- bh/SE
p_v <- 2*(1-pt(abs(t),n-p));p_v

##Jumlah ruangan = p_v_min --> X15 masuk

#Cycle 2
##Step 1
### X1
X	<- cbind(u,X0[,c(15,1)])
p	<- ncol(X)
bh	<- solve(t(X)%*%X)%*%t(X)%*%y
H	<- X%*%(solve(t(X)%*%X))%*%t(X)
e	<- (I-H)%*%y
MSE	<- (t(e)%*%e)/(n-p)
SE	<- sqrt(diag((solve(t(X)%*%X)%x%MSE)))
t	<- bh/SE
p_v	<- 2*(1-pt(abs(t),n-p))
p_v1	<- p_v[p,]

### X2
X	<- cbind(u,X0[,c(15,2)])
p	<- ncol(X)
bh	<- solve(t(X)%*%X)%*%t(X)%*%y
H	<- X%*%(solve(t(X)%*%X))%*%t(X)
e	<- (I-H)%*%y
MSE	<- (t(e)%*%e)/(n-p)
SE	<- sqrt(diag((solve(t(X)%*%X)%x%MSE)))
t	<- bh/SE
p_v	<- 2*(1-pt(abs(t),n-p))
p_v2	<- p_v[p,]

### X3
X	<- cbind(u,X0[,c(15,3)])
p	<- ncol(X)
bh	<- solve(t(X)%*%X)%*%t(X)%*%y
H	<- X%*%(solve(t(X)%*%X))%*%t(X)
e	<- (I-H)%*%y
MSE	<- (t(e)%*%e)/(n-p)
SE	<- sqrt(diag((solve(t(X)%*%X)%x%MSE)))
t	<- bh/SE
p_v	<- 2*(1-pt(abs(t),n-p))
p_v3	<- p_v[p,]

### X4
X	<- cbind(u,X0[,c(15,4)])
p	<- ncol(X)
bh	<- solve(t(X)%*%X)%*%t(X)%*%y
H	<- X%*%(solve(t(X)%*%X))%*%t(X)
e	<- (I-H)%*%y
MSE	<- (t(e)%*%e)/(n-p)
SE	<- sqrt(diag((solve(t(X)%*%X)%x%MSE)))
t	<- bh/SE
p_v	<- 2*(1-pt(abs(t),n-p))
p_v4	<- p_v[p,]

### X5
X	<- cbind(u,X0[,c(15,5)])
p	<- ncol(X)
bh	<- solve(t(X)%*%X)%*%t(X)%*%y
H	<- X%*%(solve(t(X)%*%X))%*%t(X)
e	<- (I-H)%*%y
MSE	<- (t(e)%*%e)/(n-p)
SE	<- sqrt(diag((solve(t(X)%*%X)%x%MSE)))
t	<- bh/SE
p_v	<- 2*(1-pt(abs(t),n-p))
p_v5	<- p_v[p,]

### X6
X	<- cbind(u,X0[,c(15,6)])
p	<- ncol(X)
bh	<- solve(t(X)%*%X)%*%t(X)%*%y
H	<- X%*%(solve(t(X)%*%X))%*%t(X)
e	<- (I-H)%*%y
MSE	<- (t(e)%*%e)/(n-p)
SE	<- sqrt(diag((solve(t(X)%*%X)%x%MSE)))
t	<- bh/SE
p_v	<- 2*(1-pt(abs(t),n-p))
p_v6	<- p_v[p,]

### X7
X	<- cbind(u,X0[,c(15,7)])
p	<- ncol(X)
bh	<- solve(t(X)%*%X)%*%t(X)%*%y
H	<- X%*%(solve(t(X)%*%X))%*%t(X)
e	<- (I-H)%*%y
MSE	<- (t(e)%*%e)/(n-p)
SE	<- sqrt(diag((solve(t(X)%*%X)%x%MSE)))
t	<- bh/SE
p_v	<- 2*(1-pt(abs(t),n-p))
p_v7	<- p_v[p,]

### X8
X	<- cbind(u,X0[,c(15,8)])
p	<- ncol(X)
bh	<- solve(t(X)%*%X)%*%t(X)%*%y
H	<- X%*%(solve(t(X)%*%X))%*%t(X)
e	<- (I-H)%*%y
MSE	<- (t(e)%*%e)/(n-p)
SE	<- sqrt(diag((solve(t(X)%*%X)%x%MSE)))
t	<- bh/SE
p_v	<- 2*(1-pt(abs(t),n-p))
p_v8	<- p_v[p,]

### X9
X	<- cbind(u,X0[,c(15,9)])
p	<- ncol(X)
bh	<- solve(t(X)%*%X)%*%t(X)%*%y
H	<- X%*%(solve(t(X)%*%X))%*%t(X)
e	<- (I-H)%*%y
MSE	<- (t(e)%*%e)/(n-p)
SE	<- sqrt(diag((solve(t(X)%*%X)%x%MSE)))
t	<- bh/SE
p_v	<- 2*(1-pt(abs(t),n-p))
p_v9	<- p_v[p,]

### X10
X	<- cbind(u,X0[,c(15,10)])
p	<- ncol(X)
bh	<- solve(t(X)%*%X)%*%t(X)%*%y
H	<- X%*%(solve(t(X)%*%X))%*%t(X)
e	<- (I-H)%*%y
MSE	<- (t(e)%*%e)/(n-p)
SE	<- sqrt(diag((solve(t(X)%*%X)%x%MSE)))
t	<- bh/SE
p_v	<- 2*(1-pt(abs(t),n-p))
p_v10	<- p_v[p,]

### X11
X	<- cbind(u,X0[,c(15,11)])
p	<- ncol(X)
bh	<- solve(t(X)%*%X)%*%t(X)%*%y
H	<- X%*%(solve(t(X)%*%X))%*%t(X)
e	<- (I-H)%*%y
MSE	<- (t(e)%*%e)/(n-p)
SE	<- sqrt(diag((solve(t(X)%*%X)%x%MSE)))
t	<- bh/SE
p_v	<- 2*(1-pt(abs(t),n-p))
p_v11	<- p_v[p,]

### X12
X	<- cbind(u,X0[,c(15,12)])
p	<- ncol(X)
bh	<- solve(t(X)%*%X)%*%t(X)%*%y
H	<- X%*%(solve(t(X)%*%X))%*%t(X)
e	<- (I-H)%*%y
MSE	<- (t(e)%*%e)/(n-p)
SE	<- sqrt(diag((solve(t(X)%*%X)%x%MSE)))
t	<- bh/SE
p_v	<- 2*(1-pt(abs(t),n-p))
p_v12 <- p_v[p,]

### X13
X	<- cbind(u,X0[,c(15,13)])
p	<- ncol(X)
bh	<- solve(t(X)%*%X)%*%t(X)%*%y
H	<- X%*%(solve(t(X)%*%X))%*%t(X)
e	<- (I-H)%*%y
MSE	<- (t(e)%*%e)/(n-p)
SE	<- sqrt(diag((solve(t(X)%*%X)%x%MSE)))
t	<- bh/SE
p_v	<- 2*(1-pt(abs(t),n-p))
p_v13	<- p_v[p,]

### X14
X	<- cbind(u,X0[,c(15,14)])
p	<- ncol(X)
bh	<- solve(t(X)%*%X)%*%t(X)%*%y
H	<- X%*%(solve(t(X)%*%X))%*%t(X)
e	<- (I-H)%*%y
MSE	<- (t(e)%*%e)/(n-p)
SE	<- sqrt(diag((solve(t(X)%*%X)%x%MSE)))
t	<- bh/SE
p_v	<- 2*(1-pt(abs(t),n-p))
p_v14	<- p_v[p,]

### X16
X	<- cbind(u,X0[,c(15,16)])
p	<- ncol(X)
bh	<- solve(t(X)%*%X)%*%t(X)%*%y
H	<- X%*%(solve(t(X)%*%X))%*%t(X)
e	<- (I-H)%*%y
MSE	<- (t(e)%*%e)/(n-p)
SE	<- sqrt(diag((solve(t(X)%*%X)%x%MSE)))
t	<- bh/SE
p_v	<- 2*(1-pt(abs(t),n-p))
p_v16	<- p_v[p,]

### X17
X	<- cbind(u,X0[,c(15,17)])
p	<- ncol(X)
bh	<- solve(t(X)%*%X)%*%t(X)%*%y
H	<- X%*%(solve(t(X)%*%X))%*%t(X)
e	<- (I-H)%*%y
MSE	<- (t(e)%*%e)/(n-p)
SE	<- sqrt(diag((solve(t(X)%*%X)%x%MSE)))
t	<- bh/SE
p_v	<- 2*(1-pt(abs(t),n-p))
p_v17	<- p_v[p,]

### X18
X	<- cbind(u,X0[,c(15,18)])
p	<- ncol(X)
bh	<- solve(t(X)%*%X)%*%t(X)%*%y
H	<- X%*%(solve(t(X)%*%X))%*%t(X)
e	<- (I-H)%*%y
MSE	<- (t(e)%*%e)/(n-p)
SE	<- sqrt(diag((solve(t(X)%*%X)%x%MSE)))
t	<- bh/SE
p_v	<- 2*(1-pt(abs(t),n-p))
p_v18	<- p_v[p,]

### X19
X	<- cbind(u,X0[,c(15,19)])
p	<- ncol(X)
bh	<- solve(t(X)%*%X)%*%t(X)%*%y
H	<- X%*%(solve(t(X)%*%X))%*%t(X)
e	<- (I-H)%*%y
MSE	<- (t(e)%*%e)/(n-p)
SE	<- sqrt(diag((solve(t(X)%*%X)%x%MSE)))
t	<- bh/SE
p_v	<- 2*(1-pt(abs(t),n-p))
p_v19	<- p_v[p,]

### X20
X	<- cbind(u,X0[,c(15,20)])
p	<- ncol(X)
bh	<- solve(t(X)%*%X)%*%t(X)%*%y
H	<- X%*%(solve(t(X)%*%X))%*%t(X)
e	<- (I-H)%*%y
MSE	<- (t(e)%*%e)/(n-p)
SE	<- sqrt(diag((solve(t(X)%*%X)%x%MSE)))
t	<- bh/SE
p_v	<- 2*(1-pt(abs(t),n-p))
p_v20	<- p_v[p,]

### X21
X	<- cbind(u,X0[,c(15,21)])
p	<- ncol(X)
bh	<- solve(t(X)%*%X)%*%t(X)%*%y
H	<- X%*%(solve(t(X)%*%X))%*%t(X)
e	<- (I-H)%*%y
MSE	<- (t(e)%*%e)/(n-p)
SE	<- sqrt(diag((solve(t(X)%*%X)%x%MSE)))
t	<- bh/SE
p_v	<- 2*(1-pt(abs(t),n-p))
p_v21	<- p_v[p,]

### X22
X	<- cbind(u,X0[,c(15,22)])
p	<- ncol(X)
bh	<- solve(t(X)%*%X)%*%t(X)%*%y
H	<- X%*%(solve(t(X)%*%X))%*%t(X)
e	<- (I-H)%*%y
MSE	<- (t(e)%*%e)/(n-p)
SE	<- sqrt(diag((solve(t(X)%*%X)%x%MSE)))
t	<- bh/SE
p_v	<- 2*(1-pt(abs(t),n-p))
p_v22	<- p_v[p,]

### X23
X	<- cbind(u,X0[,c(15,23)])
p	<- ncol(X)
bh	<- solve(t(X)%*%X)%*%t(X)%*%y
H	<- X%*%(solve(t(X)%*%X))%*%t(X)
e	<- (I-H)%*%y
MSE	<- (t(e)%*%e)/(n-p)
SE	<- sqrt(diag((solve(t(X)%*%X)%x%MSE)))
t	<- bh/SE
p_v	<- 2*(1-pt(abs(t),n-p))
p_v23 <- p_v[p,]

### X24
X	<- cbind(u,X0[,c(15,24)])
p	<- ncol(X)
bh	<- solve(t(X)%*%X)%*%t(X)%*%y
H	<- X%*%(solve(t(X)%*%X))%*%t(X)
e	<- (I-H)%*%y
MSE	<- (t(e)%*%e)/(n-p)
SE	<- sqrt(diag((solve(t(X)%*%X)%x%MSE)))
t	<- bh/SE
p_v	<- 2*(1-pt(abs(t),n-p))
p_v24	<- p_v[p,]

### X25
X	<- cbind(u,X0[,c(15,25)])
p	<- ncol(X)
bh	<- solve(t(X)%*%X)%*%t(X)%*%y
H	<- X%*%(solve(t(X)%*%X))%*%t(X)
e	<- (I-H)%*%y
MSE	<- (t(e)%*%e)/(n-p)
SE	<- sqrt(diag((solve(t(X)%*%X)%x%MSE)))
t	<- bh/SE
p_v	<- 2*(1-pt(abs(t),n-p))
p_v25	<- p_v[p,]

p_v1
p_v2
p_v3
p_v4
p_v5
p_v6
p_v7
p_v8
p_v9
p_v10
p_v11
p_v12
p_v13
p_v14

p_v16
p_v17
p_v18
p_v19
p_v20
p_v21
p_v22
p_v23
p_v24
p_v25

#### p_v7= p_v_min -> X7 masuk

##Step2
### X7
X	<- cbind(u,X0[,c(15,7)])
p	<- ncol(X)
bh	<- solve(t(X)%*%X)%*%t(X)%*%y
H	<- X%*%(solve(t(X)%*%X))%*%t(X)
e	<- (I-H)%*%y
MSE	<- (t(e)%*%e)/(n-p)
SE	<- sqrt(diag((solve(t(X)%*%X)%x%MSE)))
t	<- bh/SE
p_v	<- 2*(1-pt(abs(t),n-p))
p_v

#### bh(15) sig: X15 tetap dalam model

#Cycle 3
##Step 1
### X1
X	<- cbind(u,X0[,c(15,7,1)])
p	<- ncol(X)
bh	<- solve(t(X)%*%X)%*%t(X)%*%y
H	<- X%*%(solve(t(X)%*%X))%*%t(X)
e	<- (I-H)%*%y
MSE	<- (t(e)%*%e)/(n-p)
SE	<- sqrt(diag((solve(t(X)%*%X)%x%MSE)))
t	<- bh/SE
p_v	<- 2*(1-pt(abs(t),n-p))
p_v1	<- p_v[p,]

### X2
X	<- cbind(u,X0[,c(15,7,2)])
p	<- ncol(X)
bh	<- solve(t(X)%*%X)%*%t(X)%*%y
H	<- X%*%(solve(t(X)%*%X))%*%t(X)
e	<- (I-H)%*%y
MSE	<- (t(e)%*%e)/(n-p)
SE	<- sqrt(diag((solve(t(X)%*%X)%x%MSE)))
t	<- bh/SE
p_v	<- 2*(1-pt(abs(t),n-p))
p_v2	<- p_v[p,]

### X3
X	<- cbind(u,X0[,c(15,7,3)])
p	<- ncol(X)
bh	<- solve(t(X)%*%X)%*%t(X)%*%y
H	<- X%*%(solve(t(X)%*%X))%*%t(X)
e	<- (I-H)%*%y
MSE	<- (t(e)%*%e)/(n-p)
SE	<- sqrt(diag((solve(t(X)%*%X)%x%MSE)))
t	<- bh/SE
p_v	<- 2*(1-pt(abs(t),n-p))
p_v3	<- p_v[p,]

### X4
X	<- cbind(u,X0[,c(15,7,4)])
p	<- ncol(X)
bh	<- solve(t(X)%*%X)%*%t(X)%*%y
H	<- X%*%(solve(t(X)%*%X))%*%t(X)
e	<- (I-H)%*%y
MSE	<- (t(e)%*%e)/(n-p)
SE	<- sqrt(diag((solve(t(X)%*%X)%x%MSE)))
t	<- bh/SE
p_v	<- 2*(1-pt(abs(t),n-p))
p_v4	<- p_v[p,]

### X5
X	<- cbind(u,X0[,c(15,7,5)])
p	<- ncol(X)
bh	<- solve(t(X)%*%X)%*%t(X)%*%y
H	<- X%*%(solve(t(X)%*%X))%*%t(X)
e	<- (I-H)%*%y
MSE	<- (t(e)%*%e)/(n-p)
SE	<- sqrt(diag((solve(t(X)%*%X)%x%MSE)))
t	<- bh/SE
p_v	<- 2*(1-pt(abs(t),n-p))
p_v5	<- p_v[p,]

### X6
X	<- cbind(u,X0[,c(15,7,6)])
p	<- ncol(X)
bh	<- solve(t(X)%*%X)%*%t(X)%*%y
H	<- X%*%(solve(t(X)%*%X))%*%t(X)
e	<- (I-H)%*%y
MSE	<- (t(e)%*%e)/(n-p)
SE	<- sqrt(diag((solve(t(X)%*%X)%x%MSE)))
t	<- bh/SE
p_v	<- 2*(1-pt(abs(t),n-p))
p_v6	<- p_v[p,]

### X8
X	<- cbind(u,X0[,c(15,7,8)])
p	<- ncol(X)
bh	<- solve(t(X)%*%X)%*%t(X)%*%y
H	<- X%*%(solve(t(X)%*%X))%*%t(X)
e	<- (I-H)%*%y
MSE	<- (t(e)%*%e)/(n-p)
SE	<- sqrt(diag((solve(t(X)%*%X)%x%MSE)))
t	<- bh/SE
p_v	<- 2*(1-pt(abs(t),n-p))
p_v8	<- p_v[p,]

### X9
X	<- cbind(u,X0[,c(15,7,9)])
p	<- ncol(X)
bh	<- solve(t(X)%*%X)%*%t(X)%*%y
H	<- X%*%(solve(t(X)%*%X))%*%t(X)
e	<- (I-H)%*%y
MSE	<- (t(e)%*%e)/(n-p)
SE	<- sqrt(diag((solve(t(X)%*%X)%x%MSE)))
t	<- bh/SE
p_v	<- 2*(1-pt(abs(t),n-p))
p_v9	<- p_v[p,]

### X10
X	<- cbind(u,X0[,c(15,7,10)])
p	<- ncol(X)
bh	<- solve(t(X)%*%X)%*%t(X)%*%y
H	<- X%*%(solve(t(X)%*%X))%*%t(X)
e	<- (I-H)%*%y
MSE	<- (t(e)%*%e)/(n-p)
SE	<- sqrt(diag((solve(t(X)%*%X)%x%MSE)))
t	<- bh/SE
p_v	<- 2*(1-pt(abs(t),n-p))
p_v10	<- p_v[p,]

### X11
X	<- cbind(u,X0[,c(15,7,11)])
p	<- ncol(X)
bh	<- solve(t(X)%*%X)%*%t(X)%*%y
H	<- X%*%(solve(t(X)%*%X))%*%t(X)
e	<- (I-H)%*%y
MSE	<- (t(e)%*%e)/(n-p)
SE	<- sqrt(diag((solve(t(X)%*%X)%x%MSE)))
t	<- bh/SE
p_v	<- 2*(1-pt(abs(t),n-p))
p_v11	<- p_v[p,]

### X12
X	<- cbind(u,X0[,c(15,7,12)])
p	<- ncol(X)
bh	<- solve(t(X)%*%X)%*%t(X)%*%y
H	<- X%*%(solve(t(X)%*%X))%*%t(X)
e	<- (I-H)%*%y
MSE	<- (t(e)%*%e)/(n-p)
SE	<- sqrt(diag((solve(t(X)%*%X)%x%MSE)))
t	<- bh/SE
p_v	<- 2*(1-pt(abs(t),n-p))
p_v12 <- p_v[p,]

### X13
X	<- cbind(u,X0[,c(15,7,13)])
p	<- ncol(X)
bh	<- solve(t(X)%*%X)%*%t(X)%*%y
H	<- X%*%(solve(t(X)%*%X))%*%t(X)
e	<- (I-H)%*%y
MSE	<- (t(e)%*%e)/(n-p)
SE	<- sqrt(diag((solve(t(X)%*%X)%x%MSE)))
t	<- bh/SE
p_v	<- 2*(1-pt(abs(t),n-p))
p_v13 <- p_v[p,]

### X14
X	<- cbind(u,X0[,c(15,7,14)])
p	<- ncol(X)
bh	<- solve(t(X)%*%X)%*%t(X)%*%y
H	<- X%*%(solve(t(X)%*%X))%*%t(X)
e	<- (I-H)%*%y
MSE	<- (t(e)%*%e)/(n-p)
SE	<- sqrt(diag((solve(t(X)%*%X)%x%MSE)))
t	<- bh/SE
p_v	<- 2*(1-pt(abs(t),n-p))
p_v14 <- p_v[p,]

### X16
X	<- cbind(u,X0[,c(15,7,16)])
p	<- ncol(X)
bh	<- solve(t(X)%*%X)%*%t(X)%*%y
H	<- X%*%(solve(t(X)%*%X))%*%t(X)
e	<- (I-H)%*%y
MSE	<- (t(e)%*%e)/(n-p)
SE	<- sqrt(diag((solve(t(X)%*%X)%x%MSE)))
t	<- bh/SE
p_v	<- 2*(1-pt(abs(t),n-p))
p_v16 <- p_v[p,]

### X17
X	<- cbind(u,X0[,c(15,7,17)])
p	<- ncol(X)
bh	<- solve(t(X)%*%X)%*%t(X)%*%y
H	<- X%*%(solve(t(X)%*%X))%*%t(X)
e	<- (I-H)%*%y
MSE	<- (t(e)%*%e)/(n-p)
SE	<- sqrt(diag((solve(t(X)%*%X)%x%MSE)))
t	<- bh/SE
p_v	<- 2*(1-pt(abs(t),n-p))
p_v17	<- p_v[p,]

### X18
X	<- cbind(u,X0[,c(15,7,18)])
p	<- ncol(X)
bh	<- solve(t(X)%*%X)%*%t(X)%*%y
H	<- X%*%(solve(t(X)%*%X))%*%t(X)
e	<- (I-H)%*%y
MSE	<- (t(e)%*%e)/(n-p)
SE	<- sqrt(diag((solve(t(X)%*%X)%x%MSE)))
t	<- bh/SE
p_v	<- 2*(1-pt(abs(t),n-p))
p_v18	<- p_v[p,]

### X19
X	<- cbind(u,X0[,c(15,7,19)])
p	<- ncol(X)
bh	<- solve(t(X)%*%X)%*%t(X)%*%y
H	<- X%*%(solve(t(X)%*%X))%*%t(X)
e	<- (I-H)%*%y
MSE	<- (t(e)%*%e)/(n-p)
SE	<- sqrt(diag((solve(t(X)%*%X)%x%MSE)))
t	<- bh/SE
p_v	<- 2*(1-pt(abs(t),n-p))
p_v19	<- p_v[p,]

### X20
X	<- cbind(u,X0[,c(15,7,20)])
p	<- ncol(X)
bh	<- solve(t(X)%*%X)%*%t(X)%*%y
H	<- X%*%(solve(t(X)%*%X))%*%t(X)
e	<- (I-H)%*%y
MSE	<- (t(e)%*%e)/(n-p)
SE	<- sqrt(diag((solve(t(X)%*%X)%x%MSE)))
t	<- bh/SE
p_v	<- 2*(1-pt(abs(t),n-p))
p_v20	<- p_v[p,]

### X21
X	<- cbind(u,X0[,c(15,7,21)])
p	<- ncol(X)
bh	<- solve(t(X)%*%X)%*%t(X)%*%y
H	<- X%*%(solve(t(X)%*%X))%*%t(X)
e	<- (I-H)%*%y
MSE	<- (t(e)%*%e)/(n-p)
SE	<- sqrt(diag((solve(t(X)%*%X)%x%MSE)))
t	<- bh/SE
p_v	<- 2*(1-pt(abs(t),n-p))
p_v21	<- p_v[p,]

### X22
X	<- cbind(u,X0[,c(15,7,22)])
p	<- ncol(X)
bh	<- solve(t(X)%*%X)%*%t(X)%*%y
H	<- X%*%(solve(t(X)%*%X))%*%t(X)
e	<- (I-H)%*%y
MSE	<- (t(e)%*%e)/(n-p)
SE	<- sqrt(diag((solve(t(X)%*%X)%x%MSE)))
t	<- bh/SE
p_v	<- 2*(1-pt(abs(t),n-p))
p_v22	<- p_v[p,]

### X23
X	<- cbind(u,X0[,c(15,7,23)])
p	<- ncol(X)
bh	<- solve(t(X)%*%X)%*%t(X)%*%y
H	<- X%*%(solve(t(X)%*%X))%*%t(X)
e	<- (I-H)%*%y
MSE	<- (t(e)%*%e)/(n-p)
SE	<- sqrt(diag((solve(t(X)%*%X)%x%MSE)))
t	<- bh/SE
p_v	<- 2*(1-pt(abs(t),n-p))
p_v23 <- p_v[p,]

### X24
X	<- cbind(u,X0[,c(15,7,24)])
p	<- ncol(X)
bh	<- solve(t(X)%*%X)%*%t(X)%*%y
H	<- X%*%(solve(t(X)%*%X))%*%t(X)
e	<- (I-H)%*%y
MSE	<- (t(e)%*%e)/(n-p)
SE	<- sqrt(diag((solve(t(X)%*%X)%x%MSE)))
t	<- bh/SE
p_v	<- 2*(1-pt(abs(t),n-p))
p_v24	<- p_v[p,]

### X25
X	<- cbind(u,X0[,c(15,7,25)])
p	<- ncol(X)
bh	<- solve(t(X)%*%X)%*%t(X)%*%y
H	<- X%*%(solve(t(X)%*%X))%*%t(X)
e	<- (I-H)%*%y
MSE	<- (t(e)%*%e)/(n-p)
SE	<- sqrt(diag((solve(t(X)%*%X)%x%MSE)))
t	<- bh/SE
p_v	<- 2*(1-pt(abs(t),n-p))
p_v25	<- p_v[p,]

p_v1
p_v2
p_v3
p_v4
p_v5
p_v6

p_v8
p_v9
p_v10
p_v11
p_v12
p_v13
p_v14

p_v16
p_v17
p_v18
p_v19
p_v20
p_v21
p_v22
p_v23
p_v24
p_v25

#### p_v16 = p_v_min --> p_v16 masuk ke dalam model

##Step2
### X16
X	<- cbind(u,X0[,c(15,7,16)])
p	<- ncol(X)
bh	<- solve(t(X)%*%X)%*%t(X)%*%y
H	<- X%*%(solve(t(X)%*%X))%*%t(X)
e	<- (I-H)%*%y
MSE	<- (t(e)%*%e)/(n-p)
SE	<- sqrt(diag((solve(t(X)%*%X)%x%MSE)))
t	<- bh/SE
p_v	<- 2*(1-pt(abs(t),n-p))
p_v

#Cycle 4
##Step 1
### X1
X	<- cbind(u,X0[,c(16,15,7,1)])
p	<- ncol(X)
bh	<- solve(t(X)%*%X)%*%t(X)%*%y
H	<- X%*%(solve(t(X)%*%X))%*%t(X)
e	<- (I-H)%*%y
MSE	<- (t(e)%*%e)/(n-p)
SE	<- sqrt(diag((solve(t(X)%*%X)%x%MSE)))
t	<- bh/SE
p_v	<- 2*(1-pt(abs(t),n-p))
p_v1	<- p_v[p,]

### X2
X	<- cbind(u,X0[,c(16,15,7,2)])
p	<- ncol(X)
bh	<- solve(t(X)%*%X)%*%t(X)%*%y
H	<- X%*%(solve(t(X)%*%X))%*%t(X)
e	<- (I-H)%*%y
MSE	<- (t(e)%*%e)/(n-p)
SE	<- sqrt(diag((solve(t(X)%*%X)%x%MSE)))
t	<- bh/SE
p_v	<- 2*(1-pt(abs(t),n-p))
p_v2	<- p_v[p,]

### X3
X	<- cbind(u,X0[,c(16,15,7,3)])
p	<- ncol(X)
bh	<- solve(t(X)%*%X)%*%t(X)%*%y
H	<- X%*%(solve(t(X)%*%X))%*%t(X)
e	<- (I-H)%*%y
MSE	<- (t(e)%*%e)/(n-p)
SE	<- sqrt(diag((solve(t(X)%*%X)%x%MSE)))
t	<- bh/SE
p_v	<- 2*(1-pt(abs(t),n-p))
p_v3	<- p_v[p,]

### X4
X	<- cbind(u,X0[,c(16,15,7,4)])
p	<- ncol(X)
bh	<- solve(t(X)%*%X)%*%t(X)%*%y
H	<- X%*%(solve(t(X)%*%X))%*%t(X)
e	<- (I-H)%*%y
MSE	<- (t(e)%*%e)/(n-p)
SE	<- sqrt(diag((solve(t(X)%*%X)%x%MSE)))
t	<- bh/SE
p_v	<- 2*(1-pt(abs(t),n-p))
p_v4	<- p_v[p,]

### X5
X	<- cbind(u,X0[,c(16,15,7,5)])
p	<- ncol(X)
bh	<- solve(t(X)%*%X)%*%t(X)%*%y
H	<- X%*%(solve(t(X)%*%X))%*%t(X)
e	<- (I-H)%*%y
MSE	<- (t(e)%*%e)/(n-p)
SE	<- sqrt(diag((solve(t(X)%*%X)%x%MSE)))
t	<- bh/SE
p_v	<- 2*(1-pt(abs(t),n-p))
p_v5	<- p_v[p,]

### X6
X	<- cbind(u,X0[,c(16,15,7,6)])
p	<- ncol(X)
bh	<- solve(t(X)%*%X)%*%t(X)%*%y
H	<- X%*%(solve(t(X)%*%X))%*%t(X)
e	<- (I-H)%*%y
MSE	<- (t(e)%*%e)/(n-p)
SE	<- sqrt(diag((solve(t(X)%*%X)%x%MSE)))
t	<- bh/SE
p_v	<- 2*(1-pt(abs(t),n-p))
p_v6	<- p_v[p,]

### X8
X	<- cbind(u,X0[,c(16,15,7,8)])
p	<- ncol(X)
bh	<- solve(t(X)%*%X)%*%t(X)%*%y
H	<- X%*%(solve(t(X)%*%X))%*%t(X)
e	<- (I-H)%*%y
MSE	<- (t(e)%*%e)/(n-p)
SE	<- sqrt(diag((solve(t(X)%*%X)%x%MSE)))
t	<- bh/SE
p_v	<- 2*(1-pt(abs(t),n-p))
p_v8	<- p_v[p,]

### X9
X	<- cbind(u,X0[,c(16,15,7,9)])
p	<- ncol(X)
bh	<- solve(t(X)%*%X)%*%t(X)%*%y
H	<- X%*%(solve(t(X)%*%X))%*%t(X)
e	<- (I-H)%*%y
MSE	<- (t(e)%*%e)/(n-p)
SE	<- sqrt(diag((solve(t(X)%*%X)%x%MSE)))
t	<- bh/SE
p_v	<- 2*(1-pt(abs(t),n-p))
p_v9	<- p_v[p,]

### X10
X	<- cbind(u,X0[,c(16,15,7,10)])
p	<- ncol(X)
bh	<- solve(t(X)%*%X)%*%t(X)%*%y
H	<- X%*%(solve(t(X)%*%X))%*%t(X)
e	<- (I-H)%*%y
MSE	<- (t(e)%*%e)/(n-p)
SE	<- sqrt(diag((solve(t(X)%*%X)%x%MSE)))
t	<- bh/SE
p_v	<- 2*(1-pt(abs(t),n-p))
p_v10	<- p_v[p,]

### X11
X	<- cbind(u,X0[,c(16,15,7,11)])
p	<- ncol(X)
bh	<- solve(t(X)%*%X)%*%t(X)%*%y
H	<- X%*%(solve(t(X)%*%X))%*%t(X)
e	<- (I-H)%*%y
MSE	<- (t(e)%*%e)/(n-p)
SE	<- sqrt(diag((solve(t(X)%*%X)%x%MSE)))
t	<- bh/SE
p_v	<- 2*(1-pt(abs(t),n-p))
p_v11	<- p_v[p,]

### X12
X	<- cbind(u,X0[,c(16,15,7,12)])
p	<- ncol(X)
bh	<- solve(t(X)%*%X)%*%t(X)%*%y
H	<- X%*%(solve(t(X)%*%X))%*%t(X)
e	<- (I-H)%*%y
MSE	<- (t(e)%*%e)/(n-p)
SE	<- sqrt(diag((solve(t(X)%*%X)%x%MSE)))
t	<- bh/SE
p_v	<- 2*(1-pt(abs(t),n-p))
p_v12 <- p_v[p,]

### X13
X	<- cbind(u,X0[,c(16,15,7,13)])
p	<- ncol(X)
bh	<- solve(t(X)%*%X)%*%t(X)%*%y
H	<- X%*%(solve(t(X)%*%X))%*%t(X)
e	<- (I-H)%*%y
MSE	<- (t(e)%*%e)/(n-p)
SE	<- sqrt(diag((solve(t(X)%*%X)%x%MSE)))
t	<- bh/SE
p_v	<- 2*(1-pt(abs(t),n-p))
p_v13 <- p_v[p,]

### X14
X	<- cbind(u,X0[,c(16,15,7,14)])
p	<- ncol(X)
bh	<- solve(t(X)%*%X)%*%t(X)%*%y
H	<- X%*%(solve(t(X)%*%X))%*%t(X)
e	<- (I-H)%*%y
MSE	<- (t(e)%*%e)/(n-p)
SE	<- sqrt(diag((solve(t(X)%*%X)%x%MSE)))
t	<- bh/SE
p_v	<- 2*(1-pt(abs(t),n-p))
p_v14 <- p_v[p,]

### X17
X	<- cbind(u,X0[,c(16,15,7,17)])
p	<- ncol(X)
bh	<- solve(t(X)%*%X)%*%t(X)%*%y
H	<- X%*%(solve(t(X)%*%X))%*%t(X)
e	<- (I-H)%*%y
MSE	<- (t(e)%*%e)/(n-p)
SE	<- sqrt(diag((solve(t(X)%*%X)%x%MSE)))
t	<- bh/SE
p_v	<- 2*(1-pt(abs(t),n-p))
p_v17 <- p_v[p,]

### X18
X	<- cbind(u,X0[,c(16,15,7,18)])
p	<- ncol(X)
bh	<- solve(t(X)%*%X)%*%t(X)%*%y
H	<- X%*%(solve(t(X)%*%X))%*%t(X)
e	<- (I-H)%*%y
MSE	<- (t(e)%*%e)/(n-p)
SE	<- sqrt(diag((solve(t(X)%*%X)%x%MSE)))
t	<- bh/SE
p_v	<- 2*(1-pt(abs(t),n-p))
p_v18	<- p_v[p,]

### X19
X	<- cbind(u,X0[,c(16,15,7,19)])
p	<- ncol(X)
bh	<- solve(t(X)%*%X)%*%t(X)%*%y
H	<- X%*%(solve(t(X)%*%X))%*%t(X)
e	<- (I-H)%*%y
MSE	<- (t(e)%*%e)/(n-p)
SE	<- sqrt(diag((solve(t(X)%*%X)%x%MSE)))
t	<- bh/SE
p_v	<- 2*(1-pt(abs(t),n-p))
p_v19	<- p_v[p,]

### X20
X	<- cbind(u,X0[,c(16,15,7,20)])
p	<- ncol(X)
bh	<- solve(t(X)%*%X)%*%t(X)%*%y
H	<- X%*%(solve(t(X)%*%X))%*%t(X)
e	<- (I-H)%*%y
MSE	<- (t(e)%*%e)/(n-p)
SE	<- sqrt(diag((solve(t(X)%*%X)%x%MSE)))
t	<- bh/SE
p_v	<- 2*(1-pt(abs(t),n-p))
p_v20	<- p_v[p,]

### X21
X	<- cbind(u,X0[,c(16,15,7,21)])
p	<- ncol(X)
bh	<- solve(t(X)%*%X)%*%t(X)%*%y
H	<- X%*%(solve(t(X)%*%X))%*%t(X)
e	<- (I-H)%*%y
MSE	<- (t(e)%*%e)/(n-p)
SE	<- sqrt(diag((solve(t(X)%*%X)%x%MSE)))
t	<- bh/SE
p_v	<- 2*(1-pt(abs(t),n-p))
p_v21	<- p_v[p,]

### X22
X	<- cbind(u,X0[,c(16,15,7,22)])
p	<- ncol(X)
bh	<- solve(t(X)%*%X)%*%t(X)%*%y
H	<- X%*%(solve(t(X)%*%X))%*%t(X)
e	<- (I-H)%*%y
MSE	<- (t(e)%*%e)/(n-p)
SE	<- sqrt(diag((solve(t(X)%*%X)%x%MSE)))
t	<- bh/SE
p_v	<- 2*(1-pt(abs(t),n-p))
p_v22	<- p_v[p,]

### X23
X	<- cbind(u,X0[,c(16,15,7,23)])
p	<- ncol(X)
bh	<- solve(t(X)%*%X)%*%t(X)%*%y
H	<- X%*%(solve(t(X)%*%X))%*%t(X)
e	<- (I-H)%*%y
MSE	<- (t(e)%*%e)/(n-p)
SE	<- sqrt(diag((solve(t(X)%*%X)%x%MSE)))
t	<- bh/SE
p_v	<- 2*(1-pt(abs(t),n-p))
p_v23 <- p_v[p,]

### X24
X	<- cbind(u,X0[,c(16,15,7,24)])
p	<- ncol(X)
bh	<- solve(t(X)%*%X)%*%t(X)%*%y
H	<- X%*%(solve(t(X)%*%X))%*%t(X)
e	<- (I-H)%*%y
MSE	<- (t(e)%*%e)/(n-p)
SE	<- sqrt(diag((solve(t(X)%*%X)%x%MSE)))
t	<- bh/SE
p_v	<- 2*(1-pt(abs(t),n-p))
p_v24	<- p_v[p,]

### X25
X	<- cbind(u,X0[,c(16,15,7,25)])
p	<- ncol(X)
bh	<- solve(t(X)%*%X)%*%t(X)%*%y
H	<- X%*%(solve(t(X)%*%X))%*%t(X)
e	<- (I-H)%*%y
MSE	<- (t(e)%*%e)/(n-p)
SE	<- sqrt(diag((solve(t(X)%*%X)%x%MSE)))
t	<- bh/SE
p_v	<- 2*(1-pt(abs(t),n-p))
p_v25	<- p_v[p,]

p_v1
p_v2
p_v3
p_v4
p_v5
p_v6

p_v8
p_v9
p_v10
p_v11
p_v12
p_v13
p_v14


p_v17
p_v18
p_v19
p_v20
p_v21
p_v22
p_v23
p_v24
p_v25

#### p_v14 = p_v_min -> p_v14 masuk ke dalam model

##Step 2
###X14
X	<- cbind(u,X0[,c(16,15,7,14)])
p	<- ncol(X)
bh	<- solve(t(X)%*%X)%*%t(X)%*%y
H	<- X%*%(solve(t(X)%*%X))%*%t(X)
e	<- (I-H)%*%y
MSE	<- (t(e)%*%e)/(n-p)
SE	<- sqrt(diag((solve(t(X)%*%X)%x%MSE)))
t	<- bh/SE
p_v	<- 2*(1-pt(abs(t),n-p))
p_v

#Cycle 5
##Step 1
### X1
X	<- cbind(u,X0[,c(16,15,7,14,1)])
p	<- ncol(X)
bh	<- solve(t(X)%*%X)%*%t(X)%*%y
H	<- X%*%(solve(t(X)%*%X))%*%t(X)
e	<- (I-H)%*%y
MSE	<- (t(e)%*%e)/(n-p)
SE	<- sqrt(diag((solve(t(X)%*%X)%x%MSE)))
t	<- bh/SE
p_v	<- 2*(1-pt(abs(t),n-p))
p_v1	<- p_v[p,]

### X2
X	<- cbind(u,X0[,c(16,15,7,14,2)])
p	<- ncol(X)
bh	<- solve(t(X)%*%X)%*%t(X)%*%y
H	<- X%*%(solve(t(X)%*%X))%*%t(X)
e	<- (I-H)%*%y
MSE	<- (t(e)%*%e)/(n-p)
SE	<- sqrt(diag((solve(t(X)%*%X)%x%MSE)))
t	<- bh/SE
p_v	<- 2*(1-pt(abs(t),n-p))
p_v2	<- p_v[p,]

### X3
X	<- cbind(u,X0[,c(16,15,7,14,3)])
p	<- ncol(X)
bh	<- solve(t(X)%*%X)%*%t(X)%*%y
H	<- X%*%(solve(t(X)%*%X))%*%t(X)
e	<- (I-H)%*%y
MSE	<- (t(e)%*%e)/(n-p)
SE	<- sqrt(diag((solve(t(X)%*%X)%x%MSE)))
t	<- bh/SE
p_v	<- 2*(1-pt(abs(t),n-p))
p_v3	<- p_v[p,]

### X4
X	<- cbind(u,X0[,c(16,15,7,14,4)])
p	<- ncol(X)
bh	<- solve(t(X)%*%X)%*%t(X)%*%y
H	<- X%*%(solve(t(X)%*%X))%*%t(X)
e	<- (I-H)%*%y
MSE	<- (t(e)%*%e)/(n-p)
SE	<- sqrt(diag((solve(t(X)%*%X)%x%MSE)))
t	<- bh/SE
p_v	<- 2*(1-pt(abs(t),n-p))
p_v4	<- p_v[p,]

### X5
X	<- cbind(u,X0[,c(16,15,7,14,5)])
p	<- ncol(X)
bh	<- solve(t(X)%*%X)%*%t(X)%*%y
H	<- X%*%(solve(t(X)%*%X))%*%t(X)
e	<- (I-H)%*%y
MSE	<- (t(e)%*%e)/(n-p)
SE	<- sqrt(diag((solve(t(X)%*%X)%x%MSE)))
t	<- bh/SE
p_v	<- 2*(1-pt(abs(t),n-p))
p_v5	<- p_v[p,]

### X6
X	<- cbind(u,X0[,c(16,15,7,14,6)])
p	<- ncol(X)
bh	<- solve(t(X)%*%X)%*%t(X)%*%y
H	<- X%*%(solve(t(X)%*%X))%*%t(X)
e	<- (I-H)%*%y
MSE	<- (t(e)%*%e)/(n-p)
SE	<- sqrt(diag((solve(t(X)%*%X)%x%MSE)))
t	<- bh/SE
p_v	<- 2*(1-pt(abs(t),n-p))
p_v6	<- p_v[p,]

### X8
X	<- cbind(u,X0[,c(16,15,7,14,8)])
p	<- ncol(X)
bh	<- solve(t(X)%*%X)%*%t(X)%*%y
H	<- X%*%(solve(t(X)%*%X))%*%t(X)
e	<- (I-H)%*%y
MSE	<- (t(e)%*%e)/(n-p)
SE	<- sqrt(diag((solve(t(X)%*%X)%x%MSE)))
t	<- bh/SE
p_v	<- 2*(1-pt(abs(t),n-p))
p_v8	<- p_v[p,]

### X9
X	<- cbind(u,X0[,c(16,15,7,14,9)])
p	<- ncol(X)
bh	<- solve(t(X)%*%X)%*%t(X)%*%y
H	<- X%*%(solve(t(X)%*%X))%*%t(X)
e	<- (I-H)%*%y
MSE	<- (t(e)%*%e)/(n-p)
SE	<- sqrt(diag((solve(t(X)%*%X)%x%MSE)))
t	<- bh/SE
p_v	<- 2*(1-pt(abs(t),n-p))
p_v9	<- p_v[p,]

### X10
X	<- cbind(u,X0[,c(16,15,7,14,10)])
p	<- ncol(X)
bh	<- solve(t(X)%*%X)%*%t(X)%*%y
H	<- X%*%(solve(t(X)%*%X))%*%t(X)
e	<- (I-H)%*%y
MSE	<- (t(e)%*%e)/(n-p)
SE	<- sqrt(diag((solve(t(X)%*%X)%x%MSE)))
t	<- bh/SE
p_v	<- 2*(1-pt(abs(t),n-p))
p_v10	<- p_v[p,]

### X11
X	<- cbind(u,X0[,c(16,15,7,14,11)])
p	<- ncol(X)
bh	<- solve(t(X)%*%X)%*%t(X)%*%y
H	<- X%*%(solve(t(X)%*%X))%*%t(X)
e	<- (I-H)%*%y
MSE	<- (t(e)%*%e)/(n-p)
SE	<- sqrt(diag((solve(t(X)%*%X)%x%MSE)))
t	<- bh/SE
p_v	<- 2*(1-pt(abs(t),n-p))
p_v11	<- p_v[p,]

### X12
X	<- cbind(u,X0[,c(16,15,7,14,12)])
p	<- ncol(X)
bh	<- solve(t(X)%*%X)%*%t(X)%*%y
H	<- X%*%(solve(t(X)%*%X))%*%t(X)
e	<- (I-H)%*%y
MSE	<- (t(e)%*%e)/(n-p)
SE	<- sqrt(diag((solve(t(X)%*%X)%x%MSE)))
t	<- bh/SE
p_v	<- 2*(1-pt(abs(t),n-p))
p_v12	<- p_v[p,]

### X13
X	<- cbind(u,X0[,c(16,15,7,14,13)])
p	<- ncol(X)
bh	<- solve(t(X)%*%X)%*%t(X)%*%y
H	<- X%*%(solve(t(X)%*%X))%*%t(X)
e	<- (I-H)%*%y
MSE	<- (t(e)%*%e)/(n-p)
SE	<- sqrt(diag((solve(t(X)%*%X)%x%MSE)))
t	<- bh/SE
p_v	<- 2*(1-pt(abs(t),n-p))
p_v13	<- p_v[p,]

### X17
X	<- cbind(u,X0[,c(16,15,7,14,17)])
p	<- ncol(X)
bh	<- solve(t(X)%*%X)%*%t(X)%*%y
H	<- X%*%(solve(t(X)%*%X))%*%t(X)
e	<- (I-H)%*%y
MSE	<- (t(e)%*%e)/(n-p)
SE	<- sqrt(diag((solve(t(X)%*%X)%x%MSE)))
t	<- bh/SE
p_v	<- 2*(1-pt(abs(t),n-p))
p_v17	<- p_v[p,]

### X18
X	<- cbind(u,X0[,c(16,15,7,14,18)])
p	<- ncol(X)
bh	<- solve(t(X)%*%X)%*%t(X)%*%y
H	<- X%*%(solve(t(X)%*%X))%*%t(X)
e	<- (I-H)%*%y
MSE	<- (t(e)%*%e)/(n-p)
SE	<- sqrt(diag((solve(t(X)%*%X)%x%MSE)))
t	<- bh/SE
p_v	<- 2*(1-pt(abs(t),n-p))
p_v18	<- p_v[p,]

### X19
X	<- cbind(u,X0[,c(16,15,7,14,19)])
p	<- ncol(X)
bh	<- solve(t(X)%*%X)%*%t(X)%*%y
H	<- X%*%(solve(t(X)%*%X))%*%t(X)
e	<- (I-H)%*%y
MSE	<- (t(e)%*%e)/(n-p)
SE	<- sqrt(diag((solve(t(X)%*%X)%x%MSE)))
t	<- bh/SE
p_v	<- 2*(1-pt(abs(t),n-p))
p_v19	<- p_v[p,]

### X20
X	<- cbind(u,X0[,c(16,15,7,14,20)])
p	<- ncol(X)
bh	<- solve(t(X)%*%X)%*%t(X)%*%y
H	<- X%*%(solve(t(X)%*%X))%*%t(X)
e	<- (I-H)%*%y
MSE	<- (t(e)%*%e)/(n-p)
SE	<- sqrt(diag((solve(t(X)%*%X)%x%MSE)))
t	<- bh/SE
p_v	<- 2*(1-pt(abs(t),n-p))
p_v20	<- p_v[p,]

### X21
X	<- cbind(u,X0[,c(16,15,7,14,21)])
p	<- ncol(X)
bh	<- solve(t(X)%*%X)%*%t(X)%*%y
H	<- X%*%(solve(t(X)%*%X))%*%t(X)
e	<- (I-H)%*%y
MSE	<- (t(e)%*%e)/(n-p)
SE	<- sqrt(diag((solve(t(X)%*%X)%x%MSE)))
t	<- bh/SE
p_v	<- 2*(1-pt(abs(t),n-p))
p_v21	<- p_v[p,]

### X22
X	<- cbind(u,X0[,c(16,15,7,14,22)])
p	<- ncol(X)
bh	<- solve(t(X)%*%X)%*%t(X)%*%y
H	<- X%*%(solve(t(X)%*%X))%*%t(X)
e	<- (I-H)%*%y
MSE	<- (t(e)%*%e)/(n-p)
SE	<- sqrt(diag((solve(t(X)%*%X)%x%MSE)))
t	<- bh/SE
p_v	<- 2*(1-pt(abs(t),n-p))
p_v22	<- p_v[p,]

### X23
X	<- cbind(u,X0[,c(16,15,7,14,23)])
p	<- ncol(X)
bh	<- solve(t(X)%*%X)%*%t(X)%*%y
H	<- X%*%(solve(t(X)%*%X))%*%t(X)
e	<- (I-H)%*%y
MSE	<- (t(e)%*%e)/(n-p)
SE	<- sqrt(diag((solve(t(X)%*%X)%x%MSE)))
t	<- bh/SE
p_v	<- 2*(1-pt(abs(t),n-p))
p_v23	<- p_v[p,]

### X24
X	<- cbind(u,X0[,c(16,15,7,14,24)])
p	<- ncol(X)
bh	<- solve(t(X)%*%X)%*%t(X)%*%y
H	<- X%*%(solve(t(X)%*%X))%*%t(X)
e	<- (I-H)%*%y
MSE	<- (t(e)%*%e)/(n-p)
SE	<- sqrt(diag((solve(t(X)%*%X)%x%MSE)))
t	<- bh/SE
p_v	<- 2*(1-pt(abs(t),n-p))
p_v24	<- p_v[p,]

### X25
X	<- cbind(u,X0[,c(16,15,7,14,25)])
p	<- ncol(X)
bh	<- solve(t(X)%*%X)%*%t(X)%*%y
H	<- X%*%(solve(t(X)%*%X))%*%t(X)
e	<- (I-H)%*%y
MSE	<- (t(e)%*%e)/(n-p)
SE	<- sqrt(diag((solve(t(X)%*%X)%x%MSE)))
t	<- bh/SE
p_v	<- 2*(1-pt(abs(t),n-p))
p_v25	<- p_v[p,]

p_v1
p_v2
p_v3
p_v4
p_v5
p_v6

p_v8
p_v9
p_v10
p_v11
p_v12
p_v13



p_v17
p_v18
p_v19
p_v20
p_v21
p_v22
p_v23
p_v24
p_v25

#### p_v25 = p_v_min > 0.05 tidak ada yang masuk

### Model akhir
### X7, X14 X15, X16
X	<- cbind(u,X0[,c(7,14,15,16)])
p	<- ncol(X)
bh	<- solve(t(X)%*%X)%*%t(X)%*%y
H	<- X%*%(solve(t(X)%*%X))%*%t(X)
e	<- (I-H)%*%y
MSE	<- (t(e)%*%e)/(n-p)
SE	<- sqrt(diag((solve(t(X)%*%X)%x%MSE)))
t	<- bh/SE
p_v	<- 2*(1-pt(abs(t),n-p))
est	<- cbind(bh,t,p_v)
R2	<- 1-MSE/var(y)
est
R2

### Tidak ada yang dimasukan ke dalam model, model akhir adalah model dari cycle 5 langkah 1
### R2 0,0432 artinya 4,32%% varians dari respons variabel bisa dijelaskan oleh model X7,X14,X15,X16

# UJI ASUMSI KLASIK

## Uji Normalitas (Shapiro-Wilks Test)
### Hipotesis = H0 : data berdistribusi normal, H1 : data berdistribusi tidak normal
### Statistik Uji
shapiro.test(e)
### Kriteria Uji : tolak H0 jika p-value <= 0,05
### Keputusan : p-value < 0,05, maka H0 ditolak
### Kesimpulan : data berdistribusi tidak normal

## Uji Linearitas
### Hipotesis = H0 : model linier, H1 : model tidak linier
### Statistik Uji
library(lmtest)
resettest(y~X)
### Kriteria Uji : tolak H0 jika p-value <= 0,05
### Keputusan : p-value > 0,05, maka H0 diterima
### Kesimpulan : hubungan antara variabel X dan Y adalah hubungan linier

## Uji Homoskedastisitas (Breush-Pagan test)
### Hipotesis: H0: homoskedasitas, H1: heteroskedasitas
alpha <- 0.05
k <- p-1
yh <- e*e
e_h <- (I-X%*%solve(t(X)%*%X)%*%t(X))%*%yh
R2_h<- 1-(t(e_h)%*%e_h)/(n-1)/var(yh);R2_h
### Statistik uji
chi_h <- n*R2_h;chi_h
pv_h<- 1-pchisq(chi_h,k);pv_h
### titik kritis
chi_c <-qchisq(1-alpha,k);chi_c
### Keputusan: p-value<0.05 dan chi_h>chi_c maka H0 ditolak
### Kesimpulan: dengan derajat kepercayaan 95%, dapat disimpulkan bahwa data memenuhi heteroskedasitas

## Uji Signifikansi Parameter (Eksogenitas)
## Simultan
k <- matrix(1,nrow=n,ncol=n)
SSR <- t(y)%*%(H-k/n)%*%y
SSE <- t(y)%*%(I-H)%*%y
SSTO <- t(y)%*%(I-k/n)%*%y
MSR <- SSR/(p-1)
MSE <- SSE/(n-p)
### Statistik uji
F <- MSR/MSE;F
### Kriteria uji
alpha <- 0.05
fcrit <- qf(1-alpha,p-1,n-p);fcrit
pval <- 1-pf(F,p-1,n-p);pval
### Keputusan: Fhit > Ftabel dan p-value<0.05, H0 ditolak
### Kesimpulan : variabel X berpengaruh terhadap Y

## Parsial (Individual)
s2 <- (t(e)%*%e)/(n-p)
X_b <- cbind(u,X0)
s2_b <- s2[1,1]*solve(t(X_b)%*%X_b,tol=1e-25)
### Standar eror taksiran koefisien regresi
s_b <- sqrt(diag(s2_b))
## Statistik uji t dan p_value 2 arah
b_b <- solve(t(X_b)%*%X_b)%*%t(X_b)%*%y
t <- b/s_b;t
p_valueb <- 2*(matrix(c(1),1,1))-pt(abs(t),n-p);p_valueb
### titik kritis
tcrit <- qt(1-alpha/2,n-p);tcrit
### Kriteria uji : tolak h0 jika |t|>= t(1-alpha/2;n-p)
### Keputusan : H0 ditolak
### Kesimpulan : variabel X berpengaruh signifikan/nyata terhadap Y

## OUTLIER
SSE	<- t(y)%*%(I-H)%*%y
p	<- ncol(X)
s2	<- SSE/(n-p)
s	<- sqrt(s2)
e	<- (I-H)%*%y
## Semi Studentized residual
es	<- e/s[1,1]
## Studentized residual
r	<- e/matrix(c(s),n,1)/sqrt(diag(I-H))
r1	<- e/s[1,1]/sqrt(diag(I-H))
## Deleted residual
d	<- e/diag(I-H)
## Studentized deleted residual
MSEi	<- (matrix(c(SSE),n,1)-e*e/diag(I-H))/(n-p-1)
vd	<- MSEi/diag(I-H)
t	<- d/sqrt(vd);t
alpha	<- 0.05
t_cri	<- qt(1-alpha/n/2,n-p-1);t_cri
isoutlier <- abs(t)>=t_cri ; isoutlier
sum(isoutlier== 'FALSE')

#MODEL TERBAIK
bh
