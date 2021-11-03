library(Matrix)
library(usethis)
library(testthat)

# matriz densa vs difusa ----
load("~/github/netmem/data/krackhardt_friends.rda")
d <- krackhardt_friends

A = as.matrix(d)

A2 = Matrix::Matrix(d)

class(A)
class(A2)

l1 <- length(as.numeric(unlist(A)))
l2 <- as.numeric(unlist(A2))
l2 <- l2[l2 > 0]
length(l2)

# agrego dependencias ----
usethis::use_package("Matrix")
usethis::use_package("testthat", "Suggests")
usethis::use_package("usethis", "Suggests")
usethis::use_package("styler", "Suggests")

# ignoro carpeta dev
use_build_ignore("dev")

# agrego pruebas para ver si acado triad_uman_2 se comporta igual
use_test("triad_uman_2")

# en la prueba hay un error 
# Error in as.matrix(A) : object 'krackhardt_friends' not found

# lo soluciona con lazydata
# agrego LazyData: true al DESCRIPTION

## errores en triad_uman_2
# diag, t - > Matrix tiene su propia funcion

# en L1441 da un error pues dim(Eb) es NULL
# en la func original (L1437) sumo dos matrices de 21x21
# en la func optimizada (L1439) E queda como un vector de 441 elementos
# tengo que ver que pasó con A y t(A)

# esto de acá viene directo de selecccionar dentro de la fun y ejecutar hasta la L1439
class(A)
dim(A)

class(Matrix::t(A))
dim(Matrix::t(A))

# .: como A y A' estan ok, voy a generar E por partes
# paso 1: sumar A y A'
# paso 2: ver clase y dim
class(E)
dim(E)

# como esta todo OK, voy a cambir los ifelse por filtros matriciales (+ rapido, - memoria)

# ahora L1453 da error
# dim de e1 y e2 incompatible

# objeto t201 no se crea x error
# veo la dim de M
dim(M)
# no dim -> problema 
# sol: cambio L1450

# t201 orig vale 40
# reviso si M y Eb son iguales en la fun orig

# tomo M y Eb de func orig
M3 = Matrix::Matrix(M)
Eb3 = Matrix::Matrix(Eb)

# guardo los environments con el output de cada funcion

# ahora dejo la mat A optim como A2
load("~/github/netmem/dev/A-optim.rda")
A2 = A

# cargo el envir orig
load("~/github/netmem/dev/output-triad-orig.RData")

# comparo A con con A2... debe ser matriz 0
Matrix(A) - A2

# veo A'
Matrix::t(Matrix(A)) - Matrix::t(A2)

# comparo M orgin con M optim
# M <- A + t(A)
M <- ifelse((A + t(A)) > 1, 1, 0) 

M2 <- A2 + Matrix::t(A2)
M2[M2 > 1] <- 1

Matrix(M) - M2

# como la A y A2 son -es, el problema es la creacion de M vs M2
# Veo la forma equivalente de crear M
M <- A + t(A)
M[M > 1] <- 1

M3 <- ifelse((A + t(A)) > 1, 1, 0) 

M - M3
# esto deberia ser matrix 0, pero no

# ahora agrego la 2da cond del ifelse de forma explicita
# M <- ifelse((A + t(A)) > 1, 1, 0) 
M <- A + t(A)
M[M > 1] <- 1
M[M < 1] <- 0

M - M3

# al parecer hay una "trampa" con el ifelse
z = matrix(c(1,0,0,1), nrow = 2, ncol = 2)

ifelse((z + t(z)) > 1, 1, 0)

# hay un problema raro con el ifelse... en el test todo funciona menos OBS

# veo de nuevo el output parcial de la fun optim
load("~/github/netmem/dev/output-triad-optim.RData")

M <- A + Matrix::t(A)
A[7,1]
Matrix::t(A)[7,1]

M[M > 1] <- 1

# el ifelse reemplaza los 1 "originales" por 0 
# entonces tengo que "pintar" los 1 "originales" tal como en la Eb (L1445)
M <- A + Matrix::t(A)
M[M == 1] <- -999
M[M > 1] <- 1
M[M < 1] <- 0
M

# ahora llevo esto a la fun optim
# este M da el t201 = 40 correcto !!

