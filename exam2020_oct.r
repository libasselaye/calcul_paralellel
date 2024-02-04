## Fichier: 2020pc4ds-oct.r
## Etudiant : Mame Libasse MBOUP
## Description : Rendu de l'examen du PC4DS 2020
## Date : 26 octobre 2020

rm(list = ls())

################################################################################
####                                                                        ####
####                       E X E R C I C E   1                              ####
####                                                                        ####
################################################################################

#Veuillez depliez chaque question SVP

## Un coll√®gue vous transmet le  code R ci dessous : ####
calculerDistance <- function(x, y) {
n<-0; for(i in seq_along(x)) n<-n+1
lx<-levels(x)
ly<-levels(y)
nx<-length(lx)
ny<-length(ly)
tab<-matrix(0,nx,ny)
for(i in 1:nx) for(j in 1:ny) tab[i, j]<-sum(x==lx[i]&y==ly[j])
dl<-(nx-1)*(ny-1)
Tst<-sqrt(chisq.test(tab)$statistic/n/sqrt(dl))
sqrt(2*(1-Tst^2))}

Distances <- function(data) {
p <- ncol(data)
d <- matrix(0, p, p)
for (i in 1:(p - 1)){for (j in (i + 1):p){d[j,i]<-d[i,j]<-calculerDistance(data[,i],data[,j])}}
return(d)}




# Il vous demande de l'aide pour am√©liorer le temps de calcul. 

# __  1. Rajoutez une description du code ####
# en suivant le canevas ci dessous

# Description : Calcul d'une matrice de distances entre variables cat√©gorielles
# Entr√©e : deux vecteurs x et y (issues du dataframe)    
#              data :  agaricus-lepiota.data
# Sortie : renvoie une matrice d des distances 
#              ...

# __  2. √âcrivez une version en respectant le code de style Rstudio #### 
# (Corrigez la mise en format du code, le noms des objets interm√©diaires 
# et commentez-le si besoin.)

calculerDistance <- function(x, y) {
  n<-0 
  #calcul la taille de x
  for(i in seq_along(x)) n<-n+1
  #donne respectivement les valeurs distinctes qu'on trouve dans x et y
  lx<-levels(x); ly<-levels(y)
  #calcul les tailles de lx et ly
  nx<-length(lx); ny<-length(ly)
  #crÈe une matrice de nx lignes et ny colonnes remplie dee 0
  tab<-matrix(0,nx,ny)
  #pour chaque i de nx et j de nx, donne une valeur ‡ l"element i,j  
  for(i in 1:nx) { 
    for(j in 1:ny) {
      tab[i, j] <- sum(x==lx[i]&y==ly[j])
    } 
  } 
  
  dl<-(nx-1)*(ny-1)
  Tst<-sqrt(chisq.test(tab)$statistic/n/sqrt(dl))
  sqrt(2*(1-Tst^2))
}

Distances <- function(data) {
  #p prend le nombre de colonnes du dataframe
  p <- ncol(data)
  #on crÈe une matrice carÈe p lignes,pcolonnes remplies de 0
  d <- matrix(0, p, p)
  for (i in 1:(p - 1)){
    for (j in (i + 1):p){
      #pour chaque ÈlÈment i,j et j,i on lui donne la valeur obtenue depuis la fonction calculerDistance
      d[j,i]<-d[i,j]<-calculerDistance(data[,i],data[,j])
    }
  }
  #renvoie la matrice d
  return(d)
}

# __  3. Profilez le code et identifier le principal goulot d'√©tranglement ####
## Profil CalculerDistances
Rprof()
invisible(Distances(m)) # avec m en zone Test
Rprof(NULL)
summaryRprof()

# __  4. √âcrivez une version "plus" vectoris√©e de la fonction calculerDistance. ####

calculerDistance_vectorisee <- function(x, y) {
  n<-0 
  #calcul la taille de x
  for(i in seq_along(x)) n<-n+1
  #donne respectivement les valeurs distinctes qu'on trouve dans x et y
  lx<-levels(x); ly<-levels(y)
  #calcul les tailles de lx et ly
  nx<-length(lx); ny<-length(ly)
  #crÈe une matrice de nx lignes et ny colonnes remplie dee 0
  tab<-matrix(0,nx,ny)
  #pour chaque i de nx et j de nx, donne une valeur ‡ l"element i,j  
  #tab[i,j] <- lapply(x,y,sum(x==lx[i]&y==ly[j]))
  #lapply(1:nx, FUN = function(i) leave.one.out(i))
  
  dl<-(nx-1)*(ny-1)
  Tst<-sqrt(chisq.test(tab)$statistic/n/sqrt(dl))
  sqrt(2*(1-Tst^2))
}

# __  5. √âcrivez une premi√®re version en parall√®le de la fonction dans 3. ####
# en utilisant la librarie foreach (parall√©lisme implicite) et 2 noeuds de 
# calcul.
library(foreach)
library(doParallel) 
library(parallel)
#nombre de cores ‡ exploiter
k <- 6

#partition en blocs des donnÈes
nb_x = length(x)
nb_y = length(y)
x_list <- split(y_vec, rep(1:k, each = nb_x))
y_list <- split(y_vec, rep(1:k, each = nb_y))
calculdistance_foreaches <- foreach::foreach(i = x_list , .combine = c) %dopar% {
  foreach::foreach(i = y_list , .combine = c) %dopar% {
      return(calculerDistance(i,j))
  }
}

#stopper les cores
doParallel::stopImplicitCluster()

# __  6. √âcrivez une deuxi√®me version en parall√®le ####
# en utilisant la librarie parallel (parall√©lisme explicite) et 2 noeuds de 
# calcul.
library(parallel)

detectCores()           # handy function to ... detect the number of core !
cl <- makeCluster(6)
calculdistance_parallel <- parallel::parSapply(cl,x_list,x_list,FUN = calculerDistance)

#Èteindre les moteurs
parallel::stopCluster(clust)

# __  7. Obtenez les temps d'ex√©cution ####
# de toutes les versions de la fonction calculerDistance que vous avez √©crit.
# Quelle est la plus performant?
res <- microbenchmark(res1 <- calculdistance_parallel(x,y), res2 <- calculdistance_foreaches(x,y),
                      res1 <- calculerDistance_vectorisee(x,y),times = 25 )
res
autoplot(res)
# __  8. Ecrire une version plus intelligente (et rapide) de calculerDistance en R ####
calculerDistance_simplifiÈe <- function(x, y) {
  n <- length(x)
  lx<-levels(x); ly<-levels(y)
  nx<-length(lx); ny<-length(ly)
  tab<-matrix(0,nx,ny)
  for(i in 1:nx) { 
    for(j in 1:ny) {
      tab[i, j] <- sum(x==lx[i]&y==ly[j])
    } 
  } 
  
  dl<-(nx-1)*(ny-1)
  Tst<-sqrt(chisq.test(tab)$statistic/n/sqrt(dl))
  sqrt(2*(1-Tst^2))
}

# __  9. Idem que 8. mais maintenant en Rcpp ####

#VOIR FICHIER calculerDistances


# __ 10. Comparez le temps de calcul des differentes versions de la fonction ####
library(microbenchmark)
library(ggplot2)

Distances_simplifiÈe <- function(data) {
  p <- ncol(data)
  d <- matrix(0, p, p)
  for (i in 1:(p - 1)){
    for (j in (i + 1):p){
      d[j,i]<-d[i,j]<-calculerDistance_simplifiÈe(data[,i],data[,j])
    }
  }
  return(d)
}

Distances_simplifiÈe <- function(data) {
  p <- ncol(data)
  d <- matrix(0, p, p)
  for (i in 1:(p - 1)){
    for (j in (i + 1):p){
      d[j,i]<-d[i,j]<-calculerDistances_cpp(data[,i],data[,j])
    }
  }
  return(d)
}

res <- microbenchmark(res1 <- Distances_simplifiÈe(m), res2 <- Distances_simplifiÈe(m),times = 25 )
res
autoplot(res)

# test
m <- read.csv("agaricus-lepiota.data",stringsAsFactors = TRUE, header = FALSE)[, -17]
d <- Distances(m)


