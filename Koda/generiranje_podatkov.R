#generianje podatkov in predstava v tabeli 
library(ggplot2)


#1) gledamo navadane grafe,ki jih generirava s pomočjo matrike 
#a) vse povezave imajo enako vrednost
tabela_istih_utezi <- function(max_st_tock, max_utez){ #ta funkcija naredi tabelo kjer so v drugem stolpcu st tock, 3 stolp = utez =1, 4stolpec = utez=2
  st_tock <- 3:max_st_tock
  pretoki <- c()
  st= 1
  tabela_1 <- data.frame(st_tock = st_tock)
  for (i in 1: max_utez){
    for (j in 3: max_st_tock){
      izracun <- edmonds_karp(pretvorba_v_igraph(generira_matriko(j,i,i)),1,j)
      pretoki <- append(pretoki, izracun)
      
    }
    tabela_1 = cbind(tabela_1, 'utez' =  pretoki)
    pretoki <- c()
    
  
  }
  return(tabela_1)
}

#primerček za zgornjo funkcijo 
#tabela <- tabela_istih_utezi(5,5) 


#opazimo nek algoritem 
alg_iste_utezi <- function(st_tock, max_utez){
  zac_tock <- 2
  vek <- c()
  tab_iste_utezi <- data.frame()
  while (st_tock+1  != zac_tock ) {
    vek <- append(vek,zac_tock)
    for (i in 1:max_utez){
      vek <- append(vek, i*(zac_tock -1))
    }
    tab_iste_utezi <- rbind(tab_iste_utezi, vek)
    zac_tock <- zac_tock + 1
    vek <- c()
  }
  colnames(tab_iste_utezi)[1] = "stevilo tock"
  colnames(tab_iste_utezi)[3] = "X2"
  return(tab_iste_utezi)
  
}
tabela_2 <- alg_iste_utezi(5,20)
#ggplot(as.data.frame.matrix(tabela_2))??? 


#b) utezi povezav so iz intervala [a,b]  a < b, in odstranjujemo povezave 
#odstarnimo minimalno utež 

tabela_odstrani_minpov <- function(max_st_tock,a,b){
  st_tock <- max_st_tock
  pretoki <- c()
  g <- pretvorba_v_igraph(generira_matriko(st_tock,a,b))
  pretoki <- append(pretoki, edmonds_karp(g,1,st_tock))
  tab_odstrani_min <- data.frame()
  dolzina <- 0
  while (st_tock > 3) {
    while (pretoki[length(pretoki)] != 0 ) {
      min <- min(E(g)$V3)
      mesto <- which(E(g)$V3== min)[1]
      g <- delete.edges(g,E(g)[mesto])
      pretoki <- append(pretoki, edmonds_karp(g, 1, st_tock))
    }
    if (length(pretoki) > dolzina){
      dolzina <- length(pretoki)
      
    }else {
      u <- length(pretoki)
      pretoki <- c(pretoki, rep(0, dolzina - u))}
    tab_odstrani_min <- rbind(tab_odstrani_min, pretoki)
    dolzina <- dim(tab_odstrani_min)[2]
    st_tock <- st_tock - 1
    g <- pretvorba_v_igraph(generira_matriko(st_tock,a,b))
    pretoki <- c()
    pretoki <- append(pretoki, edmonds_karp(g,1,st_tock))
    
  }
  tab_odstrani_min <- cbind('stevilo tock' = c(4:max_st_tock), tab_odstrani_min)
  colnames(tab_odstrani_min)[2:dolzina] <- c(1:(dolzina-1))
  return(tab_odstrani_min)
}
#primer 
#mini <-tabela_odstrani_minpov(10,0,20)

#odstranimo maksimalno utez

tabela_odstarni_maxpov <- function(max_st_tock,a,b){
  st_tock <- max_st_tock
  pretoki <- c()
  g <- pretvorba_v_igraph(generira_matriko(st_tock,a,b))
  pretoki <- append(pretoki, edmonds_karp(g,1,st_tock))
  tab_odstrani_max <- data.frame()
  dolzina <- 0
  while (st_tock > 3) {
    while (pretoki[length(pretoki)] != 0 ) {
      max <- max(E(g)$V3)
      mesto <- which(E(g)$V3== min)[1]
      g <- delete.edges(g,E(g)[mesto])
      pretoki <- append(pretoki, edmonds_karp(g, 1, st_tock))
    }
    if (length(pretoki) > dolzina){
      dolzina <- length(pretoki)
      
    }else {
      u <- length(pretoki)
      pretoki <- c(pretoki, rep(0, dolzina - u))}
    tab_odstrani_max <- rbind(tab_odstrani_max, pretoki)
    dolzina <- dim(tab_odstrani_max)[2]
    st_tock <- st_tock - 1
    g <- pretvorba_v_igraph(generira_matriko(st_tock,a,b))
    pretoki <- c()
    pretoki <- append(pretoki, edmonds_karp(g,1,st_tock))
    
  }
  tab_odstrani_max <- cbind('stevilo tock' = c(4:max_st_tock), tab_odstrani_max)
  colnames(tab_odstrani_max)[2:dolzina] <- c(1:(dolzina-1))
  return(tab_odstrani_max)
}

#primer 
#maxi <-tabela_odstarni_maxpov(10,0,20)

#c) utezi povezav so iz intervala [a,b]  a < b, in odstranjujemo točke
tabela_odstrani_tocke <- function(max_tock, a, b){
  st_tock <- max_tock
  pretoki <- c()
  g <- pretvorba_v_igraph(generira_matriko(st_tock,a,b))
  pretoki <- append(pretoki, edmonds_karp(g,1,st_tock))
  tab_odstrani_oglisce <- data.frame()
  dolzina <- 0
  while (st_tock > 3) {
      for (i in 1: (st_tock-2)){
        k <- sample(2:(st_tock-1), size = i, replace=FALSE)
        h <- delete.vertices(g,k)
        pretoki <- append(pretoki, edmonds_karp(h,1,st_tock-i))
    
      }
    pretoki <- append(pretoki, rep(0,dolzina))
    tab_odstrani_oglisce <- rbind(tab_odstrani_oglisce, pretoki)
    st_tock <- st_tock -1 
    dolzina <- dolzina +1
    g <- pretvorba_v_igraph(generira_matriko(st_tock,a,b))
    pretoki <- c()
    pretoki <- append(pretoki, edmonds_karp(g,1,st_tock))
  }
  return(tab_odstrani_oglisce)
}



#2) generiramo s pomočjo geometrijskih grafov 


