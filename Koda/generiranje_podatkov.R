#generianje podatkov in predstava v tabeli 
library(ggplot2)
library(reshape2)


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
    tabela_1 = cbind(tabela_1, pretoki)
    pretoki <- c()
    
  
  }
  colnames(tabela_1)[2:(max_utez+1)] <- c(1:(max_utez))
  return(tabela_1)
}
tabela <- tabela_istih_utezi(5,5)


test_tabela_istih_utezi <- function(max_st_tock, max_utez){ #ta funkcija naredi tabelo kjer so v drugem stolpcu st tock, 3 stolp = utez =1, 4stolpec = utez=2
  st_tock <- 3:max_st_tock
  pretoki <- c()
  st= 1
  tocke <- c()
  for (j in 3: max_st_tock){
    for (i in 1: max_utez){
      izracun <- edmonds_karp(pretvorba_v_igraph(generira_matriko(j,i,i)),1,j)
      pretoki <- append(pretoki, izracun)
      tocke <- append(tocke, j)
    }
  }
  utez <- rep(1:max_utez, length(st_tock))
  tocke <- sort(tocke)
  tabela_1 <- data.frame("utezi" = utez, "pretok" = pretoki, "tocke" =tocke)
  return(tabela_1)
}

#primerček za zgornjo funkcijo 
test_tabela_1 <- test_tabela_istih_utezi(10,3)
p1 <- ggplot(test_tabela_1, aes(x=utezi, y=pretok, colour=tocke, group=tocke)) + 
  geom_line()
p1
#################### ta zdej dela ###########################################

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
#ggplot(tabela_2, x= 'st_tock')????

test_alg_iste_utezi <- function(st_tock, max_utez){
  zac_tock <- 2
  pretok <- c()
  tocke <- c()
  while (st_tock+1  != zac_tock ) {
    for (i in 1:max_utez){
      pretok <- append(pretok, i*(zac_tock -1))
      tocke <- append(tocke, zac_tock)
    }
    zac_tock <- zac_tock + 1
  }
  utez <- rep(1:max_utez, length(2:st_tock))
  tab_iste_utezi <- data.frame("utezi" = utez, "pretok" = pretok, "tocke" =tocke)
  return(tab_iste_utezi)
}

test_tabela_2 <- test_alg_iste_utezi(5,20)
p2 <- ggplot(test_tabela_2, aes(x=utezi, y=pretok, colour=tocke, group=tocke)) + 
  geom_line()
p2





#b) utezi povezav so iz intervala [a,b]  a < b, in odstranjujemo povezave 
#odstranimo minimalno utež 

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
  tab_odstrani_min <- cbind('stevilo tock' = c(max_st_tock:4), tab_odstrani_min)
  colnames(tab_odstrani_min)[2:(dolzina+1)] <- c(0:(dolzina-1))
  return(tab_odstrani_min)
}

#primer 
#mini <-tabela_odstrani_minpov(10,0,20)
test_tabela_odstrani_minpov <- function(max_st_tock,a,b){
  st_tock <- max_st_tock
  pretoki <- c()
  g <- pretvorba_v_igraph(generira_matriko(st_tock,a,b))
  pretoki <- append(pretoki, edmonds_karp(g,1,st_tock))
  stevilo_odstranjenih_povezav <- c(0)
  dolzina <- 0
  i <- 1
  stevilo_tock <- c(st_tock)
  while (st_tock > 1) {
    while (pretoki[length(pretoki)] != 0 ) { #ali je zadnji pretok 0? 
      min <- min(E(g)$V3)                    # najmanjša utež 
      mesto <- which(E(g)$V3== min)[1]      # na katerem mestu je
      g <- delete.edges(g,E(g)[mesto])       # odstranimo to povezavo s to utežjo
      stevilo_odstranjenih_povezav <- append(stevilo_odstranjenih_povezav, i)
      i <- i +1
      pretoki <- append(pretoki, edmonds_karp(g, 1, st_tock)) # dodamo pretok ko odstranimo povezavo 
      stevilo_tock <- append(stevilo_tock, st_tock)
    }
    if (length(pretoki) > dolzina){
      dolzina <- length(pretoki)
    }else {
      u <- length(pretoki)
      st_tock <- st_tock - 1
      g <- pretvorba_v_igraph(generira_matriko(st_tock,a,b))
      pretoki <- append(pretoki, edmonds_karp(g,1,st_tock))
      i <- 0 
      stevilo_odstranjenih_povezav <- append(stevilo_odstranjenih_povezav, i)
      i <- 1 
      stevilo_tock <- append(stevilo_tock, st_tock)
  }
  tab_odstrani_min <- data.frame('st_odstranjenih_povezav' = stevilo_odstranjenih_povezav,
                                 'pretoki' = pretoki,
                                 'stevilo_tock' = stevilo_tock)
  #tab_odstrani_min <- tab_odstrani_min [-nrow(tab_odstrani_min ),]
 }
  return(tab_odstrani_min)
}


test_mini <-test_tabela_odstrani_minpov(5,0,10)
p3 <- ggplot(test_mini, aes(x=utezi, y=pretoki, colour=stevilo_tock, 
                                group=stevilo_tock)) + geom_point()
p3

# tale noče delat :( 

ne_elegenten_nacin_tabela_odstrani_minpov <- function(tab_odstrani_min){
  st_odstr_povezav <- c(0)
  st_pov <- 1:(ncol(tab_odstrani_min) -2)
  st_odstr_povezav <- append(st_odstr_povezav, st_pov)
  st_odstr_povezav <- rep(st_odstr_povezav,nrow(tab_odstrani_min))
  pretok <- c()
  for (i in 1:nrow(tab_odstrani_min)){ 
    for(j in 2:ncol(tab_odstrani_min)){
      value <- tab_odstrani_min[i,j]
      pretok <- append(pretok, value)
    }
  }
  tocke <- tab_odstrani_min[,1]
  stevilo_tock <- c()
  for (i in tocke) {
    st <- rep(i,(length(st_pov)+1))
    stevilo_tock <- append(stevilo_tock,st)
  }
  odstrani_min <- data.frame('st_odstr_povezav' = st_odstr_povezav,
                             'pretok' = pretok, 'stevilo_tock' = stevilo_tock)
  return(odstrani_min)
}
test_mini_ne_elegantno <-ne_elegenten_nacin_tabela_odstrani_minpov(tabela_odstrani_minpov(10,0,20))
p4 <- ggplot(test_mini_ne_elegantno, aes(x=st_odstr_povezav, y=pretok, colour=stevilo_tock, 
                            group=stevilo_tock)) + geom_line()
p4





















 



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
      mesto <- which(E(g)$V3== max)[1]
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
  colnames(tab_odstrani_max)[2:(dolzina+1)] <- c(1:(dolzina))
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
  tab_odstrani_oglisce <- cbind('stevilo tock' = c(4:max_tock), tab_odstrani_oglisce)
  dim <- dim(tab_odstrani_oglisce)[2]
  colnames(tab_odstrani_oglisce)[2:(dim+1)] <- c(0:(dim -1))
  return(tab_odstrani_oglisce)
}



#2) generiramo s pomočjo geometrijskih grafov vse 3 razlicne utezi so v eni funkciji, locimo jih glede na tip

#a) odtsranimo razlicno stevilo tock

#tip = 1, ?e  igraf_razdalje_so_utezi, tip = 2 ?e igraf_razdalje_so_inverz
#tip = 3 ?e igraf_utezi_so_nakljucne

tabela_odstrani_tocke_geom <- function(g, r, tip){
  max_tock <- length(V(g))
  st_tock <- max_tock
  pretoki <- c()
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
    if (tip == 1){
      g <- igraf_razdalje_so_utezi(st_tock,r)
    }else if (tip == 2){
      igraf_razdalje_so_inverz(st_tock,r)
    }else if (tip == 3){
      igraf_utezi_so_nakljucne(st_tock,r,20) # bolj?eeee!!!!!
    }else{
      return('napacen ukaz')
    }
    
    pretoki <- c()
    pretoki <- append(pretoki, edmonds_karp(g,1,st_tock))
  }
  tab_odstrani_oglisce <- cbind('stevilo tock' = c(4:max_tock), tab_odstrani_oglisce)
  dim <- dim(tab_odstrani_oglisce)[2]
  colnames(tab_odstrani_oglisce)[2:(dim+1)] <- c(0:(dim -1))
  return(tab_odstrani_oglisce)
}

#b) odstrnimo minimalno povezavo
tabela_odstrani_minpov_geom <- function(g, r, tip){
  max_tock <- length(V(g))
  st_tock <- max_tock
  pretoki <- c()
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
    if (tip == 1){
      g <- igraf_razdalje_so_utezi(st_tock,r)
    }else if (tip == 2){
      igraf_razdalje_so_inverz(st_tock,r)
    }else if (tip == 3){
      igraf_utezi_so_nakljucne(st_tock,r,20) # bolj?eeee!!!!!
    }else{
      return('napacen ukaz')
    }
    
    pretoki <- c()
    pretoki <- append(pretoki, edmonds_karp(g,1,st_tock))
    
  }
  tab_odstrani_min <- cbind('stevilo tock' = c(4:max_tock), tab_odstrani_min)
  colnames(tab_odstrani_min)[2:(dolzina+1)] <- c(0:(dolzina-1))
  return(tab_odstrani_min)
}


# primer 
#g<- igraf_razdalje_so_inverz(6,1)
#h <- igraf_razdalje_so_utezi(6,1)
#i <- igraf_utezi_so_nakljucne(6,1,20)

#c) odtsranimo maksimalno povezavo 
tabela_odstrani_maxpov_geom <- function(g, r, tip){
  max_tock <- length(V(g))
  st_tock <- max_tock
  pretoki <- c()
  pretoki <- append(pretoki, edmonds_karp(g,1,st_tock))
  tab_odstrani_max <- data.frame()
  dolzina <- 0
  while (st_tock > 3) {
    while (pretoki[length(pretoki)] != 0 ) {
      max <- max(E(g)$V3)
      mesto <- which(E(g)$V3== max)[1]
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
    if (tip == 1){
      g <- igraf_razdalje_so_utezi(st_tock,r)
    }else if (tip == 2){
      igraf_razdalje_so_inverz(st_tock,r)
    }else if (tip == 3){
      igraf_utezi_so_nakljucne(st_tock,r,20) # bolj?eeee!!!!!
    }else{
      return('napacen ukaz')
    }
    
    pretoki <- c()
    pretoki <- append(pretoki, edmonds_karp(g,1,st_tock))
    
  }
  tab_odstrani_max <- cbind('stevilo tock' = c(4:max_tock), tab_odstrani_max)
  colnames(tab_odstrani_max)[2:(dolzina +1)] <- c(0:(dolzina-1))
  return(tab_odstrani_max)
}

#d) kak?en je pretok ob spreminjanju r-ja 
#igraf_razdalje_so_utezi --> tip 1

tabela_sprem_r_tip1 <- function(max_st_tock){
  nabor <- seq(0.1,sqrt(2), 0.1)
  d <- length(nabor)
  tab_r <- data.frame()
  pretok <- c()
  for (i in 3:max_st_tock){
    for (j in nabor){
      g <- igraf_razdalje_so_utezi(i,j)
      pretok <- append(pretok, edmonds_karp(g,1,i))
    }
    tab_r <-rbind(tab_r, pretok)
    pretok <- c()
  }
  tab_r <- cbind('stevilo tock' = c(3:max_st_tock), tab_r)
  colnames(tab_r)[2:(d+1)] <- c(nabor)
  return(tab_r)
}

#igraf_razdalje_so_inverz ---> tip 2 

tabela_sprem_r_tip2 <- function(max_st_tock){
  nabor <- seq(0.1,sqrt(2), 0.1)
  d <- length(nabor)
  tab_r <- data.frame()
  pretok <- c()
  for (i in 3:max_st_tock){
    for (j in nabor){
      g <- igraf_razdalje_so_inverz(i,j)
      pretok <- append(pretok, edmonds_karp(g,1,i))
    }
    tab_r <-rbind(tab_r, pretok)
    pretok <- c()
  }
  tab_r <- cbind('stevilo tock' = c(3:max_st_tock), tab_r)
  colnames(tab_r)[2:(d+1)] <- c(nabor)
  return(tab_r)
}

#igraf_utezi_so_nakljucne ---> tip 3 

tabela_sprem_r_tip3 <- function(max_st_tock, max_utez){
  nabor <- seq(0.1,sqrt(2), 0.1)
  d <- length(nabor)
  tab_r <- data.frame()
  pretok <- c()
  for (i in 3:max_st_tock){
    for (j in nabor){
      g <- igraf_utezi_so_nakljucne(i,j,max_utez)
      pretok <- append(pretok, edmonds_karp(g,1,i))
    }
    tab_r <-rbind(tab_r, pretok)
    pretok <- c()
  }
  tab_r <- cbind('stevilo tock' = c(3:max_st_tock), tab_r)
  colnames(tab_r)[2:(d+1)] <- c(nabor)
  return(tab_r)
}


#3) merjenje hitrosti funkcije ki izra?una pretok v primerjavi z ?e ugrajeno funkcijo. 
system.time(edmonds_karp(igraf_razdalje_so_utezi(6,1),1,6)) #primer 
#elapsed je koliko ?asa prete?e da izra?una funkcijo. 

#ze ugrajene funkcije za pretok 
#max_flow(graph, source, target, capacity = NULL)
#maxFlowFordFulkerson(nodes, arcs, directed = FALSE, source.node = 1, sink.node = nodes[length(nodes)])