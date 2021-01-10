#generianje podatkov in predstava v tabeli 
library(ggplot2)
library(reshape2)
library(optrees)


#1) gledamo navadane grafe,ki jih generirava s pomoƒçjo matrike 
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
# <- tabela_istih_utezi(5,5)


ppretvori_samo_tabela_istih_utezi <- function(max_st_tock, max_utez){ #ta funkcija naredi tabelo kjer so v drugem stolpcu st tock, 3 stolp = utez =1, 4stolpec = utez=2
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

#primerƒçek za zgornjo funkcijo 
#test_tabela_1 <- ppretvori_samo_tabela_istih_utezi(10,3)
#p1 <- ggplot(test_tabela_1, aes(x=utezi, y=pretok, colour=tocke, group=tocke)) + 
#  geom_line() +ggtitle('Pretok pri grafih z enakimi utezmi')
#p1


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


#?????
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

#test_tabela_2 <- test_alg_iste_utezi(5,20)
#p2 <- ggplot(test_tabela_2, aes(x=utezi, y=pretok, colour=tocke, group=tocke)) + 
#  geom_line()
#p2





#b) utezi povezav so iz intervala [a,b]  a < b, in odstranjujemo povezave 
#odstranimo minimalno ute≈æ 



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



pretvori_v_tabelo_za_graf <- function(tab_odstrani_min){
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
#za_graf_odstr_mini <-pretvori_v_tabelo_za_graf(mini)
#p3 <- ggplot(za_graf_odstr_mini, aes(x=st_odstr_povezav, y=pretok, colour=stevilo_tock, 
#                            group=stevilo_tock)) + geom_line() + ggtitle("Pretok grafa glede na stevilo \n odstranjenih minimalnih povezav")
#p3



#odstranimo maksimalno utez

tabela_odstrani_maxpov <- function(max_st_tock,a,b){
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
  tab_odstrani_max <- cbind('stevilo tock' = c(max_st_tock:4), tab_odstrani_max)
  colnames(tab_odstrani_max)[2:(dolzina+1)] <- c(1:(dolzina))
  return(tab_odstrani_max)
}


#primer 
#maxi <-tabela_odstrani_maxpov(10,0,20)
#za_graf_odstr_maxi <-pretvori_v_tabelo_za_graf(maxi)
#p4 <- ggplot(za_graf_odstr_maxi, aes(x=st_odstr_povezav, y=pretok, colour=stevilo_tock, 
#                                     group=stevilo_tock)) + geom_line() + ggtitle("Pretok grafa glede na stevilo \n odstranjenih maximalnih povezav")
#p4



#c) utezi povezav so iz intervala [a,b]  a < b, in odstranjujemo toƒçke
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
  tab_odstrani_oglisce <- cbind('stevilo tock' = c(max_tock:4), tab_odstrani_oglisce)
  dim <- dim(tab_odstrani_oglisce)[2]
  #colnames(tab_odstrani_oglisce)[2:(dim+1)] <- c(0:(dim -1))
  return(tab_odstrani_oglisce)
}

#odstrani_tocke <- tabela_odstrani_tocke(10,0,20)
#za_graf_odstr_tocke <-pretvori_v_tabelo_za_graf(odstrani_tocke)
#p5 <- ggplot(za_graf_odstr_tocke, aes(x=st_odstr_povezav, y=pretok, colour=stevilo_tock, 
#                                     group=stevilo_tock)) + geom_line() + xlab('st_odstranjenih_tock')  + ggtitle("Pretok grafa glede na stevilo \n odstranjenih tock")
#p5

#2) generiramo s pomoƒçjo geometrijskih grafov vse 3 razlicne utezi so v eni funkciji, locimo jih glede na tip

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
  tab_odstrani_oglisce <- cbind('stevilo tock' = c(max_tock:4), tab_odstrani_oglisce)
  dim <- dim(tab_odstrani_oglisce)[2]
  #colnames(tab_odstrani_oglisce)[2:(dim+1)] <- c(0:(dim -1))
  return(tab_odstrani_oglisce)
}

#primer: 
#r <- 1
#g <- igraf_razdalje_so_utezi(10,r)
#tip 1
#odst_ogli_1 <- tabela_odstrani_tocke_geom(g,r,1)
#za_graf_odstr_ogli_1<-pretvori_v_tabelo_za_graf(odst_ogli_1)
#p6_1 <- ggplot(za_graf_odstr_ogli_1, aes(x=st_odstr_povezav, y=pretok, colour=stevilo_tock, 
#                                      group=stevilo_tock)) + geom_line() + xlab('st_odstranjenih_tock')   + ggtitle("Pretok geometrijskega grafa, ki ima utezi enaki razdalji tock")
#p6_1

#tip 2
#g2 <- igraf_razdalje_so_inverz(10,r)
#odst_ogli_2 <- tabela_odstrani_tocke_geom(g2,r,2)
#za_graf_odstr_ogli_2<-pretvori_v_tabelo_za_graf(odst_ogli_2)
#p6_2 <- ggplot(za_graf_odstr_ogli_2, aes(x=st_odstr_povezav, y=pretok, colour=stevilo_tock, 
#                                         group=stevilo_tock)) + geom_line() +  xlab('st_odstranjenih_tock') + ggtitle("Pretok geometrijskega grafa, ki ima utezi enake inveru razdalje")
#p6_2

#tip 3
#g3 <- igraf_utezi_so_nakljucne(10,r,10)
#odst_ogli_3 <- tabela_odstrani_tocke_geom(g3,r,3)
#za_graf_odstr_ogli_3 <-pretvori_v_tabelo_za_graf(odst_ogli_3)
#p6_3 <- ggplot(za_graf_odstr_ogli_3, aes(x=st_odstr_povezav, y=pretok, colour=stevilo_tock, 
#                                         group=stevilo_tock)) + geom_line() + xlab('st_odstranjenih_tock') +ggtitle("Pretok geometrijskega grafa, ki ima utezi nakljucne")
#p6_3



#b) odstranimo minimalno povezavo
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
      g <- igraf_razdalje_so_inverz(st_tock,r)
    }else if (tip == 3){
      g <- igraf_utezi_so_nakljucne(st_tock,r,20) # bolj?eeee!!!!!
    }else{
      return('napacen ukaz')
    }
    
    pretoki <- c()
    pretoki <- append(pretoki, edmonds_karp(g,1,st_tock))
    
  }
  tab_odstrani_min <- cbind('stevilo tock' = c(max_tock:4), tab_odstrani_min)
  colnames(tab_odstrani_min)[2:(dolzina+1)] <- c(0:(dolzina-1))
  return(tab_odstrani_min)
}

# primer 
#r <- 1
#g1<- igraf_razdalje_so_inverz(10,r)
#odstrani_min_pov_geom1 <-tabela_odstrani_minpov_geom(g1,r,2)
#za_graf_odstrani_min_pov_geom1 <-pretvori_v_tabelo_za_graf(odstrani_min_pov_geom1)
#p7 <- ggplot(za_graf_odstrani_min_pov_geom1, aes(x=st_odstr_povezav, y=pretok, colour=stevilo_tock, 
#                                         group=stevilo_tock)) + geom_line()  + xlab('st_odstranjenih_min_povezav') + ggtitle("Pretok geometrijskega grafa, ki ima uteûi enake inverzu razdalj")
#p7


#g2<- igraf_razdalje_so_utezi(10,r)
#odstrani_min_pov_geom2 <-tabela_odstrani_minpov_geom(g2,r,1)
#za_graf_odstrani_min_pov_geom2 <-pretvori_v_tabelo_za_graf(odstrani_min_pov_geom2)
#p8 <- ggplot(za_graf_odstrani_min_pov_geom2, aes(x=st_odstr_povezav, y=pretok, colour=stevilo_tock, 
#                                                group=stevilo_tock)) + geom_line()  + xlab('st_odstranjenih_min_povezav') + ggtitle("Pretok geometrijskega grafa, ki ima uteûi enake razdalji toËk")
#p8

#g3<- igraf_utezi_so_nakljucne(10,r,20)
#odstrani_min_pov_geom3 <-tabela_odstrani_minpov_geom(g3,r,3)
#za_graf_odstrani_min_pov_geom3 <-pretvori_v_tabelo_za_graf(odstrani_min_pov_geom3)
#p9 <- ggplot(za_graf_odstrani_min_pov_geom3, aes(x=st_odstr_povezav, y=pretok, colour=stevilo_tock, 
#                                                 group=stevilo_tock)) + geom_line()  + xlab('st_odstranjenih_min_povezav') + ggtitle("Pretok geometrijskega grafa, ki ima nakljuËne uteûi")
#p9


#c) odstranimo maksimalno povezavo 
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
      g <- igraf_razdalje_so_inverz(st_tock,r)
    }else if (tip == 3){
      g <- igraf_utezi_so_nakljucne(st_tock,r,20) # bolj?eeee!!!!!
    }else{
      return('napacen ukaz')
    }
    
    pretoki <- c()
    pretoki <- append(pretoki, edmonds_karp(g,1,st_tock))
    
  }
  tab_odstrani_max <- cbind('stevilo tock' = c(max_tock:4), tab_odstrani_max)
  colnames(tab_odstrani_max)[2:(dolzina +1)] <- c(0:(dolzina-1))
  return(tab_odstrani_max)
}

#primeri

#odstrani_max_pov_geom1 <-tabela_odstrani_maxpov_geom(g1,r,2)
#za_graf_odstrani_max_pov_geom1 <-pretvori_v_tabelo_za_graf(odstrani_max_pov_geom1)
#p7_1 <- ggplot(za_graf_odstrani_max_pov_geom1, aes(x=st_odstr_povezav, y=pretok, colour=stevilo_tock, 
#                                                group=stevilo_tock)) + geom_line()  + xlab('st_odstranjenih_max_povezav') + ggtitle("Pretok geometrijskega grafa, ki ima uteûi enake inverzu razdalj")
#p7_1



#odstrani_max_pov_geom2 <-tabela_odstrani_maxpov_geom(g2,r,1)
#za_graf_odstrani_max_pov_geom2 <-pretvori_v_tabelo_za_graf(odstrani_max_pov_geom2)
#p8_1 <- ggplot(za_graf_odstrani_max_pov_geom2, aes(x=st_odstr_povezav, y=pretok, colour=stevilo_tock, 
#                                                 group=stevilo_tock)) + geom_line()  + xlab('st_odstranjenih_max_povezav') + ggtitle("Pretok geometrijskega grafa, ki ima uteûi enake razdalji toËk")
#p8_1

#odstrani_max_pov_geom3 <-tabela_odstrani_maxpov_geom(g3,r,3)
#za_graf_odstrani_max_pov_geom3 <-pretvori_v_tabelo_za_graf(odstrani_max_pov_geom3)
#p9_1 <- ggplot(za_graf_odstrani_max_pov_geom3, aes(x=st_odstr_povezav, y=pretok, colour=stevilo_tock, 
#                                                 group=stevilo_tock)) + geom_line()  + xlab('st_odstranjenih_max_povezav') + ggtitle("Pretok geometrijskega grafa, ki ima nakljuËne uteûi")
#p9_1




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

pretvori_v_tabelo_za_graf_r <- function(tab_odstrani_min){
  nabor <- seq(0.1,sqrt(2), 0.1)
  r <- rep(nabor,nrow(tab_odstrani_min))
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
  odstrani_min <- data.frame('r' = r,
                             'pretok' = pretok, 'stevilo_tock' = stevilo_tock)
  return(odstrani_min)
}




#primer
#r_tip_1 <- tabela_sprem_r_tip1(10)
#r_tip_1_1 <-pretvori_v_tabelo_za_graf_r(r_tip_1)
#p10 <- ggplot(r_tip_1_1, aes(x=r, y=pretok, colour=stevilo_tock, 
#                    group=stevilo_tock)) + geom_line() + ggtitle("Pretok geometrijskega grafa glede na r, \nËe so uteûi razdalje")
#p10




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

#primer
#r_tip_2 <- tabela_sprem_r_tip2(10)
#r_tip_2_1 <-pretvori_v_tabelo_za_graf_r(r_tip_2)
#p11 <- ggplot(r_tip_2_1, aes(x=r, y=pretok, colour=stevilo_tock, 
#                             group=stevilo_tock)) + geom_line() + ggtitle("Pretok geometrijskega grafa glede na r,\nËe so uteûi inverzi razdalj")
#p11





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

#r_tip_3 <- tabela_sprem_r_tip3(10,10)
#r_tip_3_1 <-pretvori_v_tabelo_za_graf_r(r_tip_3)
#p12 <- ggplot(r_tip_3_1, aes(x=r, y=pretok, colour=stevilo_tock, 
#                            group=stevilo_tock)) + geom_line() + ggtitle("Pretok geometrijskega grafa glede na r,\nËe so uteûi izbrano nakljuËno in je najveËja moûna uteû 10")
#p12



#3) merjenje hitrosti funkcije ki izra?una pretok v primerjavi z ?e ugrajeno funkcijo.

hitrost_funkcij <- function(max_st_tock, max_utez){
  tab_hitr <- data.frame()
  speed <- c()
  for (i in 2:max_st_tock){
    mat <- generira_matriko(i, 0, max_utez)
    g <- pretvorba_v_igraph(mat)
    prvi <- system.time(edmonds_karp(g, 1, i))[3]
    arcs <- oceti_in_sinovi(mat)
    h <- maxFlowFordFulkerson(c(1:i), arcs, directed = TRUE, source.node = 1, sink.node = i)
    drugi <- system.time(h)[3]
    tab_hitr <- rbind(tab_hitr, c(prvi, drugi))
    speed <- c()
  }
  tab_hitr <- cbind('st_tock' =c(3:(max_st_tock+1)), tab_hitr)
  colnames(tab_hitr)[2:3] <- c('najina fun pretok', 'vgrajena fun')
  return(tab_hitr)
}

#hitrost_funkcij(10,10)
