library(igraph)
library(prodlim)
library(foreach)
library(collections)

#generiranje matrike sosednosti
generira_matriko <- function(st_tock, min_stevil = 0, max_stevil){  # tuki mora mit minimum več ali enako 0 
  matrika <- matrix(0, st_tock, st_tock)
  # Prvi stolpec more bit vedno 0, zadna vrstica in diagonala
  vek_stevil <- floor(runif(st_tock^2, min=min_stevil, max=max_stevil))
  matrika <-  matrix(vek_stevil, nrow = st_tock, ncol = st_tock)
  for (i in  1: st_tock){
    matrika[i,1] <- 0
    matrika[st_tock, i] <- 0 
  }
  diag(matrika) <- rep(0, st_tock)
  return(matrika)
}

#iz matrike zgeneriramo graf 
graf <- function(matrika){
  a <- matrika
  st_tock <- nrow(matrika)
  vek_povezav <- c()
  vek_utezi <- c()
  for (i in 1:(st_tock-1)){ # zadnja vrstica je itak 0 pa jo lah spustimo 
    for (j in 2: (st_tock)){ # prvi stolpec je itak 0 pa ga lah spustimo 
      if (a[i,j] == 0){
        st_tock <- st_tock + 0 # sam da neki nardi 
      } else {
        vek_utezi <- append(vek_utezi, a[i,j])
        vek_povezav <- append(vek_povezav, c(i,j))
      }
    }
  }
  povezan_graf <- graph(edges = vek_povezav, st_tock) %>% set_edge_attr("label", value = vek_utezi)
  plot(povezan_graf)
}

#naredi matriko, kjer so navedene povezave oče sin in v tretjem stolpcu uteži 
oceti_in_sinovi <- function(matrika){
  zacetna_tocka <- 1
  # sinovi <- list()
  koncna_tocka <- nrow(matrika)
  stevilo_vrstic <- koncna_tocka - 1 + (koncna_tocka - 2)*(koncna_tocka-2)
  matrika_oce_sin <- matrix(0,stevilo_vrstic,3)
  vrstica <- 1
  for (i in 1:koncna_tocka) {
    for (j in 1: koncna_tocka){
      # sinovi[[i]] <- generira_matriko[i,]
      if (matrika[i,j] != 0){
        matrika_oce_sin[vrstica,1] <- i
        matrika_oce_sin[vrstica,2] <- j
        matrika_oce_sin[vrstica,3] <- matrika[i,j]
        vrstica <- vrstica + 1  
      }
    }
  }
  matrika_oce_sin<- matrika_oce_sin[apply(matrika_oce_sin, 1, function(x) !all(x==0)),] #odstrani vrstice kjer so ničle 
  return(matrix(matrika_oce_sin, ncol=3)) # dodano
}

vse_poti <- function(matrika){
  st_tock <- nrow(matrika)
  mat_poti <- oceti_in_sinovi(matrika)
  graf_1 <- mat_poti[,-3]
  graf_1<- graph.data.frame(graf_1)
  if (st_tock > 1){
  return(all_simple_paths(graf_1, from= 1, to= st_tock))
  }
}
  

utezi_na_poti <- function(matrika){
  st_tock <- nrow(matrika)
  poti <- vse_poti(matrika)
  mat_oce_sin <- oceti_in_sinovi(matrika)
  mat_oce_sin_2 <- as.data.frame(mat_oce_sin[, -3])
  #doloci utezi od poti
  utezi_poti <- list()
  utezi <- c()
  #if (sum(matrika[1,])== 0){
  #  stop("nimam več izhodov")
  #}
  for (i in 1:length(poti)){
    pot <- as.vector(poti[[i]])
    for (j in (1:(length(pot)-1))){
      st_vrstice <-row.match(c(pot[j],pot[j+1]), mat_oce_sin_2)
      utez <- mat_oce_sin[st_vrstice, 3]
      utezi <- append(utezi, utez)
    }
    utezi_poti[[i]] <- utezi
    utezi <- c()
  }
  return(utezi_poti)
}



posodobljena_generirana_matrika <- function(a,polozaj_minimuma,minimum_na_poti, vse_mozne_poti,i){
  izbrana_pot <- as.vector(vse_mozne_poti[[i]])
  for (j in 1:(length(izbrana_pot)-1)){ # treba bo z indeksi 
    zacetek_delne_poti <- izbrana_pot[j]
    konec_delne_poti <- izbrana_pot[j+1]
    a[zacetek_delne_poti, konec_delne_poti] <- a[zacetek_delne_poti, konec_delne_poti] - minimum_na_poti
  }
  return(a)
}



#funkcija največjega pretoka, neučinkovita za grafe z več kot 7 točkami
najvecji_pretok <- function(matrika){
  max_pretok <- 0
  mat_oce_sin <- oceti_in_sinovi(matrika)
  mat_oce_sin_2 <- as.data.frame(mat_oce_sin[, -3])
  utezi_poti <- utezi_na_poti(matrika)
  vse_mozne_poti<- vse_poti(matrika)
  stevec <- length(utezi_poti)
  while (stevec != 0){ # dolžina uteženih poti pa vseh možnih poti je enaka 
    v <- utezi_poti[[1]] # vektor utezi na poti
    minimum_na_poti <- min(v) # minimum utezi na tej poti
    max_pretok <- max_pretok + minimum_na_poti # pretok povečamo za to utež
    polozaj_minimuma <- which(v == minimum_na_poti)[1] # indeks na katerem mestu je minimum v utezeh 
    #mat_oce_sin <- posodobi_mat_oce_sin(mat_oce_sin, mat_oce_sin_2, minimum_na_poti,vse_mozne_poti,1)   ---> ne dela glih najbl
    matrika <- posodobljena_generirana_matrika(matrika,polozaj_minimuma,minimum_na_poti, vse_mozne_poti,1)
    if (sum(matrika[1,]) <= 0){
      return(max_pretok)
    } 
    if ((sum(matrika[,ncol(matrika)]) <= 0)){
      return(max_pretok)
    }
    utezi_poti <- utezi_na_poti(matrika)  # tle se prtož pr zadnjem koraku k matriko ustav (ampak zdej k je ta IF se ne bi smel)
    vse_mozne_poti<- vse_poti(matrika)
    stevec <- length(utezi_poti)
  
  }
}


#bolj učinkovita različica 
pretvorba_v_igraph <- function(matrika){
  vozlisca <- 1: (nrow(matrika))
  povezave <- oceti_in_sinovi(matrika)
  utezi <- povezave[,3]
  g <- graph_from_data_frame(povezave, directed = TRUE, vertices = vozlisca) %>% set_edge_attr("label", value = utezi )
  print(plot.igraph(g))
  return(g)
}
#igraph <- pretvorba_v_igraph(matrika) #vedno treba prvo preden uporabimo spodnjo funkcijo, matriko spremeniti v igraf

pregled_v_sirino <- function(graf, s, t) {
  n <- length(V(graf))
  starsi <- rep(NA, n)
  obiskani <- rep(FALSE, n)
  fifo_1 <- deque()
  fifo_1$push(s)
  obiskani[s] <- TRUE
  while (fifo_1$size() > 0) {
    u <- fifo_1$popleft()
    for (sos in neighbors(graf, u)){
      utez <- E(graf)$V3[get.edge.ids(graf, c(u,sos))]
      if (! obiskani[sos] && utez > 0) {
        fifo_1$push(sos)
        obiskani[sos] <- TRUE
        starsi[sos] <- u
        if (sos == t) {
          pot <- t
          while (t != s) {
            t <- starsi[t]
            pot <- c(t, pot)
          }
          return(list(obstaja=TRUE, pot=pot))
        }
      }
    }
  }
  return(list(obstaja=FALSE))
  
}


edmonds_karp <- function(igraf, s, t) {
  bfs <- pregled_v_sirino(igraf, s, t)
  pretok <- 0
  #povezave <- get.edges(igraf, c(1:gsize(igraf)))
  #utezi <- E(igraf)$V3
  
  while (bfs$obstaja) {
    starsi_drugace <- rep(bfs$pot, each=2)[-1]
    starsi_drugace <- starsi_drugace[-length(starsi_drugace)]
    povezave <- get.edge.ids(igraf,starsi_drugace)
    utezi_poti <- E(igraf)$V3[povezave] #vren vtezi poti po tej poti k sva jo dubla 
    min_poti <- min(utezi_poti)
    pretok <- pretok + min_poti
    E(igraf)$V3[povezave] <- utezi_poti - min_poti
    igraf <- delete.edges(igraf, which(E(igraf)$V3==0))
    bfs <- pregled_v_sirino(igraf, s, t)
  }
  
  return(pretok)
  
  
}

#g <- make_ring(10) %>%
#set_edge_attr("name", value = LETTERS[1:10])
#edge_attr_names(g)
#g2 <- delete_edge_attr(g, "name")
#edge_attr_names(g2)
#a <- generira_matriko(5,0,5)
#graf(a)
#b <- pretvorba_v_igraph(a)
# b <- b - edge("c|d") tko odstranimo povezavo
#E(b)$V3[1] <- E(b)$V3[1] -2 #tko zbrišeš utež 
# E(b)$V3
#E(b)$V3[get.edge.ids(b, c(1,2,2,5))] dobimo vektor utezi pozi
# gsize(g) ---> število povezav v grafu
# edge_attr(g) ---> uteži na povezavah
# get.edges(g,c(1:6)) ---> matrika povezav (oče-sin)
#b <- pretvorba_v_igraph(generira_matriko(7,0,5))
#c <- pretvorba_v_igraph(generira_matriko(5,0,10))
#d <- pretvorba_v_igraph(generira_matriko(9,0,8))
#get.shortes.paths(d,s,t)



#GEOMETRIJSKI GRAFI 
#1) �e ima geometrijski graf take utezi kot je razdalja 
izracunaj_razdaljo <- function(x, y, matrika){ 
  razdalja <- 0 # za�etno razdaljo nastavimo na 0 
  for(i in 1:(length(x)-1)){
    for (j in 2:length(x)) {
      #if (j>i){
        matrika [i,j] <- round(sqrt((x[j]-x[i])^2 +(y[j]-y[i])^2), digits = 2); 
        
      #}
    }
  }
  matrika
}

najblizja_tocka_izhodisca <- function(x,y){
  najmanjsa_razdalja <- 2 # najve�ja je lah koren z 2 
  na_katerem_mestu_se_nahaja <- 0 
  for (i in 1:length(x)){
    razdalja_od_izhodisca <- sqrt((x[i]-0)^2 +(y[i]-0)^2)
    if (razdalja_od_izhodisca < najmanjsa_razdalja){
      najmanjsa_razdalja <- razdalja_od_izhodisca
      na_katerem_mestu_se_nahaja <- i
    } else {
      na_katerem_mestu_se_nahaja <- na_katerem_mestu_se_nahaja + 0
    }
  }
  return(na_katerem_mestu_se_nahaja)
}


najbolj_oddaljena_tocka_izhodisca <- function(x,y){
  najvecja_razdalja <- 0 # najve�ja je lah koren z 2 
  na_katerem_mestu_se_nahaja <- 0 
  for (i in 1:length(x)){
    razdalja_od_izhodisca <- sqrt((x[i]-0)^2 +(y[i]-0)^2)
    if (razdalja_od_izhodisca > najvecja_razdalja){
      najvecja_razdalja <- razdalja_od_izhodisca
      na_katerem_mestu_se_nahaja <- i
    } else {
      na_katerem_mestu_se_nahaja <- na_katerem_mestu_se_nahaja + 0
    }
  }
  return(na_katerem_mestu_se_nahaja)
}


igraf_razdalje_so_utezi <- function(st_tock,r){ #tuki dobimo matriko = matrika razdalj
  x <- runif(st_tock); # absica
  y <- runif(st_tock); # ordinata
  najblizja <- najblizja_tocka_izhodisca(x,y)
  x <- c(x[najblizja], x[-najblizja]) # dodano
  y <- c(y[najblizja], y[-najblizja]) # dodano
  najbolj_oddaljena <- najbolj_oddaljena_tocka_izhodisca(x,y)
  x <- c(x[-najbolj_oddaljena], x[najbolj_oddaljena]) # dodano
  y <- c(y[-najbolj_oddaljena], y[najbolj_oddaljena]) # dodano
  nicelna_matrika <- matrix(0,st_tock,st_tock); 
  matrika <- izracunaj_razdaljo(x,y,nicelna_matrika)
  for (i in 1:nrow(matrika)){
    for (j in 1: ncol(matrika)){
      if (matrika[i,j] < 0){
        matrika[i,j] <- 0
      }
      if ( matrika[i,j] > r){
        matrika[i,j] <- 0
      }
    }
  }
  return(pretvorba_v_igraph(matrika))
}

#2) utez je inverz povezave (se pravi bli�je sta to�ki, ve�ja je ute�)

igraf_razdalje_so_inverz <- function(st_tock, r){
  graf <- igraf_razdalje_so_utezi(st_tock, r)
  E(graf)$V3 <- 1/E(graf)$V3
  E(graf)$label <- 1/E(graf)$label
  print(plot.igraph(graf))
  return(graf)
} 

#3) utezi so na povezavah naklju�no izbrane 

igraf_utezi_so_nakljucne <- function(st_tock, r,max_stevilo){
  graf <- igraf_razdalje_so_utezi(st_tock, r)
  t <- sample(1:max_stevilo, length(E(graf)$V3), replace=TRUE)
  E(graf)$V3 <-t
  E(graf)$label <- t
  print(plot.igraph(graf))
  return(graf)
}
