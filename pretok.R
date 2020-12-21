library(igraph)
library(prodlim)
library(foreach)
library(collections)

#generiranje matrike sosednosti
generira_matriko <- function(st_tock, min_stevil = 0, max_stevil){  # tuki mora mit minimum veƒç ali enako 0 
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
  st_tock <- sqrt(length(a))
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

#naredi matriko, kjer so navedene povezave oƒçe sin in v tretjem stolpcu ute≈æi 
oceti_in_sinovi <- function(matrika){
  zacetna_tocka <- 1
  # sinovi <- list()
  koncna_tocka <- sqrt(length(matrika))
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
  matrika_oce_sin<- matrika_oce_sin[apply(matrika_oce_sin[,-1], 1, function(x) !all(x==0)),] #odstrani vrstice kjer so niƒçle 
  return(matrika_oce_sin)
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
  st_tock <- sqrt(length(matrika))
  poti <- vse_poti(matrika)
  mat_oce_sin <- oceti_in_sinovi(matrika)
  mat_oce_sin_2 <- as.data.frame(mat_oce_sin[, -3])
  #doloci utezi od poti
  utezi_poti <- list()
  utezi <- c()
  #if (sum(matrika[1,])== 0){
  #  stop("nimam veË izhodov")
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


posodobi_mat_oce_sin <- function(mat_oce_sin, mat_oce_sin_2, minimum_na_poti,vse_mozne_poti,i){
  v <- as.vector(vse_mozne_poti[[i]])
  for (k in 2:length(v)){
    zac_p <- v[k-1]
    kon_p <- v[k]
    st_vrstice <-row.match(c(zac_p,kon_p), mat_oce_sin_2)
    mat_oce_sin[st_vrstice,3] <- mat_oce_sin[st_vrstice,3] - minimum_na_poti
  }
  return(mat_oce_sin)
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


#iz matrike ocetje sinovi s posodobljenimi uteûmi naredi novo matriko
mat_iz_ocetje_sinovi <- function(oceti_in_sinovi){
  zozena_oce_sin <-oceti_in_sinovi[-which(rowSums(oceti_in_sinovi==0)>0),]
  zozena_oce_sin_2 <- zozena_oce_sin[,-3]
  st_tock <-max(max(zozena_oce_sin_2))
  nova_gen_mat <- matrix(0, st_tock, st_tock)
  for (i in zozena_oce_sin[,1]){
    for (j in zozena_oce_sin[,2]){
      vrstica <- row.match(c(i,j), zozena_oce_sin_2)
      if (is.na(vrstica) == FALSE){
        dodani_element <- zozena_oce_sin[vrstica,3]
        nova_gen_mat[i,j] <- dodani_element
      }
    }
  }
  return(nova_gen_mat)  
  
  
}
#funkcija najveËjega pretoka, neuËinkovita za grafe z veË kot 7 toËkami
najvecji_pretok <- function(matrika){
  max_pretok <- 0
  mat_oce_sin <- oceti_in_sinovi(matrika)
  mat_oce_sin_2 <- as.data.frame(mat_oce_sin[, -3])
  utezi_poti <- utezi_na_poti(matrika)
  vse_mozne_poti<- vse_poti(matrika)
  stevec <- length(utezi_poti)
  while (stevec != 0){ # dolûina uteûenih poti pa vseh moûnih poti je enaka 
    v <- utezi_poti[[1]] # vektor utezi na poti
    minimum_na_poti <- min(v) # minimum utezi na tej poti
    max_pretok <- max_pretok + minimum_na_poti # pretok poveËamo za to uteû
    polozaj_minimuma <- which(v == minimum_na_poti)[1] # indeks na katerem mestu je minimum v utezeh 
    #mat_oce_sin <- posodobi_mat_oce_sin(mat_oce_sin, mat_oce_sin_2, minimum_na_poti,vse_mozne_poti,1)   ---> ne dela glih najbl
    matrika <- posodobljena_generirana_matrika(matrika,polozaj_minimuma,minimum_na_poti, vse_mozne_poti,1)
    if (sum(matrika[1,]) <= 0){
      return(max_pretok)
    } 
    if ((sum(matrika[,ncol(matrika)]) <= 0)){
      return(max_pretok)
    }
    utezi_poti <- utezi_na_poti(matrika)  # tle se prtoû pr zadnjem koraku k matriko ustav (ampak zdej k je ta IF se ne bi smel)
    vse_mozne_poti<- vse_poti(matrika)
    stevec <- length(utezi_poti)
  
  }
}




#bolj uËinkovita razliËica 
pretvorba_v_igraph <- function(matrika){
  vozlisca <- 1: (nrow(matrika))
  povezave <- oceti_in_sinovi(matrika)
  utezi <- oceti_in_sinovi(matrika)[,3]
  g <- graph_from_data_frame(povezave, directed = TRUE, vertices = vozlisca) %>% set_edge_attr("label", value = utezi )
  print(plot.igraph(g))
  return(g)
}
#igraph <- pretvorba_v_igraph(matrika) #vedno treba prvo preden uporabimo spodnjo funkcijo, matriko spremeniti v igraf
pregled_v_sirino <- function(igraph,s,t){
  st_vrstic <- t
  starsi <- c()
  obiskani <- rep(FALSE, st_vrstic)
  fifo_1 <- deque()
  fifo_1$push(s)
  obiskani[1] <- TRUE
  stevec <- 2
  while (fifo_1$size() > 0) {
    u <- fifo_1$popleft() 
    for (indeks in neighbors(igraph, u)){
      obiskani[indeks] = TRUE # te smo obiskal, 2,3,5
      for (vrednost in neighbors(igraph, indeks)){
        if ((obiskani[vrednost] == FALSE) ){ #& (vrednosti > 0)
          fifo_1$push(indeks)
          obiskani[vrednost] = TRUE
          starsi[stevec] = indeks
        
        }
      }
    }
      
  }
  return(list(obiskani[t],starsi))
  
}


edmonds_karp <- function(igraf, s,t){
  starsi <- pregled_v_sirino(igraf,s,t)[[2]]
  starsi[1] <- 1
  starsi[length(starsi) +1] <- t
  ali_obstaja <- pregled_v_sirino(igraf,s,t)[[1]]
  pretok <- 0
  #povezave <- get.edges(igraf, c(1:gsize(igraf)))
  #utezi <- E(igraf)$V3

  while (ali_obstaja == TRUE) {
    pretok_poti <- Inf
    min_poti <- Inf # min bo sproti primerju
    starsi_drugace = rep(starsi, each=2)[-1]
    starsi_drugace = starsi_drugace[-length(starsi_drugace)]
    utezi_poti <- E(igraf)$V3[get.edge.ids(igraf,starsi_drugace)] #vren vtezi poti po tej poti k sva jo dubla 
    min_poti <- min(utezi_poti)
    print(min_poti)
    pretok = pretok + min_poti
    for (i in utezi_poti){
      E(igraf)$V3[get.edge.ids(igraf,starsi_drugace)][i] <- E(igraf)$V3[get.edge.ids(igraf,starsi_drugace)][i] - min_poti
    }
    

    ali_obstaja <- pregled_v_sirino(igraf,s, t)[[1]]
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
#E(b)$V3[1] <- E(b)$V3[1] -2 #tko zbriöeö uteû 
# E(b)$V3
#E(b)$V3[get.edge.ids(b, c(1,2,2,5))] dobimo vektor utezi pozi
# gsize(g) ---> ötevilo povezav v grafu
# edge_attr(g) ---> uteûi na povezavah
# get.edges(g,c(1:6)) ---> matrika povezav (oËe-sin)
