library(igraph)
library(prodlim)
library(foreach)
library(collections)



#generiranje matrike sosednosti
generira_matriko <- function(st_tock, min_stevil = 0, max_stevil){  # tuki mora mit minimum veÄ ali enako 0 
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



#bolj uèinkovita razlièica 
pretvorba_v_igraph <- function(matrika){
  vozlisca <- 1: (nrow(matrika))
  povezave <- oceti_in_sinovi(matrika)
  utezi <- oceti_in_sinovi(matrika)[,3]
  g <- graph_from_data_frame(povezave, directed = TRUE, vertices = vozlisca) %>% set_edge_attr("label", value = utezi )
  print(plot.igraph(g))
  return(g)
}


#igraph <- pretvorba_v_igraph(matrika) #vedno treba prvo preden uporabimo spodnjo funkcijo, matriko spremeniti v igraf
pregled_v_sirino_3 <- function(igraph,s,t, starsi, stevec = 2 ){ # starsi so najprej prazni 
  obiskani <- rep(FALSE, t) # najprej noben ni obiskan
  obiskani[1] <- TRUE # to toèko kjer zaènemo gledam nastavimo, kot obiskana
  u <- s # iz kje gledamo sosede
  if (length(neighbors(igraph, 1)) == 0){ # izvor nima veè sosedov
    return(list(obiskani[t], u)) # ta 1 je sam da neki vrne
  }
  for (indeks in neighbors(igraph, u)){ # zdej naprej pogledamo vse sosede 
    obiskani[indeks] = TRUE # te smo obiskal, 2,3,5
  if (obiskani[t] == TRUE){
    starsi[stevec] <- u
    #igraph <- delete.edges(igraph,{u}|{t}) # to bomo rabl v endmonds karpu
    pot <- starsi[2:length(starsi)]
    pot[length(pot)+1] <- t
    return(list(obiskani[t], pot))}
  else {
    obiskani <- obiskani[2:length(obiskani)] #prvi je itak obiskan 
    tocka_od_katere_gledamo <- which(obiskani) + 1 
    tocka_od_katere_gledamo <- tocka_od_katere_gledamo[1]
    starsi[stevec] <- u
    stevec <- stevec + 1 
    return(pregled_v_sirino_3(igraph, tocka_od_katere_gledamo, t, starsi, stevec))
    }
  }
}

edmonds_karp_2 <- function(igraf){
  s <- 1 # sej je vedno 1
  t <- length(V(igraf))
  vrednost_in_pot <- pregled_v_sirino(igraf,s,t,c(),2) #nova spremenljivka, da ne klièe dvakrat pregleda v širino 
  pot_vozlisc <- vrednost_in_pot[[2]]
  starsi <- pot_vozlisc # 
  ali_obstaja <- vrednost_in_pot[[1]]
  pretok <- 0 # pretok nastavimo na 0 
  #povezave <- get.edges(igraf, c(1:gsize(igraf)))
  #utezi <- E(igraf)$V3
  while (ali_obstaja == TRUE) {
    starsi_drugace = rep(starsi, each=2)[-1]
    starsi_drugace = starsi_drugace[-length(starsi_drugace)]
    utezi_poti <- E(igraf)$V3[get.edge.ids(igraf,starsi_drugace)]#vren vtezi poti po tej poti k sva jo dubla 
    min_poti <- min(utezi_poti)
    pretok <- pretok + min_poti
    for (i in 1:length(utezi_poti)){  # tle notri se posodobi uteži, vsaj ena na tej poti postane 0 in ni veè uporabna 
      E(igraf)$V3[get.edge.ids(igraf,starsi_drugace)][i] <- E(igraf)$V3[get.edge.ids(igraf,starsi_drugace)][i] - min_poti
      E(igraf)$label[get.edge.ids(igraf,starsi_drugace)][i] <- E(igraf)$label[get.edge.ids(igraf,starsi_drugace)][i] - min_poti # zarad plota 
    }
    
    igraf <- delete.edges(igraf, which(E(igraf)$V3==0)) # izbrišemo uteži, ki so enake 0, ker niso veè uporabne 

    vrednost_in_pot <- pregled_v_sirino(igraf,s,t,c(),2)
    ali_obstaja <- vrednost_in_pot[[1]]
    starsi <- vrednost_in_pot[[2]]
    
  }
  
  return(pretok)
  
  
}





