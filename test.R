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
pregled_v_sirino <- function(igraph,s,t, starsi, stevec = 2 ){ # starsi so najprej prazni 
  obiskani <- rep(FALSE, t) # najprej noben ni obiskan
  #fifo_1 <- deque() 
  #fifo_1$push(s) # toèka od kje naprej nej gleda 
  obiskani[1] <- TRUE # to toèko kjer zaènemo gledam nastavimo, kot obiskana
  #stevec <- 2 # števec nastavimo na 2
  u <- s # iz kje gledamo sosede 
  for (indeks in neighbors(igraph, u)){ # zdej naprej pogledamo vse sosede 
    obiskani[indeks] = TRUE # te smo obiskal, 2,3,5
    print(indeks)}
  if (obiskani[t] == TRUE){
    starsi[stevec] <- u
    #igraph <- delete.edges(igraph,{u}|{t}) # to bomo rabl v endmonds karpu
    print("kaj ti, jst sm konèou, ti dam pot")
    pot <- starsi[2:length(starsi)]
    pot[length(pot)+1] <- t
    return( pot )}
    else {
      obiskani <- obiskani[2:length(obiskani)] #prvi je itak obiskan 
      tocka_od_katere_gledamo <- which(obiskani) + 1 
      tocka_od_katere_gledamo <- tocka_od_katere_gledamo[1]
      starsi[stevec] <- u
      stevec <- stevec + 1 
      print("sem šel else, delam rekurzijo")
      return(pregled_v_sirino(igraph, tocka_od_katere_gledamo, t, starsi, stevec))
    }
}
