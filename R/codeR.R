#################################################
# GENERATION D'UNE BOUCLE VALIDE
#################################################

generate_random_loop <- function(n, m){

  edges <- list()

  # boucle autour de la grille
  for(i in 1:(m-1)){
    edges[[length(edges)+1]] <- list(x1=i,y1=1,x2=i+1,y2=1)
  }

  for(i in 1:(n-1)){
    edges[[length(edges)+1]] <- list(x1=m,y1=i,x2=m,y2=i+1)
  }

  for(i in m:2){
    edges[[length(edges)+1]] <- list(x1=i,y1=n,x2=i-1,y2=n)
  }

  for(i in n:2){
    edges[[length(edges)+1]] <- list(x1=1,y1=i,x2=1,y2=i-1)
  }

  edges
}

#################################################
# CALCUL DES INDICES
#################################################

compute_clues <- function(n, m, edges){

  grid <- matrix(0,nrow=n,ncol=m)

  for(i in 1:n){
    for(j in 1:m){

      count <- 0

      for(e in edges){

        if(
          (e$x1==j & e$y1==i & e$x2==j+1 & e$y2==i) |
          (e$x1==j & e$y1==i & e$x2==j & e$y2==i+1) |
          (e$x1==j+1 & e$y1==i & e$x2==j+1 & e$y2==i+1) |
          (e$x1==j & e$y1==i+1 & e$x2==j+1 & e$y2==i+1)
        ){
          count <- count + 1
        }

      }

      grid[i,j] <- count

    }
  }

  grid
}

#################################################
# SUPPRIMER CERTAINS INDICES
#################################################

remove_clues <- function(grid,p=0.5){

  n <- nrow(grid)
  m <- ncol(grid)

  for(i in 1:n){
    for(j in 1:m){

      if(runif(1)<p){
        grid[i,j] <- NA
      }

    }
  }

  grid
}

#################################################
# GENERATEUR COMPLET
#################################################

generate_slitherlink <- function(n=5,m=5){

  solution <- generate_random_loop(n,m)

  grid <- compute_clues(n,m,solution)

  grid <- remove_clues(grid,0.5)

  list(
    grid=grid,
    solution=solution
  )

}

#################################################
# DETECTION SEGMENT DEPUIS CLIC
#################################################

create_edge <- function(x1,y1,x2,y2){

  list(x1=x1,y1=y1,x2=x2,y2=y2)

}

edge_from_click <- function(click){

  x <- click$x
  y <- click$y

  gx <- floor(x)
  gy <- floor(y)

  dx <- x-gx
  dy <- y-gy

  if(abs(dy)<abs(dx)){
    return(create_edge(gx,gy,gx+1,gy))
  }else{
    return(create_edge(gx,gy,gx,gy+1))
  }

}

#################################################
# COMPARER SEGMENTS
#################################################

same_edge <- function(e1,e2){

  (e1$x1==e2$x1 &
     e1$y1==e2$y1 &
     e1$x2==e2$x2 &
     e1$y2==e2$y2)

}

#################################################
# GESTION SEGMENTS
#################################################

toggle_edge <- function(edges,edge){

  for(i in seq_along(edges)){
    if(same_edge(edges[[i]],edge)){
      return(edges[-i])
    }
  }

  edges[[length(edges)+1]] <- edge
  edges
}

add_edge_from_click <- function(edges,click){

  edge <- edge_from_click(click)

  toggle_edge(edges,edge)

}

#################################################
# AFFICHAGE
#################################################

draw_game <- function(grid,edges){

  n <- nrow(grid)
  m <- ncol(grid)

  plot(c(0,m+1),c(0,n+1),type="n",asp=1,axes=FALSE,xlab="",ylab="")

  for(i in 1:(m+1)){
    segments(i,1,i,n+1,col="grey")
  }

  for(j in 1:(n+1)){
    segments(1,j,m+1,j,col="grey")
  }

  for(i in 1:n){
    for(j in 1:m){

      if(!is.na(grid[i,j])){
        text(j+0.5,n-i+1.5,grid[i,j],cex=1.5)
      }

    }
  }

  for(e in edges){

    segments(
      e$x1,
      e$y1,
      e$x2,
      e$y2,
      col="blue",
      lwd=3
    )

  }

}

#################################################
# VERIFIER REGLES
#################################################

count_edges_cell <- function(edges,i,j){

  count <- 0

  for(e in edges){

    if(
      (e$x1==j & e$y1==i) |
      (e$x2==j & e$y2==i)
    ){
      count <- count+1
    }

  }

  count
}

check_rules <- function(grid,edges){

  n <- nrow(grid)
  m <- ncol(grid)

  for(i in 1:n){
    for(j in 1:m){

      if(!is.na(grid[i,j])){

        if(count_edges_cell(edges,i,j)!=grid[i,j]){
          return(FALSE)
        }

      }

    }
  }

  TRUE
}

#################################################
# VERIFIER BOUCLE
#################################################

check_single_loop <- function(edges){

  if(length(edges)<4){
    return(FALSE)
  }

  vertices <- list()

  for(e in edges){

    v1 <- paste(e$x1,e$y1)
    v2 <- paste(e$x2,e$y2)

    vertices[[v1]] <- (vertices[[v1]] %||% 0)+1
    vertices[[v2]] <- (vertices[[v2]] %||% 0)+1

  }

  for(v in vertices){

    if(v!=2){
      return(FALSE)
    }

  }

  TRUE
}
