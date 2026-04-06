#################################################
# OUTILS
#################################################

normalize_edge <- function(edge) {
  if (edge$x1 > edge$x2 || (edge$x1 == edge$x2 && edge$y1 > edge$y2)) {
    return(list(
      x1 = edge$x2, y1 = edge$y2,
      x2 = edge$x1, y2 = edge$y1
    ))
  }
  edge
}

create_edge <- function(x1, y1, x2, y2) {
  normalize_edge(list(x1 = x1, y1 = y1, x2 = x2, y2 = y2))
}

same_edge <- function(e1, e2) {
  e1 <- normalize_edge(e1)
  e2 <- normalize_edge(e2)
  e1$x1 == e2$x1 && e1$y1 == e2$y1 && e1$x2 == e2$x2 && e1$y2 == e2$y2
}

#################################################
# GENERATION
#################################################

generate_random_loop <- function(n, m) {
  # 1. Choisir une case de départ
  cx <- sample(1:m, 1); cy <- sample(1:n, 1)
  inside_cells <- paste(cy, cx, sep="_")

  # 2. Expansion de la forme
  iterations <- floor((n * m) * 0.4)
  for(i in 1:iterations) {
    neighbors <- c()
    for(cell in inside_cells) {
      coords <- as.numeric(strsplit(cell, "_")[[1]])
      r <- coords[1]; c <- coords[2]
      pots <- list(c(r-1,c), c(r+1,c), c(r,c-1), c(r,c+1))
      for(p in pots) {
        if(p[1] >= 1 && p[1] <= n && p[2] >= 1 && p[2] <= m) {
          p_str <- paste(p[1], p[2], sep="_")
          if(!(p_str %in% inside_cells)) neighbors <- c(neighbors, p_str)
        }
      }
    }
    if(length(neighbors) == 0) break
    inside_cells <- unique(c(inside_cells, sample(unique(neighbors), 1)))
  }

  # 3. Extraction des arêtes de bordure (La clé est ici)
  # On va compter combien de fois chaque arête apparaît
  edge_counts <- list()

  for(cell in inside_cells) {
    coords <- as.numeric(strsplit(cell, "_")[[1]])
    r <- coords[1]; c <- coords[2]

    # Récupérer les 4 arêtes de la case
    c_edges <- cell_edges(r, c)
    for(e in c_edges) {
      # Créer une clé unique "x1_y1_x2_y2" toujours triée
      key <- paste(e$x1, e$y1, e$x2, e$y2, sep="_")
      if(is.null(edge_counts[[key]])) {
        edge_counts[[key]] <- 1
      } else {
        edge_counts[[key]] <- edge_counts[[key]] + 1
      }
    }
  }

  # Une arête appartient à la boucle UNIQUEMENT si elle n'est pas partagée
  # (count == 1). Si count == 2, elle est à l'intérieur de la forme.
  final_edges <- list()
  for(key in names(edge_counts)) {
    if(edge_counts[[key]] == 1) {
      coords <- as.numeric(strsplit(key, "_")[[1]])
      final_edges[[length(final_edges) + 1]] <- list(
        x1=coords[1], y1=coords[2], x2=coords[3], y2=coords[4]
      )
    }
  }

  return(final_edges)
}

cell_edges <- function(i, j) {
  # i = ligne, j = colonne
  list(
    create_edge(j-1, i-1, j, i-1), # haut
    create_edge(j-1, i, j, i),     # bas
    create_edge(j-1, i-1, j-1, i), # gauche
    create_edge(j, i-1, j, i)      # droite
  )
}

compute_clues <- function(n, m, edges) {
  grid <- matrix(0, nrow = n, ncol = m)
  for (i in 1:n) {
    for (j in 1:m) {
      c_edges <- cell_edges(i, j)
      count <- 0
      for (ce in c_edges) {
        # On ne compte l'arête que si elle est EXACTEMENT dans la liste
        # des arêtes de bordure renvoyées par generate_random_loop
        for (e in edges) {
          if (same_edge(ce, e)) {
            count <- count + 1
            break # On passe à l'arête suivante de la cellule
          }
        }
      }
      grid[i, j] <- count
    }
  }
  # Dans un Slitherlink valide, un indice 4 est impossible.
  # On peut forcer le remplacement au cas où, mais la logique ci-dessus
  # devrait déjà limiter le score à 3 maximum.
  grid
}

remove_clues <- function(grid, p = 0.5) {
  mask <- matrix(runif(length(grid)) < p, nrow = nrow(grid))
  grid[mask] <- NA
  grid
}

generate_slitherlink <- function(n = 5, m = 5, p_hide = 0.45) {
  solution <- generate_random_loop(n, m)
  grid <- compute_clues(n, m, solution)
  grid <- remove_clues(grid, p_hide)
  list(grid = grid, solution = solution)
}

#################################################
# INTERACTION ET CLIC
#################################################

edge_from_click <- function(click, n, m) {
  if (is.null(click)) return(NULL)

  x <- click$x
  y <- n - click$y # Inversion directe pour coller au dessin n-y

  gx <- floor(x)
  gy <- floor(y)

  # Sécurité sur les bords
  if (x < -0.2 || x > m + 0.2 || y < -0.2 || y > n + 0.2) return(NULL)

  candidates <- list()
  centers <- matrix(NA, nrow=0, ncol=2)

  # On teste les 4 arêtes autour du carré cliqué
  # Horizontal Haut
  candidates[[1]] <- create_edge(gx, gy, gx + 1, gy)
  centers <- rbind(centers, c(gx + 0.5, gy))
  # Horizontal Bas
  candidates[[2]] <- create_edge(gx, gy + 1, gx + 1, gy + 1)
  centers <- rbind(centers, c(gx + 0.5, gy + 1))
  # Vertical Gauche
  candidates[[3]] <- create_edge(gx, gy, gx, gy + 1)
  centers <- rbind(centers, c(gx, gy + 0.5))
  # Vertical Droite
  candidates[[4]] <- create_edge(gx + 1, gy, gx + 1, gy + 1)
  centers <- rbind(centers, c(gx + 1, gy + 0.5))

  dists <- sqrt((centers[,1] - x)^2 + (centers[,2] - y)^2)
  closest <- which.min(dists)

  if (dists[closest] > 0.45) return(NULL)
  candidates[[closest]]
}

toggle_edge <- function(edges, edge) {
  if (length(edges) == 0) return(list(edge))
  for (k in seq_along(edges)) {
    if (same_edge(edges[[k]], edge)) return(edges[-k])
  }
  edges[[length(edges) + 1]] <- edge
  edges
}

add_edge_from_click <- function(edges, click, n, m) {
  edge <- edge_from_click(click, n, m)
  if (is.null(edge)) return(edges)
  toggle_edge(edges, edge)
}

#################################################
# AFFICHAGE
#################################################

draw_game <- function(grid, edges) {
  n <- nrow(grid)
  m <- ncol(grid)

  # On ajoute une petite marge pour ne pas couper les points
  plot(NULL, xlim=c(-0.2, m+0.2), ylim=c(-0.2, n+0.2),
       asp=1, axes=FALSE, xlab="", ylab="", xaxs="i", yaxs="i")

  rect(0, 0, m, n, col="#F8FAFC", border="#D0D7DE", lwd=2)

  # Indices
  for (i in 1:n) {
    for (j in 1:m) {
      if (!is.na(grid[i,j])) {
        text(j-0.5, n-i+0.5, labels=grid[i,j], cex=1.5, font=2, col="#1A1D21")
      }
    }
  }

  # Points aux intersections
  for (i in 0:m) {
    for (j in 0:n) {
      points(i, j, pch=16, cex=0.8, col="#444444")
    }
  }

  # Segments du joueur
  if (length(edges) > 0) {
    for (e in edges) {
      segments(
        e$x1, n - e$y1,
        e$x2, n - e$y2,
        col = "#2C7BE5", lwd = 5, lend=1
      )
    }
  }
}

#################################################
# VERIFICATION
#################################################

check_rules <- function(grid, edges) {
  n <- nrow(grid)
  m <- ncol(grid)
  for (i in 1:n) {
    for (j in 1:m) {
      if (!is.na(grid[i,j])) {
        c_edges <- cell_edges(i, j)
        count <- 0
        for (ce in c_edges) {
          for (e in edges) if (same_edge(ce, e)) count <- count + 1
        }
        if (count != grid[i,j]) return(FALSE)
      }
    }
  }
  TRUE
}

check_single_loop <- function(edges) {
  if (length(edges) < 4) return(FALSE)

  vertices <- list()
  adj <- list()

  for (e in edges) {
    v1 <- paste(e$x1, e$y1, sep="_")
    v2 <- paste(e$x2, e$y2, sep="_")

    # Compte les degrés
    vertices[[v1]] <- (if(is.null(vertices[[v1]])) 0 else vertices[[v1]]) + 1
    vertices[[v2]] <- (if(is.null(vertices[[v2]])) 0 else vertices[[v2]]) + 1

    # Adjacence
    adj[[v1]] <- c(adj[[v1]], v2)
    adj[[v2]] <- c(adj[[v2]], v1)
  }

  # Règle 1 : Chaque point touché doit avoir un degré de 2
  degs <- unlist(vertices)
  if (any(degs != 2)) return(FALSE)

  # Règle 2 : Une seule composante connexe
  start_node <- names(adj)[1]
  visited <- character(0)
  to_visit <- c(start_node)

  while(length(to_visit) > 0) {
    curr <- to_visit[1]
    to_visit <- to_visit[-1]
    if (!(curr %in% visited)) {
      visited <- c(visited, curr)
      to_visit <- c(to_visit, adj[[curr]])
    }
  }

  return(length(visited) == length(adj))
}
