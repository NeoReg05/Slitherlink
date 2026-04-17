#################################################
# 1. OUTILS DE BASES ET NORMALISATION
#################################################

normalize_edge <- function(edge) {
  if (edge$x1 > edge$x2 || (edge$x1 == edge$x2 && edge$y1 > edge$y2)) {
    return(list(x1 = edge$x2, y1 = edge$y2, x2 = edge$x1, y2 = edge$y1))
  }
  edge
}

create_edge <- function(x1, y1, x2, y2) {
  normalize_edge(list(x1 = x1, y1 = y1, x2 = x2, y2 = y2))
}

same_edge <- function(e1, e2) {
  # On s'assure que les deux sont normalisés avant comparaison
  e1 <- normalize_edge(e1)
  e2 <- normalize_edge(e2)
  e1$x1 == e2$x1 && e1$y1 == e2$y1 && e1$x2 == e2$x2 && e1$y2 == e2$y2
}

cell_edges <- function(i, j) {
  # i = ligne (y), j = colonne (x)
  list(
    create_edge(j-1, i-1, j, i-1), # haut
    create_edge(j-1, i, j, i),     # bas
    create_edge(j-1, i-1, j-1, i), # gauche
    create_edge(j, i-1, j, i)      # droite
  )
}

#################################################
# 2. FONCTIONS OUTILS DE GÉNÉRATION
#################################################

# Récupère tous les voisins des cases "inside" (avec doublons pour compter les contacts)
get_all_neighbors <- function(inside_cells, n, m) {
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
  return(neighbors)
}

# Extraction des arêtes de bordure via un dictionnaire (évite les 4)
extract_borders <- function(inside_cells) {
  edge_counts <- new.env()
  for(cell in inside_cells) {
    coords <- as.numeric(strsplit(cell, "_")[[1]])
    for(cand in cell_edges(coords[1], coords[2])) {
      key <- paste(cand$x1, cand$y1, cand$x2, cand$y2, sep="_")
      edge_counts[[key]] <- if(exists(key, edge_counts)) edge_counts[[key]] + 1 else 1
    }
  }

  final_edges <- list()
  for(key in ls(edge_counts)) {
    if(edge_counts[[key]] == 1) {
      pts <- as.numeric(strsplit(key, "_")[[1]])
      final_edges[[length(final_edges) + 1]] <- list(x1=pts[1], y1=pts[2], x2=pts[3], y2=pts[4])
    }
  }
  return(final_edges)
}

#################################################
# 3. ALGORITHMES DE GÉNÉRATION PAR DIFFICULTÉ
#################################################

generate_loop_easy <- function(n, m) {
  cx <- sample(1:m, 1); cy <- sample(1:n, 1)
  inside_cells <- paste(cy, cx, sep="_")
  iterations <- floor((n * m) * 0.5)
  for(i in 1:iterations) {
    neighbors <- get_all_neighbors(inside_cells, n, m)
    if(length(neighbors) == 0) break
    # On privilégie les cases qui densifient la forme
    inside_cells <- unique(c(inside_cells, sample(neighbors, 1)))
  }
  extract_borders(inside_cells)
}

generate_loop_medium <- function(n, m) {
  cx <- sample(1:m, 1); cy <- sample(1:n, 1)
  inside_cells <- paste(cy, cx, sep="_")
  iterations <- floor((n * m) * 0.4)
  for(i in 1:iterations) {
    neighbors <- get_all_neighbors(inside_cells, n, m)
    if(length(neighbors) == 0) break
    inside_cells <- unique(c(inside_cells, sample(unique(neighbors), 1)))
  }
  extract_borders(inside_cells)
}

generate_loop_hard <- function(n, m) {
  cx <- sample(1:m, 1); cy <- sample(1:n, 1)
  inside_cells <- paste(cy, cx, sep="_")

  # On baisse à 45% pour éviter la saturation du 10x10
  iterations <- floor((n * m) * 0.45)

  for(i in 1:iterations) {
    all_neighbors <- get_all_neighbors(inside_cells, n, m)
    if(length(all_neighbors) == 0) break

    counts <- table(all_neighbors)
    valid_neighbors <- names(counts)[counts == 1]

    # Si le serpent est coincé, on autorise n'importe quel voisin
    if(length(valid_neighbors) == 0) {
      valid_neighbors <- names(counts)
    }

    inside_cells <- unique(c(inside_cells, sample(valid_neighbors, 1)))
  }
  extract_borders(inside_cells)
}

#################################################
# 4. LOGIQUE DU JEU ET INDICES
#################################################

compute_clues <- function(n, m, edges) {
  grid <- matrix(0, nrow = n, ncol = m)
  for (i in 1:n) {
    for (j in 1:m) {
      c_edges <- cell_edges(i, j)
      count <- 0
      for (ce in c_edges) {
        for (e in edges) {
          if (same_edge(ce, e)) {
            count <- count + 1
            break
          }
        }
      }
      grid[i, j] <- count
    }
  }
  grid
}

remove_clues <- function(grid, p = 0.5) {
  mask <- matrix(runif(length(grid)) < p, nrow = nrow(grid))
  grid[mask] <- NA
  grid
}

generate_slitherlink <- function(n = 5, m = 5, difficulty = "medium") {
  p_hide <- switch(difficulty, easy = 0.25, medium = 0.45, hard = 0.65)

  valid_grid <- FALSE
  while(!valid_grid) {
    solution <- switch(difficulty,
                       easy = generate_loop_easy(n, m),
                       medium = generate_loop_medium(n, m),
                       hard = generate_loop_hard(n, m))

    grid_full <- compute_clues(n, m, solution)
    # Sécurité anti-4
    if (!any(grid_full == 4, na.rm = TRUE)) valid_grid <- TRUE
  }

  grid_hidden <- remove_clues(grid_full, p_hide)
  list(grid = grid_hidden, solution = solution)
}

#################################################
# 5. INTERACTION ET AFFICHAGE
#################################################

edge_from_click <- function(click, n, m) {
  if (is.null(click)) return(NULL)
  x <- click$x
  y <- n - click$y
  gx <- floor(x); gy <- floor(y)
  if (x < -0.2 || x > m + 0.2 || y < -0.2 || y > n + 0.2) return(NULL)

  candidates <- list(
    create_edge(gx, gy, gx + 1, gy),     # Haut
    create_edge(gx, gy + 1, gx + 1, gy + 1), # Bas
    create_edge(gx, gy, gx, gy + 1),     # Gauche
    create_edge(gx + 1, gy, gx + 1, gy + 1)  # Droite
  )
  centers <- matrix(c(gx+0.5, gy, gx+0.5, gy+1, gx, gy+0.5, gx+1, gy+0.5), ncol=2, byrow=TRUE)
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

draw_game <- function(grid, edges) {
  n <- nrow(grid); m <- ncol(grid)
  plot(NULL, xlim=c(-0.2, m+0.2), ylim=c(-0.2, n+0.2), asp=1, axes=FALSE, xlab="", ylab="")
  rect(0, 0, m, n, col="#F8FAFC", border="#D0D7DE", lwd=2)

  for (i in 1:n) {
    for (j in 1:m) {
      if (!is.na(grid[i,j])) {
        text(j-0.5, n-i+0.5, labels=grid[i,j], cex=1.5, font=2, col="#1A1D21")
      }
    }
  }
  for (i in 0:m) {
    for (j in 0:n) points(i, j, pch=16, cex=0.8, col="#444444")
  }
  if (length(edges) > 0) {
    for (e in edges) {
      segments(e$x1, n - e$y1, e$x2, n - e$y2, col = "#2C7BE5", lwd = 5, lend=1)
    }
  }
}

#################################################
# 6. VÉRIFICATIONS
#################################################

check_rules <- function(grid, edges) {
  n <- nrow(grid); m <- ncol(grid)
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
  adj <- list()
  for (e in edges) {
    v1 <- paste(e$x1, e$y1, sep="_"); v2 <- paste(e$x2, e$y2, sep="_")
    adj[[v1]] <- c(adj[[v1]], v2); adj[[v2]] <- c(adj[[v2]], v1)
  }
  if (any(sapply(adj, length) != 2)) return(FALSE)

  start_node <- names(adj)[1]
  visited <- character(0); to_visit <- c(start_node)
  while(length(to_visit) > 0) {
    curr <- to_visit[1]; to_visit <- to_visit[-1]
    if (!(curr %in% visited)) {
      visited <- c(visited, curr)
      to_visit <- c(to_visit, adj[[curr]])
    }
  }
  return(length(visited) == length(adj))
}
