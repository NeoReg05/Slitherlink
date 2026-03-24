#################################################
# OUTILS
#################################################

normalize_edge <- function(edge) {
  if (
    edge$x1 > edge$x2 ||
    (edge$x1 == edge$x2 && edge$y1 > edge$y2)
  ) {
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

  e1$x1 == e2$x1 &&
    e1$y1 == e2$y1 &&
    e1$x2 == e2$x2 &&
    e1$y2 == e2$y2
}

#################################################
# GENERATION D'UNE BOUCLE SIMPLE
#################################################

generate_random_loop <- function(n, m) {
  # boucle rectangulaire aléatoire interne
  top    <- sample(1:(n - 1), 1)
  bottom <- sample((top + 1):n, 1)
  left   <- sample(1:(m - 1), 1)
  right  <- sample((left + 1):m, 1)

  edges <- list()

  for (j in left:(right - 1)) {
    edges[[length(edges) + 1]] <- create_edge(j, top, j + 1, top)
    edges[[length(edges) + 1]] <- create_edge(j, bottom, j + 1, bottom)
  }

  for (i in top:(bottom - 1)) {
    edges[[length(edges) + 1]] <- create_edge(left, i, left, i + 1)
    edges[[length(edges) + 1]] <- create_edge(right, i, right, i + 1)
  }

  edges
}

#################################################
# CALCUL DES INDICES
#################################################

cell_edges <- function(i, j) {
  list(
    create_edge(j, i, j + 1, i),       # haut
    create_edge(j, i, j, i + 1),       # gauche
    create_edge(j + 1, i, j + 1, i + 1), # droite
    create_edge(j, i + 1, j + 1, i + 1)  # bas
  )
}

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

generate_slitherlink <- function(n = 5, m = 5, p_hide = 0.45) {
  solution <- generate_random_loop(n, m)
  grid <- compute_clues(n, m, solution)
  grid <- remove_clues(grid, p_hide)

  list(
    grid = grid,
    solution = solution
  )
}

#################################################
# CLIC UTILISATEUR
#################################################

edge_from_click <- function(click, n, m) {
  x <- click$x
  y <- click$y

  gx <- floor(x)
  gy <- floor(y)

  dx <- x - gx
  dy <- y - gy

  # bornes de sécurité
  gx <- max(1, min(gx, m))
  gy <- max(1, min(gy, n))

  # choix de l'arête la plus proche
  d_left   <- dx
  d_right  <- 1 - dx
  d_bottom <- dy
  d_top    <- 1 - dy

  dmin <- min(d_left, d_right, d_bottom, d_top)

  if (dmin == d_left && gx >= 1) {
    edge <- create_edge(gx, gy, gx, gy + 1)
  } else if (dmin == d_right && gx + 1 <= m + 1) {
    edge <- create_edge(gx + 1, gy, gx + 1, gy + 1)
  } else if (dmin == d_bottom && gy >= 1) {
    edge <- create_edge(gx, gy, gx + 1, gy)
  } else {
    edge <- create_edge(gx, gy + 1, gx + 1, gy + 1)
  }

  edge
}

toggle_edge <- function(edges, edge) {
  if (length(edges) == 0) {
    return(list(edge))
  }

  for (k in seq_along(edges)) {
    if (same_edge(edges[[k]], edge)) {
      return(edges[-k])
    }
  }

  edges[[length(edges) + 1]] <- edge
  edges
}

add_edge_from_click <- function(edges, click, n, m) {
  edge <- edge_from_click(click, n, m)
  toggle_edge(edges, edge)
}

#################################################
# AFFICHAGE
#################################################

draw_game <- function(grid, edges) {
  n <- nrow(grid)
  m <- ncol(grid)

  plot(
    x = c(1, m + 1),
    y = c(1, n + 1),
    type = "n",
    asp = 1,
    axes = FALSE,
    xlab = "",
    ylab = "",
    xaxs = "i",
    yaxs = "i"
  )

  # fond
  rect(1, 1, m + 1, n + 1, col = "#F8FAFC", border = NA)

  # quadrillage
  for (i in 1:(m + 1)) {
    segments(i, 1, i, n + 1, col = "#D0D7DE", lwd = 1)
  }

  for (j in 1:(n + 1)) {
    segments(1, j, m + 1, j, col = "#D0D7DE", lwd = 1)
  }

  # points
  for (i in 1:(m + 1)) {
    for (j in 1:(n + 1)) {
      points(i, j, pch = 16, cex = 0.7)
    }
  }

  # indices
  for (i in 1:n) {
    for (j in 1:m) {
      if (!is.na(grid[i, j])) {
        text(j + 0.5, n - i + 1.5, labels = grid[i, j], cex = 1.4, font = 2)
      }
    }
  }

  # segments joueur
  for (e in edges) {
    segments(
      e$x1,
      n - e$y1 + 2,
      e$x2,
      n - e$y2 + 2,
      col = "#2C7BE5",
      lwd = 4
    )
  }
}

#################################################
# VERIFICATION DES CONTRAINTES
#################################################

count_edges_around_cell <- function(edges, i, j) {
  c_edges <- cell_edges(i, j)
  count <- 0

  for (ce in c_edges) {
    for (e in edges) {
      if (same_edge(ce, e)) {
        count <- count + 1
      }
    }
  }

  count
}

check_rules <- function(grid, edges) {
  n <- nrow(grid)
  m <- ncol(grid)

  for (i in 1:n) {
    for (j in 1:m) {
      if (!is.na(grid[i, j])) {
        if (count_edges_around_cell(edges, i, j) != grid[i, j]) {
          return(FALSE)
        }
      }
    }
  }

  TRUE
}

#################################################
# VERIFICATION DE LA BOUCLE UNIQUE
#################################################

check_single_loop <- function(edges) {
  if (length(edges) < 4) return(FALSE)

  vertices <- list()

  add_degree <- function(key) {
    if (is.null(vertices[[key]])) vertices[[key]] <<- 0
    vertices[[key]] <<- vertices[[key]] + 1
  }

  for (e in edges) {
    v1 <- paste(e$x1, e$y1, sep = "_")
    v2 <- paste(e$x2, e$y2, sep = "_")
    add_degree(v1)
    add_degree(v2)
  }

  degs <- unlist(vertices)
  if (any(!(degs %in% c(0, 2)))) return(FALSE)
  if (any(degs == 1 | degs > 2)) return(FALSE)

  # graphe d'adjacence
  adj <- list()
  for (e in edges) {
    v1 <- paste(e$x1, e$y1, sep = "_")
    v2 <- paste(e$x2, e$y2, sep = "_")
    adj[[v1]] <- unique(c(adj[[v1]], v2))
    adj[[v2]] <- unique(c(adj[[v2]], v1))
  }

  # parcours pour vérifier qu'il n'y a qu'une seule composante
  start <- names(adj)[1]
  visited <- character(0)
  stack <- c(start)

  while (length(stack) > 0) {
    v <- stack[length(stack)]
    stack <- stack[-length(stack)]

    if (!(v %in% visited)) {
      visited <- c(visited, v)
      neigh <- adj[[v]]
      stack <- unique(c(stack, neigh))
    }
  }

  length(visited) == length(adj)
}

