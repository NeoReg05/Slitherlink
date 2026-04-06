library(shiny)
library(bslib)

# Charger le code des fonctions (Vérifie bien le chemin du fichier)
source("codeR.R")

ui <- page_sidebar(
  title = "Slitherlink R",
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#2C7BE5"
  ),
  sidebar = sidebar(
    h4("Paramètres"),
    selectInput("grid_size", "Taille de la grille",
                choices = c("5 x 5" = 5, "7 x 7" = 7, "10 x 10" = 10),
                selected = 5),
    selectInput("difficulty", "Difficulté",
                choices = c("Facile" = "easy", "Moyen" = "medium", "Difficile" = "hard"),
                selected = "medium"),
    actionButton("new", "Nouvelle partie", class = "btn-primary w-100"),
    hr(),
    actionButton("check", "Vérifier la solution", class = "btn-success w-100"),
    actionButton("reset", "Effacer tout", class = "btn-outline-danger w-100 mt-2"),
    hr(),
    helpText("Cliquez sur les points pour relier les arêtes. Formez une boucle unique.")
  ),

  card(
    full_screen = TRUE,
    card_header(textOutput("status")),
    card_body(
      plotOutput("game", click = "plot_click", height = "600px")
    )
  )
)

server <- function(input, output, session) {

  current_game <- reactiveVal(NULL)
  player_edges <- reactiveVal(list())
  msg_status   <- reactiveVal("Bonne chance !")

  # Fonction de génération propre
  new_game <- function() {
    # On s'assure que l'input est bien là et on le convertit en numérique
    req(input$grid_size)
    size <- as.numeric(input$grid_size)

    p_hide <- switch(input$difficulty,
                     easy = 0.2,
                     medium = 0.4,
                     hard = 0.6)

    # ATTENTION : Correction ici (on utilise size au lieu de n et m)
    game <- generate_slitherlink(n = size, m = size, p_hide = p_hide)

    current_game(game)
    player_edges(list())
    msg_status("Nouvelle partie commencée")
  }

  # Initialisation et bouton New
  # On utilise bindEvent pour éviter que ça tourne en boucle
  observeEvent(input$new, {
    new_game()
  }, ignoreInit = FALSE)

  # Gestion des clics
  observeEvent(input$plot_click, {
    req(current_game())
    grid <- current_game()$grid

    new_list <- add_edge_from_click(
      edges = player_edges(),
      click = input$plot_click,
      n = nrow(grid),
      m = ncol(grid)
    )
    player_edges(new_list)
  })

  # Reset
  observeEvent(input$reset, {
    player_edges(list())
    msg_status("Boucle réinitialisée")
  })

  # Rendu graphique
  output$game <- renderPlot({
    req(current_game())
    draw_game(current_game()$grid, player_edges())
  })

  # Status Text
  output$status <- renderText({ msg_status() })

  # Vérification finale
  observeEvent(input$check, {
    req(current_game())
    grid <- current_game()$grid
    edges <- player_edges()

    res_rules <- check_rules(grid, edges)
    res_loop  <- check_single_loop(edges)

    if (res_rules && res_loop) {
      msg_status("FÉLICITATIONS ! Vous avez réussi.")
      showModal(modalDialog(title = "Victoire !", "Vous avez résolu le puzzle !", footer = modalButton("Bravo")))
    } else {
      err_msg <- if(!res_rules) "Les indices ne sont pas respectés." else "La boucle est incomplète ou multiple."
      msg_status(paste("Essaye encore :", err_msg))
      showModal(modalDialog(title = "Pas encore...", err_msg, easyClose = TRUE))
    }
  })
}

shinyApp(ui, server)

