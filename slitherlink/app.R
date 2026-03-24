library(shiny)
library(bslib)

source("../R/codeR.R")

ui <- page_sidebar(
  title = "Slitherlink",
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#2C7BE5"
  ),

  sidebar = sidebar(
    h4("Paramètres"),
    numericInput("n", "Nombre de lignes", value = 5, min = 3, max = 10),
    numericInput("m", "Nombre de colonnes", value = 5, min = 3, max = 10),
    selectInput(
      "difficulty",
      "Difficulté",
      choices = c("Facile" = "easy", "Moyen" = "medium", "Difficile" = "hard"),
      selected = "medium"
    ),
    actionButton("new", "Nouvelle partie", class = "btn-primary"),
    actionButton("reset", "Effacer la boucle"),
    actionButton("check", "Vérifier"),
    hr(),
    h5("Aide"),
    p("Clique près d'une arête pour la tracer ou l'effacer."),
    p("Le but est de former une seule boucle fermée respectant les indices.")
  ),

  card(
    full_screen = TRUE,
    card_header("Grille"),
    card_body(
      div(
        style = "margin-bottom: 10px;",
        strong(textOutput("status", inline = TRUE))
      ),
      plotOutput("game", click = "plot_click", height = "700px")
    )
  )
)

server <- function(input, output, session) {

  current_game <- reactiveVal(NULL)
  player_edges <- reactiveVal(list())
  status_text <- reactiveVal("Partie en cours")

  new_game <- function() {
    p_hide <- switch(
      input$difficulty,
      easy = 0.25,
      medium = 0.45,
      hard = 0.65
    )

    game <- generate_slitherlink(
      n = input$n,
      m = input$m,
      p_hide = p_hide
    )

    current_game(game)
    player_edges(list())
    status_text("Partie en cours")
  }

  observeEvent(TRUE, {
    new_game()
  }, once = TRUE)

  observeEvent(input$new, {
    new_game()
  })

  observeEvent(input$reset, {
    player_edges(list())
    status_text("Boucle effacée")
  })

  observeEvent(input$plot_click, {
    req(current_game())
    edges_new <- add_edge_from_click(
      edges = player_edges(),
      click = input$plot_click,
      n = nrow(current_game()$grid),
      m = ncol(current_game()$grid)
    )
    player_edges(edges_new)
    status_text("Partie en cours")
  })

  output$game <- renderPlot({
    req(current_game())
    draw_game(
      grid = current_game()$grid,
      edges = player_edges()
    )
  })

  observeEvent(input$check, {
    req(current_game())

    rules_ok <- check_rules(current_game()$grid, player_edges())
    loop_ok  <- check_single_loop(player_edges())

    if (!rules_ok) {
      status_text("Certaines contraintes des cases ne sont pas respectées.")
      showModal(modalDialog(
        title = "Vérification",
        "Certaines contraintes des cases ne sont pas respectées.",
        easyClose = TRUE
      ))
    } else if (!loop_ok) {
      status_text("Les segments ne forment pas une boucle unique valide.")
      showModal(modalDialog(
        title = "Vérification",
        "Les segments ne forment pas une boucle unique valide.",
        easyClose = TRUE
      ))
    } else {
      status_text("Bravo ! La solution est correcte.")
      showModal(modalDialog(
        title = "Vérification",
        "Bravo ! La solution est correcte.",
        easyClose = TRUE
      ))
    }
  })

  output$status <- renderText({
    status_text()
  })
}

shinyApp(ui, server)
