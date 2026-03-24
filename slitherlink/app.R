library(shiny)

source("../R/codeR.R")

ui <- fluidPage(

  titlePanel("Slitherlink"),

  sidebarLayout(

    sidebarPanel(

      actionButton("new","Nouvelle grille"),
      actionButton("reset","Reset"),
      actionButton("check","Confirmer")

    ),

    mainPanel(

      plotOutput(
        "game",
        click="plot_click",
        height="600px"
      )

    )

  )

)

server <- function(input, output, session){

  game <- generate_slitherlink(5,5)

  grid <- reactiveVal(game$grid)
  edges <- reactiveVal(list())

  output$game <- renderPlot({

    draw_game(grid(),edges())

  })

  observeEvent(input$new,{

    game <- generate_slitherlink(5,5)

    grid(game$grid)
    edges(list())

  })

  observeEvent(input$reset,{

    edges(list())

  })

  observeEvent(input$plot_click,{

    edges(
      add_edge_from_click(
        edges(),
        input$plot_click
      )
    )

  })

  observeEvent(input$check,{

    rules_ok <- check_rules(grid(),edges())
    loop_ok <- check_single_loop(edges())

    msg <- ""

    if(!rules_ok){
      msg <- "Contraintes non respectées"
    }else if(!loop_ok){
      msg <- "La boucle n'est pas valide"
    }else{
      msg <- "Bravo ! Solution correcte"
    }

    showModal(
      modalDialog(msg)
    )

  })

}

shinyApp(ui,server)
