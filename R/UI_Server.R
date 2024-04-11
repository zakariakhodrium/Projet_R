library(shiny)
ui <- fluidPage(
  titlePanel("Picross"),
  tags$style(type="text/css", "#controls { width: 60%; margin: 0 auto; }"),
  fluidRow(
    column(4,
           div(id = "controls",
               wellPanel(
                 sliderInput("gridSize", "Grid size", min = 5, max = 15, value = 5),
                 selectInput("diff", "Difficulty", choices = c("Easy", "Medium", "Hard"), selected = "Medium"),
                 actionButton("update", "Create game", style = "background-color: red; color: white;"),  # Add the update button with red background
                 actionButton("verify", "Verify", style = "background-color: green; color: white;")  # Add the solve button with green background
               )
           )
    ),
    column(8,
           wellPanel(
             conditionalPanel(
               condition = "output.gridExists",
               plotOutput("grid", click = "grid_click", height = "800px")  # Increase the height of the grid
             ),
             conditionalPanel(
               condition = "!output.gridExists",
               tags$div(
                 style = "text-align: center; padding: 50px;",
                 tags$h1("Welcome to Picross!", style = "color: #3399FF;"),
                 tags$p("Please select the parameters and create the game.", style = "font-size: 20px;")
               )
             )
           )
    )
  )
)



# Server
server <- function(input, output, session) {
  # Reactive values for the grid size, grid state, indications and counters
  grid_size <- reactiveVal()
  grid_state <- reactiveVal()
  indications <- reactiveVal()
  solution <- reactiveVal()

  observeEvent(input$update, {  # Listen for clicks on the update button
    grid_size(input$gridSize)  # Update grid size
    result <- nonogramecreate(grid_size(), input$diff)
    solution(result$X)  # Store the solution
    #print(solution)
    indications(list(horizontal = result$matindichori, vertical = result$matindicvert))  # Update indications
    grid_state(matrix(0, nrow = grid_size(), ncol = grid_size()))  # Initialize grid_state with a blank grid
  })

  observeEvent(input$verify, {  # Listen for clicks on the verify button
    print(rotate90(grid_state()))
    #print(solution()== rotate90(grid_state()))
    #transform all cells that are 2 to 0
    test<-rotate90(grid_state())
    test[test == 2] <- 0
    if(all(test==solution())) {
      showNotification("Congratulations! You have solved the game.", type = "message")
    } else {
      # If not all cells are filled, show a warning message
      showNotification("The game is not yet solved. Keep trying!", type = "warning")
    }
  })

  output$gridExists <- reactive({
    !is.null(grid_state()) && !is.null(indications())
  })

  outputOptions(output, "gridExists", suspendWhenHidden = FALSE)

  output$grid <- renderPlot({
    # Draw the grid based on the state of each cell
    if (!is.null(grid_state()) && !is.null(grid_size()) && !is.null(indications())) {
      draw_grid(grid_state(), grid_size(), indications())
    }
  }, res = 100)

  observeEvent(input$grid_click, {
    # Update the state of the clicked cell
    if (!is.null(grid_state()) && !is.null(grid_size())) {
      x <- floor(input$grid_click$x)
      y <- floor(input$grid_click$y)
      if(x < 1 || x > grid_size() || y < 1 || y > grid_size()) {
        return()
      }
      current_state <- grid_state()
      if(current_state[x,y]==2){
        current_state[x,y]=-1
      }
      current_state[x, y] <- current_state[x, y]+1
      grid_state(current_state)

      # Redraw the grid
      draw_grid(grid_state(), grid_size(), indications())
    }
  })
}

# Run the app
#' Run the Picross Shiny App
#'
#' This function launches the Picross Shiny App.
#' 
#' @details The Picross Shiny App allows users to play Picross puzzles interactively.
#' 
#' 
#' @export
Piccross_R <- function(){
  shinyApp(ui = ui, server = server)
}

  


