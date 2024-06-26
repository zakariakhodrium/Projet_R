library(shiny)
rotate90 <- function(mat) {
  return(t(mat[, ncol(mat):1]))
}

nonogramecreate <- function(n, diff) {
  p <- 0.7  # Define p here
  y <- 0
  indicationh <- c()
  indicationv <- c()
  compteurH <- rep(0, n)
  compteurV <- rep(0, n)
  matindichori <- matrix(data = 0, nrow = n, ncol = floor(n/2) + 1)
  matindicvert <- matrix(data = 0, nrow = floor(n/2) + 1, ncol = n)
  matrice <- rbinom(n * n, 1, p)
  X <- matrix(matrice, n, byrow = TRUE)
  
  if (diff == "Easy") {
    random_integer1 <- sample(1:n, 1)
    random_integer2 <- sample(1:n, 1)
    
    X[random_integer1, ] <- 1
    X[, random_integer2] <- 1
    
  }
  
  if (diff == "Hard") {
    a <- n + 1
    while(a == n + 1) {
      a <- 0
      matrice <- rbinom(n * n, 1, p)
      X <- matrix(matrice, n, byrow = TRUE)
      
      # Pour les lignes horizontales
      for (i in 1:n) {
        for(j in 1:n) {
          if (X[i, j] == 1) {
            y <- y + 1
          }
          if (X[i, j] == 0 & y != 0) {
            indicationh <- append(indicationh, y)
            y <- 0
            compteurH[i] <- compteurH[i] + 1
          }
        }
        if (y != 0) {
          indicationh <- append(indicationh, y)
          y <- 0
          compteurH[i] <- compteurH[i] + 1
        }
        while (length(indicationh) < floor(n/2) + 1) {
          indicationh <- append(indicationh, NA)
        }
        
        print(a)
        if (a < sum(indicationh, na.rm = TRUE) + compteurH[i]) {
          print("hello")
          print(compteurH)
          print(indicationh)
          a <- sum(indicationh, na.rm = TRUE) + compteurH[i]
        }
        
        matindichori[i, ] <- indicationh
        matindichori[is.na(matindichori)] <- ""
        indicationh <- c()
      }
      
      X_t = t(X) # Transposer la matrice
      
      for (i in 1:n) {
        for(j in 1:n)
        {
          if (X_t[i,j]==1)
          {
            y=y+1
          }
          if (X_t[i,j]==0 & y!=0)
          {
            indicationv=append(indicationv,y)
            y=0
            compteurV[i]=compteurV[i]+1
          }
        }
        if (y!=0)
        {
          indicationv=append(indicationv,y)
          y=0
          compteurV[i]=compteurV[i]+1
        }
        while (length(indicationv)<floor(n/2)+1) 
        {
          indicationv=append(indicationv,NA)
          
        }
        
        matindicvert[,i]=indicationv
        matindicvert[is.na(matindicvert)] <- ""
        indicationv=c()
        if (a < sum(indicationv, na.rm = TRUE) + compteurV[i]) {
          a <- sum(indicationv, na.rm = TRUE) + compteurV[i]
        }
      }
    }
  }
  
  else{       
    for (i in 1:n) {
      for(j in 1:n) {
        if (X[i, j] == 1) {
          y <- y + 1
        }
        if (X[i, j] == 0 & y != 0) {
          indicationh <- append(indicationh, y)
          y <- 0
          compteurH[i] <- compteurH[i] + 1
        }
      }
      if (y != 0) {
        indicationh <- append(indicationh, y)
        y <- 0
        compteurH[i] <- compteurH[i] + 1
      }
      while (length(indicationh) < floor(n/2) + 1) {
        indicationh <- append(indicationh, NA)
      }
      
      matindichori[i, ] <- indicationh
      matindichori[is.na(matindichori)] <- ""
      indicationh <- c()
    }   
    # Pour les colonnes verticales
    X_t = t(X) # Transposer la matrice
    for (i in 1:n) {
      for(j in 1:n)
      {
        if (X_t[i,j]==1)
        {
          y=y+1
        }
        if (X_t[i,j]==0 & y!=0)
        {
          indicationv=append(indicationv,y)
          y=0
          compteurV[i]=compteurV[i]+1
        }
      }
      if (y!=0)
      {
        indicationv=append(indicationv,y)
        y=0
        compteurV[i]=compteurV[i]+1
      }
      while (length(indicationv)<floor(n/2)+1) 
      {
        indicationv=append(indicationv,NA)
        
      }
      
      matindicvert[,i]=indicationv
      matindicvert[is.na(matindicvert)] <- ""
      indicationv=c()
      
    }
  }
  
  
  return(list(matindichori = matindichori, matindicvert = matindicvert, X = X, compteurH = compteurH, compteurV = compteurV))
}

# Function to draw the grid
draw_grid <- function(grid_state, gridSize, indications) {
  
  plot(1:gridSize, 1:gridSize, type = "n", xlab = "", ylab = "", xaxt = 'n', yaxt = 'n', xlim = c(-gridSize*0.10, gridSize+1), ylim = c(1, gridSize*1.5))
  for(x in 1:gridSize) {
    for(y in 1:gridSize) {
      # Draw the rectangle with a border first
      rect(x, y, x + 1, y + 1, col = ifelse(grid_state[x, y] == 1, "black", "white"), border = "black", lwd = 1)
      
      # Then draw the lines if needed
      if(grid_state[x, y] == 2) {
        offset = 0.1  # Define an offset
        lines(c(x + offset, x + 1 - offset), c(y + offset, y + 1 - offset), col = "red", lwd = 2)
        lines(c(x + 1 - offset, x + offset), c(y + offset, y + 1 - offset), col = "red", lwd = 2)
      }
    }
  }
  # Define margins to accommodate the indications
  par(mar = c(0, gridSize, 0, 0))
  for (i in 1:gridSize) {
    for (j in floor(gridSize/2):1) {
      text(x = i+0.5, y = gridSize+1.2+j*(gridSize/28+0.1), paste(indications$vertical[(gridSize/2)-j+1, i], collapse = " "), adj = c(1, 1), font = 2, cex=1.7/log(gridSize))
      
    }
    text(x = 1, y = gridSize-i+1.7, paste(indications$horizontal[i,], collapse = " "), adj = c(1, 1), font = 2, cex=1.7/log(gridSize))
  }
}


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
shinyApp(ui = ui, server = server)