#' Create a Nonogram Puzzle
#'
#' This function generates a Nonogram puzzle with specified dimensions and difficulty level.
#'
#' @param n The size of the Nonogram puzzle grid (number of rows/columns).
#' @param diff The difficulty level of the puzzle. Options are "Easy" and "Hard".
#' 
#' @return A list containing the following components:
#'   - \code{matindichori} A matrix representing horizontal indications for each row.
#'   - \code{matindicvert} A matrix representing vertical indications for each column.
#'   - \code{X} The generated Nonogram grid.
#'   - \code{compteurH} A vector containing counts of filled cells for each row.
#'   - \code{compteurV} A vector containing counts of filled cells for each column.
#' 
#' @details The Nonogram puzzle is generated based on a random matrix of 0s and 1s, where
#'   0 represents an empty cell and 1 represents a filled cell. The function also generates
#'   horizontal and vertical indications based on the filled cells. The difficulty level 
#'   determines whether additional constraints are applied to generate the puzzle.
#' 
#' @examples
#' # Generate an easy Nonogram puzzle with a 5x5 grid
#' nonogramecreate(5, "Easy")
#' 
#' # Generate a hard Nonogram puzzle with a 10x10 grid
#' nonogramecreate(10, "Hard")
#' 
#' @export
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

#' Draw a Grid with State and Indications
#'
#' This function draws a grid with specified grid state and indications.
#'
#' @param grid_state A matrix representing the state of the grid. Each element 
#' represents a cell in the grid, with values:
#'   - 0: Empty cell
#'   - 1: Filled cell
#'   - 2: Cell with diagonal lines
#' @param gridSize The size of the grid (number of rows/columns).
#' @param indications A list containing horizontal and vertical indications for 
#' the grid. The list should have two elements:
#'   - horizontal: A character vector containing indications for each row.
#'   - vertical: A matrix containing indications for each column.
#' 
#' @details The grid is drawn using base R plotting functions. Empty cells are 
#' represented by white rectangles, filled cells by black rectangles, and cells 
#' with diagonal lines by red diagonal lines. The horizontal and vertical 
#' indications are placed at the margins of the grid.
#' 
#' @examples
#' # Define grid state and indications
#' grid_state <- matrix(c(0, 1, 2, 1, 0, 1, 2, 2, 0), nrow = 3)
#' indications <- list(horizontal = c("A", "B", "C"), 
#'                     vertical = matrix(c("1", "2", "3", "4", "5", "6"), nrow = 3))
#' # Draw the grid
#' draw_grid(grid_state, 3, indications)
#' 
#' @export
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
#' Rotate a Matrix by 90 Degrees
#'
#' This function rotates a given matrix by 90 degrees clockwise.
#'
#' @param mat A matrix to be rotated.
#' 
#' @return The input matrix rotated by 90 degrees.
#' 
#' @examples
#' mat <- matrix(1:4, nrow = 2)
#' rotate90(mat)
#' # Output:
#' #      [,1] [,2]
#' # [1,]    3    1
#' # [2,]    4    2
#' 
#' @export
rotate90 <- function(mat) {
  return(t(mat[, ncol(mat):1]))
}

rotate90 <- function(mat) {
  return(t(mat[, ncol(mat):1]))
}
