library(shiny)

source("wordlist.R")

ui <- fluidPage(
  textInput("guess", ""),
  actionButton("go", "Go"),
  verbatimTextOutput("result", placeholder = TRUE),
)


# Set the random seed based on the date, so that the same word is used during
# each day.
set.seed(as.integer(Sys.Date()))
target <- sample(words_common, 1)

server <- function(input, output) {

  output$result <- renderText({
    if (! input$guess %in% words_all) {
      req(FALSE, cancelOutput = TRUE)
    }

    result <- check_words(target, input$guess)
    format_result(result)
  }) |>
    bindEvent(input$go)
}


format_result <- function(r) {
  out_str <- ""
  for (i in seq_along(r$letters)) {
    if (r$result[i] == "correct") {
      out_str <- paste0(out_str, "[", r$letters[i], "]")
    } else if (r$result[i] == "in-word") {
      out_str <- paste0(out_str, "(", r$letters[i], ")")
    } else {
      out_str <- paste0(out_str, " ", r$letters[i], " ")
    }
  }
  out_str
}

# target: "gives"
# guess:  "aisle"
compare_words <- function(target_str, guess_str) {
  if (nchar(target_str) != nchar(guess_str)) {
    stop("target and guess string must be the same length.")
  }

  target <- strsplit(target_str, "")[[1]]
  guess <- strsplit(guess_str, "")[[1]]
  result <- character(nchar(guess_str))

  for (i in seq_along(target)) {
    if (guess[i] == target[i]) {
      result[i] <- "correct"
    } else if (guess[i] %in% target) {
      result[i] <- "in-word"
    } else {
      result[i] <- "not-in-word"
    }
  }

  result
}


check_words <- function(target_str, guess_str) {
  compare_result <- compare_words(target_str, guess_str)
  correct <- FALSE
  if (all(compare_result == "correct")) {
    correct <- TRUE
  }

  list(
    word = guess_str,
    letters = strsplit(guess_str, "")[[1]],
    result = compare_result,
    correct = correct
  )
}


shinyApp(ui, server)
