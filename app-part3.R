library(shiny)

source("wordlist.R")

ui <- fluidPage(
  textInput("guess", ""),
  actionButton("go", "Go"),
  verbatimTextOutput("result", placeholder = TRUE),
  p("[x] means that the letter is correct."),
  p("(x) means that the letter is in the word, but in the wrong place."),
  verbatimTextOutput("keyboard", placeholder = TRUE)
)


# Set the random seed based on the date, so that the same word is used during
# each day.
set.seed(as.integer(Sys.Date()))
target <- sample(words_common, 1)

server <- function(input, output) {

  all_guesses <- reactiveVal(character())

  output$result <- renderText({
    if (! input$guess %in% words_all) {
      req(FALSE, cancelOutput = TRUE)
    }

    all_guesses_new <- c(all_guesses(), input$guess)
    all_guesses(all_guesses_new)

    out_str <- vapply(all_guesses(), function(guess) {
      result <- check_words(target, guess)
      format_result(result)
    }, character(1))

    paste(out_str, collapse = "\n")
  }) |>
    bindEvent(input$go)

  output$keyboard <- renderText({
    keys <- paste(
      " q  w  e  r  t  y  u  i  o  p ",
      "  a  s  d  f  g  h  j  k  l ",
      "   z  x  c  v  b  n  m ",
      sep = "\n"
    )

    used_letters <- paste(all_guesses(), collapse = "")
    used_letters <- strsplit(used_letters, "")[[1]]
    used_letters <- unique(used_letters)

    for (letter in used_letters) {
      keys <- sub(letter, " ", keys)
    }

    keys
  })


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
  remaining <- character(0)
  result <- rep("not-in-word", 5)

  for (i in seq_along(guess)) {
    if (guess[i] == target[i]) {
      result[i] <- "correct"
    } else {
      remaining <- c(remaining, target[i])
    }
  }

  for (i in seq_along(guess)) {
    if (guess[i] != target[i] && guess[i] %in% remaining) {
      result[i] <- "in-word"
      remaining <- remaining[-match(guess[i], remaining)]
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
