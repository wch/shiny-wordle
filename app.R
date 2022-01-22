library(shiny)
library(htmltools)

source("wordlist.R")

ui <- fluidPage(
  title = "Shiny wordle",
  tags$style(HTML("
  .container-fluid {
      text-align: center;
  }
  #results {
      margin: 10px 2px;
  }
  #results > .word {
      margin: 5px;
  }
  #results > .word > .letter {
      display: inline-block;
      width: 50px;
      height: 50px;
      text-align: center;
      vertical-align: middle;
      border-radius: 3px;
      line-height: 50px;
      font-size: 32px;
      font-weight: bold;
      vertical-align: middle;
      user-select: none;
      color: white;
      font-family: 'Clear Sans', 'Helvetica Neue', Arial, sans-serif;
  }
  #results > .word > .correct {
      background-color: #6a5;
  }
  #results > .word > .in-word {
      background-color: #db5;
  }
  #results > .word > .not-in-word {
      background-color: #888;
  }
  .endgame-content {
      font-family: Helvetica, Arial, sans-serif;
      display: inline-block;
      line-height: 1.4;
      letter-spacing: .2em;
      margin: 20px 8px;
      width: fit-content;
      padding: 20px;
      border-radius: 5px;
      box-shadow: 4px 4px 19px rgb(0 0 0 / 17%);
  }")),
  h3("Shiny wordle"),
  uiOutput("results"),
  uiOutput("endgame"),
  uiOutput("input_group_ui"),
  uiOutput("new_game_ui"),
  verbatimTextOutput("keyboard", placeholder = TRUE),
  div(
    style="display: inline-block;",
    checkboxInput("hard", "Hard mode")
  )
)


server <- function(input, output) {
  target_word <- reactiveVal(sample(words_common, 1))
  all_guesses <- reactiveVal(list())
  finished <- reactiveVal(FALSE)

  reset_game <- function() {
    target_word(sample(words_common, 1))
    all_guesses(list())
    finished(FALSE)
  }

  observeEvent(input$go, {
    req(input$guess)
    guess <- tolower(input$guess)
    guess <- sub(" +$", "", guess) # Remove trailing spaces

    if (! guess %in% words_all)
      return()

    if (input$hard) {
      # # Letters in the target word that the player has previously
      # # guessed correctly.
      # matched_letters = used_letters().intersection(set(target_word()))
      # if not set(guess).issuperset(matched_letters):
      #     return
    }

    all_guesses_new <- all_guesses()

    check_result <- check_word(guess, target_word())
    all_guesses_new[[length(all_guesses_new) + 1]] <- check_result
    all_guesses(all_guesses_new)

    if (isTRUE(check_result$win)) {
        finished(TRUE)
    }

    updateTextInput(inputId = "guess", value = "")
  })

  output$results <- renderUI({
    res <- lapply(all_guesses(), function(guess) {
      letters <- strsplit(guess$word, "")[[1]]
      row <- mapply(
        letters,
        guess$matches,
        FUN = function(letter, match) {
          # This will have the value "correct", "in-word", or "not-in-word", and
          # those values are also used as CSS class names.
          match_type <- match
          div(toupper(letter), class = paste("letter", match_type))
        },
        SIMPLIFY = FALSE,
        USE.NAMES = FALSE
      )
      div(class = "word", row)
    })

    tagList(res)
  })

  output$input_group_ui <- renderUI({
    if (finished())
      return()

    input_guess <- textInput("guess", "", placeholder="Enter 5-letter word")
    input_guess <- htmltools::tagQuery(input_guess)
    input_guess$addAttrs(style="display: inline-block;")
    input_guess$children("input")$addAttrs(spellcheck="false", autocomplete="off")
    input_guess <- input_guess$allTags()

    keypress_js <- tags$script(HTML("
      document.getElementById('guess').focus();
      document.getElementById('guess')
          .addEventListener('keypress', function(e) {
              if (e.code === 'Enter') {
                  // Trigger a click on the action button
                  document.getElementById('go').click();
              }
          });
      document.getElementById('go')
          .addEventListener('click', function(e) {
              document.getElementById('guess').focus();
          });
    "))

    tagList(
      input_guess,
      actionButton("go", "Go"),
      keypress_js
    )
  })

  output$new_game_ui <- renderUI({
    if (!finished())
      return()

    actionButton("new_game", "New Game")
  })

  observeEvent(input$new_game, {
    reset_game()
  })

  used_letters <- reactive({
    all_guess_words <- lapply(all_guesses(), function(guess) {
      strsplit(guess$word, "")[[1]]
    })
    unique(unlist(all_guess_words))
  })



  output$keyboard <- renderText({
    keys <- paste(
      " q  w  e  r  t  y  u  i  o  p ",
      "  a  s  d  f  g  h  j  k  l ",
      "   z  x  c  v  b  n  m ",
      sep = "\n"
    )

    for (letter in used_letters()) {
      keys <- sub(letter, " ", keys)
    }

    keys
  })


  output$endgame <- renderUI({
    if (!finished())
      return()

    lines <- lapply(all_guesses(), function(guess) {
      line <- vapply(guess$matches, function(match) {
        switch(match,
          "correct" = "🟩",
          "in-word" = "🟨",
          "not-in-word" = "⬜"
        )
      }, character(1))

      div(paste(line, collapse = ""))
    })

    div(class = "endgame-content", lines)
  })

}

check_word <- function(guess_str, target_str) {
  guess <- strsplit(guess_str, "")[[1]]
  target <- strsplit(target_str, "")[[1]]
  remaining <- character(0)

  if (length(guess) != length(target)) {
    stop("Word lengths don't match.")
  }

  result <- rep("not-in-word", length(guess))

  # First pass: find matches in correct position. Letters in the target that do
  # not match the guess are added to the remaining list.
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

  list(
    word = guess_str,
    matches = result,
    win = all(result == "correct")
  )
}

shinyApp(ui, server)
