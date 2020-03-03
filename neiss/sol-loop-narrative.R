library(tidyverse)
library(vroom)
library(shiny)

if (!exists("injuries")) {
  injuries <- vroom::vroom("injuries.tsv.gz")
  products <- vroom::vroom("products.tsv")
  population <- vroom::vroom("population.tsv")
}

ui <- fluidPage(
  #<< first-row
  fluidRow(
    column(8,
      selectInput("code", "Product",
        choices = setNames(products$prod_code, products$title),
        width = "100%"
      )
    ),
    column(2, selectInput("y", "Y axis", c("rate", "count"))),
    column(2, numericInput("nr", "Rows", value = 4, min = 2, max = 10))
  ),
  #>>
  fluidRow(
    column(4, tableOutput("diag")),
    column(4, tableOutput("body_part")),
    column(4, tableOutput("location"))
  ),
  fluidRow(
    column(12, plotOutput("age_sex"))
  ),
  #<< narrative-ui
  fluidRow(
    column(2, actionButton("previous_story", "Previous story")),
    column(8, textOutput("narrative")),
    column(2, actionButton("next_story", "Next story"))
  )
  #>>
)

count_top <- function(df, var, n = 5) {
  df %>%
    mutate({{ var }} := fct_lump(fct_infreq({{ var }}), n = n)) %>%
    group_by({{ var }}) %>%
    summarise(n = as.integer(sum(weight)))
}

server <- function(input, output, session) {
  selected <- reactive(injuries %>% filter(prod_code == input$code))
  #<< n-factors
  nf <- reactive(input$nr - 1)
  #>>

  #<< tables
  output$diag <- renderTable(count_top(selected(), diag, nf()), width = "100%")
  output$body_part <- renderTable(count_top(selected(), body_part, nf()), width = "100%")
  output$location <- renderTable(count_top(selected(), location, nf()), width = "100%")
  #>>

  summary <- reactive({
    selected() %>%
      count(age, sex, wt = weight) %>%
      left_join(population, by = c("age", "sex")) %>%
      mutate(rate = n / population * 1e4)
  })

  #<< plot
  output$age_sex <- renderPlot({
    if (input$y == "count") {
      summary() %>%
        ggplot(aes(age, n, colour = sex)) +
        geom_line() +
        labs(y = "Estimated number of injuries") +
        theme_grey(15)
    } else {
      summary() %>%
        ggplot(aes(age, rate, colour = sex)) +
        geom_line(na.rm = TRUE) +
        labs(y = "Injuries per 10,000 people") +
        theme_grey(15)
    }
  })
  #>>

  #<< narrative-server
  idx <- reactive({
    idx  <- input$next_story - input$previous_story
    nnar <- nrow(selected())
    if (idx < 0) {idx <- nnar + idx}
    if (idx > nnar) {idx <- 1}
    idx
  })

  output$narrative <- renderText({
    selected() %>% slice(idx()) %>% pull(narrative)
  })
  #>>
}

shinyApp(ui, server)
