rm(list = ls(all = TRUE))
library(shiny)
library(shinythemes)
library(ggplot2)
ui <- shinyUI(fluidPage(
  theme = shinytheme("slate"), titlePanel("KS Test"),

  fluidRow(
    column(2, selectInput("dist",
      "Distribution 1",
      choices = c(
        "Normal" = "rnorm",
        "Binomial" = "rbinom",
        "Beta" = "rbeta",
        "gamma" = "rgamma",
        "Logistic" = "rlogis",
        "Uniform" = "runif"
      ),
      selected = "rnorm"
    )),
    column(2, selectInput("dist2",
      "Distribution 2",
      choices = c(
        "Normal" = "rnorm",
        "Binomial" = "rbinom",
        "Beta" = "rbeta",
        "gamma" = "rgamma",
        "Logistic" = "rlogis",
        "Uniform" = "runif"
      ),
      selected = "rnorm"
    )),
    column(
      3,
      tags$p("KS Test:"),
      textOutput("ks_test")
    ),
    column(2, wellPanel(
      uiOutput("ui_1a")
    ), wellPanel(
      uiOutput("ui_1b")
    ), wellPanel(
      uiOutput("ui_1c")
    )), column(2, wellPanel(
      uiOutput("ui_2a")
    ), wellPanel(
      uiOutput("ui_2b")
    ), wellPanel(
      uiOutput("ui_2c")
    ))
  ),
  fluidRow(
    column(
      4,
      plotOutput("plot1")
    ),
    column(
      4,
      plotOutput("plot2")
    ),
    column(
      4,
      plotOutput("plot3")
    )
  )
))



server <- function(input, output, session) {
  output$ui_1a <- renderUI({
    if (is.null(input$dist)) {
      return()
    }
    switch(input$dist,
      "rnorm" =
        sliderInput("a",
          "Mean Dist 1",
          value = 0,
          min = -50,
          max = 50
        ),
      "rbeta" = numericInput("a", "alpha_shape",
        value = .1
      ),
      "rgamma" =
        numericInput("a", "shape1",
          value = 12
        ),
      "rbinom" = numericInput("a", "Prob",
        value = .1
      ),
      "rlogis" = sliderInput("a",
        "Mean Dist Log.",
        value = 0,
        min = -50,
        max = 50
      ),
      "runif" = sliderInput("a",
        "Min.",
        value = 0,
        min = -50,
        max = 50
      )
    )
  })
  output$ui_1b <- renderUI({
    if (is.null(input$dist)) {
      return()
    }
    switch(input$dist,
      "rnorm" =
        numericInput("b", "SD1",
          value = 12
        ),
      "rbeta" =
        numericInput("b", "beta_shape",
          value = 12
        ),
      "rgamma" =
        numericInput("b", "shape2",
          value = 12
        ),
      "rbinom" = numericInput("b", "Number of Trials",
        value = 12
      ),
      "rlogis" = numericInput("b", "SD LOG",
        value = 12
      ),
      "runif" = sliderInput("b",
        "Max.",
        value = 0,
        min = -50,
        max = 50
      )
    )
  })
  output$ui_1c <- renderUI({
    if (is.null(input$dist)) {
      return()
    }
    switch(input$dist,
      "rnorm" =
        numericInput("c", "N",
          value = 12
        ),
      "rbeta" =
        numericInput("c", "N",
          value = 12
        ),
      "rgamma" =
        numericInput("c", "N",
          value = 12
        ),
      "rbinom" =
        numericInput("c", "N",
          value = 12
        ),
      "rlogis" =
        numericInput("c", "N",
          value = 12
        ),
      "runif" =
        numericInput("c", "N",
          value = 12
        )
    )
  })
  output$plot1 <- renderPlot({
    if (input$dist == "rnorm") {
      mean <- input$a
      std <- input$b
      no1 <- input$c
      x <- rnorm(no1, mean, std)
      ggplot(as.data.frame(x), aes(x = x)) + geom_histogram(aes(y = ..density..)) +
        geom_density(alpha = 0.25, fill = "dodgerblue1") + geom_vline(aes(xintercept = mean(x)), size = 2, linetype = "dashed")
    }
    else if (input$dist == "rbeta") {
      alpha <- input$a
      beta <- input$b
      no1 <- input$c
      x <- rbeta(no1, alpha, beta)
      ggplot(as.data.frame(x), aes(x = x)) + geom_histogram(aes(y = ..density..)) +
        geom_density(alpha = 0.25, fill = "dodgerblue1") + geom_vline(aes(xintercept = mean(x)), size = 2, linetype = "dashed")
    }
    else if (input$dist == "rgamma") {
      shape1 <- input$a
      shape2 <- input$b
      no1 <- input$c
      x <- rgamma(no1, shape1, shape2)
      ggplot(as.data.frame(x), aes(x = x)) + geom_histogram(aes(y = ..density..)) +
        geom_density(alpha = 0.25, fill = "dodgerblue1") + geom_vline(aes(xintercept = mean(x)), size = 2, linetype = "dashed")
    }
    else if (input$dist == "rbinom") {
      prob <- input$a
      trials <- input$b
      no1 <- input$c
      x <- rbinom(no1, trials, prob)
      ggplot(as.data.frame(x), aes(x = x)) + geom_histogram(aes(y = ..density..)) +
        geom_density(alpha = 0.25, fill = "dodgerblue1") + geom_vline(aes(xintercept = mean(x)), size = 2, linetype = "dashed")
    }
    else if (input$dist == "rlogis") {
      mean <- input$a
      sd <- input$b
      no1 <- input$c
      x <- rlogis(no1, mean, sd)
      ggplot(as.data.frame(x), aes(x = x)) + geom_histogram(aes(y = ..density..)) +
        geom_density(alpha = 0.25, fill = "dodgerblue1") + geom_vline(aes(xintercept = mean(x)), size = 2, linetype = "dashed")
    }
    else if (input$dist == "runif") {
      min <- input$a
      max <- input$b
      no1 <- input$c
      x <- runif(no1, min, max)
      ggplot(as.data.frame(x), aes(x = x)) + geom_histogram(aes(y = ..density..)) +
        geom_density(alpha = 0.25, fill = "dodgerblue1") + geom_vline(aes(xintercept = mean(x)), size = 2, linetype = "dashed")
    }
  })

  output$ui_2a <- renderUI({
    if (is.null(input$dist2)) {
      return()
    }
    switch(input$dist2,
      "rnorm" =
        sliderInput("a2",
          "Mean Dist 2",
          value = 0,
          min = -50,
          max = 50
        ),
      "rbeta" = numericInput("a2", "alpha_shape2",
        value = .1
      ),
      "rgamma" =
        numericInput("a2", "shape2",
          value = 12
        ),
      "rbinom" = numericInput("a2", "Prob2",
        value = .1
      ),
      "rlogis" = sliderInput("a2",
        "Mean Dist Log.2",
        value = 0,
        min = -50,
        max = 50
      ),
      "runif" = sliderInput("a2",
        "Min.2",
        value = -50,
        min = -50,
        max = 50
      )


      ##        "Logistic" = "rlogis",
      # "Uniform" = "runif"
    )
  })
  output$ui_2b <- renderUI({
    if (is.null(input$dist2)) {
      return()
    }
    switch(input$dist2,
      "rnorm" =
        numericInput("b2", "SD2",
          value = 12
        ),
      "rbeta" =
        numericInput("b2", "beta_shape2",
          value = 12
        ),
      "rgamma" =
        numericInput("b2", "shape2",
          value = 12
        ),
      "rbinom" = numericInput("b2", "Number of Trials",
        value = 12
      ),
      "rlogis" = numericInput("b2", "SD LOG",
        value = 12
      ),
      "runif" = sliderInput("b2",
        "Max.",
        value = 0,
        min = -50,
        max = 50
      )
    )
  })
  output$ui_2c <- renderUI({
    if (is.null(input$dist2)) {
      return()
    }
    switch(input$dist2,
      "rnorm" =
        numericInput("c2", "N",
          value = 12
        ),
      "rbeta" =
        numericInput("c2", "N",
          value = 12
        ),
      "rgamma" =
        numericInput("c2", "N",
          value = 12
        ),
      "rbinom" =
        numericInput("c2", "N",
          value = 12
        ),
      "rlogis" =
        numericInput("c2", "N",
          value = 12
        ),
      "runif" =
        numericInput("c2", "N",
          value = 12
        )
    )
  })
  output$plot2 <- renderPlot({
    if (input$dist2 == "rnorm") {
      mean2 <- input$a2
      std2 <- input$b2
      no2 <- input$c2
      y <- rnorm(no2, mean2, std2)
      ggplot(as.data.frame(y), aes(x = y)) + geom_histogram(aes(y = ..density..)) +
        geom_density(alpha = 0.25, fill = "darkorange") + geom_vline(aes(xintercept = mean(y)), size = 2, linetype = "dashed")
    }
    else if (input$dist2 == "rbeta") {
      alpha2 <- input$a2
      beta2 <- input$b2
      no2 <- input$c2
      y <- rbeta(no2, alpha2, beta2)
      ggplot(as.data.frame(y), aes(x = y)) + geom_histogram(aes(y = ..density..)) +
        geom_density(alpha = 0.25, fill = "darkorange") + geom_vline(aes(xintercept = mean(y)), size = 2, linetype = "dashed")
    }
    else if (input$dist2 == "rgamma") {
      shape22 <- input$a2
      shape22a <- input$b2
      no2 <- input$c2
      y <- rgamma(no2, shape22, shape22a)
      ggplot(as.data.frame(y), aes(x = y)) + geom_histogram(aes(y = ..density..)) +
        geom_density(alpha = 0.25, fill = "darkorange") + geom_vline(aes(xintercept = mean(y)), size = 2, linetype = "dashed")
    }
    else if (input$dist2 == "rbinom") {
      prob <- input$a2
      trials <- input$b2
      no2 <- input$c2
      y <- rbinom(no2, trials, prob)
      ggplot(as.data.frame(y), aes(x = y)) + geom_histogram(aes(y = ..density..)) +
        geom_density(alpha = 0.25, fill = "darkorange") + geom_vline(aes(xintercept = mean(y)), size = 2, linetype = "dashed")
    }
    else if (input$dist2 == "rlogis") {
      mean2 <- input$a2
      sd2 <- input$b2
      no2 <- input$c2
      y <- rlogis(no2, mean2, sd2)
      ggplot(as.data.frame(y), aes(x = y)) + geom_histogram(aes(y = ..density..)) +
        geom_density(alpha = 0.25, fill = "darkorange") + geom_vline(aes(xintercept = mean(y)), size = 2, linetype = "dashed")
    }
    else if (input$dist2 == "runif") {
      min2 <- input$a2
      max2 <- input$b2
      no2 <- input$c2
      y <- runif(no2, min2, max2)
      ggplot(as.data.frame(y), aes(x = y)) + geom_histogram(aes(y = ..density..)) +
        geom_density(alpha = 0.25, fill = "darkorange") + geom_vline(aes(xintercept = mean(y)), size = 2, linetype = "dashed")
    }
  })

  data <- reactive({
    z <- input$dist
    a <- input$a
    b <- input$b
    c <- input$c
    eq <- paste(z, "(", c, ",", a, ",", b, ")", sep = "")
    return(eq)
  })

  data2 <- reactive({
    z2 <- input$dist2
    a2 <- input$a2
    b2 <- input$b2
    c2 <- input$c2
    eq2 <- paste(z2, "(", c2, ",", a2, ",", b2, ")", sep = "")
    return(eq2)
  })
  output$plot3 <- renderPlot({
    x <- data()
    x <- eval(parse(text = x))

    y <- data2()
    y <- eval(parse(text = y))

    qqplot(x, y)
  })

  data3 <- reactive({
    z2 <- input$dist2
    a2 <- input$a2
    b2 <- input$b2
    c2 <- input$c2
    eq2 <- paste(z2, "(", c2, ",", a2, ",", b2, ")", sep = "")

    z <- input$dist
    a <- input$a
    b <- input$b
    c <- input$c
    eq <- paste(z, "(", c, ",", a, ",", b, ")", sep = "")
    x <- data()
    x <- eval(parse(text = x))

    y <- data2()
    y <- eval(parse(text = y))
    z <- ks.test(x, y)[[2]]
    return(z)
  })

  output$ks_test <- renderPrint({
    data3()
  })
}

shinyApp(ui = ui, server = server)
