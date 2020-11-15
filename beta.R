#####   BETA  
require(dplyr)
Beta = function(a,b) {
  dens = function(x) {(1/beta(a,b))*x^(a-1)*(1-x)^(b-1)}
  par(mfrow=c(2,1))
  curve(dens, from=0, to=1)
  abline(v=a/(a+b), lty=2, col='red')
  curve(pbeta(x,a,b), from=0, to=1)
  media = a/(a+b)
  varianza = a*b/((a+b)^2*(a+b+1))
  mediana = (a-1/3)/(a+b-2/3)
  moda = ifelse(
    a>1 & b>1, (a-1)/(a+b-2), 
    ifelse(a>=1 & b<1,1,ifelse(a<1 & b>=1, 0, NA))
    )
  summary = data.frame(media,varianza,mediana,moda)
  return(summary)}


library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Beta Distribution"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("a","Parameter 1",2),
      numericInput("b","Parameter 2",3),
      tableOutput("table"),
      uiOutput("ex1"),
      uiOutput("ex2"),
      uiOutput("ex3"),
      uiOutput("ex4"),
      uiOutput("ex5"),
      uiOutput("ex6")
    ),
    mainPanel(
      plotOutput("beta", height="800px")
    )
  )
)


server <- function(input, output, session) {
  
  output$beta <- renderPlot({
    Beta(input$a,input$b)
    })
  
  output$table <- renderTable({
    Beta(input$a, input$b)
  })
  
  output$ex1 <- renderUI({
    invalidateLater(5000,session)
    x <- round(rbeta(1, input$a, input$b), 3)
    withMathJax(sprintf("If \\(X\\) is a Beta random variable, then
                        $$P(X \\leq %.03f ) = %.03f$$", x, pbeta(x, input$a, input$b)))
  })
  
  output$ex2 <- renderUI({  
    withMathJax(sprintf("if \\(X\\) is a \\(Beta(a,b)\\) with a>1 & b>1, then we have:
                        $$ \\Rightarrow E(X) = \\frac{a}{a+b}$$
                        $$ \\Rightarrow Me = \\frac{a - \\frac{1}{3}}{a + b - \\frac{2}{3}}$$
                        $$ \\Rightarrow Mo = \\frac{a - 1}{a + b - 2}$$
                        $$ \\Rightarrow V(X) = \\frac{ab}{(a+b)^{2}(a+b+1)} $$"))})
  
  output$ex3 <- renderUI({    
    withMathJax(sprintf("if \\(X\\) is a Beta(1,1) $$ \\Rightarrow X \\sim U(0,1)$$"))})
  
  output$ex4 <- renderUI({ 
    withMathJax(sprintf("if \\(X\\) is \\(Beta(a,b)\\) $$ \\Rightarrow 1 - X  \\sim Beta(b,a)$$"))})
  
  
  output$ex5 <- renderUI({ 
    withMathJax(sprintf("if \\(X\\) is \\(Beta(a,1)\\) $$ \\Rightarrow -ln(X) \\sim Exp(a)$$"))})
  
  output$ex5 <- renderUI({ 
    withMathJax(sprintf("if \\(X\\) is \\(U(0,1)\\) $$ \\Rightarrow X^{\\frac{1}{a}} \\sim Beta(a,1)$$"))})
  
  output$ex6 <- renderUI({ 
    withMathJax(sprintf("if \\(X\\) is \\(Gamma(a,c)\\) and \\(Y\\) is \\(Gamma(b,c)\\) $$ \\Rightarrow \\frac{X}{X+Y} \\sim Beta(a,b)$$"))})  
  
  
}

shinyApp(ui=ui, server=server)
