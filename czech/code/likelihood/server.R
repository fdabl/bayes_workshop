library('shiny')

shinyServer(function(input, output) {
      plot.LR <- function(k, n, p1, p2, L1, L2) {
        # inspired / taken from @AlexEtz last blogpost
        curve((dbinom(k, n, x) / max(dbinom(k, n, x))), xlim = c(0,1), ylab = "Likelihood",
              xlab = "Probability of correct answer", las=1, main = "Likelihood function for binomials", lwd = 3,
              cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5)
        points(p1, L1, cex = 2, pch = 21, bg = "cyan")
        points(p2, L2, cex = 2, pch = 21, bg = "cyan")
        lines(c(p1, p2), c(L1, L1), lwd = 3, lty = 2, col = "cyan")
        lines(c(p2, p2), c(L1, L2), lwd = 3, lty = 2, col = "cyan")
        abline(v = k/n, lty = 5, lwd = 1, col = "grey73")
      }
      
      output$LRplot <- renderPlot({
        k <- input$k
        N <- input$N
        p1 <- input$p1
        p2 <- input$p2
        
        MLE <- dbinom(k, N, k/N)
        L1 <- dbinom(k, N, prob = p1) / MLE
        L2 <- dbinom(k, N, prob = p2) / MLE
        
        if (k <= N) plot.LR(k, N, p1, p2, L1, L2)
      })
      
      output$LRatio1 <- renderText({
        k <- input$k
        N <- input$N
        p1 <- input$p1
        p2 <- input$p2
        
        MLE <- dbinom(k, N, k/N)
        L1 <- dbinom(k, N, prob = p1) / MLE
        L2 <- dbinom(k, N, prob = p2) / MLE
        
        paste("L1 / L2: ", round(L1 / L2, 3))
      })
      
      output$LRatio2 <- renderText({
        k <- input$k
        N <- input$N
        p1 <- input$p1
        p2 <- input$p2
        
        MLE <- dbinom(k, N, k/N)
        L1 <- dbinom(k, N, prob = p1) / MLE
        L2 <- dbinom(k, N, prob = p2) / MLE
        
        paste("L2 / L1: ", round(L2 / L1, 3))
      })
    }
)