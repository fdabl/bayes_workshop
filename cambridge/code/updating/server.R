shinyServer(function(input, output) {
      update_plot <- function(a = 1, b = 1, k = 0, N = 0, null = NULL, CI = NULL, ymax = 'auto') {
        x <- seq(.001, .999, .001) ## set up for creating the distributions
        y1 <- dbeta(x, a, b) # data for prior curve
        y3 <- dbeta(x, a + k, b + N - k) # data for posterior curve
        y2 <- dbeta(x, 1 + k, 1 + N - k) # data for likelihood curve, plotted as the posterior from a beta(1,1)
        y.max <- ifelse(is.numeric(ymax), ymax, 1.25 * max(y1, y2, y3, 1.6))
        title <- paste0('Beta(', a, ', ', b, ')', ' to Beta(', a + k, ', ', b + N - k, ')')
        
        plot(x, y1, xlim = c(0, 1), ylim = c(0, y.max), type = 'l', ylab = 'Density', lty = 2,
             xlab = 'Probability of success', las = 1, main = title, lwd=3,
             cex.lab = 1.5, cex.main = 1.5, col = 'skyblue', axes = FALSE)
        
        axis(1, at = seq(0, 1, .2)) #adds custom x axis
        axis(2, las = 1) # custom y axis
        
        if (N != 0) {
            # if there is new data, plot likelihood and posterior
            lines(x, y2, type = 'l', col = 'darkorange', lwd = 2, lty = 3)
            lines(x, y3, type = 'l', col = 'darkorchid1', lwd = 5)
            legend('topleft', c('Prior', 'Posterior', 'Likelihood'), col = c('skyblue', 'darkorchid1', 'darkorange'), 
                   lty = c(2, 1, 3), lwd = c(3, 5, 2), bty = 'n', y.intersp = 1, x.intersp = .4, seg.len =.7)
                
            ## adds null points on prior and posterior curve if null is specified and there is new data
            if (is.numeric(null)) {
                    ## Adds points on the distributions at the null value if there is one and if there is new data
                    points(null, dbeta(null, a, b), pch = 21, bg = 'blue', cex = 1.5)
                    points(null, dbeta(null, a + k, b + N - k), pch = 21, bg = 'darkorchid', cex = 1.5)
                    abline(v=null, lty = 5, lwd = 1, col = 'grey73')
                    ##lines(c(null,null),c(0,1.11*max(y1,y3,1.6))) other option for null line
              }
        }
        
        ## Specified CI% but no null? Calc and report only CI
        if(is.numeric(CI) && !is.numeric(null)) {
              CI.low <- qbeta((1 - CI)/2, a + k, b + N - k)
              CI.high <- qbeta(1 - (1 - CI)/2, a + k, b + N - k)
              
              SEQlow <- seq(0, CI.low, .001)
              SEQhigh <- seq(CI.high, 1, .001)
              
              ## Adds shaded area for x% Posterior CIs
              cord.x <- c(0, SEQlow, CI.low) ## set up for shading
              cord.y <- c(0, dbeta(SEQlow, a + k, b + N - k), 0) ## set up for shading
              polygon(cord.x, cord.y, col='orchid', lty= 3) ## shade left tail
              cord.xx <- c(CI.high, SEQhigh, 1) 
              cord.yy <- c(0, dbeta(SEQhigh, a + k, b + N - k), 0)
              polygon(cord.xx, cord.yy, col='orchid', lty=3) ## shade right tail
              return(list( 'Posterior CI lower' = round(CI.low, 3), 'Posterior CI upper' = round(CI.high, 3)))
        }
        
        ## Specified null but not CI%? Calculate and report BF only 
        if(is.numeric(null) && !is.numeric(CI)){
            null.H0 <- dbeta(null, a, b)
            null.H1 <- dbeta(null, a + k, b + N - k)
            CI.low <- qbeta((1 - CI)/2, a + k, b + N - k)
            CI.high <- qbeta(1 - (1 - CI)/2, a + k, b + N - k)
            return(list('BF01 (in favor of H0)' = round(null.H1/null.H0, 3), 'BF10 (in favor of H1)' = round(null.H0/null.H1, 3)))
        }
        
        ## Specified both null and CI%? Calculate and report both
        if(is.numeric(null) && is.numeric(CI)){
                null.H0 <- dbeta(null, a, b)
                null.H1 <- dbeta(null, a + k, b + N - k)
                CI.low <- qbeta((1 - CI)/2, a + k, b + N - k)
                CI.high <- qbeta(1 - (1 - CI)/2, a + k, b + N - k)
                
                SEQlow <- seq(0, CI.low, .001)
                SEQhigh <- seq(CI.high, 1, .001)
                
                ## Adds shaded area for x% Posterior CIs
                cord.x <- c(0, SEQlow, CI.low) ## set up for shading
                cord.y <- c(0, dbeta(SEQlow, a + k, b + N - k), 0) ## set up for shading
                polygon(cord.x, cord.y, col = 'orchid', lty = 3) ## shade left tail
                cord.xx <- c(CI.high, SEQhigh, 1) 
                cord.yy <- c(0, dbeta(SEQhigh, a + k, b + N - k), 0)
                polygon(cord.xx, cord.yy, col = 'orchid', lty = 3) ## shade right tail
                return(list('BF01 (in favor of H0)' = round(null.H1/null.H0, 3), 'BF10 (in favor of H1)' = round(null.H0/null.H1, 3),
                            'Posterior CI lower' = round(CI.low, 3), 'Posterior CI upper' = round(CI.high, 3)))
        }
      }
      output$update_plot <- renderPlot({
        update_plot(input$a, input$b, input$k, input$N)
      })
      
      output$BF_01 <- renderText({
        a <- input$a
        b <- input$b
        k <- input$k
        N <- input$N
        paste('BF_01', round(dbeta(.5, a, b) / dbeta(.5, a + k, b + N - k), 3))
      })
      
      output$BF_10 <- renderText({
        a <- input$a
        b <- input$b
        k <- input$k
        N <- input$N
        paste('BF_10', round(1 / (dbeta(.5, a, b) / dbeta(.5, a + k, b + N - k)), 3))
      })
})

