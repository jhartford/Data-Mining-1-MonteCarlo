Homework 1
========================================================

## Part 1




Generate an n by m matrix of random normal and random uniform variables.


```r
n = 100
m = 100

matnorm <- c()
matunif <- c()

for (i in c(1:m)) {
    matnorm <- cbind(matnorm, rnorm(n, 0, 1))
    matunif <- cbind(matunif, runif(n, 0, 1))
}
```


We can confirm their distribution by plotting the generated variables:


```r
p1 <- qplot(as.vector(matunif), binwidth = 1/30, xlab = "Random Uniform")
p2 <- qplot(as.vector(matnorm), binwidth = 5/30, xlab = "Random Normal")
grid.arrange(p1, p2, ncol = 2)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 



Using the `cor` function we can get the correlations between the generated variables as follows:



```r
corrnorm <- cor(matnorm)
corrunif <- cor(matunif)
correlations <- data.frame(corr = c(corrnorm[lower.tri(corrnorm)], corrunif[lower.tri(corrunif)]), 
    dist = c(rep("Normal", times = length(corrnorm[lower.tri(corrnorm)])), rep("Uniform", 
        times = length(corrunif[lower.tri(corrunif)]))))
```



Interestingly, plotting the correlation coefficients between the respective variables shows no systematic difference between the normal and uniform variables.



```r
p1 <- qplot(x = corr, data = correlations, fill = dist, geom = "density", alpha = I(0.5))
p2 <- ggplot(correlations, aes(x = corr, fill = dist)) + geom_histogram(data = correlations, 
    alpha = 0.5, position = "identity", binwidth = 1/60)
grid.arrange(p1, p2, ncol = 2)
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 


---

## Part 2

The function below repeatedly adds random columns, $x_1...x_k \sim \mathcal{N}(0,1)$ and fits a regression on $y \sim \mathcal{N}(0,1)$ until $r^2 > 0.95$. The breakout parameter gives the value at which the loop breaks if $r^2 < 0.95$ in order to prevent an infinite loop.



```r
testr2 <- function(threshold = 0.95, breakout = 200) {
    nvars <- 0
    data <- data.frame(y = rnorm(100, 0, 1))
    for (i in c(1:breakout)) {
        data <- cbind(data, rnorm(100, 0, 1))
        names(data) <- c("y", paste("x", c(1:i), sep = ""))
        r2 <- summary(lm(y ~ ., data))$r.squared
        if (r2 > threshold) {
            nvars <- i
            break
        }
    }
    return(i)
}
```


In order to get an indication of the distribution of the $k$, the number of $x_i$ variables required for $r^2>0.95$
, we run the ```testr2``` function 30 times and summarise the results.

```r
k <- sapply(rep(0.95, 200), testr2, breakout = 200)
```



```r
qplot(k, binwidth = 0.5)
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 

