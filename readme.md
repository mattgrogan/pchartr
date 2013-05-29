pchartr
=======

pchartr is a collection of tools for analyzing control charts, specifically p-charts.

Usage Scenarios
===============

Ultimately this package will serve two purposes:  

1. Provide support for programatically detecting out-of-control points in a control chart.  
2. Provide functionality for determining the minimum sample size to detect an out-of-control situation with a specified degree of confidence.  

Functions
=========

There are functions for checking control charts for violations of the eight [Nelson rules](http://en.wikipedia.org/wiki/Nelson_rules).

```
nelson.rule1() # Checks for any points outside the upper and lower control limits.
nelson.rule2() # Checks for nine or more points in a row on the same side of the mean.
nelson.rule3() # Checks for six or more consecutive increasing or decreasing points.
nelson.rule4() # Checks for fourteen or more points alternating in direction.
nelson.rule5() # Checks for two out of three points that are more than two standard deviations from the mean.
nelson.rule6() # Checks for four out of five points that are more than one standard deviation from the mean
nelson.rule7() # Checks for fifteen consecutive points within one standard deviation of the mean.
nelson.rule8() # Checks for eight consecutive points with none within one standard deviation of the mean.
```

And a function to check all eight rules:

```
check_nelson_rules() # Checks all eight rules.
```

Example
=======


```r
library(pchartr)

# Set up data
x <- c(0.0912, 0.0942, 0.1032, 0.1043, 0.1068, 0.1068, 0.0876, 0.1108, 0.105, 
    0.0887, 0.0881, 0.099, 0.1184, 0.1502, 0.0986, 0.0966, 0.0876, 0.0933, 0.0954, 
    0.1155)

# Caclulate the mean value
mean <- rep(mean(x), length(x))

# Calculate upper and lower control limits
n <- 500
sd <- sqrt(mean(x) * (1 - mean(x))/n)
ucl <- mean + 3 * sd
lcl <- mean - 3 * sd

# Nelson's first rule checks for any points outside the upper and lower
# control limits
nelson.rule <- nelson.rule1(x, mean, ucl, lcl)

# Plot the results
plot(nelson.rule, ylim = c(0, 0.2), xlab = "Time", ylab = "p", main = "Nelson Rule One")
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1.png) 

```
## NULL
```

