pchartr
=======

pchartr is a collection of tools for analyzing control charts, specifically p-charts.

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
