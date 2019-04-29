Playing with Triangles
================
Jess Robinson
April 29, 2019

``` r
library(tidyverse)
```

    ## ── Attaching packages ────────────────────────────────────────────────────── tidyverse 1.2.1 ──

    ## ✔ ggplot2 3.0.0     ✔ purrr   0.2.5
    ## ✔ tibble  1.4.2     ✔ dplyr   0.7.6
    ## ✔ tidyr   0.8.1     ✔ stringr 1.3.1
    ## ✔ readr   1.1.1     ✔ forcats 0.3.0

    ## Warning: package 'dplyr' was built under R version 3.5.1

    ## ── Conflicts ───────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
#given the hypotenuse, this calculates the length of both of other sides of an isosceles right-triangle
pythagorean1 <- function(c){
  sides <- sqrt(c/2)
  return(sides)
}

#given any two sides, this calculates the length of the other side of an isosceles right-triangle
pythagorean2 <- function(a, b){
    if (a == b) {
    return (sqrt(2*(a^2)))
  } else if((a > b) && ((a^2 - 2*(b^2)) == 0)) {
    return(b)    #where a is the hypotenuse
  } else if((b > a) && ((b^2 - 2*(a^2)) == 0)) {
    return(a)    #where b is the hypothenuse
  } else{
   return("This is not an isoceles right triangle.")
  }
}

#given the hypotenuse and a side, this calculates the length of the other side
pythagorean3 <- function(a, b){
  if(a > b) {
  return (sqrt(a^2 - 2*(b^2)))
  } else if(b > a) {
  return (sqrt(b^2 - 2*(a^2)))
  } else{
  return ("This is not a hypotenuse-side pairing.")
  }
}

#given the two smaller sides, this calculates the length of the hypotenuse
pythagorean4 <- function(a, b){
  hypotenuse <- sqrt(a^2 + b^2)
  return(hypotenuse)
}

#given two identified sides, this calculates the length of the other side 
pythagorean5 <- function(a = NULL , b = NULL, c = NULL) {
  sides <- c(a,b,c)
  if (length(sides) <= 1 | length(sides) >= 3) {
  stop ("Please enter two sides.")
  } else if (!is.numeric(sides)) {
  stop ("Please enter numeric values.")
  } else if (is.null(c)) {
  return (sqrt(a^2 + b^2)) 
  } else if (is.null(b)) {
  return (sqrt(c^2 - a^2)) 
  } else if (is.null(a)){
  return (sqrt(c^2 - b^2))    
  } else {
  stop ("Please try again.")
  }
}
```

if ((length(sides) &lt;= 1) | (length(sides &gt;= 3))) { return ("Please enter two sides.") pythagorean(a) sides &lt;- sort(c(a, b, c))

if ((is.null(a) && is.null(b)) | (is.null(b) && is.null(c))|(is.null(a) && is.null(c))){
