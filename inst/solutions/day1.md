day1
================
John
12/6/2020

``` r
library(advent2020)
```

Read data.

``` r
dat <- get_data(1)
dat <- read.csv(textConnection(dat), header = FALSE)[['V1']]
```

For each expense x

  - Check if 2020 - x is in the list
  - If so return x \* (2020 - x)
  - Stop iterating when you find a hit

<!-- end list -->

``` r
solution_d1 <- function(dat) {
  if (length(dat) == 0) stop('No solution')
  item <- dat[1]
  rest <- dat[-1]
  if (is.element(2020 - item, rest)) return(item * (2020 - item))
  solution_d1(rest)
}
```

Copy solution to clipboard for output.

``` r
answer_d1 <- write_to_clipboard(solution_d1(dat))
```
