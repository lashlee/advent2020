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
  if (length(dat) == 0) return(0)
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

Part 2 wants you to return the product of the three numbers that sum to
2020.

My approach is

  - Parametrize the desired sum from the first part
  - For each expense
      - Use that expense as the desired sum parameter
      - Rerun part 1 module to check if that expense has partners

<!-- end list -->

``` r
solution_d1_redux <- function(dat, sum) {
  if (length(dat) == 1) return(0)
  item <- dat[1]
  rest <- dat[-1]
  if (is.element(sum - item, rest)) return(item * (sum - item))
  solution_d1_redux(rest, sum)
}
solution_d1_p2 <- function(dat) {
  if (length(dat) == 1) stop('No solutions')
  item <- dat[1]
  rest <- dat[-1]
  pair_product <- solution_d1_redux(rest, 2020 - item)
  if (pair_product > 0) return(item * pair_product)
  solution_d1_p2(rest)
}
```

Copy solution to clipboard for output.

``` r
answer_d1_p2 <- write_to_clipboard(solution_d1_p2(dat))
```
