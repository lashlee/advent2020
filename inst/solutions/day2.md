day2
================
John
12/6/2020

``` r
library(advent2020)
```

Read data.

``` r
dat <- get_data(2)
dat <- read.csv(textConnection(dat), header = FALSE)[['V1']]
```

For each line x

  - parse x
  - check condition

Then add the number of true conditions

``` r
solution_d2 <- function(dat, acc = 0) {
  if (length(dat) == 0) return(acc)
  item <- dat[1]
  rest <- dat[-1]
  pats <- c(
    low = "(\\d+)-\\d+ [a-z]+: .+",
    hii = "\\d+-(\\d+) [a-z]+: .+",
    key = "\\d+-\\d+ ([a-z]+): .+",
    str = "\\d+-\\d+ [a-z]+: (.+)"
  )
  line_dat <- vapply(pats, function(s) sub(s, "\\1", item), NA_character_)
  hits <- nchar(line_dat[["str"]]) - 
    nchar(gsub(line_dat[["key"]], "", line_dat[["str"]]))
  is_valid <- hits >= as.integer(line_dat[["low"]]) & 
    hits <= as.integer(line_dat[["hii"]])
  solution_d2(rest, acc + is_valid)
}
```

Copy solution to clipboard for output.

``` r
answer_d2 <- write_to_clipboard(solution_d2(dat))
```

For part two you change the definition of validity. Now exactly one of
low and hii must be the key character.

``` r
solution_d2_p2 <- function(dat, acc = 0) {
  if (length(dat) == 0) return(acc)
  item <- dat[1]
  rest <- dat[-1]
  pats <- c(
    low = "(\\d+)-\\d+ [a-z]+: .+",
    hii = "\\d+-(\\d+) [a-z]+: .+",
    key = "\\d+-\\d+ ([a-z]+): .+",
    str = "\\d+-\\d+ [a-z]+: (.+)"
  )
  line_dat <- vapply(pats, function(s) sub(s, "\\1", item), NA_character_)
  is_valid <- 1 == sum(line_dat[["key"]] == c(
    substr(line_dat[["str"]], line_dat[["low"]], line_dat[["low"]]), 
    substr(line_dat[["str"]], line_dat[["hii"]], line_dat[["hii"]])
  ))
  solution_d2_p2(rest, acc + is_valid)
}
```

Copy solution to clipboard for output.

``` r
answer_d2_p2 <- write_to_clipboard(solution_d2_p2(dat))
```
