## code to prepare `DATASET` dataset goes here
library(dplyr)
library(lubridate)

toy_claims <- tibble(
  ID = c(rep("A", 8),
         rep("B", 8),
         rep("D", 6)),
  date = ymd("2020-01-01", "2020-01-31", "2020-03-01", "2020-05-30", "2020-06-29", "2020-07-29", "2020-08-28", "2020-09-27",
             "2020-01-01", "2020-01-31", "2020-03-01", "2020-05-30", "2020-06-29", "2020-07-29", "2020-08-28", "2020-09-27",
             "2020-01-01", "2020-01-31", "2020-03-01", "2020-05-30", "2020-08-28", "2020-09-27"),
  days_supply = c(30, 60, 60, rep(30, 5),
                  rep(30, 3), rep(60, 2), rep(30, 3), 
                  rep(60, 3), 30, 60, 30)
)
usethis::use_data(toy_claims, overwrite = TRUE, compress = "xz")




# Creating big_data toy set for performance
set.seed(8)
s.size = 100000
big_data_toy <- tibble(
  ID = sample(c(LETTERS[1:4]), s.size, replace = TRUE),
  date = sample(seq(ymd("2015-01-01"), ymd("2020-12-31"), by = "day"), s.size, replace = TRUE),
  days_supply = sample(c(30, 60, 90), s.size, replace = TRUE, prob = c(0.85, 0.10, 0.05)),
  drug_class = sample(c("ClassA", "ClassB"), s.size, replace = TRUE, prob = c(0.75, 0.25))
)
usethis::use_data(big_data_toy, overwrite = TRUE, compress = "xz")

