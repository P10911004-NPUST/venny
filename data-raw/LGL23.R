cmat <- read.csv("./data-raw/count_matrix.csv", row.names = 1)
sample_info <- read.csv("./data-raw/sample_info.csv", row.names = 1)

LGL23 <- list(count_matrix = cmat, sample_info = sample_info)

usethis::use_data(LGL23)
