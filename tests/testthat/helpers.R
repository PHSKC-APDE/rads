library(data.table)

# Confirm that every combination of `group_cols` is present in `dt`, and that
# `count_col` is never NA (i.e., missing combinations were zero-filled rather
# than dropped). Returns the zero-filled data.table invisibly for further
# inspection by individual tests.
expect_complete_table <- function(dt, group_cols, count_col) {
  full_grid <- do.call(data.table::CJ, c(lapply(group_cols, function(col) unique(dt[[col]])),
                                         list(sorted = FALSE)))
  data.table::setnames(full_grid, group_cols)

  data.table::setkeyv(full_grid, group_cols)
  dt_key <- data.table::copy(dt)
  data.table::setkeyv(dt_key, group_cols)

  testthat::expect_equal(nrow(dt), nrow(full_grid))
  testthat::expect_equal(nrow(full_grid[!dt_key]), 0L)
  testthat::expect_false(anyNA(dt[[count_col]]))

  invisible(dt)
}
