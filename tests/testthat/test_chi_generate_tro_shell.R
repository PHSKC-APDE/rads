test_that("chi_generate_template injest template format") {
  set <- c(rep(1,7),rep(2,4))
  cat1 <- c("King County", "Cities/neighborhoods", "Poverty", "Race", "Race/ethnicity","Regions", "Big cities",
            "King County", "Poverty", "Race", "Regions")
  cat1_varname <- c("chi_geo_kc", "hra20_name", "pov200grp", "race3", "race4","chi_geo_region", "bigcities",
                    "chi_geo_kc", "pov200grp", "race3","chi_geo_region")
  kingCounty <- c("x", rep(NA, 6), "x", rep(NA, 3))
  wastate <- rep(NA, 11)
  demgroup <- rep("x", 11)
  crosstabs <- rep("x", 11)
  trends <- c("x",NA,NA, "x","x", "x", "x",
              "x",NA,"x", "x")
  set_indicator_keys <- c(rep("key1, key2, key3",7),rep("key4, key5",4))
  template <- data.table(set,
                         cat1,
                         cat1_varname,
                         kingCounty,
                         wastate,
                         demgroup,
                         crosstabs,
                         trends,
                         set_indicator_keys)
  DT <- chi_generate_tro_shell(ph.analysis_set, 2021, 2022, 3, 5)
  expect_equal(nrow(DT), 264)
  expect_equal(length(unique(DT$indicator_key)),5)
  expect_equal(DT[tab == "trends",][1]$end - DT[tab == "trends",][1]$start,2)
}
