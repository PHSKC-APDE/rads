test_that("chi_generate_template injest template format") {
  set <- c(rep(1,5),rep(2,3))
  cat1 <- c("King County", "Cities/neighborhoods", "Poverty", "Regions","Big cities",
            "King County", "Poverty", "Regions")
  cat1_varname <- c("chi_geo_kc", "hra20_name", "pov200grp", "chi_geo_region", "bigcities",
                    "chi_geo_kc", "pov200grp", "chi_geo_region")
  kingCounty <- c("x", rep(NA, 4), "x", rep(NA, 2))
  wastate <- rep(NA, 8)
  demgroup <- rep("x", 8)
  crosstabs <- rep("x", 8)
  trends <- c("x",NA,NA, "x","x",
              "x",NA,"x")
  set_indicator_keys <- c(rep("key1, key2, key3",5),rep("key4, key5",3))
  template <- data.frame(set,
                         cat1,
                         cat1_varname,
                         kingCounty,
                         wastate,
                         demgroup,
                         crosstabs,
                         trends,
                         set_indicator_keys)
  chi_generate_instructions(ph.analysis_set = template,)
}
