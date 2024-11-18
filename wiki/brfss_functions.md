# BRFSS Functions


# Introduction

The Behavioral Risk Factor Surveillance System (BRFSS) is a gold mine of
public health data - but like any mine, you need the right tools to
extract the value. Since BRFSS is a complex survey, analyses need to
account for the survey design and weights to get accurate results. When
we want to analyze multiple years together (which we often do to
increase precision), we need to adjust those survey weights to avoid
overestimating our population.

This vignette will show you how to easily work with King County BRFSS
data while properly handling all these survey design considerations.
Don’t worry - the functions do the heavy lifting for you! We’ll cover
everything from finding available variables to getting properly weighted
estimates.

Note that the [BRFSS ETL process has its own
repository](https://github.com/PHSKC-APDE/BRFSS) and questions regarding
the data should be directed to the data steward.

# Checking Variable Availability Across Years

One quirk of BRFSS data is that not all questions are asked every year.
Before diving into analysis, it’s helpful to check which variables are
available for your time period of interest. The `list_dataset_columns()`
function makes this easy.

For example, let’s see what variables are available for 2022:

``` r
# Check variables for a single year
vars_2022 <- list_dataset_columns("brfss", year = 2022)
head(vars_2022)
nrow(vars_2022)
```

| var.names | year(s) |
|:----------|:--------|
| addepev3  | 2022    |
| age       | 2022    |
| age5_v1   | 2022    |
| age5_v2   | 2022    |
| agestd    | 2022    |
| agestd2   | 2022    |

    [1] 158

If you’re planning to analyze multiple years together, you can check
variable availability across years:

``` r
# Check variables across multiple years
vars_2019_2022 <- list_dataset_columns("brfss", year = 2019:2022)
head(vars_2019_2022)
nrow(vars_2019_2022)
```

| var.names | year(s)   |
|:----------|:----------|
| aceindx1  | 2019-2022 |
| aceindx2  | 2019-2022 |
| acescor1  | 2019-2022 |
| acescor2  | 2019-2022 |
| addepev3  | 2019-2022 |
| age       | 2019-2022 |

    [1] 260

A key thing to note: when checking multiple years,
`list_dataset_columns()` shows variables that appear in *any* of those
years. This means a variable might not be available for every year in
your range. If you need a variable that’s consistently available across
all your years of interest, you may want to check year by year or
consult the BRFSS questionnaires.

# Getting BRFSS Data

There are two equivalent ways to get BRFSS data: using
`get_data('brfss')` or `get_data_brfss()`. Both functions will:

1.  Load the data you request
2.  Automatically adjust weights if you’re analyzing multiple years
3.  Survey-set the data so it’s ready for analysis
4.  Return a [dtsurvey](https://github.com/PHSKC-APDE/dtsurvey) object,
    which is a data.table friendly survey object

Let’s see both methods in action:

## Method 1: Using `get_data()`

``` r
brfss_data1 <- get_data(
  dataset = "brfss",
  cols = c("chi_year", "age", "race4", "chi_sex", "obese"),
  year = 2019:2022
)
```

    Your data was survey set with the following parameters:
     - valid years = 2019-2022
     - adjusted survey weight = `default_wt` 
     - strata = `x_ststr`

## Method 2: Using `get_data_brfss()` directly

``` r
brfss_data2 <- get_data_brfss(
  cols = c("chi_year", "age", "race4", "chi_sex", "obese"),
  year = 2019:2022
)
```

    Your data was survey set with the following parameters:
     - valid years = 2019-2022
     - adjusted survey weight = `default_wt` 
     - strata = `x_ststr`

## Confirm that the two methods are identical

``` r
if(identical(brfss_data1, brfss_data2)){
  cat('\U0001f642 The two methods provide identical BRFSS data')
}
```

    🙂 The two methods provide identical BRFSS data

Both methods return an identical
[`dtsurvey`](https://github.com/PHSKC-APDE/dtsurvey) object that’s ready
for analysis with
[`calc()`](https://github.com/PHSKC-APDE/rads/wiki/calc).

Notice that the functions provide an informative message regarding the
survey object parameters. These will be hidden in the examples below,
but are always produced when getting or survey setting BRFSS data.

Since BRFSS survey weights are designed to represent the population, we
can verify that our multi-year weight adjustments are working properly
by comparing population sizes. We expect the adjusted weights to
represent an “average” population that falls between the earliest and
latest years’ populations since King County’s population has been
growing. Let’s verify our weight adjustments are working as expected:

## Calculate the survey population at the beginning and end of the period

``` r
pop_2019 <- sum(brfss_data1[chi_year == 2019]$finalwt1)
pop_2022 <- sum(brfss_data1[chi_year == 2022]$finalwt1)
```

## Calculate the adjusted population for the combined period

``` r
pop_adjusted <- sum(brfss_data1$default_wt)
```

## Is the value of the adjusted population between that for 2019 and 2022?

``` r
if(pop_2022 > pop_adjusted & pop_adjusted > pop_2019){
  cat('\U0001f642 pop_adjusted is between pop_2019 and pop_2022')
}
```

    🙂 pop_adjusted is between pop_2019 and pop_2022

# Working with HRAs and Regions

BRFSS data presents a unique challenge when analyzing Health Reporting
Areas (HRAs) because it comes with ZIP codes rather than HRA
assignments. Since ZIP codes don’t perfectly align with HRA boundaries,
we need to account for this uncertainty in our analyses.

To handle this, we use a statistical technique called multiple
imputation. When you request HRA-related columns (`hra20_id`,
`hra20_name`, or `chi_geo_region`), the data is returned as an
[`imputationList`](https://cran.r-project.org/web/packages/mitools/mitools.pdf)
object containing 10 different versions of the data. Each version
represents a different possible way that ZIP codes could be assigned to
HRAs based on their overlap. This approach allows us to capture the
uncertainty in our geographic assignments and incorporate it into our
statistical estimates.

``` r
# Get data including HRA information
brfss_hra <- get_data_brfss(
  cols = c("chi_year", "age", "race4", "chi_sex", "obese", "hra20_name"),
  year = 2019:2022
)

# Confirm we generated an imputationList of 10 dtsurvey objects
if(inherits(brfss_hra, "imputationList") & 
   length(brfss_hra$imputations) == 10 & 
   inherits(brfss_hra$imputations[[1]], "dtsurvey")){
  cat('\U0001f642 brfss_hra is an imputationList of 10 dtsurvey objects')
}
```

    🙂 brfss_hra is an imputationList of 10 dtsurvey objects

Don’t worry if this seems complex - the
[`calc()`](https://github.com/PHSKC-APDE/rads/wiki/calc) function knows
how to handle these
[imputationList](https://cran.r-project.org/web/packages/mitools/mitools.pdf)
objects automatically!

# Modifying BRFSS Data

There are two scenarios where you might need to modify BRFSS data:

1.  Adding custom variables for your analysis

2.  Applying transformations to existing variables

Before making any modifications, first consider whether your changes
should be standardized. If you’re creating variables that will be used
across multiple projects (CHI, CHNA, Communities Count, etc.), contact
the [BRFSS ETL steward](https://github.com/PHSKC-APDE/BRFSS) about
adding them to the analytic ready dataset.

For truly custom analyses, your modification approach will depend on
whether you’re working with a simple
[dtsurvey](https://github.com/PHSKC-APDE/dtsurvey) object or an
[imputationList](https://cran.r-project.org/web/packages/mitools/mitools.pdf).
Let’s look at each case:

## Modifying a [dtsurvey](https://github.com/PHSKC-APDE/dtsurvey)

You can modify a [dtsurvey](https://github.com/PHSKC-APDE/dtsurvey)
object using data.table commands without disrupting its survey settings.
If you want to use dplyr commands, you may break the internals of the
[dtsurvey](https://github.com/PHSKC-APDE/dtsurvey) and would be wise to
survey set it again following the instruction in the “Survey Setting and
Creating Custom Weights” section below.

Regardless of whether you use data.table or dplyr commands, you are
strongly encouraged to create new variables as needed rather than
overwriting and deleting existing ones.

## Modifying an [ImputationList](https://cran.r-project.org/web/packages/mitools/mitools.pdf)

When working with HRA or region data, modifications become more complex
since we need to maintain consistency across all 10 imputed datasets.
Here’s the step-by-step example that you can follow to help you in this
process:

### 1. Get your BRFSS data and confirm it is an [imputationList](https://cran.r-project.org/web/packages/mitools/mitools.pdf)

``` r
brfss <- get_data_brfss(
  cols = c("age", "hra20_id"),
  year = 2019:2023
)
inherits(brfss, "imputationList")
```

    [1] TRUE

### 2. Keep the first imputation and confirm it is a data.table/[dtsurvey](https://github.com/PHSKC-APDE/dtsurvey) object

``` r
brfss <- brfss$imputations[[1]]
class(brfss)
```

    [1] "dtsurvey"   "data.table" "data.frame"

### 3. Create or modify a variable

In this step, the same guidelines apply that were mentioned in the
‘Modifying a [dtsurvey](https://github.com/PHSKC-APDE/dtsurvey)’ section
above.

``` r
brfss[, age_category := fcase(age %in% 18:66, "working age",
                              age >= 67, "retirement age",
                              default = NA_character_
)]
```

### 4. Drop existing HRA or region variables (because they will be replaced)

``` r
brfss[, intersect(c('hra20_id', 'hra20_name', 'chi_geo_region'), names(brfss)) := NULL]
```

### 5. Convert your [dtsurvey](https://github.com/PHSKC-APDE/dtsurvey) object to a list of 10 dtsurvey objects

This step transforms your single
[dtsurvey](https://github.com/PHSKC-APDE/dtsurvey) object into a list of
10 [dtsurvey](https://github.com/PHSKC-APDE/dtsurvey) objects, each
representing a different potential assignment of ZIP codes to HRAs. The
[dtsurvey](https://github.com/PHSKC-APDE/dtsurvey) object contains 10
columns of hra20_ids, and we create a separate table for each one. We
merge on `hra20_name` and `chi_geo_region` to maintain geographic
consistency between HRAs and regions.

``` r
brfss <- lapply(1:10, function(i) {
  temp_dt <- copy(brfss)
  temp_dt[, hra20_id := get(paste0("hra20_id_", i))]
  temp_dt <- merge(
    temp_dt,
    rads.data::spatial_hra20_to_region20[, c("hra20_id", "hra20_name", "region_name")],
    by = "hra20_id",
    all.x = TRUE,
    all.y = FALSE
  )
  setnames(temp_dt, "region_name", "chi_geo_region")
  return(temp_dt)
})
```

### 6. Convert list to an [imputationList](https://cran.r-project.org/web/packages/mitools/mitools.pdf)

``` r
brfss <- mitools::imputationList(brfss)
```

# Survey Setting and Creating Custom Weights

You might need to use `pool_brfss_weights()` in two scenarios:

1.  When analyzing specific years where certain questions were asked
2.  When you need to restore proper survey settings after using
    non-data.table commands for data manipulation

While `get_data_brfss()` automatically creates weights and survey sets
imported data, you can create new weights and re-survey set the data
using `pool_brfss_weights()`. Here are argument descriptions:

- `ph.data`: Your BRFSS dataset (can be a data.frame, data.table,
  [dtsurvey](https://github.com/PHSKC-APDE/dtsurvey), or
  [imputationList](https://cran.r-project.org/web/packages/mitools/mitools.pdf))
- `years`: Vector of years you want to analyze together
- `year_var`: Name of the year column (defaults to ‘chi_year’)
- `old_wt_var`: Name of the original weight variable (defaults to
  ‘finalwt1’)
- `new_wt_var`: Name for your new weight variable
- `strata`: Name of the strata variable (defaults to ‘x_ststr’)

Let’s see it in action:

``` r
# Create weights for odd years only
brfss_odd_years <- pool_brfss_weights(
  ph.data = brfss_data1,
  years = c(2019, 2021),
  new_wt_var = "odd_year_wt"  # Name for the new weight variable
)

# Verify the sum of adjusted weights appear to be a weighted average of individual years
pop_2019 <- sum(brfss_odd_years[chi_year == 2019]$finalwt1)
pop_2021 <- sum(brfss_odd_years[chi_year == 2021]$finalwt1)
pop_2019_2021 <- sum(brfss_odd_years$odd_year_wt)
comparisons <- data.table(Description = c('2019', '2021', '2019, 2021'), 
                          `Pop 18-100` = c(pop_2019, pop_2021, pop_2019_2021))
comparisons[, `Pop 18-100` := format(`Pop 18-100`, big.mark = ',')]
head(comparisons)
```

| Description | Pop 18-100 |
|:------------|:-----------|
| 2019        | 1,367,097  |
| 2021        | 1,860,435  |
| 2019, 2021  | 1,651,470  |

# Analyzing BRFSS Data with [`calc()`](https://github.com/PHSKC-APDE/rads/wiki/calc)

Now for the fun part - analyzing our data! The
[`calc()`](https://github.com/PHSKC-APDE/rads/wiki/calc) function
handles all the survey design considerations for us. Let’s look at some
examples:

## Calculate obesity prevalence by sex and race (using a [dtsurvey](https://github.com/PHSKC-APDE/dtsurvey) object)

``` r
obesity_by_group <- calc(
  ph.data = brfss_data1,
  what = "obese",
  by = c("chi_sex", "race4"),
  metrics = c("mean", "rse"),
  proportion = TRUE  # Since obesity is binary
)
head(obesity_by_group)
```

| chi_sex | race4 | variable | mean | level | mean_se | mean_lower | mean_upper | rse |
|:---|:---|:---|---:|:---|---:|---:|---:|---:|
| Male | AIAN | obese | 0.3145740 | NA | 0.0956222 | 0.1539557 | 0.5365011 | 30.397357 |
| Male | Black | obese | 0.2540244 | NA | 0.0302701 | 0.1991500 | 0.3180148 | 11.916204 |
| Male | Asian | obese | 0.0969335 | NA | 0.0126692 | 0.0747563 | 0.1248025 | 13.069995 |
| Male | NHPI | obese | 0.2459367 | NA | 0.0816872 | 0.1168424 | 0.4456836 | 33.214724 |
| Male | Hispanic | obese | 0.3098841 | NA | 0.0264844 | 0.2603779 | 0.3641684 | 8.546553 |
| Male | White | obese | 0.2445188 | NA | 0.0084376 | 0.2283578 | 0.2614361 | 3.450709 |

## Calculate obesity prevalence by HRA20 (using an [imputationList](https://cran.r-project.org/web/packages/mitools/mitools.pdf))

``` r
obesity_by_hra20 <- calc(
  ph.data = brfss_hra,
  what = "obese",
  by = c("hra20_name"),
  metrics = c("mean", "rse"),
  proportion = TRUE  
)
head(obesity_by_hra20)
```

| hra20_name | level | variable | rse | mean | mean_se | mean_lower | mean_upper |
|:---|:---|:---|---:|---:|---:|---:|---:|
| Auburn - North | NA | obese | 12.33667 | 0.3269882 | 0.0440281 | 0.2399374 | 0.4140390 |
| Auburn - South | NA | obese | 14.14332 | 0.3561001 | 0.0580984 | 0.2413867 | 0.4708136 |
| Bear Creek and Greater Sammamish | NA | obese | 15.45107 | 0.2358673 | 0.0385144 | 0.1600071 | 0.3117275 |
| Bellevue - Central | NA | obese | 20.00561 | 0.1469236 | 0.0388625 | 0.0682875 | 0.2255596 |
| Bellevue - Northeast | NA | obese | 15.28179 | 0.1996892 | 0.0358325 | 0.1288697 | 0.2705086 |
| Bellevue - South | NA | obese | 20.51088 | 0.1562690 | 0.0347160 | 0.0881736 | 0.2243643 |

As noted in the [calc()
wiki](https://github.com/PHSKC-APDE/rads/wiki/calc#example-analyses-with-resampled-datamultiple-imputation),
when working with an
[imputationList](https://cran.r-project.org/web/packages/mitools/mitools.pdf),
the proportion argument is ignored. However, we include it here to
maintain consistent
[`calc()`](https://github.com/PHSKC-APDE/rads/wiki/calc) usage
regardless of whether you’re working with a
[dtsurvey](https://github.com/PHSKC-APDE/dtsurvey) object or an
[imputationList](https://cran.r-project.org/web/packages/mitools/mitools.pdf).

# Common Gotchas

1.  **Variable Availability**: Always check if your variables of
    interest are available in all years you want to analyze. Some BRFSS
    questions are only asked in certain years.

2.  **Survey Weights**: Make sure you’re using the appropriate weights
    for your analysis period. If you subset your data to specific years
    after loading it, you will need to recalculate weights using
    `pool_brfss_weights()`.

3.  **Survey Design**: Always use
    [`calc()`](https://github.com/PHSKC-APDE/rads/wiki/calc) for
    analysis to ensure proper handling of the survey design. Don’t
    calculate means or totals directly from the data.

4.  **HRA / Region Analyses**: When working with HRAs or regions, you’ll
    get an
    [`imputationList`](https://cran.r-project.org/web/packages/mitools/mitools.pdf).
    Don’t worry -
    [`calc()`](https://github.com/PHSKC-APDE/rads/wiki/calc) knows how
    to handle this! Just analyze it like you would any other BRFSS data.

# Conclusion

Working with BRFSS data requires careful attention to survey weights and
design, but the functions we’ve covered make this process
straightforward. Remember:

- Check variable availability with `list_dataset_columns()`
- Get data with `get_data_brfss()` or `get_data()`
- Modify [dtsurvey](https://github.com/PHSKC-APDE/dtsurvey) or
  [imputationList](https://cran.r-project.org/web/packages/mitools/mitools.pdf)
  objects using data.table syntax
- Create custom weights if needed with `pool_brfss_weights()`
- Analyze using `calc()`

Happy analyzing!

– *Updated by dcolombara, 2024-11-18*
