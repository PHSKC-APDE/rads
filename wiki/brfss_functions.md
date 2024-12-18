# BRFSS Functions


# Introduction

The Behavioral Risk Factor Surveillance System (BRFSS) is a gold mine of
public health data – but like any mine, you need the right tools to
extract the value. Since BRFSS is a complex survey, analyses need to
account for the survey design and weights to get accurate results. The
survey design includes stratification and weighting to ensure the sample
represents the full population, accounting for who was more or less
likely to be included in the survey. When we want to analyze multiple
years together (which we often do to increase precision), we need to
adjust those survey weights to avoid overestimating our population.

This vignette will show you how to easily work with King County BRFSS
data while properly handling all these survey design considerations.
Don’t worry - the functions do the heavy lifting for you! We’ll cover
everything from finding available variables to getting properly weighted
estimates.

Note that the [BRFSS ETL process has its own
repository](https://github.com/PHSKC-APDE/BRFSS) and questions regarding
the data should be directed to the data steward.

# Load essential packages

``` r
library(rads)
library(data.table)
```

# Checking Variable Availability Across Years

One quirk of BRFSS data is that not all questions are asked every year.
Before diving into analysis, it’s helpful to check which variables are
available for your time period of interest. The `list_dataset_columns()`
function makes this easy.

Since the Washington State and King County datasets are distinct, you
can specify which one you want with the `kingco` argument. When
`kingco = TRUE`, you will receive the list of columns in the King County
dataset. When `kingco = FALSE`, you will receive the list of columns in
the Washington State dataset. The default is `kingco = TRUE`.

For example, let’s see what variables are available for 2023 King County
data:

``` r
# Check variables for a single year
vars_2023 <- list_dataset_columns("brfss", year = 2023)
head(vars_2023)
nrow(vars_2023)
```

| var.names | year(s) |
|:----------|:--------|
| addepev3  | 2023    |
| age       | 2023    |
| age5_v1   | 2023    |
| age5_v2   | 2023    |
| agestd    | 2023    |
| agestd2   | 2023    |

    [1] 176

If you’re planning to analyze multiple years together, you can check
variable availability across years:

``` r
# Check variables across multiple years
vars_2019_2023 <- list_dataset_columns("brfss", year = 2019:2023)
head(vars_2019_2023)
nrow(vars_2019_2023)
```

| var.names | year(s)   |
|:----------|:----------|
| aceindx1  | 2019-2021 |
| aceindx2  | 2019-2021 |
| acescor1  | 2019-2020 |
| acescor2  | 2019-2020 |
| addepev3  | 2019-2023 |
| age       | 2019-2023 |

    [1] 279

Notice that the `year(s)` column is not constant because BRFSS does not
ask every question in every year.

# Getting BRFSS Data

There are two equivalent ways to get BRFSS data: using
`get_data('brfss')` or `get_data_brfss()`. Both functions will:

1.  Load the data you request into memory
2.  Automatically adjust weights if you’re analyzing multiple years
3.  Survey-set the data so it’s ready for analysis
4.  Return a [dtsurvey](https://github.com/PHSKC-APDE/dtsurvey) object,
    which is a data.table friendly survey object analyzable with
    `rads::calc`

As with `list_dataset_columns()`, by default you will receive King
County data but can specify Washington State data with the
`kingco = FALSE` argument.

Let’s see both methods in action:

## Method 1: Using `get_data()`

This is the general interface that you can use to access any of APDE’s
analytic ready data.

``` r
brfss_full <- get_data(
  dataset = "brfss",
  cols = c("chi_year", "age", "race4", "chi_sex", "prediab1"),
  year = 2019:2023
)
```

    Your data was survey set with the following parameters is ready for rads::calc():
     - valid years = 2019-2023
     - adjusted survey weight = `default_wt` 
     - strata = `x_ststr`

## Method 2: Using `get_data_brfss()`

``` r
brfss_full_alt <- get_data_brfss(
  cols = c("chi_year", "age", "race4", "chi_sex", "prediab1"),
  year = 2019:2023
)
```

    Your data was survey set with the following parameters is ready for rads::calc():
     - valid years = 2019-2023
     - adjusted survey weight = `default_wt` 
     - strata = `x_ststr`

These methods return an identical
[`dtsurvey`](https://github.com/PHSKC-APDE/dtsurvey) object that’s ready
for analysis with
[`calc()`](https://github.com/PHSKC-APDE/rads/wiki/calc).

Notice that the functions provide an informative message regarding the
survey object parameters. These will be hidden in the examples below,
but are always produced when getting or survey setting BRFSS data.

Since BRFSS weights are designed to represent the population, we can
verify that our multi-year weight adjustments are working properly by
comparing population sizes. We expect the adjusted weights to represent
an “average” population that falls between the earliest and latest
years’ populations since King County’s population has been growing.
Let’s verify our weight adjustments are working as expected:

### Calculate the survey population at the beginning and end of the period

``` r
pop_2019 <- sum(brfss_full[chi_year == 2019]$finalwt1)
pop_2023 <- sum(brfss_full[chi_year == 2023]$finalwt1)
```

### Calculate the adjusted population for the combined period

``` r
pop_adjusted <- sum(brfss_full$default_wt)
```

### Is the value of the adjusted population between that for 2019 and 2023?

``` r
if(pop_2023 > pop_adjusted & pop_adjusted > pop_2019){
  cat('\U0001f642 pop_adjusted is between pop_2019 and pop_2023')
}
```

    🙂 pop_adjusted is between pop_2019 and pop_2023

# Working with HRAs and Regions

BRFSS data presents a unique challenge when analyzing Health Reporting
Areas (HRAs) because it comes with ZIP codes rather than HRA
assignments. Since ZIP codes don’t perfectly align with HRA boundaries,
we need to account for this uncertainty in our analyses.

To handle this, we use a statistical technique called multiple
imputation. When you request HRA-related columns (`hra20_id`,
`hra20_name`, or `chi_geo_region`), the function returns an
[`imputationList`](https://cran.r-project.org/web/packages/mitools/mitools.pdf)
object containing 10 different versions of the data. Each version
represents a different possible way that ZIP codes could be assigned to
HRAs based on their overlap. This approach allows us to capture the
uncertainty in our geographic assignments and incorporate it into our
statistical estimates.

*Note:* APDE decided to use 10 imputations based on an extensive
empirical assessment by Daniel Casey. This is fixed in the ETL process
and is not configurable.

``` r
# Get data including HRA information
brfss_hra <- get_data_brfss(
  cols = c("chi_year", "age", "race4", "chi_sex", "prediab1", "obese", "hra20_name"),
  year = 2019:2023
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
[`calc()`](https://github.com/PHSKC-APDE/rads/wiki/calc) function
automatically handles these
[imputationList](https://cran.r-project.org/web/packages/mitools/mitools.pdf)
objects.

# Modifying BRFSS Data

There are times when you might need to modify BRFSS data. For example,
you might want to create a new variable. Before making any
modifications, first consider whether your changes should be
standardized. If you’re creating variables that will be used across
multiple projects (CHI, CHNA, Communities Count, etc.) or repeatedly
year after year, contact the [BRFSS ETL
steward](https://github.com/PHSKC-APDE/BRFSS) and politely request the
addition of these changes to the analytic ready dataset.

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

### 1. Get your BRFSS [ImputationList](https://cran.r-project.org/web/packages/mitools/mitools.pdf)

``` r
brfss <- get_data_brfss(
  cols = c("age", "hra20_id"),
  year = 2019:2023
)
inherits(brfss, "imputationList") # confirms it is an imputationList
```

    [1] TRUE

### 2. Keep the first item in the list (a [dtsurvey](https://github.com/PHSKC-APDE/dtsurvey)/data.table object)

``` r
brfss <- brfss$imputations[[1]]
inherits(brfss, 'dtsurvey')
```

    [1] TRUE

``` r
inherits(brfss, 'data.table')
```

    [1] TRUE

### 3. Create or modify a variable

In this step, the same guidelines apply that were mentioned in the
‘Modifying a [dtsurvey](https://github.com/PHSKC-APDE/dtsurvey)’ section
above.

``` r
# data.table::fcase is an implementation of SQL's CASE WHEN & comparable to dplyr::case_when
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
10 [dtsurvey](https://github.com/PHSKC-APDE/dtsurvey) objects. Each
object represents a different potential assignment of ZIP codes to HRAs.
The [dtsurvey](https://github.com/PHSKC-APDE/dtsurvey) object contains
10 columns of hra20_ids, and we create a separate table for each one. We
merge on `hra20_name` and `chi_geo_region` to maintain geographic
consistency between HRAs and regions.

``` r
xwalk_hra_region <- rads.data::spatial_hra20_to_region20[, c("hra20_id", "hra20_name", "region_name")]

brfss <- lapply(1:10, function(i) {
  temp_dt <- copy(brfss)
  temp_dt[, hra20_id := get(paste0("hra20_id_", i))]
  temp_dt <- merge(
    temp_dt,
    xwalk_hra_region,
    by = "hra20_id",
    all.x = TRUE,
    all.y = FALSE
  )
  setnames(temp_dt, "region_name", "chi_geo_region")
  return(temp_dt)
})
```

### 6. Convert a standard list to an [imputationList](https://cran.r-project.org/web/packages/mitools/mitools.pdf)

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
using `pool_brfss_weights()`. Here are brief argument descriptions, see
the `pool_brfss_weights()` help file for details:

- `ph.data`: Your BRFSS dataset (can be a data.frame, data.table,
  [dtsurvey](https://github.com/PHSKC-APDE/dtsurvey), or
  [imputationList](https://cran.r-project.org/web/packages/mitools/mitools.pdf))
- `years`: Vector of years you want to analyze together
- `year_var`: Name of the year column (defaults to ‘chi_year’)
- `old_wt_var`: Name of the original weight variable (defaults to
  ‘finalwt1’)
- `new_wt_var`: Name for your new weight variable
- `wt_method`: Name of the method used to rescale your weights. Options
  include ‘obs’, ‘pop’, and ‘simple’ (defaults to ‘obs’)
- `strata`: Name of the strata variable (defaults to ‘x_ststr’)

Let’s see it in action:

``` r
# Create weights for odd years only
brfss_odd_years <- pool_brfss_weights(
  ph.data = brfss_full,
  years = c(2019, 2021),
  new_wt_var = "odd_year_wt"  # Name for the new weight variable
)

# Verify the adjusted population falls between the estimated single year populations
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
| 2019, 2021  | 1,635,624  |

# Analyzing BRFSS Data with [`calc()`](https://github.com/PHSKC-APDE/rads/wiki/calc)

Now for the fun part - analyzing our data! The
[`calc()`](https://github.com/PHSKC-APDE/rads/wiki/calc) function
handles all the survey design considerations for us. Let’s look at some
examples:

## Calculate prediabetes prevalence by sex and race (using a [dtsurvey](https://github.com/PHSKC-APDE/dtsurvey) object)

``` r
prediab_by_group <- calc(
  ph.data = brfss_full,
  what = "prediab1",
  by = c("chi_sex", "race4"),
  metrics = c("mean", "rse"),
  proportion = TRUE  # Since prediab is binary
)
head(prediab_by_group)
```

| chi_sex | race4 | variable | mean | level | mean_se | mean_lower | mean_upper | rse |
|:---|:---|:---|---:|:---|---:|---:|---:|---:|
| Male | AIAN | prediab1 | 0.2284831 | NA | 0.1021132 | 0.0839714 | 0.4889464 | 44.691821 |
| Male | Black | prediab1 | 0.1187814 | NA | 0.0235966 | 0.0796585 | 0.1734967 | 19.865542 |
| Male | Asian | prediab1 | 0.1548570 | NA | 0.0167577 | 0.1247503 | 0.1906468 | 10.821374 |
| Male | NHPI | prediab1 | 0.2287657 | NA | 0.1063999 | 0.0810745 | 0.4993124 | 46.510431 |
| Male | Hispanic | prediab1 | 0.1363339 | NA | 0.0177889 | 0.1050209 | 0.1751559 | 13.048070 |
| Male | White | prediab1 | 0.1045665 | NA | 0.0061411 | 0.0931256 | 0.1172313 | 5.872895 |

## Calculate prediabetes prevalence by HRA20 (using an [imputationList](https://cran.r-project.org/web/packages/mitools/mitools.pdf))

``` r
prediab_by_hra20 <- calc(
  ph.data = brfss_hra,
  what = "prediab1",
  by = c("hra20_name"),
  metrics = c("mean", "rse"),
  proportion = TRUE  
)
head(prediab_by_hra20)
```

| hra20_name | level | variable | rse | mean | mean_se | mean_lower | mean_upper |
|:---|:---|:---|---:|---:|---:|---:|---:|
| Auburn - North | NA | prediab1 | 21.26871 | 0.1102088 | 0.0337919 | 0.0423262 | 0.1780914 |
| Auburn - South | NA | prediab1 | 36.16134 | 0.1169498 | 0.0544384 | 0.0054861 | 0.2284136 |
| Bear Creek and Greater Sammamish | NA | prediab1 | 21.51135 | 0.1386791 | 0.0394330 | 0.0602520 | 0.2171063 |
| Bellevue - Central | NA | prediab1 | 31.26468 | 0.1438465 | 0.0439368 | 0.0565102 | 0.2311828 |
| Bellevue - Northeast | NA | prediab1 | 25.85384 | 0.1055342 | 0.0390999 | 0.0280815 | 0.1829869 |
| Bellevue - South | NA | prediab1 | 21.35119 | 0.1674016 | 0.0421968 | 0.0837991 | 0.2510041 |

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

## Calculate prediabetes & obesity prevalence by HRA20 & sex (using an [imputationList](https://cran.r-project.org/web/packages/mitools/mitools.pdf))

``` r
prediab_obese_hra20_sex <- calc(
  ph.data = brfss_hra,
  what = c("prediab1", "obese"),
  by = c("hra20_name", "chi_sex"),
  metrics = c("mean", "rse"),
  proportion = TRUE  
)
head(prediab_obese_hra20_sex)
```

| hra20_name | chi_sex | level | variable | rse | mean | mean_se | mean_lower | mean_upper |
|:---|:---|:---|:---|---:|---:|---:|---:|---:|
| Auburn - North | Male | NA | prediab1 | 32.45115 | 0.0962221 | 0.0445188 | 0.0061933 | 0.1862510 |
| Auburn - North | Male | NA | prediab1 | 32.45115 | 0.2829049 | 0.0498270 | 0.1842308 | 0.3815789 |
| Auburn - North | Male | NA | obese | 14.64666 | 0.0962221 | 0.0445188 | 0.0061933 | 0.1862510 |
| Auburn - North | Male | NA | obese | 14.64666 | 0.2829049 | 0.0498270 | 0.1842308 | 0.3815789 |
| Auburn - North | Female | NA | prediab1 | 27.67092 | 0.1227671 | 0.0498892 | 0.0230234 | 0.2225109 |
| Auburn - North | Female | NA | prediab1 | 27.67092 | 0.4004819 | 0.0585218 | 0.2838316 | 0.5171322 |

# Comparing `calc(..., where = ...)` with `pool_brfss_weights`

Those with experience using `calc()` might be wondering, “*Why would we
need to use `pool_brfss_weights()` to analyze a subset of years when we
could just use the `where` argument in `calc`?*” The short answer is
that the methods are identical – as long as you are only interested in
the mean, standard error, RSE, and confidence intervals. However, if you
want to know the survey weighted number of people within a given
demographic or with a condition, you need to use `pool_brfss_weights()`.
The following example analyzing data for 2022 compares the results from
the two methods.

## Setting Up the Comparison

``` r
# Get 5 years of BRFSS data
brfss_where <- get_data_brfss(cols = c('chi_year', 'obese'), year = 2019:2023)

# Survey set a copy of the data for 2022
brfss_pooled <- pool_brfss_weights(ph.data = brfss_where, years = 2022, new_wt_var = 'wt_2022')
```

Use `calc()` to generate 2022 obesity prevalence

``` r
method_where <- calc(ph.data = brfss_where,
                    what = 'obese',
                    where = chi_year == 2022,
                    metrics = c("mean", "rse", "total"),
                    proportion = TRUE )

method_pooled <- calc(ph.data = brfss_pooled,
                    what = 'obese',
                    metrics = c("mean", "rse", "total"),
                    proportion = TRUE )
```

## Comparing Mean Estimates

Check if the mean, standard error, RSE, and CI values are equal

``` r
all.equal(method_where[, .(variable, mean, mean_se, mean_lower, mean_upper, rse)], 
          method_pooled[, .(variable, mean, mean_se, mean_lower, mean_upper, rse)])
```

    [1] TRUE

## Comparing Population Totals

Check if the survey weighted ‘total’ values are equal

``` r
all.equal(method_where[, .(variable, total, total_se, total_lower, total_upper)], 
          method_pooled[, .(variable, total, total_se, total_lower, total_upper)])
```

    [1] "Column 'total': Mean relative difference: 2.30656"

## Key Takeaway

This shows us that the mean, standard error, RSE, and CI are identical
for the two methods but the totals differ. Please remember, *to get the
correct survey weighted population you must use `pool_brfss_weights`*.

# Suppression & Reliability

Please refer to the
[APDE_SmallNumberUpdate.xlsx](https://kc1.sharepoint.com/:x:/r/teams/DPH-APDEData/_layouts/15/Doc.aspx?sourcedoc=%7B44562E46-6E45-44B1-9BAC-38EED75E9222%7D&file=APDE_SmallNumberUpdate.xlsx&action=default&mobileredirect=true&DefaultItemOpen=1)
file on SharePoint for details.

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
    Don’t worry –
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

– *Updated by dcolombara, 2024-12-17*
