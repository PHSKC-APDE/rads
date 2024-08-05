# Utility Functions


# Introduction

The `rads` package includes a suite of small utility functions designed
to simplify common data manipulation tasks and accelerate analysis
workflows.

The core utility functions we’ll cover in this vignette are:

- [`calc_age()`](#calc_age): Calculate age based on two dates
- [`convert_to_date()`](#convert_to_date): Convert various formats to
  Date objects
- [`create_dictionary()`](#create_dictionary): Generate a comprehensive
  data dictionary for a data.table or data.frame
- [`format_time()`](#format_time-and-format_time_simple): Format numeric
  values into human-readable time chunks
- [`format_time_simple()`](#format_time-and-format_time_simple): Format
  numeric values into a single time chunk
- [`generate_yaml()`](#generate_yaml): Generate a YAML file for loading
  R data to SQL
- [`multi_t_test()`](#multi_t_test): Perform t-tests for Multiple
  Comparisons with Aggregate Estimates
- [`round2()`](#round2): Improved rounding function
- [`string_clean()`](#string_clean): Clean string and factor columns
- [`std_error()`](#std_error): Calculate standard error of the mean
- [`substrRight()`](#substrright): Extract substring from the right
- [`tsql_validate_field_types()`](#tsql_validate_field_types): Validate
  R data against field types for SQL Server
- [`tsql_chunk_loader()`](#tsql_chunk_loader): Load large datasets to
  SQL Server in chunks
- [`validate_hhsaw_key()`](#validate_hhsaw_key): Validates HHSAW keys
  and, if possible, opens a connection to HHSAW

Let’s explore each of these functions with examples to demonstrate their
usage and benefits.

# Setting up the environment

First, let’s load the necessary packages:

``` r
library(rads)
library(data.table)
```

## calc_age()

The `calc_age()` function calculates age in years based on two dates.
This function has identified numerous mistakes in ages provided in DOH
vital statistics.

### `calc_age` is better than subtracting the years

``` r
birth_date <- as.Date("1990-08-02")
current_date <- as.Date("2024-08-01")
simple_age <- year(current_date) - year(birth_date)
proper_age <- calc_age(from = birth_date, to = current_date)

head(data.table(birth_date, current_date, simple_age, proper_age))
```

| birth_date | current_date | simple_age | proper_age |
|:-----------|:-------------|-----------:|-----------:|
| 1990-08-02 | 2024-08-01   |         34 |         33 |

### `calc_age` is better than days / 365.25

``` r
dates <- data.table(
  birth = seq.Date(as.Date('2000-02-25'), as.Date('2000-03-02'), 1), 
  death = seq.Date(as.Date('2024-03-02'), as.Date('2024-02-25'), -1))
dates[, simple_age := round(as.integer(death - birth)/365.25)]
dates[, proper_age := calc_age(birth, death)]

head(dates)
```

| birth      | death      | simple_age | proper_age |
|:-----------|:-----------|-----------:|-----------:|
| 2000-02-25 | 2024-03-02 |         24 |         24 |
| 2000-02-26 | 2024-03-01 |         24 |         24 |
| 2000-02-27 | 2024-02-29 |         24 |         24 |
| 2000-02-28 | 2024-02-28 |         24 |         24 |
| 2000-02-29 | 2024-02-27 |         24 |         23 |
| 2000-03-01 | 2024-02-26 |         24 |         23 |

## convert_to_date()

`convert_to_date()` attempts to convert various US style date formats to
R Dates.

``` r
dates <- data.table(orig = c("2024-07-25", "07/26/2024", 
                             "07-27-2024", "20240728", 
                             "29 July, 2024", "July 30, 2024"))
dates[, rads_dates := convert_to_date(orig)]
dates[, simple_dates := as.Date(orig)]

head(dates)
```

| orig          | rads_dates | simple_dates |
|:--------------|:-----------|:-------------|
| 2024-07-25    | 2024-07-25 | 2024-07-25   |
| 07/26/2024    | 2024-07-26 | NA           |
| 07-27-2024    | 2024-07-27 | NA           |
| 20240728      | 2024-07-28 | NA           |
| 29 July, 2024 | 2024-07-29 | NA           |
| July 30, 2024 | 2024-07-30 | NA           |

This function is particularly useful when dealing with inconsistent date
formats within a single vector or column of your data.

## create_dictionary()

The `create_dictionary()` function generates a comprehensive data
dictionary for a given data.table or data.frame. It provides details
about variable types, sample values, and can incorporate descriptions
and notes from a reference table. The function can be used to both
understand a new dataset and document the products of your ETL
processes.

### Basic usage

Let’s create a sample dataset and generate a dictionary:

``` r
dt <- data.table(
  name = c("Alicia", "Bianca", "Clarence", "Dwane", "Eve"),
  gender = factor(c("F", "M", "M", "M", "F")),
  age = c(25, 30, 35, 40, 45),
  score = c(85.5, 92.0, 78.5, 88.0, 95.5)
)

dict <- create_dictionary(ph.data = dt, source = "sample_data")

head(dict, 10)
```

| source      | varname | vartype   | values   | factor_labels | desc | notes | dict_updated |
|:------------|:--------|:----------|:---------|:--------------|:-----|:------|:-------------|
| sample_data | name    | character | Alicia   | NA            | NA   | NA    | 2024-08-05   |
| sample_data | name    | character | Bianca   | NA            | NA   | NA    | 2024-08-05   |
| sample_data | name    | character | Clarence | NA            | NA   | NA    | 2024-08-05   |
| sample_data | name    | character | Dwane    | NA            | NA   | NA    | 2024-08-05   |
| sample_data | name    | character | Eve      | NA            | NA   | NA    | 2024-08-05   |
| sample_data | gender  | factor    | 1        | F             | NA   | NA    | 2024-08-05   |
| sample_data | gender  | factor    | 2        | M             | NA   | NA    | 2024-08-05   |
| sample_data | age     | integer   | 25       | NA            | NA   | NA    | 2024-08-05   |
| sample_data | age     | integer   | 30       | NA            | NA   | NA    | 2024-08-05   |
| sample_data | age     | integer   | 35       | NA            | NA   | NA    | 2024-08-05   |

### Suppressing sensitive information

You can suppress sample values for sensitive columns:

``` r
dict_suppressed <- create_dictionary(ph.data = dt, 
                                     source = "sample_data", 
                                     suppress = c("name"))

head(dict_suppressed, 10)
```

| source      | varname | vartype   | values       | factor_labels | desc | notes | dict_updated |
|:------------|:--------|:----------|:-------------|:--------------|:-----|:------|:-------------|
| sample_data | name    | character | *suppressed* | NA            | NA   | NA    | 2024-08-05   |
| sample_data | gender  | factor    | 1            | F             | NA   | NA    | 2024-08-05   |
| sample_data | gender  | factor    | 2            | M             | NA   | NA    | 2024-08-05   |
| sample_data | age     | integer   | 25           | NA            | NA   | NA    | 2024-08-05   |
| sample_data | age     | integer   | 30           | NA            | NA   | NA    | 2024-08-05   |
| sample_data | age     | integer   | 35           | NA            | NA   | NA    | 2024-08-05   |
| sample_data | age     | integer   | 40           | NA            | NA   | NA    | 2024-08-05   |
| sample_data | age     | integer   | 45           | NA            | NA   | NA    | 2024-08-05   |
| sample_data | score   | numeric   | 78.5         | NA            | NA   | NA    | 2024-08-05   |
| sample_data | score   | numeric   | 85.5         | NA            | NA   | NA    | 2024-08-05   |

### Incorporating reference information

You can include additional descriptions and notes using a reference
table:

``` r
ref_table <- data.table(
  source = rep("sample_data", 4),
  varname = c("name", "age", "gender", "score"),
  desc = c("Full name", "Age in years", "Gender", "Test score"),
  notes = c("May contain special characters", 
            "Rounded to nearest year", "Binary classification", 
            "Range: 0-100")
)

dict_with_ref <- create_dictionary(ph.data = dt, 
                                   source = "sample_data", 
                                   ph.ref = ref_table)

head(dict_with_ref, 8)
```

| source      | varname | vartype   | values   | factor_labels | desc         | notes                          | dict_updated |
|:------------|:--------|:----------|:---------|:--------------|:-------------|:-------------------------------|:-------------|
| sample_data | name    | character | Alicia   | NA            | Full name    | May contain special characters | 2024-08-05   |
| sample_data | name    | character | Bianca   | NA            | Full name    | May contain special characters | 2024-08-05   |
| sample_data | name    | character | Clarence | NA            | Full name    | May contain special characters | 2024-08-05   |
| sample_data | name    | character | Dwane    | NA            | Full name    | May contain special characters | 2024-08-05   |
| sample_data | name    | character | Eve      | NA            | Full name    | May contain special characters | 2024-08-05   |
| sample_data | gender  | factor    | 1        | F             | Gender       | Binary classification          | 2024-08-05   |
| sample_data | gender  | factor    | 2        | M             | Gender       | Binary classification          | 2024-08-05   |
| sample_data | age     | integer   | 25       | NA            | Age in years | Rounded to nearest year        | 2024-08-05   |

### Handling numerous unique values

For datasets with many unique values, create_dictionary() provides
summaries:

``` r
set.seed(98104)
large_dt <- data.table(
  id = 1:1000,
  category = sample(letters[1:10], 1000, replace = TRUE),
  value = round2(rnorm(1000), 2)
)

large_dict <- create_dictionary(ph.data = large_dt, 
                                source = "large_sample")

head(large_dict, 8)
```

| source       | varname  | vartype   | values                  | factor_labels | desc | notes | dict_updated |
|:-------------|:---------|:----------|:------------------------|:--------------|:-----|:------|:-------------|
| large_sample | id       | integer   | min = 1, max = 1000     | NA            | NA   | NA    | 2024-08-05   |
| large_sample | category | character | a                       | NA            | NA   | NA    | 2024-08-05   |
| large_sample | category | character | b                       | NA            | NA   | NA    | 2024-08-05   |
| large_sample | category | character | c                       | NA            | NA   | NA    | 2024-08-05   |
| large_sample | category | character | d                       | NA            | NA   | NA    | 2024-08-05   |
| large_sample | category | character | e                       | NA            | NA   | NA    | 2024-08-05   |
| large_sample | category | character | …                       | NA            | NA   | NA    | 2024-08-05   |
| large_sample | value    | numeric   | min = -3.33, max = 3.73 | NA            | NA   | NA    | 2024-08-05   |

## format_time() and format_time_simple()

These functions format numeric values (including dates) into
human-readable time chunks. They are often used for summarizing years
and can be used for identifying unexpected gaps.

``` r
years <- c(2001:2003, 
           2005, 
           2008, 
           2011:2012)

dates <- as.Date(c("2024-01-01", "2024-01-02", "2024-01-03",
                   "2024-01-06", 
                   "2024-01-09", "2024-01-10", "2024-01-11", "2024-01-12"))

dt <- data.table(desc = c('format_time', 'format_time_simple', '',
                          'format_time', 'format_time_simple'), 
                 data = c(format_time(years), format_time_simple(years), '', 
                          format_time(dates), format_time_simple(dates)))
head(dt)
```

| desc               | data                                                           |
|:-------------------|:---------------------------------------------------------------|
| format_time        | 2001-2003, 2005, 2008, 2011-2012                               |
| format_time_simple | 2001-2012                                                      |
|                    |                                                                |
| format_time        | 2024-01-01 to 2024-01-03, 2024-01-06, 2024-01-09 to 2024-01-12 |
| format_time_simple | 2024-01-01 to 2024-01-12                                       |

## generate_yaml()

`generate_yaml()` generates a YAML file for SQL loading based on a
data.frame or data.table.

``` r
data <- data.table(
  id = 1:5,
  name = c("Alicia", "Bianca", "Clarence", "Dwane", "Eve"),
  age = c(25.1, 30.2, 35.3, 40.4, 45.5)
)

yaml_output <- generate_yaml(data, schema = "dbo", table = "example_table", datasource = 'WADOH_CHS')
print(yaml_output)
```

    $datasource
    [1] "WADOH_CHS"

    $schema
    [1] "dbo"

    $table
    [1] "example_table"

    $vars
    $vars$id
    [1] "INT"

    $vars$name
    [1] "NVARCHAR(18)"

    $vars$age
    [1] "NUMERIC(38,5)"

The output can be be used to specify the field_types for the
`tsql_validate_field_types()` and `tsql_chunk_loader()` functions
described below.

``` r
field_types <- unlist(yaml_output$vars)
print(field_types)
```

                 id            name             age 
              "INT"  "NVARCHAR(18)" "NUMERIC(38,5)" 

## multi_t_test()

The `multi_t_test()` function performs t-tests comparing multiple groups
against a reference group using means, standard errors, and the sample
size (when available).

### Basic usage

Let’s create a sample dataset and use `multi_t_test()` to compare
birthweights across different maternal age groups:

``` r
# Sample data
age_groups <- c("18-24", "25-29", "30-34", "35-39", "40+")
birthweight_means <- c(3150, 3450, 3400, 3250, 3100)  # in grams
birthweight_ses <- c(50, 45, 40, 55, 60)
sample_sizes <- c(500, 800, 750, 400, 200)
reference_group <- 3  # comparing all groups to the 30-34 age group

# Perform multi_t_test
birthweight_comparison <- multi_t_test(means = birthweight_means,
                                       ses = birthweight_ses,
                                       reference_index = reference_group,
                                       n = sample_sizes,
                                       alternative = 'two.sided',
                                       df_method = "estimated", 
                                       alpha = 0.05)

# Add age group labels to the results
birthweight_comparison[, Age_Group := age_groups]

head(birthweight_comparison)
```

| comparison           | diff_means |   ci_lower |  ci_upper |   p.value | significant | t.statistic |        df | df_method | Age_Group |
|:---------------------|-----------:|-----------:|----------:|----------:|:------------|------------:|----------:|:----------|:----------|
| Group 1 vs Reference |       -250 | -375.64316 | -124.3568 | 0.0001005 | TRUE        |  -3.9043440 | 1054.3852 | estimated | 18-24     |
| Group 2 vs Reference |         50 |  -68.09846 |  168.0985 | 0.4064107 | FALSE       |   0.8304548 | 1536.8996 | estimated | 25-29     |
| Group 3 - Referent   |          0 |         NA |        NA |        NA | NA          |          NA |        NA | estimated | 30-34     |
| Group 4 vs Reference |       -150 | -283.49100 |  -16.5090 | 0.0276885 | TRUE        |  -2.2056439 |  811.7334 | estimated | 35-39     |
| Group 5 vs Reference |       -300 | -441.76996 | -158.2300 | 0.0000390 | TRUE        |  -4.1602515 |  394.4939 | estimated | 40+       |

### Key features

1.  **Flexible degrees of freedom calculation**: The function offers
    four methods for calculating degrees of freedom:

    - “**estimated**” (default): Uses the Welch–Satterthwaite equation
    - “**conservative**”: Uses the minimum possible degrees of freedom
      (df = 2); lower Type I Error (false +) and higher Type II Error
      (false -)
    - “**moderate**”: Uses the number of groups minus 1 (df = k - 1)
    - “**liberal**”: Assumes infinite degrees of freedom (df = Inf);
      higher Type 1 Error (false +) and lower Type II Error (false -)

2.  **Sample size estimation**: If sample sizes are not provided, the
    function can estimate them based on the distribution of mean values.

3.  **Adjustment for multiple comparisons (optional)**: If desired, it
    will adjust p-values for multiple comparisons using one of two
    methods (Holm-Bonferroni or Benjamini-Hochberg).

4.  **Comprehensive output**: The function returns a data.table
    containing comparison results, including difference in means,
    confidence intervals, p-values, and significance indicators.

## round2()

`round2()` rounds numbers the way we learned in elementary school: if
the digit to the right of the rounding position is 5 or greater, round
up; otherwise, round down. ***Note!*** *Base R’s `round` function
follows a different logic (IEC 60559 / IEEE 754 standards), which
specifies rounding to the nearest even number.*

``` r
set.seed(98104)
dt <- data.table(orig = seq(0.5, 5.5, 1))
dt[, round2 := round2(orig)]
dt[, baseR := round(orig)]
dt[round2 != baseR, different := '*']
head(dt)
```

| orig | round2 | baseR | different |
|-----:|-------:|------:|:----------|
|  0.5 |      1 |     0 | \*        |
|  1.5 |      2 |     2 | NA        |
|  2.5 |      3 |     2 | \*        |
|  3.5 |      4 |     4 | NA        |
|  4.5 |      5 |     4 | \*        |
|  5.5 |      6 |     6 | NA        |

Like the base R `round` function, `round2` can also be used to round
large numbers to convey uncertainty in estimates

``` r
# round to hundreds
round2(123456, -2)
```

    [1] 123500

``` r
# round to thousands
round2(123456, -3)
```

    [1] 123000

## string_clean()

`string_clean()` cleans string and factor columns in a data.frame or
data.table. It eliminates random white spaces and pseudo-white spaces
and sets blanks to true NA.

``` r
# create table 
dt <- data.table(
  name = c(" John   ", "Jane  Doe", "Bob Smith", "  ", "  Edward  Jenner"),
  category = factor(c("A ", "   B", "", "D", "E      "))
)
head(dt)
```

                   name category
                 <char>   <fctr>
    1:          John          A 
    2:        Jane  Doe        B
    3:        Bob Smith         
    4:                         D
    5:   Edward  Jenner  E      

``` r
# clean table
string_clean(dt)
head(dt)
```

                name category
              <char>   <fctr>
    1:          John        A
    2:      Jane Doe        B
    3:     Bob Smith     <NA>
    4:          <NA>        D
    5: Edward Jenner        E

## std_error()

`std_error()` calculates the standard error of the mean for a vector,
columns in a table, or a list. R does not have a built in standard error
of the mean function, so we created one for `rads`.

### Use with a vector

``` r
set.seed(98104)
x <- rnorm(1000)
se <- std_error(x)
print(paste("Standard Error:", round2(se, 5)))
```

    [1] "Standard Error: 0.03207"

### Use with a data.table / data.frame

``` r
set.seed(98104)
dt <- data.table(x = rnorm(1000), y = rnorm(1000))
se <- std_error(dt)
se <- round2(se, 5)
print(se)
```

          x       y 
    0.03207 0.03235 

### Use with a list

``` r
set.seed(98104)
mylist <- list(x = rnorm(1000), y = rnorm(1000))
se <- std_error(mylist) # returns a list with the standard errors
se <- lapply(se, round2, n = 5)
print(se)
```

    $x
    [1] 0.03207

    $y
    [1] 0.03235

## substrRight()

`substrRight()` extracts a substring from the right side of a string. It
complements the base R function `substr`.

``` r
text <- "Hello, World!"
substr_right <- substrRight(text, 2, 6)
print(substr_right)
```

    [1] "World"

This function is useful when you need to extract the right hand portions
of strings, e.g., the last 4 digits of a phone number.

## tsql_validate_field_types()

`tsql_validate_field_types()` validates whether a named vector of TSQL
data types is compatible with a data.table. This function is useful when
you have a yaml file specifying the TSQL field types to be used when
uploading your data to a server.

### A successful validation

``` r
dt <- data.table(
  id = 1:5,
  name = c("Alicia", "Bianca", "Clarence", "Dwane", "Eve"),
  age = c(25, 30, 35, 40, 45), 
  time = Sys.time()
)

field_types <- c(id = "int", 
                 name = "varchar(50)", 
                 age = "int", 
                 time = 'datetime')
validation_result <- tsql_validate_field_types(dt, field_types)
```

    🙂 Success! Your desired TSQL data types are suitable for your dataset.

``` r
head(validation_result)
```

| colname | R_type    | tsql_type | is_valid | issue |
|:--------|:----------|:----------|:---------|:------|
| age     | numeric   | int       | TRUE     | NA    |
| id      | integer   | int       | TRUE     | NA    |
| name    | character | varchar   | TRUE     | NA    |
| time    | POSIXct   | datetime  | TRUE     | NA    |

### A failed validation

``` r
dt <- data.table(
  id = 1:5,
  name = c("Alicia", "Bianca", "Clarence", "Dwane", "Eve"),
  age = as.character(c(25, 30, 35, 40, 45)), 
  time = Sys.Date()
)

field_types <- c(id = "int", 
                 name = "varchar(50)", 
                 age = "int", # should generate an error
                 time = 'datetime') # should generate an error
validation_result <- tsql_validate_field_types(dt, field_types)
```

    Error in tsql_validate_field_types(dt, field_types): 
    🛑👿 The following columns in your dataset did not align with the proposed TSQL field types:
         column: age, R Type: character, TSQL Type: int, issue: Incompatible types
         column: time, R Type: Date, TSQL Type: datetime, issue: Incompatible types

## tsql_chunk_loader()

`tsql_chunk_loader()` loads large datasets to Microsoft SQL Server in
‘chunks’. It is suitable for datasets too large for single upload. For
extreme sizes try using
[bcp](https://learn.microsoft.com/en-us/sql/tools/bcp-utility?view=sql-server-ver16&tabs=windows).

Benefits include:

- Loads large datasets in chunks, preventing memory and batch size
  issues
- Reduces risk of network timeouts during data transfer
- Attempts multiple uploads per chunk for error resilience
- Provides progress tracking
- Allows SQL data type specification and validation
- Verifies complete data upload

``` r
 mydt = data.table(col1 = 1:10000L,  # create integer
                   col2 = 1:10000/3) # create float
 mydt[, col3 := as.Date(Sys.Date()) - col1] # create date
 mydt[, col4 := as.character(col3)] # create string
 
 myfieldtypes <- c(col1 = 'int', col2 = 'float', 
                   col3 = 'date', col4 = 'nvarchar(20)')

 tsql_chunk_loader(
   ph.data = mydt,
   db_conn = rads::validate_hhsaw_key(), # connect to Azure 16 hhs_analytics_workspace
   chunk_size = 3333,
   schema_name = Sys.getenv("USERNAME"),
   table_name = 'JustTesting',
   overwrite = TRUE,
   append = FALSE,
   field_types = myfieldtypes,
   validate_field_types = TRUE, # uses tsql_validate_field_types()
   validate_upload = TRUE
 )
```

    🙂 Success! Your desired TSQL data types are suitable for your dataset.

    2024-08-05 15:30:39.613543: Loading chunk 1 of 4: rows 1-3,333

    2024-08-05 15:30:39.987563: Loading chunk 2 of 4: rows 3,334-6,666

    2024-08-05 15:30:40.527268: Loading chunk 3 of 4: rows 6,667-9,999

    2024-08-05 15:30:41.026392: Loading chunk 4 of 4: rows 10,000-10,000

    🙂 🎉 🎊 🌈 
     Congratulations! All the rows in {ph.data} were successfully uploaded.

## validate_hhsaw_key()

`validate_hhsaw_key()` validates HHSAW keys **and** opens a connection
to HHSAW when possible.

``` r
 myConnection <- validate_hhsaw_key(hhsaw_key = 'hhsaw')
 print(myConnection)
```

    <OdbcConnection> DColombara@kingcounty.gov@kcitazrhpasqlprp16
      Database: hhs_analytics_workspace
      Microsoft SQL Server Version: 12.00.5564

# Conclusion

These utility functions provide tools for common data manipulation and
analysis tasks. By leveraging these functions, you can streamline your
workflow, improve code readability, and handle various data-related
challenges more efficiently.

Remember to consult the individual function documentation for more
detailed information on usage and parameters. Happy coding!

– *Updated by dcolombara, 2024-08-05*
