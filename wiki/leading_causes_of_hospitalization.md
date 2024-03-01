# Leading Causes of Hospitalization

## **Six Steps to Identifying the Leading Causes of Hospitalization**

### Some important notes before we begin

-   In early 2024, APDE created a generalized reference table for
    categorizing ICD-9/10-CM codes
    (`kcitazrhpasqlprp16.azds.kingcounty.gov` \>\>
    `[hhs_analytics_workspace].[ref].[icdcm_codes]`). This table,
    accessible through `rads::chars_icd_ccs()`, was designed to ensure
    uniformity in analyzing claims data across various projects,
    including analyses of the leading causes of hospitalization.

-   The [AHRQ](https://www.ahrq.gov/) sponsors the [Healthcare Cost and
    Utilization Project (HCUP)](https://hcup-us.ahrq.gov/), a
    Federal-State-Industry partnership which develops databases and
    software tools for health care data. HCUP developed the ‘[Clinical
    Classifications Software Refined
    (CCSR)](https://hcup-us.ahrq.gov/toolssoftware/ccsr/dxccsr.jsp) for
    ICD-10-CM diagnoses, which aggregates more than 70,000 ICD-10-CM
    diagnosis codes into over 530 clinically meaningful categories.’ The
    CCSR is the inspiration for the ‘CCS’ table available through
    `rads::chars_icd_ccs()` / `[ref].[icdcm_codes]`. As of February 29,
    2024, the CCS table provides the aggregation of 15,016 ICD-9-CM
    codes and 100,666 ICD-10-CM codes into 7 `superlevel`, 22 `broad`
    level, 176 `midlevel`, and 515 `detailed` level categories.

-   The CCS `detailed` level categorizations form the basis for our
    leading causes of hospitalization analyses because they most closely
    resemble those used in [AHRQ](https://www.ahrq.gov/) publications on
    the same topic (e.g., [this 2018
    report)](https://hcup-us.ahrq.gov/reports/statbriefs/sb277-Top-Reasons-Hospital-Stays-2018.pdf).
    However, when calculating the leading causes of hospitalization,
    there are two necessary modifications to the `detailed`
    categorization. First, consistent with AHRQ’s approach in examining
    ‘*non-maternal, non-neonatal inpatient stays*’, we omit all
    hospitalizations related to pregnancy and childbirth. Second,
    following historical discussions with our community partners, it was
    agreed that the *intent* of injuries holds more significance than
    the *mechanism*. Consequently, we employ an injury matrix to count
    injuries by their *intent*, and we exclude injuries from the main
    analysis to avoid duplicative counting.

-   This is the protocol for the leading causes of *inpatient
    hospitalization.* You should ***NOT*** necessarily follow this
    protocol for leading causes of ED visits or customized claims
    analyses.

-   Additional relevant information regarding analysis of
    hospitalization data may be available in the [APDE Analysis
    Guide](https://kc1.sharepoint.com/:w:/r/teams/DPH-APDEData/_layouts/15/Doc.aspx?sourcedoc=%7BA49D6E2C-6FF4-49E9-95D4-7AE7B2C34E5D%7D&file=APDE_Analysis_Guide.docx&action=default&mobileredirect=true).

-   The [CHARS directory in the CHI
    repo](https://github.com/PHSKC-APDE/chi/tree/main/chars) has the
    code used to create the leading causes of death for CHNA / CHI. It
    is somewhat complex because of the various iterations needed for
    these projects. However, underneath the complexity, it *should*
    follow the structure described above.

### Overview

The code below provides an example of how to conduct a leading causes of
hospitalization analysis using RADS. For the sake of simplicity, we
limit the stratification to gender (female & male) for the years 2017 to
2021.

This protocol has six steps:

1.  Set up R

2.  Get data

3.  Get counts

4.  Merge population data onto counts

5.  Calculate age standardized rates

6.  Identify top 10 leading causes based on counts

## (1) Set up R

``` r
rm(list=ls())
library(rads)
library(data.table)
```

## (2) Get data

Pull the relevant years of inpatient data from SQL …

``` r
chars <- get_data_chars(year = 2017:2021, kingco = TRUE)
```

Split off the the chars data that will be used with
`rads::chars_injury_matrix_count()`

``` r
# Identify and save the external cause data
chars_injury <- chars[injury_nature_narrow == 1 &
                        !is.na(injury_intent) &
                        !is.na(injury_mechanism)]
```

Split off the the chars data that will be used with
`rads::chars_icd_ccs_count`

``` r
# Identify and save the non-external cause data
chars_non_injury <- chars[!(injury_nature_narrow == 1 &
                              !is.na(injury_intent) &
                              !is.na(injury_mechanism))]
```

## (3) Get counts

### Get counts irrespective of cause

``` r
counts_any <- chars[, .(hospitalizations = .N), 
                    .(chi_sex, chi_age)]

counts_any <- counts_any[, .(cause_category = 'All causes', 
                             cause = 'All causes', 
                             gender = chi_sex, 
                             age = chi_age, 
                             hospitalizations)]
```

### Get counts of external causes *by intent*

``` r
counts_intent <- rads::chars_injury_matrix_count(
  ph.data = chars_injury, 
  intent = '*', # all intents
  mechanism = 'none', # ignore mechanism
  group_by = c('chi_sex', 'chi_age'), 
  def = 'narrow', 
  primary_ecode = TRUE, 
  kingco = TRUE
)
```

Let’s confirm that we accounted for every external injury …

``` r
identical(
  as.integer(sum(counts_intent[mechanism == 'Any mechanism' & intent == 'Any intent']$hospitalizations)), 
  nrow(chars_injury)
)
```

    [1] TRUE

### Get counts of CCS `detailed` causes

First, generate the actual CCS `detailed` counts.

``` r
counts_ccs <- rads::chars_icd_ccs_count(
  ph.data = chars_non_injury,
  icdcm_version = 10,
  icdcm = NULL,
  detailed = '', # all possible detailed causes
  icdcol = 'diag1',
  group_by = c('chi_sex', 'chi_age'),
  kingco = TRUE,
  mykey = 'hhsaw'
)
```

Let’s confirm that we accounted for every non-injury hospitalization …

``` r
identical(
  as.integer(sum(counts_ccs$hospitalizations)), 
  nrow(chars_non_injury)
)
```

    [1] TRUE

Now, identify a vector of the ‘detailed’ causes of interest. There is no
need to exclude injury related causes from this vector because they were
excluded due to partitioning the data in Step 2.

``` r
# Get table of CCS detailed values that will be subset
detailed.causes <- unique(rads::chars_icd_ccs()[, .(superlevel, broad, midlevel, detailed)])

# Exclude birth (maternal & neonatal) related hospitalizations
detailed.causes <- detailed.causes[superlevel != 'Pregnancy or birth complications'] 

# Create a vector of detailed causes
detailed.causes <- detailed.causes$detailed
```

Finally, subset the CCS counts to the `detailed` causes of interest

``` r
counts_ccs <- counts_ccs[detailed_desc %in% detailed.causes]
```

### Combine counts

``` r
# Tidy CCS table
counts_ccs <- merge(counts_ccs, 
                    unique(rads::chars_icd_ccs()[, .(detailed, superlevel)]), # merge on superlevel
                    by.x = 'detailed_desc', by.y = 'detailed', 
                    all.x = TRUE, all.y = FALSE)
counts_ccs <- counts_ccs[, .(cause_category = superlevel, 
                             cause = detailed_desc, 
                             gender = chi_sex, 
                             age = chi_age, 
                             hospitalizations)]

# Tidy external cause intent table
counts_intent <- counts_intent[, .(cause_category = 'Injuries', 
                                   cause = intent, 
                                   gender = chi_sex, 
                                   age = chi_age, 
                                   hospitalizations)]
counts_intent <- counts_intent[cause != 'Any intent'] 

# Append CCS & intent count tables
counts <- rbind(counts_any, 
                counts_ccs, 
                counts_intent)

# Drop if missing gender because cannot merge with population
counts <- counts[!is.na(gender)]

# Make sure age is an integer for merging with population
counts[, age := as.integer(age)] 
```

Take a peek at the final counts table …

``` r
head(counts); tail(counts)
```

       cause_category      cause gender   age hospitalizations
               <char>     <char> <fctr> <int>            <num>
    1:     All causes All causes   Male    30             2277
    2:     All causes All causes   Male    36             2571
    3:     All causes All causes Female    59             4380
    4:     All causes All causes   Male    52             3969
    5:     All causes All causes   Male    68             5694
    6:     All causes All causes   Male    85             3363

       cause_category         cause gender   age hospitalizations
               <char>        <char> <fctr> <int>            <num>
    1:       Injuries unintentional   Male    95               84
    2:       Injuries unintentional   Male    96               71
    3:       Injuries unintentional   Male    97               59
    4:       Injuries unintentional   Male    98               37
    5:       Injuries unintentional   Male    99               17
    6:       Injuries unintentional   Male   100               30

### Ensure the count table has a row for every permutation of factors

When you generated `counts_any` above, there was no guarantee that you
had a count for *every* age and gender. The code below will ensure that
our count table has every possible logical combination of our
categorical variables with `hospitalizations == 0` when there were no
events.

``` r
# Get unique components for creation of dummy table
category_cause <- unique(counts[, .(cause_category, cause)]) 
gender_age <- CJ(gender = unique(counts$gender), age = 0:100) # cross join gender & age

# Generate the dummy table
dummy <- merge(category_cause[, constant := 1], 
               gender_age[, constant := 1], 
               by = 'constant', # constant will merge all rows
               all = TRUE, 
               allow.cartesian = TRUE)[, constant := NULL]

# Merge counts onto the dummy table
counts <- merge(dummy, 
                counts, 
                by = c('cause_category', 'cause', 'gender', 'age'),
                all = TRUE)

# Fill in any NA hospitalization values with 0
counts[is.na(hospitalizations), hospitalizations := 0]
```

## (4) Merge corresponding population onto the count data

Be sure that your call to rads::get_population provides the same years
and demographic strata that you have in your death counts!

``` r
# Get corresponding population
pop <- get_population(years = 2017:2021, group_by = c('genders', 'ages'))
pop <- pop[, .(pop, age, gender)]

# Merge the population data onto the count data
combo <- merge(counts, 
               pop, 
               by = c('age', 'gender'), 
               all = TRUE)
```

## (5) Calculate age standardized rates

``` r
rates <- rads::age_standardize(
  ph.data = combo, 
  my.count = 'hospitalizations', 
  my.pop = 'pop', 
  group_by = c('gender', 'cause', 'cause_category')
)
rates <- rates[, reference_pop := NULL]
```

Take a peek at the rates …

``` r
head(rates)
```

       gender                                      cause
       <char>                                     <char>
    1: Female                                 All causes
    2: Female Abnormal findings related to substance use
    3: Female                  Alcohol-related disorders
    4: Female         Anxiety and fear-related disorders
    5: Female              Bipolar and related disorders
    6: Female                 Cannabis-related disorders
                    cause_category  count     pop crude.rate crude.lci crude.uci
                            <char>  <num>   <num>      <num>     <num>     <num>
    1:                  All causes 457945 5540281    8265.74   8241.81   8289.71
    2: Behavioral health disorders      0 5540281       0.00      0.00      0.07
    3: Behavioral health disorders   3170 5540281      57.22     55.24     59.24
    4: Behavioral health disorders    361 5540281       6.52      5.86      7.22
    5: Behavioral health disorders   4700 5540281      84.83     82.43     87.29
    6: Behavioral health disorders     68 5540281       1.23      0.95      1.56
       adj.rate adj.lci adj.uci
          <num>   <num>   <num>
    1:  7841.68 7818.54 7864.88
    2:     0.00    0.00    0.09
    3:    52.76   50.91   54.67
    4:     7.00    6.28    7.77
    5:    82.57   80.18   85.01
    6:     1.25    0.97    1.60

## (6) Identify leading causes based on COUNTS (using random ties method)

``` r
# Create ranking (minus one, because want 'All causes' to have a rank of zero)
ranking <- rates[, ranking := frank(-count, ties.method = 'dense') -1, .(gender)]

# Keep top 10 rankings
ranking <- ranking[ranking %in% 0:10]

# Sort the data by gender and ranking
setorder(ranking, gender, ranking)

# Keep the columns of interest
ranking <- ranking[, .(gender, 
                       ranking, 
                       cause_category, 
                       cause, 
                       count,  
                       rate = adj.rate, 
                       rate_lower = adj.lci, 
                       rate_upper = adj.uci)]
```

View the top three leading causes of hospitalization by gender …

``` r
print(ranking[, .SD[1:4], gender])
```

       gender ranking      cause_category         cause  count    rate rate_lower
       <char>   <num>              <char>        <char>  <num>   <num>      <num>
    1: Female       0          All causes    All causes 457945 7841.68    7818.54
    2: Female       1 Infectious diseases    Septicemia  23684  382.62     377.67
    3: Female       2            Injuries unintentional  16796  269.61     265.47
    4: Female       3    Chronic diseases  Hypertension  15843  254.03     250.02
    5:   Male       0          All causes    All causes 348097 6534.99    6512.75
    6:   Male       1 Infectious diseases    Septicemia  26244  487.32     481.25
    7:   Male       2            Injuries unintentional  16720  313.60     308.72
    8:   Male       3    Chronic diseases  Hypertension  11950  233.48     229.17
       rate_upper
            <num>
    1:    7864.88
    2:     387.61
    3:     273.81
    4:     258.10
    5:    6557.30
    6:     493.45
    7:     318.55
    8:     237.85

## :star2: Finished!

– *Updated by dcolombara, February 29, 2024*
