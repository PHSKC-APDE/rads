# Leading Causes of Death

## **Six Steps to Identifying the Leading Causes of Death**

### Some important notes before we begin

-   The CDC’s [National Center for Health Statistics
    (NCHS)](https://www.cdc.gov/nchs/index.htm) collects death
    certificate data from state vital statistics offices. NCHS performs
    extensive quality assurance, codes the causes of death using the
    10th revision of the International Classification of Diseases
    (ICD-10), and sends the standardized death data back to state vital
    statistics offices. They also compile the data to form a
    comprehensive national database of mortality statistics. Each year
    they publish an instruction manual with ICD-10 cause of death lists
    for tabulating mortality statistics, [like this one from
    2020](https://www.cdc.gov/nchs/data/dvs/Part9InstructionManual2020-508.pdf).
    The two key tables for the purposes of PHSKC analyses (including
    leading causes of death analyses) are:

    -   **113 Selected Causes of Death, Enterocolitis due to
        *Clostridium difficile*, and COVID-19,** which is used for the
        general analysis of mortality and for ranking leading causes of
        death. APDE uses this list for individuals aged 1 year and
        older.. You can view this list with `rads::death_113()` and can
        generate counts using `rads::death_113_count()`.

    -   **130 Selected Causes of Infant Death**, which is used for the
        analysis of infant mortality and for ranking leading causes of
        infant death. APDE uses this list for infants, i.e., those who
        are under 1 year old. You can view this list with
        `rads::death_130()` and can generate counts using
        `rads::death_130_count()`.

-   Infant death analyses are special.

    -   In addition to using a different set of cause of death
        functions, *infant death analyses do not use the standard death
        certificate & population data* available from
        `rads::get_data_death()` and `rads::get_population()`.

        -   The ***numerator*** comes from the infant death files, which
            are downloaded from
            [secureaccess.wa.gov/doh/chsdatafiles/](https://secureaccess.wa.gov/doh/chsdatafiles/)
            and saved (as of 2024-03-01) in
            `\\dphcifs\APDE-CDIP\InfantDeath\`. This has not gone
            through an ETL process, so you will have to clean the
            necessary variables as needed.

        -   The ***denominator*** is the number of *live births* rather
            than the estimated population with age zero (though they are
            very similar).

    -   The ***rate*** is *per 1,000* rather than per 100,000.

    -   We define King County infant deaths by **either** birth to a
        mother who is a King County resident **or** the death of an
        infant who is a King County resident.

    -   If we do not have have geocoded InfantDeath data, we can extract
        geographic data from the main birth data by linking with the
        `birth_cert_encrypt`. For those that were included due to being
        a resident at the time of death, we can extract geographies from
        the death data by linking with the `state_file_number`. However,
        this will not be relevant below because we are not stratifying
        by region or HRA.

    -   Since the numerator and the denominator come from two different
        datasets, it is easiest to treat them as counts (infant deaths)
        and populations (live births) for use with
        `rads::age_standardize`. Age adjusting for a single age group
        doesn’t make sense, so we use the crude results and confidence
        intervals.

-   By default, [rads death
    functions](https://github.com/PHSKC-APDE/rads/wiki/death_functions)
    use the ICD-10 codes in the `underlying_cod_code` column available
    through `rads::get_data_death()`. This is the column that you will
    want to use for an *underlying cause of death* analysis, which is
    almost certainly what you will want to do. If you want to change
    this default, think deeply about your rationale while holding your
    breath for 2 minutes and 30 seconds.

-   Additional relevant information regarding analysis of death data may
    be available in the [APDE Analysis
    Guide](https://kc1.sharepoint.com/:w:/r/teams/DPH-APDEData/_layouts/15/Doc.aspx?sourcedoc=%7BA49D6E2C-6FF4-49E9-95D4-7AE7B2C34E5D%7D&file=APDE_Analysis_Guide.docx&action=default&mobileredirect=true).

-   The [death directory in the CHI
    repo](https://github.com/PHSKC-APDE/chi/tree/main/death) has the
    code used to create the leading causes of death for CHNA / CHI. It
    is somewhat complex because of the various iterations needed for
    these projects. However, underneath the complexity, it ***should***
    follow the structure described above.

### Overview

The code below provides an example of how to conduct a leading causes of
death analysis using RADS. For the sake of simplicity, we limit the
stratification to gender (female & male) for the years 2018 to 2022.

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

### Non-infant deaths (ages ≥ 1)

``` r
death <- get_data_death(year = 2018:2022, 
                        cols = c('chi_geo_kc', 'chi_year', 'chi_sex', 
                                 'chi_age', 'underlying_cod_code'), 
                        kingco = TRUE)

# Non-infant deaths (will use NCHS 113 Selected Causes of Death)
death <- death[chi_age != 0] 
```

### Infant deaths (ages \< 1)

``` r
# Identify the files to import
infdeath.files = list.files(path = "//dphcifs/APDE-CDIP/InfantDeath/raw/", 
                            pattern = "^InfantDeathF2[0-9][0-9][0-9]", 
                            full.names = TRUE)
infdeath.files <- grep("201[8-9]|202[0-2]", infdeath.files, value = T)

# Import data and standardize critical column names
infdeath <- lapply(
  X = as.list(infdeath.files), 
  FUN = function(X){
    # read files
    ifelse(grepl("csv$", X), tempx <- fread(X), tempx <- setDT(openxlsx::read.xlsx(X)))
    
    # first column is always birt cert number
    setnames(tempx, old = names(tempx)[1], new = 'birth_cert_encrypt') 
    
    # tidy column names
    setnames(tempx, gsub(" |\\.", "_", tolower(names(tempx)))) 
    setnames(tempx, 'death_state_file_number', 'state_file_number')
    setnames(tempx, 'residence_state_nchs_code', 'residence_state_death')
    setnames(tempx, 'residence_county_wa_code', 'residence_county_death')
    setnames(tempx, 'mother_residence_state_nchs_code', 'residence_state_birth')
    setnames(tempx, 'mother_residence_county_wa_code', 'residence_county_birth',
             skip_absent = T)
    setnames(tempx, 'mother_residence_county_city_wa', 'residence_county_birth',
             skip_absent = T)

    # county of birth sometimes has city code too, but first two digits are for county
    tempx[, residence_county_birth := substr(as.character(residence_county_birth), 1, 2)] 
    
    # keep final set of variables
    tempx <- tempx[, .(birth_cert_encrypt, state_file_number,
                       residence_state_birth, residence_county_birth, 
                       residence_state_death, residence_county_death,
                       underlying_cod_code, chi_sex = sex, 
                       chi_year = as.integer(substr(birth_cert_encrypt, 1, 4)) )]
  })

# Append the list of infant mortality tables
infdeath <- rbindlist(infdeath, use.names = TRUE)
```

Tidy the infant mortality data you just imported

``` r
# Generic string cleaning
rads::sql_clean(infdeath)

# Clean sex variable
infdeath[chi_sex == 'N', chi_sex := NA]
infdeath[chi_sex == 'M', chi_sex := 'Male']
infdeath[chi_sex == 'F', chi_sex := 'Female']

# Eliminate nonsense birth certificate numbers
infdeath[, birth_cert_encrypt := suppressWarnings(as.integer(birth_cert_encrypt))] 

# Replace all 'missing' with true NA
infdeath[, (names(infdeath)) := lapply(
  .SD, function(x) fifelse(x %in% c('99', 'U'), NA, x)),
  .SDcols = names(infdeath)]

# Identify KC data 
infdeath[
  (residence_state_birth == 48 & residence_county_birth == 17) # mother KC resident @ birth
  | (residence_state_death == 48 & residence_county_death == 17), # infant KC resident @ death
        chi_geo_kc := "King County"]
infdeath <- infdeath[chi_geo_kc == 'King County']

# Drop geography vars no longer needed
infdeath[, c('residence_county_death', 'residence_county_birth', 
             'residence_state_death', 'residence_state_birth') := NULL]
```

### Reference tables

Get *reference tables* with `cause`, `causeid`, and `cause_category`
values for infant and non-infant causes of death. These will be used to
aggregate causes of death into more meaningful categories.

``` r
# Categories of non-infant deaths
death_groups <- rads.data::icd_nchs113causes_raw[, .(cause = leading.cause.group.alias, 
                                                     causeid, 
                                                     cause_category = cause.category)]

# Categories of infant deaths
infdeath_groups <- rads.data::icd_nchs130causes_raw[, .(cause = leading.cause.group.alias, 
                                                        causeid, 
                                                        cause_category = cause.category)]
```

Take a peek at one of the reference tables you just created …

``` r
head(death_groups)
```

                                     cause causeid     cause_category
                                    <char>   <int>             <char>
    1:               Salmonella infections       1 Infectious disease
    2:           Shigellosis and amebiasis       2 Infectious disease
    3: Certain other intestinal infections       3 Infectious disease
    4:                        Tuberculosis       4 Infectious disease
    5:                        Tuberculosis       5 Infectious disease
    6:                      Whooping cough       6 Infectious disease

As you can see with Tuberculosis in rows 4 & 5, multiple `causeid`s will
sometimes be rolled up into one of the 58 `cause` values. Therefore the
`cause` values allow us to aggregate the `causeid` values into fewer
categories.

## (3) Get counts

### Non-infant deaths (ages ≥ 1)

``` r
# Do not specify`cause` or `causeids` arguments in order to get all causes of death
death_count <- death_113_count(
  ph.data = death, 
  icdcol = 'underlying_cod_code',
  group_by = c('chi_sex', 'chi_age'),
  kingco = TRUE 
)
```

Tidy the table of non-infant death counts

``` r
# Ensure all rows have a causeid (will be missing when there was a zero count)
death_count <- merge(death_count[, causeid := NULL], 
                     rads::death_113(), 
                     by = 'cause.of.death', 
                     all.x= TRUE, all.y= FALSE)

# Add on the leading cause of death group identifier ----
death_count <- merge(death_count, 
                     death_groups, 
                     by = 'causeid', 
                     all.x= TRUE, all.y= FALSE)

# Fill in values for 'All causes' and COVID-19
death_count[cause.of.death %in% c('All causes', 'COVID-19 (U07.1)'), 
            cause := cause.of.death]
death_count[cause.of.death == 'All causes', cause_category := cause.of.death]
death_count[cause.of.death == 'COVID-19 (U07.1)', cause_category := 'Infectious disease']

# Delete if missing cause (these are cause.of.death == 'Missing/Unknown')
death_count <- death_count[!is.na(cause)]

# Delete if 'unspecified', 'not elsewhere classified', etc. 
death_count <- death_count[cause != '999']

# Delete if missing chi_sex because cannot merge with population
death_count <- death_count[!is.na(chi_sex)]

# Ensure age is an integer for merging with population
death_count[, chi_age := as.integer(chi_age)] 

# Sum deaths for each unique strata and keep select columns 
# This is necessary because a single `cause` can map to more than one `cause.of.death`
death_count <- death_count[ ,
                            .(deaths = sum(deaths)),
                            .(age = chi_age,
                              gender = chi_sex, 
                              cause, 
                              cause_category)]
```

Take a peek at the tidied table of non-infant death counts

``` r
head(death_count[deaths >= 10]) # suppress view
```

         age gender      cause cause_category deaths
       <int> <fctr>     <char>         <char>  <int>
    1:     1 Female All causes     All causes     14
    2:     1   Male All causes     All causes     13
    3:     2   Male All causes     All causes     11
    4:     7   Male All causes     All causes     11
    5:    12   Male All causes     All causes     10
    6:    13 Female All causes     All causes     10

### Infant deaths (ages\< 1)

``` r
# Do not specify`cause` or `causeids` arguments in order to get all causes of death
infdeath_count <- death_130_count(
  ph.data = infdeath, 
  icdcol = 'underlying_cod_code',
  group_by = c('chi_sex'),
  kingco = TRUE 
)
```

Tidy the table of infant death counts

``` r
# Ensure all rows have a causeid (will be missing when there was a zero count)
infdeath_count <- merge(infdeath_count[, causeid := NULL], 
                     rads::death_130(), 
                     by = 'cause.of.death', 
                     all.x= TRUE, all.y= FALSE)

# Add on the leading cause of death group identifier ----
infdeath_count <- merge(infdeath_count, 
                     infdeath_groups, 
                     by = 'causeid', 
                     all.x= TRUE, all.y= FALSE)

# Fill in values for 'All causes' and COVID-19
infdeath_count[cause.of.death %in% c('All causes', 'COVID-19 (U07.1)'), 
            cause := cause.of.death]
infdeath_count[cause.of.death == 'All causes', cause_category := cause.of.death]
infdeath_count[cause.of.death == 'COVID-19 (U07.1)', cause_category := 'Infectious disease']

# Delete if missing cause (these are cause.of.death == 'Missing/Unknown')
infdeath_count <- infdeath_count[!is.na(cause)]

# Delete if 'unspecified', 'not elsewhere classified', etc. 
infdeath_count <- infdeath_count[cause != '999']

# Delete if missing chi_sex because cannot merge with population
infdeath_count <- infdeath_count[!is.na(chi_sex)]

# Sum deaths for each unique strata and keep select columns 
infdeath_count <- infdeath_count[ , 
                                  .(deaths = sum(deaths), age = 0L), 
                                  .(gender = chi_sex, 
                                    cause, 
                                    cause_category)]
```

Take a peek at the tidied table of infant death counts

``` r
head(infdeath_count[deaths >= 10]) # suppress view
```

       gender                     cause cause_category deaths   age
       <char>                    <char>         <char>  <int> <int>
    1: Female                All causes     All causes    217     0
    2:   Male                All causes     All causes    286     0
    3: Female    Maternal complications          Other     11     0
    4:   Male    Maternal complications          Other     38     0
    5: Female Complications of placenta          Other     12     0
    6:   Male Complications of placenta          Other     10     0

## (4) Merge corresponding population onto the count data

### Non-infants (ages ≥ 1)

Be sure that your call to `rads::get_population` provides the same years
and demographic strata that you have in your death counts!

``` r
# Get population for the years and demographic strata corresponding to your death counts
pop <- get_population(years = 2017:2021, ages = 1:100, group_by = c('genders', 'ages'))
pop <- pop[, .(pop, age, gender)]

# Merge the population data onto the non-infant death count data
combo <- merge(death_count, 
               pop, 
               by = c('age', 'gender'), 
               all.x= TRUE, all.y= TRUE)
```

### Infants (ages\< 1)

Remember that the infant population should be the number of *live
births*.

``` r
# Get infant population from birth data - by sex
infpop <- rads::get_data_birth(cols = c('chi_sex'), year = 2017:2022, kingco = T)
infpop <- infpop[, .(pop = .N), .(gender = as.character(chi_sex))]
infpop <- infpop[!is.na(gender)]

# Merge the population (live birth) data onto the infant death count data
infcombo <- merge(infdeath_count, 
               infpop, 
               by = c('gender'), 
               all.x= TRUE, all.y= TRUE)
```

## (5) Calculate age standardized rates

### Non-infant deaths (ages ≥ 1)

``` r
# Non-infant deaths are age standardized
rates <- rads::age_standardize(
  ph.data = combo, 
  my.count = 'deaths', 
  my.pop = 'pop', 
  group_by = c('gender', 'cause', 'cause_category')
)

rates <- rates[, .(gender, 
                   ranking = NA_integer_, 
                   cause_category, 
                   cause, 
                   count, 
                   rate = adj.rate,
                   rate_lower = adj.lci,
                   rate_upper = adj.uci)]
```

Take a peek at the non-infant rates …

``` r
head(rates[count >= 10]) # suppress view
```

       gender ranking     cause_category                               cause count
       <char>   <int>             <char>                              <char> <num>
    1: Female      NA         All causes                          All causes 34015
    2: Female      NA Infectious disease                    COVID-19 (U07.1)   986
    3: Female      NA Infectious disease Certain other intestinal infections   127
    4: Female      NA Infectious disease                          Septicemia   297
    5: Female      NA Infectious disease                     Viral hepatitis    47
    6: Female      NA Infectious disease                         HIV disease    16
         rate rate_lower rate_upper
        <num>      <num>      <num>
    1: 533.11     527.36     538.91
    2:  15.45      14.49      16.47
    3:   1.99       1.65       2.39
    4:   4.70       4.18       5.29
    5:   0.71       0.52       0.96
    6:   0.25       0.14       0.42

### Infant deaths (ages\< 1)

``` r
# Infant deaths are NOT age standardized because there is only one age
infrates <- rads::age_standardize(
  ph.data = infcombo, 
  my.count = 'deaths', 
  my.pop = 'pop', 
  group_by = c('gender', 'cause', 'cause_category'), 
  per = 1000 # 100,000 except for infant mortality
)

infrates <- infrates[, .(gender, 
                         ranking = NA_integer_, 
                         cause_category, 
                         cause, 
                         count, 
                         rate = crude.rate,
                         rate_lower = crude.lci,
                         rate_upper = crude.uci)]
```

Take a peek at the infant rates …

``` r
head(infrates[count >= 10]) # suppress view
```

       gender ranking cause_category                      cause count  rate
       <char>   <int>         <char>                     <char> <num> <num>
    1: Female      NA     All causes                 All causes   217  3.08
    2: Female      NA          Other     Maternal complications    11  0.16
    3: Female      NA          Other  Complications of placenta    12  0.17
    4: Female      NA          Other            Short gestation    16  0.23
    5: Female      NA          Other Other perinatal conditions    14  0.20
    6: Female      NA          Other   Congenital malformations    70  0.99
       rate_lower rate_upper
            <num>      <num>
    1:       2.69       3.52
    2:       0.08       0.28
    3:       0.09       0.30
    4:       0.13       0.37
    5:       0.11       0.33
    6:       0.78       1.26

## (6) Identify leading causes based on COUNTS (using random ties method)

### Non-infant deaths (ages ≥ 1)

``` r
# Create ranking (minus one, because want 'All causes' to have a rank of zero)
ranking <- rates[, ranking := frank(-count, ties.method = 'dense')-1, .(gender)]

# Keep top 10 rankings
ranking <- ranking[ranking %in% 0:10]

# Sort the data by gender and ranking
setorder(ranking, gender, ranking)
```

View the top three leading causes of death by gender …

``` r
# Infant causes of death
print(ranking[count >= 10, .SD[1:4], gender])
```

       gender ranking  cause_category                  cause count   rate
       <char>   <int>          <char>                 <char> <num>  <num>
    1: Female       0      All causes             All causes 34015 533.11
    2: Female       1 Chronic disease                 Cancer  7160 115.24
    3: Female       2 Chronic disease          Heart disease  6079  92.64
    4: Female       3 Chronic disease    Alzheimer's disease  3145  47.15
    5:   Male       0      All causes             All causes 37058 765.71
    6:   Male       1 Chronic disease          Heart disease  7889 166.16
    7:   Male       2 Chronic disease                 Cancer  7549 152.35
    8:   Male       3 Injury/violence Unintentional injuries  3482  62.30
       rate_lower rate_upper
            <num>      <num>
    1:     527.36     538.91
    2:     112.54     118.00
    3:      90.29      95.05
    4:      45.49      48.86
    5:     757.70     773.80
    6:     162.41     169.99
    7:     148.82     155.94
    8:      60.19      64.48

### Infant deaths (ages\< 1)

``` r
# Create ranking
infranking <- infrates[, ranking := frank(-rate, ties.method = 'dense')-1, .(gender)]

# Keep top 10 rankings
infranking <- infranking[ranking %in% 0:10]

# Sort the data by gender and ranking
setorder(infranking, gender, ranking)
```

View the top three leading causes of death by gender …

``` r
# Infant causes of death
print(infranking[count >= 10, .SD[1:4], gender])
```

       gender ranking cause_category                    cause count  rate
       <char>   <int>         <char>                   <char> <num> <num>
    1: Female       0     All causes               All causes   217  3.08
    2: Female       1          Other Congenital malformations    70  0.99
    3: Female       2          Other                     SIDS    28  0.40
    4: Female       3          Other          Short gestation    16  0.23
    5:   Male       0     All causes               All causes   286  3.90
    6:   Male       1          Other Congenital malformations    63  0.86
    7:   Male       2          Other   Maternal complications    38  0.52
    8:   Male       3          Other          Short gestation    27  0.37
       rate_lower rate_upper
            <num>      <num>
    1:       2.69       3.52
    2:       0.78       1.26
    3:       0.26       0.57
    4:       0.13       0.37
    5:       3.46       4.38
    6:       0.66       1.10
    7:       0.37       0.71
    8:       0.24       0.54

## :star2: Finished!

– *Updated by dcolombara, 2024-03-01*
