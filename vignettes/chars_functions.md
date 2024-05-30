# CHARS Functions

# Introduction

The [rads package](https://github.com/PHSKC-APDE/rads/) has a suite of
tools designed to facilitate and accelerate the analysis of standardized
CHARS (Comprehensive Hospital Abstract Reporting System) data. Combining
the `rads` functions below with the clean CHARS data on our servers
should allow APDE analysts to conduct custom analyses with relative
ease. The core `rads` CHARS function are:

-   `get_data_chars()`: easily download standardized CHARS data from SQL
    into R (2012+)
-   `chars_icd_ccs()`: view available CHARS ICD-9-CM and ICD-10-CM
    descriptions as well as ‘superlevel’, ‘broad’, ‘midlevel’, and
    ‘detailed’ aggregations derived from [AHRQ’s HCUP
    CCSR](https://hcup-us.ahrq.gov/toolssoftware/ccsr/ccs_refined.jsp)
    that can be used with `chars_icd_ccs_count()`
-   `chars_icd_ccs_count()`: generate counts of CHARS hospitalizations
    using ICD-9-CM or ICD-10-CM descriptions or ‘superlevel’, ‘broad’,
    ‘midlevel’, and ‘detailed’ categories.
-   `chars_injury_matrix()`: view all available intents and mechanisms
    that can be used with `chars_injury_matrix_count` (2012+)
-   `chars_injury_matrix_count()`: generate counts of injury related
    hospitalizations by intent and mechanism (2012+)

All of these functions have detailed help files that are accessible by
typing `?function_name`, e.g. `?get_data_chars`. Some examples for how
to use these functions are given below.

***A few quick notes before we begin …***

-   `get_data_chars()` can provide you with ICD-9-CM data (2012-2015) as
    well as ICD-10-CM data (2016+).
-   `chars_injury_matrix()` and `chars_injury_matrix_count()` are
    agnostic as to whether the underlying data are ICD-9-CM or
    ICD-10-CM.
-   `chars_icd_ccs()` & `chars_icd_ccs_count()` need you to specify
    which ICD-CM version you have in your data. This means you can
    analyse 2012-2015 data or 2016+ data, but not both at the same time
    in a single command.
-   If you want to create age-adjusted rates, we recommend you read the
    [age_standardize](https://github.com/PHSKC-APDE/rads/wiki/age_standardize)
    and
    [calculating_rates_with_rads](https://github.com/PHSKC-APDE/rads/wiki/calculating_rates_with_rads)
    vignettes after working through this one.

# Set up the environment

``` r
rm(list=ls())
library(rads)
library(data.table)
```

# get_data_chars()

`get_data_chars()` takes four potential arguments:

-   `cols`: the names of the columns that you want to download. You will
    dramatically speed up the download time if you only specify the
    columns of genuine interest. However, calculation of CHARS injuries
    uses many columns, not just the `DIAG1` column. So, if you are
    interested in injuries, you might want to tough it out and download
    all the relevant years of CHARS data.
-   `year`: the year(s) of interest, from 2012 to the present.
-   `kingco`: logical (T|F) OR ‘zip’. True or false specifies whether to
    limit the download to King County, based on the county of residence
    variable. If you pass `kingco = 'zip'`, it will download King
    County, but will define it based on zip codes instead.
-   `wastate`: logical (T|F). When false, data will include Oregon.
-   `inpatient`: logical (T|F). When false, data will include
    observation patients (i.e., outpatients).
-   `deaths`: logical (T|F). When true, the data will include those who
    died while in the hospital.
-   `topcode`: logical (T|F). When true, `chi_age` will be top coded to
    100 to match population data top coding.

If you do not specify any of the arguments, you will get all CHARS data
columns, for the latest year, for King County (defined by the county of
residence indicator), limited to inpatients, including those who died
while hospitalized, with deaths top coded to 100.

``` r
charsDT <- get_data_chars(year = 2021)
dim(charsDT) # dimensions of the downloaded CHARS data  
```

    [1] 149099    146

``` r
names(charsDT)[1:6] # names of the first 6 columns
```

    [1] "seq_no"   "rec_key"  "staytype" "hospital" "lineno"   "zipcode" 

``` r
unique(charsDT$chi_geo_kc) # confirm data is limited to King County
```

    [1] "King County"

``` r
unique(charsDT$chi_year) # check the year
```

    [1] 2021

``` r
max(charsDT$chi_age, na.rm = T) # check top coding
```

    [1] 100

# chars_icd_ccs()

`chars_icd_ccs()` takes three arguments:

-   `ref_typ`: specifies the hospital diagnosis descriptions that are of
    interest to you. Acceptable options include: ‘all’, ‘icdcm’,
    ‘superlevel’, ‘broad’, ‘midlevel’, & ‘detailed’.
-   `mykey`: Identifies the keyring:: service that you use to access the
    Health & Human Services Analytic Workspace (HHSAW). To see the
    keyring:: services you currently have in your R installation, type
    `keyring::key_list()`.
-   `icdcm_version`: specifies the ICD-CM version that you want to
    reference. Acceptable options include: 9 & 10, with 10 being the
    default.

Do not attempt to manually browse through `chars_icd_ccs()` … you will
lose your mind because it has more than 100,000 rows! Rather, use it to
identify the type of non-injury hospitalization of interest. The
structure is simple and (hopefully!) self-explanatory. Let’s take a look
at the first three rows as an example by typing `chars_icd_ccs()[1:3]`:

<table style="width:100%;">
<colgroup>
<col style="width: 6%" />
<col style="width: 29%" />
<col style="width: 11%" />
<col style="width: 19%" />
<col style="width: 12%" />
<col style="width: 12%" />
<col style="width: 8%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">icdcm_code</th>
<th style="text-align: left;">icdcm</th>
<th style="text-align: left;">superlevel</th>
<th style="text-align: left;">broad</th>
<th style="text-align: left;">midlevel</th>
<th style="text-align: left;">detailed</th>
<th style="text-align: right;">icdcm_version</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">A00</td>
<td style="text-align: left;">Cholera</td>
<td style="text-align: left;">Infectious diseases</td>
<td style="text-align: left;">Diseases of the digestive system</td>
<td style="text-align: left;">Intestinal infection</td>
<td style="text-align: left;">Intestinal infection</td>
<td style="text-align: right;">10</td>
</tr>
<tr class="even">
<td style="text-align: left;">A000</td>
<td style="text-align: left;">Cholera due to Vibrio cholerae 01, biovar
cholerae</td>
<td style="text-align: left;">Infectious diseases</td>
<td style="text-align: left;">Diseases of the digestive system</td>
<td style="text-align: left;">Intestinal infection</td>
<td style="text-align: left;">Intestinal infection</td>
<td style="text-align: right;">10</td>
</tr>
<tr class="odd">
<td style="text-align: left;">A001</td>
<td style="text-align: left;">Cholera due to Vibrio cholerae 01, biovar
eltor</td>
<td style="text-align: left;">Infectious diseases</td>
<td style="text-align: left;">Diseases of the digestive system</td>
<td style="text-align: left;">Intestinal infection</td>
<td style="text-align: left;">Intestinal infection</td>
<td style="text-align: right;">10</td>
</tr>
</tbody>
</table>

Teaching about [regular expression, a.k.a.
*regex*](https://stat.ethz.ch/R-manual/R-devel/library/base/html/regex.html)
and filtering is outside the bounds of this vignette. However, I imagine
you will usually want to use aggregated hospitalization data so I
encourage you to look at the unique values of superlevel, broad,
midlevel, and detailed data. For example, let’s examine the CCSR broad
categories with `chars_icd_ccs(ref_type = 'broad')`:

<table>
<colgroup>
<col style="width: 87%" />
<col style="width: 12%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">broad</th>
<th style="text-align: right;">icdcm_version</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Diseases of the digestive system</td>
<td style="text-align: right;">10</td>
</tr>
<tr class="even">
<td style="text-align: left;">Certain infectious and parasitic
diseases</td>
<td style="text-align: right;">10</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Diseases of the genitourinary system</td>
<td style="text-align: right;">10</td>
</tr>
<tr class="even">
<td style="text-align: left;">Diseases of the eye and adnexa</td>
<td style="text-align: right;">10</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Diseases of the ear and mastoid
process</td>
<td style="text-align: right;">10</td>
</tr>
<tr class="even">
<td style="text-align: left;">Endocrine, nutritional and metabolic
diseases</td>
<td style="text-align: right;">10</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Diseases of the circulatory system</td>
<td style="text-align: right;">10</td>
</tr>
<tr class="even">
<td style="text-align: left;">Diseases of the blood and blood-forming
organs and certain disorders involving the immune mechanism</td>
<td style="text-align: right;">10</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Dental diseases</td>
<td style="text-align: right;">10</td>
</tr>
<tr class="even">
<td style="text-align: left;">Neoplasms</td>
<td style="text-align: right;">10</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Diseases of the nervous system</td>
<td style="text-align: right;">10</td>
</tr>
<tr class="even">
<td style="text-align: left;">Mental, behavioral and neurodevelopmental
disorders</td>
<td style="text-align: right;">10</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Factors influencing health status and
contact with health services</td>
<td style="text-align: right;">10</td>
</tr>
<tr class="even">
<td style="text-align: left;">Injury, poisoning and certain other
consequences of external causes</td>
<td style="text-align: right;">10</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Diseases of the musculoskeletal system and
connective tissue</td>
<td style="text-align: right;">10</td>
</tr>
<tr class="even">
<td style="text-align: left;">Diseases of the respiratory system</td>
<td style="text-align: right;">10</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Diseases of the skin and subcutaneous
tissue</td>
<td style="text-align: right;">10</td>
</tr>
<tr class="even">
<td style="text-align: left;">Symptoms, signs and abnormal clinical and
laboratory findings, not elsewhere classified</td>
<td style="text-align: right;">10</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Pregnancy, childbirth and the
puerperium</td>
<td style="text-align: right;">10</td>
</tr>
<tr class="even">
<td style="text-align: left;">Certain conditions originating in the
perinatal period</td>
<td style="text-align: right;">10</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Congenital malformations, deformations and
chromosomal abnormalities</td>
<td style="text-align: right;">10</td>
</tr>
<tr class="even">
<td style="text-align: left;">External causes of morbidity</td>
<td style="text-align: right;">10</td>
</tr>
</tbody>
</table>

# chars_icd_ccs_count()

`chars_icd_ccs_count()` allows the user to get CHARS counts by ICD-CM
code, ICD-cm description, or the superlevel, broad, midlevel, and
detailed categories. I provide examples of each of these below, in order
of decreasing granularity / specificity using hypertensive heart disease
as a case study.

<table style="width:100%;">
<colgroup>
<col style="width: 7%" />
<col style="width: 30%" />
<col style="width: 11%" />
<col style="width: 23%" />
<col style="width: 8%" />
<col style="width: 8%" />
<col style="width: 9%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">icdcm_code</th>
<th style="text-align: left;">icdcm</th>
<th style="text-align: left;">superlevel</th>
<th style="text-align: left;">broad</th>
<th style="text-align: left;">midlevel</th>
<th style="text-align: left;">detailed</th>
<th style="text-align: right;">icdcm_version</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">I110</td>
<td style="text-align: left;">Hypertensive heart disease with heart
failure</td>
<td style="text-align: left;">Chronic diseases</td>
<td style="text-align: left;">Diseases of the circulatory system</td>
<td style="text-align: left;">Hypertension</td>
<td style="text-align: left;">Hypertension</td>
<td style="text-align: right;">10</td>
</tr>
</tbody>
</table>

However, before we begin, let’s review the possible arguments used by
`chrs_icd_ccs_count()`:

-   `ph.data`: the name of a person level data.table/data.frame of CHARS
    data with ICD10-cm codes
-   `icdcm_version`: specifies the ICD-CM version that you want to
    reference. Acceptable options include: 9 & 10, with 10 being the
    default.
-   `icdcm`: the ICD-CM code of interest OR its description. It is case
    insensivitive and partial strings are allowed.
-   `superlevel`: ‘superlevel’ level descriptions that are of interest.
    Case insensivitive and partial strings are allowed.
-   `broad`: CCSR derived ‘broad’ level descriptions that are of
    interest. Case insensivitive and partial strings are allowed.
-   `midlevel`: ’midlevel level descriptions that are of interest. Case
    insensivitive and partial strings are allowed.
-   `detailed`: CCSR derived ‘detailed’ level descriptions that are of
    interest. Case insensivitive and partial strings are allowed.
-   `icdcol`: the name of the column in `ph.data` that contains the
    ICD10-cm codes. Default is `diag1`, which is provided when you use
    `get_data_chars()`.
-   `group_by`: identifies the variables by which you want to group
    (a.k.a., stratify) the results.
-   `kingco`: logical (T|F) specifying whether to limit the data
    analysis to King County. Only works if ph.data still has the
    `chi_geo_kc` column.
-   `mykey`: Identifies the keyring:: service that you use to access the
    Health & Human Services Analytic Workspace (HHSAW). To see the
    keyring:: services you currently have in your R installation, type
    `keyring::key_list()`.

## getting CHARS counts by ICD-10-CM code

``` r
  mycode <- chars_icd_ccs_count(ph.data = charsDT, 
                                icdcm = 'I110')
```

<table>
<thead>
<tr class="header">
<th style="text-align: left;">icdcm_desc</th>
<th style="text-align: left;">hospitalizations</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Hypertensive heart disease with heart
failure</td>
<td style="text-align: left;">1,748</td>
</tr>
</tbody>
</table>

## getting CHARS counts by ICD-10-CM description

``` r
  mydesc <- chars_icd_ccs_count(ph.data = charsDT, 
                                icdcm = 'hypertensive heart disease')
```

<table>
<colgroup>
<col style="width: 77%" />
<col style="width: 22%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">icdcm_desc</th>
<th style="text-align: left;">hospitalizations</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Hypertensive heart disease</td>
<td style="text-align: left;">0</td>
</tr>
<tr class="even">
<td style="text-align: left;">Hypertensive heart disease with heart
failure</td>
<td style="text-align: left;">1,748</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Hypertensive heart disease without heart
failure</td>
<td style="text-align: left;">3</td>
</tr>
<tr class="even">
<td style="text-align: left;">Pre-existing hypertensive heart disease
compl preg/chldbrth</td>
<td style="text-align: left;">0</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Pre-existing hypertensive heart disease
comp pregnancy</td>
<td style="text-align: left;">0</td>
</tr>
<tr class="even">
<td style="text-align: left;">Pre-existing hypertensive heart disease
comp childbirth</td>
<td style="text-align: left;">0</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Pre-existing hypertensive heart disease
comp the puerperium</td>
<td style="text-align: left;">0</td>
</tr>
</tbody>
</table>

Note that, since the string matches are not case sensitive and find
partial matches, the table returned has more than 1 row. If we wanted to
return the exact table that we saw with the ICD10-cm code, we would have
to specify the full string exactly.

``` r
  mydesc <- chars_icd_ccs_count(ph.data = charsDT, 
                                icdcm = 'hypertensive heart disease with heart failure')
```

<table>
<thead>
<tr class="header">
<th style="text-align: left;">icdcm_desc</th>
<th style="text-align: left;">hospitalizations</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Hypertensive heart disease with heart
failure</td>
<td style="text-align: left;">1,748</td>
</tr>
</tbody>
</table>

## getting CHARS counts by CCSR detailed description

``` r
  detailed <- chars_icd_ccs_count(ph.data = charsDT, 
                                detailed = '^hypertension$')
```

<table>
<thead>
<tr class="header">
<th style="text-align: left;">detailed_desc</th>
<th style="text-align: left;">hospitalizations</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Hypertension</td>
<td style="text-align: left;">5,908</td>
</tr>
</tbody>
</table>

Note that the number of hospitalizations has increased (5,908 \>\>
1,748). This is because there are 74 values of `icdcm_code` when
`chars_icd_ccs()[detailed == 'Hypertension']`. In other words, it
aggregates icdcm = ‘I110’ along with 73 other icdcm codes.

## getting CHARS counts by midlevel description

``` r
  midlevel <- chars_icd_ccs_count(ph.data = charsDT, 
                                  midlevel = '^hypertension$')
```

<table>
<thead>
<tr class="header">
<th style="text-align: left;">midlevel_desc</th>
<th style="text-align: left;">hospitalizations</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Hypertension</td>
<td style="text-align: left;">6,233</td>
</tr>
</tbody>
</table>

When using the midlevel aggregation, we see that our count increased yet
again (6,233 \> 5,908). This is because there are 2 detailed level
aggregations that are combined into this single midlevel value. We can
see this when we type
`unique(chars_icd_ccs()[midlevel == 'Hypertension']$detailed)`, which
returns:

    [1] "Hypertension"           "Essential hypertension"

## getting CHARS counts by CCSR broad description

``` r
  broad <- chars_icd_ccs_count(ph.data = charsDT, 
                                broad = 'Diseases of the circulatory system')
```

<table>
<thead>
<tr class="header">
<th style="text-align: left;">broad_desc</th>
<th style="text-align: left;">hospitalizations</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Diseases of the circulatory system</td>
<td style="text-align: left;">20,917</td>
</tr>
</tbody>
</table>

Since there are even fewer CCSR broad categories (22), it further
aggregates individual causes of hospitalization.

## getting CHARS counts by superlevel description

``` r
  superlevel <- chars_icd_ccs_count(ph.data = charsDT, 
                                    superlevel = '^chronic diseases$')
```

<table>
<thead>
<tr class="header">
<th style="text-align: left;">superlevel_desc</th>
<th style="text-align: left;">hospitalizations</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Chronic diseases</td>
<td style="text-align: left;">55,349</td>
</tr>
</tbody>
</table>

Again, there are even fewer superlevel categories, so it further
aggregates individual causes of hospitalization.

# chars_injury_matrix()

The `chars_injury_matrix()` function does not take any arguments. Just
type it in your console and you’ll see the a table of all available
pre-specified combinations of mechanisms and intents for injury related
hospitalizations. Let’s take a look at the top 10 rows with
`chars_injury_matrix()[1:10]`.

<table>
<thead>
<tr class="header">
<th style="text-align: left;">mechanism</th>
<th style="text-align: left;">intent</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">any</td>
<td style="text-align: left;">assault</td>
</tr>
<tr class="even">
<td style="text-align: left;">bites_stings</td>
<td style="text-align: left;">assault</td>
</tr>
<tr class="odd">
<td style="text-align: left;">cut_pierce</td>
<td style="text-align: left;">assault</td>
</tr>
<tr class="even">
<td style="text-align: left;">drowning</td>
<td style="text-align: left;">assault</td>
</tr>
<tr class="odd">
<td style="text-align: left;">fall</td>
<td style="text-align: left;">assault</td>
</tr>
<tr class="even">
<td style="text-align: left;">fire_burn</td>
<td style="text-align: left;">assault</td>
</tr>
<tr class="odd">
<td style="text-align: left;">firearm</td>
<td style="text-align: left;">assault</td>
</tr>
<tr class="even">
<td style="text-align: left;">machinery</td>
<td style="text-align: left;">assault</td>
</tr>
<tr class="odd">
<td style="text-align: left;">motor_vehicle_nontraffic</td>
<td style="text-align: left;">assault</td>
</tr>
<tr class="even">
<td style="text-align: left;">motor_vehicle_traffic</td>
<td style="text-align: left;">assault</td>
</tr>
</tbody>
</table>

If you just want to see a list of the available intents, type
`unique(chars_injury_matrix()[]$intent)`:

    [1] "any"           "assault"       "intentional"   "legal"        
    [5] "undetermined"  "unintentional"

Similarly, to see the available mechanisms, type
`unique(chars_injury_matrix()[]$mechanism)`:

     [1] "any"                      "bites_stings"            
     [3] "cut_pierce"               "drowning"                
     [5] "fall"                     "fire_burn"               
     [7] "firearm"                  "machinery"               
     [9] "motor_vehicle_nontraffic" "motor_vehicle_traffic"   
    [11] "mvt_motorcyclist"         "mvt_occupant"            
    [13] "mvt_other"                "mvt_pedal_cyclist"       
    [15] "mvt_pedestrian"           "mvt_unspecified"         
    [17] "natural_environmental"    "other_land_transport"    
    [19] "other_specified"          "other_transport"         
    [21] "overexertion"             "pedal_cyclist"           
    [23] "pedestrian"               "poisoning"               
    [25] "poisoning_drug"           "poisoning_nondrug"       
    [27] "struck_by_against"        "suffocation"             
    [29] "unspecified"             

# chars_injury_matrix_count()

The `chars_injury_matrix_count()` function is similar to the
`chars_icd_ccs_count()` function above, except that it counts injury
related hospitalizations. `chars_injury_matrix_count()` takes seven
potential arguments:

-   `ph.data`: the name of a person level data.table/data.frame of CHARS
    data downloaded with `get_data_chars()`. Note that the intents and
    mechanisms are pre-calculated so you will need to ensure `ph.data`
    has the relevant mechanism\_\* and intent\_\* columns. The easiest
    way to do this is to have `get_data_chars()` download all the
    columns.
-   `intent`: the injury intent of interest. Partial strings are
    allowed.
-   `mechanism`: the injury mechanism of interest. Partial strings are
    allowed.
-   `group_by`: identifies the variables by which you want to group
    (a.k.a., stratify) the results.
-   `def`: acceptable values are ‘narrow’ or ‘broad’. It specifies
    whether you want to use the CDC’s recommended ‘narrow’ approach,
    which requires that the **principal diagnosis** of an injury
    hospitalization be a nature-of-injury ICD-10-CM code. Or,
    alternatively, the ‘broad’ definition that searches all available
    diagnosis fields on the hospital discharge record. See [this
    document](https://kc1.sharepoint.com/teams/DPH-APDEData/Shared%20Documents/Forms/AllItems.aspx?id=%2Fteams%2FDPH%2DAPDEData%2FShared%20Documents%2FCHARS%2FAPDE%20injury%20hospitalization%20method%5F2023%2Epdf&parent=%2Fteams%2FDPH%2DAPDEData%2FShared%20Documents%2FCHARS)
    for details.
-   `primary_ecode`: logical (T|F) specifying whether to limit the
    analysis to using just the primary ecode (i.e., the `injury_ecode`
    variable), rather than all available ecodes. The vast majority of
    the time you will want to keep the default setting.
-   `kingco`: logical (T|F) specifying whether to limit the data
    analysis to King County. Only works if ph.data still has the
    `chi_geo_kc` column.

## Specifying a single intent and ignoring the mechanism

``` r
  mat1 <- chars_injury_matrix_count(ph.data = charsDT, 
                              intent = 'assault', 
                              mechanism = 'none')
```

    Column names for 'chars' data are taken from all available years.
    Column names for 'chars' data are taken from all available years.

<table>
<thead>
<tr class="header">
<th style="text-align: left;">mechanism</th>
<th style="text-align: left;">intent</th>
<th style="text-align: right;">hospitalizations</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Any mechanism</td>
<td style="text-align: left;">assault</td>
<td style="text-align: right;">278</td>
</tr>
</tbody>
</table>

## Specifying more than one intent and ignoring the mechanism

``` r
  mat2 <- chars_injury_matrix_count(ph.data = charsDT, 
                              intent = 'assault|undetermined', 
                              mechanism = 'none')
```

<table>
<thead>
<tr class="header">
<th style="text-align: left;">mechanism</th>
<th style="text-align: left;">intent</th>
<th style="text-align: right;">hospitalizations</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Any mechanism</td>
<td style="text-align: left;">assault</td>
<td style="text-align: right;">278</td>
</tr>
<tr class="even">
<td style="text-align: left;">Any mechanism</td>
<td style="text-align: left;">undetermined</td>
<td style="text-align: right;">52</td>
</tr>
</tbody>
</table>

Note that you can also specify more than one intent or mechanism using a
vector with separated values.

``` r
  mat2.alt <- chars_injury_matrix_count(ph.data = charsDT, 
                                        intent = c('assault', 'undetermined'), 
                                        mechanism = 'none')
  identical(mat2, mat2.alt)
```

    [1] TRUE

## Specifying a single mechanism and ignoring the intent

``` r
  mat3 <- chars_injury_matrix_count(ph.data = charsDT, 
                              intent = 'none', 
                              mechanism = 'motor_vehicle_traffic')
```

<table>
<thead>
<tr class="header">
<th style="text-align: left;">mechanism</th>
<th style="text-align: left;">intent</th>
<th style="text-align: right;">hospitalizations</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">motor_vehicle_traffic</td>
<td style="text-align: left;">Any intent</td>
<td style="text-align: right;">756</td>
</tr>
</tbody>
</table>

## What happens if you specify ‘none’ for both the mechanism and intent?

You get hospitalizations due to any injury.

``` r
  mat4 <- chars_injury_matrix_count(ph.data = charsDT, 
                              intent = 'none', 
                              mechanism = 'none')
```

<table>
<thead>
<tr class="header">
<th style="text-align: left;">mechanism</th>
<th style="text-align: left;">intent</th>
<th style="text-align: left;">hospitalizations</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Any mechanism</td>
<td style="text-align: left;">Any intent</td>
<td style="text-align: left;">7,462</td>
</tr>
</tbody>
</table>

## What happens if you don’t specify the mechanism and intent?

You get every possible combination of mechanism and intent. Let’s look
at just the top 10 for convenience.

``` r
  mat5 <- chars_injury_matrix_count(ph.data = charsDT)[1:10]
```

<table>
<thead>
<tr class="header">
<th style="text-align: left;">mechanism</th>
<th style="text-align: left;">intent</th>
<th style="text-align: left;">hospitalizations</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Any mechanism</td>
<td style="text-align: left;">Any intent</td>
<td style="text-align: left;">7,462</td>
</tr>
<tr class="even">
<td style="text-align: left;">Any mechanism</td>
<td style="text-align: left;">assault</td>
<td style="text-align: left;">278</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Any mechanism</td>
<td style="text-align: left;">intentional</td>
<td style="text-align: left;">572</td>
</tr>
<tr class="even">
<td style="text-align: left;">Any mechanism</td>
<td style="text-align: left;">legal</td>
<td style="text-align: left;">7</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Any mechanism</td>
<td style="text-align: left;">undetermined</td>
<td style="text-align: left;">52</td>
</tr>
<tr class="even">
<td style="text-align: left;">Any mechanism</td>
<td style="text-align: left;">unintentional</td>
<td style="text-align: left;">6,553</td>
</tr>
<tr class="odd">
<td style="text-align: left;">bites_stings</td>
<td style="text-align: left;">Any intent</td>
<td style="text-align: left;">25</td>
</tr>
<tr class="even">
<td style="text-align: left;">bites_stings</td>
<td style="text-align: left;">assault</td>
<td style="text-align: left;">0</td>
</tr>
<tr class="odd">
<td style="text-align: left;">bites_stings</td>
<td style="text-align: left;">intentional</td>
<td style="text-align: left;">0</td>
</tr>
<tr class="even">
<td style="text-align: left;">bites_stings</td>
<td style="text-align: left;">legal</td>
<td style="text-align: left;">0</td>
</tr>
</tbody>
</table>

## How different are the `narrow` and `broad` definitions?

``` r
  mat6 <- chars_injury_matrix_count(ph.data = charsDT, 
                              intent = 'none', 
                              mechanism = 'none', 
                              def = 'narrow')

  mat7 <- chars_injury_matrix_count(ph.data = charsDT, 
                              intent = 'none', 
                              mechanism = 'none', 
                              def = 'broad')
  
  deftable <- rbind(cbind(def = 'narrow', mat6),
                    cbind(def = 'broad', mat7))
```

<table>
<thead>
<tr class="header">
<th style="text-align: left;">def</th>
<th style="text-align: left;">mechanism</th>
<th style="text-align: left;">intent</th>
<th style="text-align: left;">hospitalizations</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">narrow</td>
<td style="text-align: left;">Any mechanism</td>
<td style="text-align: left;">Any intent</td>
<td style="text-align: left;">7,462</td>
</tr>
<tr class="even">
<td style="text-align: left;">broad</td>
<td style="text-align: left;">Any mechanism</td>
<td style="text-align: left;">Any intent</td>
<td style="text-align: left;">11,618</td>
</tr>
</tbody>
</table>

These tables show that there is a huge difference in the number of
hospitalizations, dependent upon the definition that you use. Unless you
have a specific rationale for changing it, please use the default in
your analyses (i.e., `def = 'narrow'`).

# Conclusion

We know this was a lot to process. The good news is that this vignette
isn’t going anywhere. If you remember (a) that this vignette exists and
(b) where to find it, you’ll be in good shape to take on standard CHARS
analyses in the future.

If you’ve read through this vignette and the corresponding help files
and are still confused, please feel free to reach out for assistance.
You may have found a bug, who knows? Good luck!
