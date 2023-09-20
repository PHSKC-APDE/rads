
# Introduction

The [rads package](https://github.com/PHSKC-APDE/rads/) has a suite of tools designed to facilitate and accelerate the analysis of standardized CHARS (Comprehensive Hospital Abstract Reporting System) data. Combining the `rads` functions below with the clean CHARS data on our servers should allow APDE analysts to conduct custom analyses with relative ease. The core `rads` CHARS function are:

-   `get_data_chars()`: easily download standardized CHARS data from SQL into R (2012+)
-   `chars_icd_ccs()`: view available CHARS ICD10-cm and [CCS (HCUP Clinical Classification Software)](https://hcup-us.ahrq.gov/toolssoftware/ccsr/ccs_refined.jsp) descriptions that can be used with `chars_icd_ccs_count()` (2016+ only)
-   `chars_icd_ccs_count()`: generate counts of CHARS hospitalizations using ICD10-cm codes, descriptions, or CCS levels (2016+ only)
-   `chars_injury_matrix()`: view all available intents and mechanisms that can be used with `chars_injury_matrix_count` (2012+)
-   `chars_injury_matrix_count()`: generate counts of injury related hospitalizations by intent and mechanism (2012+)

All of these functions have detailed help files that are accessible by typing `?function_name`, e.g. `?get_data_chars`. Some examples for how to use these functions are given below.

***A few quick notes before we begin ...***

-   CHARS data switched from ICD9-cm to ICD10-cm diagnosis coding in 2016. While `get_data_chars()` can provide you with ICD9-cm data (2012-2015), `chars_icd_ccs()`, `chars_icd_ccs_count()` only work with ICD10-cm hospitalization codes (those used since 2016).
-   If you want to create age-adjusted rates, we recommend you read the [age_standardize](https://github.com/PHSKC-APDE/rads/wiki/age_standardize) and [calculating_rates_with_rads](https://github.com/PHSKC-APDE/rads/wiki/calculating_rates_with_rads) vignettes after working through this one.

# Set up the environment


```r
rm(list=ls())
library(rads)
library(data.table)
```



# get_data_chars()

`get_data_chars()` takes four potential arguments:

-   `cols`: the names of the columns that you want to download. You will dramatically speed up the download time if you only specify the columns of genuine interest. However, calculation of CHARS injuries uses many columns, not just the `DIAG1` column. So, if you are interested in injuries, you might want to tough it out and download all the relevant years of CHARS data.
-   `year`: the year(s) of interest, from 2012 to the present.
-   `kingco`: logical (T\|F) OR 'zip'. True or false specifies whether to limit the download to King County, based on the county of residence variable. If you pass `kingco = 'zip'`, it will download King County, but will define it based on zip codes instead.
-   `wastate`: logical (T\|F). When false, data will include Oregon.
-   `inpatient`: logical (T\|F). When false, data will include observation patients (i.e., outpatients).
-   `deaths`: logical (T\|F). When true, the data will include those who died while in the hospital.
-   `topcode`: logical (T\|F). When true, `chi_age` will be top coded to 100 to match population data top coding.

If you do not specify any of the arguments, you will get all CHARS data columns, for the latest year, for King County (defined by the county of residence indicator), limited to inpatients, including those who died while hospitalized, with deaths top coded to 100.


```r
charsDT <- get_data_chars(year = 2021)
dim(charsDT) # dimensions of the downloaded CHARS data  
```

```
## [1] 149099    141
```

```r
names(charsDT)[1:6] # names of the first 6 columns
```

```
## [1] "zipcode"  "seq_no"   "rec_key"  "staytype" "hospital" "lineno"
```

```r
unique(charsDT$chi_geo_kc) # confirm data is limited to King County
```

```
## [1] "King County"
```

```r
unique(charsDT$chi_year) # check the year
```

```
## [1] 2021
```

```r
max(charsDT$chi_age, na.rm = T) # check top coding
```

```
## [1] 100
```

# chars_icd_ccs()

Do not attempt to manually browse through `chars_icd_ccs()` ... you will lose your mind because it has more than 70,000 rows! Rather, use it to identify the type of non-injury hospitalization of interest. The structure is simple and (hopefully!) self-explanatory. Let's take a look at the first three rows as an example by typing `chars_icd_ccs()[1:3]`:


|icd10cm_code |icd10cm                                            |level1                           |level2               |level3               |
|:------------|:--------------------------------------------------|:--------------------------------|:--------------------|:--------------------|
|A000         |Cholera due to Vibrio cholerae 01, biovar cholerae |Diseases of the digestive system |Intestinal infection |Intestinal infection |
|A001         |Cholera due to Vibrio cholerae 01, biovar eltor    |Diseases of the digestive system |Intestinal infection |Intestinal infection |
|A009         |Cholera, unspecified                               |Diseases of the digestive system |Intestinal infection |Intestinal infection |

Teaching about [regular expression, a.k.a. *regex*](https://stat.ethz.ch/R-manual/R-devel/library/base/html/regex.html) and filtering is outside the bounds of this vignette. However, I imagine you will usually want to use aggregated hospitalization data so I encourage you to look at the unique values of level1, level2, and level3 data. For example, let's examine the CCS Level 1 categories.


```r
  unique(chars_icd_ccs()$level1)
```

```
##  [1] "Diseases of the digestive system"                                                 
##  [2] "Diseases of the nervous system and sense organs"                                  
##  [3] "Infectious and parasitic diseases"                                                
##  [4] "Diseases of the respiratory system"                                               
##  [5] "Diseases of the musculoskeletal system and connective tissue"                     
##  [6] "Diseases of the skin and subcutaneous tissue"                                     
##  [7] "Certain conditions originating in the perinatal period"                           
##  [8] "Complications of pregnancy; childbirth; and the puerperium"                       
##  [9] "Diseases of the circulatory system"                                               
## [10] "Diseases of the genitourinary system"                                             
## [11] "Symptoms; signs; and ill-defined conditions and factors influencing health status"
## [12] "Neoplasms"                                                                        
## [13] "Diseases of the blood and blood-forming organs"                                   
## [14] "Injury and poisoning"                                                             
## [15] "Endocrine; nutritional; and metabolic diseases and immunity disorders"            
## [16] "Congenital anomalies"                                                             
## [17] "Mental Illness"                                                                   
## [18] "Residual codes; unclassified; all E codes"
```

# chars_icd_ccs_count()

`chars_icd_ccs_count()` allows the user to get CHARS counts by ICD10-cm code, ICD10-cm description, or any of the three CCS levels. I provide examples of each of these below, in order of decreasing granularity / specificity using hypertensive heart disease as a case study.


|icd10cm_code |icd10cm                                       |level1                             |level2       |level3                                                     |
|:------------|:---------------------------------------------|:----------------------------------|:------------|:----------------------------------------------------------|
|I110         |Hypertensive heart disease with heart failure |Diseases of the circulatory system |Hypertension |Hypertension with complications and secondary hypertension |

However, before we begin, let's review the eight possible arguments used by `chrs_icd_ccs_count()`:

-   `ph.data`: the name of a person level data.table/data.frame of CHARS data with ICD10-cm codes
-   `icd10cm`: the ICD10-cm code of interest OR its description. Partial strings are allowed.
-   `level1`: the level 1 CCS code of interest. Partial strings are allowed.
-   `level2`: the level 2 CCS code of interest. Partial strings are allowed.
-   `level3`: the level 3 CCS code of interest. Partial strings are allowed.
-   `icdcol`: the name of the column in `ph.data` that contains the ICD10-cm codes. Default is `diag1`, which is provided when you use `get_data_chars()`.
-   `group_by`: identifies the variables by which you want to group (a.k.a., stratify) the results.
-   `kingco`: logical (T\|F) specifying whether to limit the data analysis to King County. Only works if ph.data still has the `chi_geo_kc` column.

## getting CHARS counts by ICD10-cm code


```r
  mycode <- chars_icd_ccs_count(ph.data = charsDT, 
                                icd10cm = 'I110')
```


|icd10cm_desc                                  |hospitalizations |
|:---------------------------------------------|:----------------|
|Hypertensive heart disease with heart failure |1,748            |

## getting CHARS counts by ICD10-cm description


```r
  mydesc <- chars_icd_ccs_count(ph.data = charsDT, 
                                icd10cm = 'hypertensive heart disease')
```


|icd10cm_desc                                                |hospitalizations |
|:-----------------------------------------------------------|:----------------|
|Hypertensive heart disease with heart failure               |1,748            |
|Hypertensive heart disease without heart failure            |3                |
|Pre-existing hypertensive heart disease comp childbirth     |0                |
|Pre-existing hypertensive heart disease comp the puerperium |0                |

Note that, since the strings matches are not case sensitive and find partial matches, the table returned has more than 1 row. If we wanted to return the exact table that we saw with the ICD10-cm code, we would have to specify the full string exactly.


```r
  mydesc <- chars_icd_ccs_count(ph.data = charsDT, 
                                icd10cm = 'hypertensive heart disease with heart failure')
```


|icd10cm_desc                                  |hospitalizations |
|:---------------------------------------------|:----------------|
|Hypertensive heart disease with heart failure |1,748            |

## getting CHARS counts by CCS Level 3 description


```r
  mylevel3 <- chars_icd_ccs_count(ph.data = charsDT, 
                                level3 = 'hypertension with complications and secondary hypertension')
```


|level3_desc                                                |hospitalizations |
|:----------------------------------------------------------|:----------------|
|Hypertension with complications and secondary hypertension |4,801            |

Note that the number of hospitalizations has increased (4,801 \>\> 1,748). This is because the 283 Level 3 CCS categories aggregate many individual causes of hospitalization.

## getting CHARS counts by CCS Level 2 description


```r
  mylevel2 <- chars_icd_ccs_count(ph.data = charsDT, 
                                level2 = 'hypertension')
```


|level2_desc  |hospitalizations |
|:------------|:----------------|
|Hypertension |4,806            |

Since there are even fewer Level 2 CCS categories (136), it further aggregates individual causes of hospitalization.

## getting CHARS counts by CCS Level 2 description


```r
  mylevel1 <- chars_icd_ccs_count(ph.data = charsDT, 
                                level1 = 'diseases of the circulatory system')
```


|level1_desc                        |hospitalizations |
|:----------------------------------|:----------------|
|Diseases of the circulatory system |18,232           |

There are only 18 Level 1 CCS categories, so there is a great deal of aggregation and the substantial increase in hospitalizations should not surprise us.

# chars_injury_matrix()

The `chars_injury_matrix()` function does not take any arguments. Just type it in your console and you'll see the a table of all available pre-specified combinations of mechanisms and intents for injury related hospitalizations. Let's take a look at the top 10 rows with `chars_injury_matrix()[1:10]`.


|mechanism                |intent  |
|:------------------------|:-------|
|adverse_effects          |assault |
|bites_stings             |assault |
|cut_pierce               |assault |
|drowning                 |assault |
|fall                     |assault |
|fire_burn                |assault |
|firearm                  |assault |
|machinery                |assault |
|motor_vehicle_nontraffic |assault |
|motor_vehicle_traffic    |assault |

If you just want to see a list of the available intents, type `unique(chars_injury_matrix()[]$intent)`:


```
## [1] "assault"       "intentional"   "legal"         "other"         "undetermined"  "unintentional"
```

Similarly, to see the available mechanisms, type `unique(chars_injury_matrix()[]$mechanism)`:


```
##  [1] "adverse_effects"          "bites_stings"             "cut_pierce"               "drowning"                 "fall"                    
##  [6] "fire_burn"                "firearm"                  "machinery"                "motor_vehicle_nontraffic" "motor_vehicle_traffic"   
## [11] "mvt_motorcyclist"         "mvt_occupant"             "mvt_other"                "mvt_pedal_cyclist"        "mvt_pedestrian"          
## [16] "mvt_unspecified"          "natural_environmental"    "other_land_transport"     "other_specified"          "other_transport"         
## [21] "overexertion"             "pedal_cyclist"            "pedestrian"               "poisoning"                "poisoning_drug"          
## [26] "poisoning_nondrug"        "struck_by_against"        "suffocation"              "unspecified"
```

# chars_injury_matrix_count()

The `chars_injury_matrix_count()` function is similar to the `chars_icd_ccs_count()` function above, except that it counts injury related hospitalizations. `chars_injury_matrix_count()` takes five potential arguments:

-   `ph.data`: the name of a person level data.table/data.frame of CHARS data downloaded with `get_data_chars()`. Note that the intents and mechanisms are pre-calculated so you will need to ensure `ph.data` has the relevant mechanism\_\* and intent\_\* columns. The easiest way to do this is to have `get_data_chars()` download all the columns.
-   `intent`: the injury intent of interest. Partial strings are allowed.
-   `mechanism`: the injury mechanism of interest. Partial strings are allowed.
-   `group_by`: identifies the variables by which you want to group (a.k.a., stratify) the results.
-   `def`: acceptable values are 'narrow' or 'broad'. It specifies whether you want to use the CDC's recommended 'narrow' approach, which requires that the **principal diagnosis** of an injury hospitalization be a nature-of-injury ICD-10-CM code. Or, alternatively, the 'broad' definition that searches all available diagnosis fields on the hospital discharge record. See [this document](https://kc1.sharepoint.com/teams/DPH-APDEData/Shared%20Documents/Forms/AllItems.aspx?id=%2Fteams%2FDPH%2DAPDEData%2FShared%20Documents%2FCHARS%2FAPDE%20injury%20hospitalization%20method%5F2023%2Epdf&parent=%2Fteams%2FDPH%2DAPDEData%2FShared%20Documents%2FCHARS) for details.
-   `primary_ecode`: logical (T\|F) specifying whether to limit the analysis to using just the primary ecode (i.e., the `injury_ecode` variable), rather than all available ecodes. The vast majority of the time you will want to keep the default setting.
-   `kingco`: logical (T\|F) specifying whether to limit the data analysis to King County. Only works if ph.data still has the `chi_geo_kc` column.

## Specifying a single intent and ignoring the mechanism


```r
  mat1 <- chars_injury_matrix_count(ph.data = charsDT, 
                              intent = 'assault', 
                              mechanism = 'none')
```


|mechanism     |intent  | hospitalizations|
|:-------------|:-------|----------------:|
|Any mechanism |assault |              278|

## Specifying more than one intent and ignoring the mechanism


```r
  mat2 <- chars_injury_matrix_count(ph.data = charsDT, 
                              intent = 'assault|undetermined', 
                              mechanism = 'none')
```


|mechanism     |intent       | hospitalizations|
|:-------------|:------------|----------------:|
|Any mechanism |assault      |              278|
|Any mechanism |undetermined |               52|

Note that you can also specify more than one intent or mechanism using a vector with separated values.


```r
  mat2.alt <- chars_injury_matrix_count(ph.data = charsDT, 
                                        intent = c('assault', 'undetermined'), 
                                        mechanism = 'none')
  identical(mat2, mat2.alt)
```

```
## [1] TRUE
```

## Specifying a single mechanism and ignoring the intent


```r
  mat3 <- chars_injury_matrix_count(ph.data = charsDT, 
                              intent = 'none', 
                              mechanism = 'motor_vehicle_traffic')
```


|mechanism             |intent     | hospitalizations|
|:---------------------|:----------|----------------:|
|motor_vehicle_traffic |Any intent |              756|

## What happens if you specify 'none' for both the mechanism and intent?

You get hospitalizations due to any injury.


```r
  mat4 <- chars_injury_matrix_count(ph.data = charsDT, 
                              intent = 'none', 
                              mechanism = 'none')
```


|mechanism     |intent     |hospitalizations |
|:-------------|:----------|:----------------|
|Any mechanism |Any intent |7,462            |

## What happens if you don't specify the mechanism and intent?

You get every possible combination of mechanism and intent. Let's look at just the top 10 for convenience.


```r
  mat5 <- chars_injury_matrix_count(ph.data = charsDT)[1:10]
```


|mechanism       |intent        |hospitalizations |
|:---------------|:-------------|:----------------|
|Any mechanism   |Any intent    |7,462            |
|Any mechanism   |assault       |278              |
|Any mechanism   |intentional   |572              |
|Any mechanism   |legal         |7                |
|Any mechanism   |other         |0                |
|Any mechanism   |undetermined  |52               |
|Any mechanism   |unintentional |6,553            |
|adverse_effects |Any intent    |0                |
|adverse_effects |assault       |0                |
|adverse_effects |intentional   |0                |

## How different are the `narrow` and `broad` definitions?


```r
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


|def    |mechanism     |intent     |hospitalizations |
|:------|:-------------|:----------|:----------------|
|narrow |Any mechanism |Any intent |7,462            |
|broad  |Any mechanism |Any intent |11,618           |

These tables show that there is a huge difference in the number of hospitalizations, dependent upon the definition that you use. Unless you have a specific rationale for changing it, please use the default in your analyses (i.e., `def = 'narrow'`).

# Conclusion

We know this was a lot to process. The good news is that this vignette isn't going anywhere. If you remember (a) that this vignette exists and (b) where to find it, you'll be in good shape to take on standard CHARS analyses in the future.

If you've read through this vignette and the corresponding help files and are still confused, please feel free to reach out for assistance. You may have found a bug, who knows? Good luck!
