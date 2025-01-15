# Creating Reproducible Examples

When you're stuck on a coding problem and need help, following these guidelines will help us understand and solve your issue more quickly. Plus, you might even solve it yourself while putting together the example! Here's what we need to help you effectively.

## Help Us Help You

* **Problem Synopsis**: What exactly is the problem? 
  * If possible, please copy and paste the actual error message.

* **Attempted Solutions**: What have you already tried to fix this problem?

* **Expected Output**: What exactly did you expect as the output? A list? A data.frame? A vector? A new column in a data.frame? etc. 

* **Minimal Code**: Include only the code necessary to reproduce the issue. While you may have a complex pipeline, isolate the specific components related to the problem. This makes it easier for others to understand and solve the issue quickly.

* **Data Access**: Since we share access to common datasets, you can reference the actual data you're using. However, include the complete pipeline from data loading to the problematic code, ensuring paths are referenced correctly (see below). 
  * Note that this is different from submitting an issue outside of PHSKC. In that case you should also strive for a minimal dataset and might consider using [reprex](https://reprex.tidyverse.org/) or [datapasta](https://milesmcbain.github.io/datapasta/index.html).

* **File Paths**: Think carefully whether others can use your filepaths:
  * Remove all references to your `C:\` drive
  * Use the full file path instead of any mappings (e.g., `\\dphcifs\APDE-CDIP\Shapefiles` rather than `A:\Shapefiles`)

* **Environment Information**: Always include:
  * R version
  * Package versions for relevant dependencies
  * Random seeds if your code involves stochastic processes

## Benefits of Creating Minimal Examples

* **Faster Problem Resolution**: By providing focused code, others can quickly understand and address the issue
* **Self-Troubleshooting**: Often, the process of creating a minimal example helps identify the problem's source
* **Skill Development**: Learning to isolate issues and create minimal examples improves your debugging and programming skills

## Examples

### :cry: Bad Example
```r
# Can you help with the following code? 
# It doesn't work. I don't know why. 

# Loading multiple libraries
library(rads)
library(data.table)
library(dplyr)
library(ggplot2)
library(purrr)
library(plotly)
library(stringr)
library(jsonlite)
library(DBI)

# Reading from hard-coded path
dt <- fread("A:/ACS/PUMS_data/2023_1_year/prepped_R_files/2023_1_year_person.rds")

# Multiple data manipulation steps
dt[agep >= 5, chinese := 0][chinese==0 & lanp %in% c(1970, 2000, 2030, 2050) , chinese := 1]
dt[agep >= 5, korean := 0][korean==0 & lanp == 2575, korean := 1]
dt[agep >= 5, vietn := 0][vietn==0 & lanp == 1960, vietn := 1]
dt[agep >= 5, tagalog := 0][tagalog==0 & lanp %in% c(2910, 2920, 3190), tagalog := 1]
dt[agep >= 5, spanish := 0][spanish==0 & lanp == 1200, spanish := 1]
dt[agep >= 5, unspeclang := 0][unspeclang==0 & lanp == 9999, unspeclang := 1]
dt[cit %in% c(1:3), citizenship := 1]
dt[cit %in% c(4), citizenship := 2]
dt[cit %in% c(5), citizenship := 3]
dt[, citizenship := as.character(factor(citizenship, levels = 1:3, labels = c("US-born", "Foreign-born, naturalized", "Non-citizen")))]
dt[esr %in% c(1, 2, 4, 5), employ := 'Employed']
dt[esr %in% c(6), employ := 'Not in labor force']
dt[esr %in% c(3), employ := 'Unemployed']
dt[agep >= 16, employ_binary := 0][esr %in% c(1, 2, 4, 5), employ_binary := 1]
# ... (150 more lines of data preparation)

# The actual issue is here somewhere, buried under unnecessary code
# exploration #1 ... didn't work for mysterious reasons
for(myvar in c("vietn", "unspeclang", "uimale1864", "uige400", "uifem1864", "ui1864",
            "tagalog", "state", "spanish", "region", "pwgtp", "povu5",
            "povu18", "povold", "pov75", "pov74", "pov200", "pov100", "pooreng",
            "multigen", "korean", "gcl", "fer", "eng", "employ_binary", "region",
            "drem", "dratx", "drat", "dphy", "dout", "deye", "dear", "ddrs", "cow",
            "citwp", "citizenship", "cit", "chinese", "agep", "GEpov200") ){
  # changed my mind and want to try a different way
  message(myvar)
  calc(ph.data = dt,
       what = myvar,
       where = agep >= 65,
       by = 'chi_sex')
}

# exploration #2 ... didn't work for mysterious reasons
for(myvar in c("vietn", "unspeclang", "uimale1864", "uige400", "uifem1864", "ui1864",
            "tagalog", "state", "spanish", "region", "pwgtp", "povu5",
            "povu18", "povold", "pov75", "pov74", "pov200", "pov100", "pooreng",
            "multigen", "korean", "gcl", "fer", "eng", "employ_binary", "region",
            "drem", "dratx", "drat", "dphy", "dout", "deye", "dear", "ddrs", "cow",
            "citwp", "citizenship", "cit", "chinese", "agep", "GEpov200") ){
  # changed my mind and want to try a different way
  message(myvar)
  calc(ph.data = dt,
       what = myvar,
       where = agep <65,
       by = 'chi_sex')
}

# try to make a table
mytable <- rbindlist(lapply(
  X = c("vietn", "unspeclang", "uimale1864", "uige400", "uifem1864", "ui1864", 
        "tagalog", "state", "spanish", "region", "pwgtp", "povu5", 
        "povu18", "povold", "pov75", "pov74", "pov200", "pov100", "pooreng", 
        "multigen", "korean", "gcl", "fer", "eng", "employ_binary", "employ", 
        "drem", "dratx", "drat", "dphy", "dout", "deye", "dear", "ddrs", "cow", 
        "citwp", "citizenship", "cit", "chinese", "agep", "GEpov200"), 
  FUN = function(X){
    temp <- calc(ph.data = dt, 
                 what = X, 
                 where = agep >= 65, 
                 by = 'chi_sex')
  }
), use.names = T)  
```

### :grin: Good Example
```r
# Can you help me with the following code?
# I am trying to use rbindlist(lapply(...)) to make a table 
# of PUMS estimates for seniors by sex. However, it breaks 
# when I run it. 

# I tried running the code outsdie of rbindlist(lapply(...))
# and it seems to work. I then added a line of code so I 
# could see which variable was causing the problem. It looks
# like `employ` is causing the problem, but I don't know 
# how to fix it. The error says, "Error in smean.character 
# (employ, na.rm = T, var_type = c("se", "ci")"

# R version 4.3.2
# data.table 1.14.8
# rads 1.3.2
library(rads)
library(data.table)

# Reproducible setup
dt <- fread("//dphcifs/APDE-CDIP/ACS/PUMS_data/2023_1_year/prepped_R_files/2023_1_year_person.rds")

# Only necessary data prep
dt[esr %in% c(1, 2, 4, 5), employ := 'Employed']
dt[esr %in% c(6), employ := 'Not in labor force']
dt[esr %in% c(3), employ := 'Unemployed']

# The specific issue
mytable <- rbindlist(lapply(
  X = c("employ"), 
  FUN = function(X){
    message(X)
    temp <- calc(ph.data = dt, 
                 what = X, 
                 where = agep >= 65, 
                 by = 'chi_sex')
  }
), use.names = T)  

# Expected output:
# a data.table with columns for:
# chi_sex: Male / Female
# level: Employed / Not in labor force / Unemployed
# variable: employ
# mean, mean_se, mean_lower, mean_upper, numerator, denominator
```


## Additional Resources

For more detailed information about creating reproducible examples:
* [How to make a great R reproducible example](https://stackoverflow.com/questions/5963269/how-to-make-a-great-r-reproducible-example)
* [How to create a Minimal, Reproducible Example](https://stackoverflow.com/help/minimal-reproducible-example)