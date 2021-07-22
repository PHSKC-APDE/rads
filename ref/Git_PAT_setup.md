# How to access files from a private repo in R

## Problem: you need to pull a file/data from a private repo
* Imagine you want to pull in a csv file from a private repo you have access to.
* On public repos, you simply pull in the data from 'https://raw.githubusercontent.com/<account_name>/<repo>/<dir>/<file.csv>', usually copying the file name from your web browser.
* However, in a private repo, that URL has a suffix after the '.csv' part that looks like '?token=AEWDGFSWVNEWWF5AB8WPXKY2ZND'. This is a temporary access token that will only work in your code for a short period of time. You need a longer-lasting solution.

## Solution overview: use your own personal access token
The general approach is to set up a permanent token that you will use in R, using the following broad steps:
1. Set up the token in Github.
2. Add the token to your Windows environmental variables.
3. Use the `httr` package to send your credentials when accessing the file.

### Set up your personal access token (PAT)
1. Go to [https://github.com/settings/tokens](https://github.com/settings/tokens).
2. Click the button that says 'Generate new token' (you may need to enter your password).
3. In the note field, add something along the lines of 'win_env' so you know where this token will be used.
4. Under the scopes, check the 'repo' option. Everything else can be unchecked.
5. Click 'Generate token'.
6. A 40-character string will appear. This is the only time you will see the PAT so **do not close your browser yet**. Copy this character string.
7. If you mess things up and the PAT disappears, restart at step 1 with a new token (first, delete the token you just created after you click on the link in step 1).

### Add the PAT to your Windows environmental variables
1. Hit the windows key and type "edit environment variables" in the search bar. 
2. Hit enter to open `Environment Variables` window
3. Click `Edit...` in the top half of the window (i.e., the part that has the title "User variables for xyz")
4. In the `Variable name:`box, type **GITHUB_TOKEN**
5. In the `Variable value:` box, **paste your PAT** (40-character string that you copied from github.com)
6. Click `OK`
7. While you're here, you might as well save the same PAT with another commonly referenced name. Repeat steps 3 through 6, but change GITHUB_TOKEN to **GITHUB_PAT** when you repeat step 4.
8. Click `OK` on the bottom right of the to close the `Environment Variables` window.

### Use the PAT to access the private repo
First test that your PAT is working as hoped by typing the following in the R console:
    `auth <- Sys.getenv("GITHUB_TOKEN")`

Make sure that the auth object is your PAT. If not, seek assistance at R office hours or from a colleague.
 
You can now use the authenticate command in the `httr` package to pass your PAT. Here are some examples of how to access code:

**get a YAML file**
config_file <- yaml::yaml.load(httr::GET(  url = "https://raw.githubusercontent.com/PHSKC-APDE/bskhs/master/etl/load_raw/load_load_raw.bskhs_2017_phone_web.yaml",
  httr::authenticate(auth, "")))

**run R code**
eval(parse(text = httr::content(httr::GET(
  url = "https://raw.githubusercontent.com/PHSKC-APDE/bskhs/master/etl/scripts_general/load_table_bskhs.R",
  httr::authenticate(auth, "")), "text")))

**get a CSV**
bskhs_2019_names <- vroom::vroom(httr::content(httr::GET(
  url = "https://raw.githubusercontent.com/PHSKC-APDE/bskhs/master/etl/ref/field_names.csv",
  httr::authenticate(Sys.getenv("GITHUB_PAT"), "")), type = "text"))
 












