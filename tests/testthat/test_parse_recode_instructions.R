library(data.table)

a = data.table(old_var = c("v1", "v1", "v2", "v2", 'v3', 'V4', 'V4'),
               old_value = c(1,2, 1, 2, "", 1 , 2),
               new_var = c("re1", "re1", "re2", "re2", 're3', 're4', 're4'),
               new_value = c(0, 1, 0, 1, "", NA, 'NA'),
               new_label = c("Female", "Male", "Female", "Male", "", NA, 'NA'),
               start_year = "",
               end_year = "")


#test all recoding
parse_recode_instructions(a)

#test simple renames

