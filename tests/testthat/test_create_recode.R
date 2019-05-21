dat = data.table(V1 = rep(1:3, 2), V2 = 1:6, V3 = 6:11)
lab = data.table(var_name = c('V1', 'V1','V1', 'V1', rep('V2', 7)),
                 data_value = c('', 1:3, "", 1:6),
                 label_col = c('var1', 'a', 'b','c','var2',paste0('canda', 1:6)))
lab = rbind(lab, data.table(var_name = 'v3', data_value = "", label_col = 'POTATO'))
#to mimic the actual label dataset, drop some of the var names
lab[, id:= rowid(var_name)]
lab[id != which.min(id), var_name := ""]

a = format_single_year(input = dat, labels = lab, 'var_name', 'data_value', 'label_col')

#Test data structure/see if it works
re = create_recode(old_var = 'v3', old_values = 6:11, new_var = 'v3_re', new_values = c(rep(1:2, 100)[seq(length(6:11))]), new_variable_label = 'recoded V3')

#test new value recycline

