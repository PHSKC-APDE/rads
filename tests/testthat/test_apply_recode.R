library(data.table)

#simple recode tests
a = create_recode('V1', 'V2', 'a', 'b', 'b', '', '')
b = data.table('a')
t1 = apply_recode(b, 2016, a, F)
t2 = apply_recode(b, 2016, a, T)

#more complex
data_vector = factor(rep(letters[1:3],3),letters[1:3])
old_vals = letters[1:2]
new_vals = LETTERS[1:2]
d = data.table(V1 = data_vector)

#creating a new column
rec = create_recode('V1', 'V2', old_vals, new_vals, new_label = new_vals)
e = apply_recode(d, 2016, rec)

#overwriting and existing one
d1 = copy(d)
d1[, V2:=V1]
rec = create_recode('V1', 'V2', old_vals, new_vals, new_label = new_vals)
e1 = apply_recode(d1, 2016, rec)

#what happens when try to recode a factor(e.g. the label) rather than the value?
