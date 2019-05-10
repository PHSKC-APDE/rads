
#create a fake dataset for testing
d = expand.grid(district_id = 1:3, grade_id = 0:12, race = c('ai', 'as', 'b','nhpi', 'w','hisp','other'), sex = 1:2)
setDT(d)
d[grade_id <=6, school_id := paste0(district_id, '_elem')]
d[between(grade_id,7,8), school_id := paste0(district_id, '_mid')]
d[grade_id >=9, school_id := paste0(district_id, '_high')]
d[, n := 1:nrow(d)]

#reshape to expected format
d_re = dcast(d, district_id + grade_id + school_id ~ race+sex, value.var = 'n')

#try normally
res1 = format_enrollment_data(input = d_re, district_code = 'district_id', school_code = 'school_id', grade = 'grade_id',
                             m_ai = 'ai_1', f_ai = 'ai_2', m_as = 'as_1', f_as = 'as_2', m_b = 'b_1', f_b = 'b_2',
                             m_nhpi = 'nhpi_1', f_nhpi = 'nhpi_2',m_w = 'w_1', f_w = 'w_2', m_hisp = 'hisp_1', f_hisp = 'hisp_2',
                             m_other = 'other_1', f_other = 'other_2')


#try the reduce logic
d_re[, c('test_1', 'test_2') := 10]
res2 = format_enrollment_data(input = d_re, district_code = 'district_id', school_code = 'school_id', grade = 'grade_id',
                              m_ai = 'ai_1', f_ai = 'ai_2', m_as = 'as_1', f_as = 'as_2', m_b = 'b_1', f_b = 'b_2',
                              m_nhpi = 'nhpi_1', f_nhpi = 'nhpi_2',m_w = 'w_1', f_w = 'w_2', m_hisp = 'hisp_1', f_hisp = 'hisp_2',
                              m_other = c('other_1', 'test_1'), f_other = c('other_2', 'test_2'))

#try when some things are set to NA

#try when some rows are missing
#d_miss = d_re[sample(1:nrow(d_re),size = round(.9 * nrow(d_re))),]
res2 = format_enrollment_data(input = d_re, district_code = 'district_id', school_code = 'school_id', grade = 'grade_id',
                              m_ai = 'ai_1', f_ai = 'ai_2', m_as = 'as_1', f_as = 'as_2', m_b = 'b_1', f_b = 'b_2',
                              m_nhpi = 'nhpi_1', f_nhpi = 'nhpi_2',m_w = 'w_1', f_w = 'w_2', m_hisp = 'hisp_1', f_hisp = 'hisp_2',
                              m_other = c('other_1', 'test_1'), f_other = c('other_2', 'test_2'))

#try with columns that don't exist

#try with naughty argument passing (e.g. numerics, logicals, NAs, NULLs)

#manual graveyard
input = d_re;district_code = 'district_id'; school_code = 'school_id'; grade = 'grade_id';
m_ai = 'ai_1'; f_ai = 'ai_2'; m_as = 'as_1'; f_as = 'as_2'; m_b = 'b_1'; f_b = 'b_2';
m_nhpi = 'nhpi_1'; f_nhpi = 'nhpi_2';m_w = 'w_1'; f_w = 'w_2'; m_hisp = 'hisp_1'; f_hisp = 'hisp_2';
m_other = c('other_1', 'test_1'); f_other = c('other_2', 'test_2')
