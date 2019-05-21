library('data.table')

#no years specificed
a = create_recode('v1', 'v2', 'a', 'b', 'b', '', '')

#numeric
a = create_recode('v1', 'v2', 'a', 'b', 'b', '', numeric())

#bad year mismatch
a = create_recode('v1', 'v2', 'a', 'b', 'b', '1000000000', '99000')
