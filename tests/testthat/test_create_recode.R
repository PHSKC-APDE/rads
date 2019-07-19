# library('data.table')
#
# #no years specificed
# a = create_recode('v1', 'v2', 'a', 'b', 'b', '', '')
#
# #numeric
# a = create_recode('v1', 'v2', 'a', 'b', 'b', '', numeric())
#
# #bad year mismatch
# a = create_recode('v1', 'v2', 'a', 'b', 'b', '1000000000', '99000')
#
# #see if a range will pass
# a = create_recode('v1', 'v2', c('[0-10]', 11, '(12 - 15)'), c(1,2,3), c('b1','b2','b3'), '', '')
