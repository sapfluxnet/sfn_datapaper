
# 0. Data needed to obtain figures, tables and results --------------------

source('read_metadata.R')

# 1. Choose map quality -----------------------------------------------------
# 'high' quality for definitive fig, 'draft' is faster

plot_quality <- 'draft'

if (plot_quality=='draft') {
  globforest <- globforest_rec_0_5
  print('Draft quality')
} else if (plot_quality=='high') {
  globforest <- globforest_rec_0_1
  print('High quality')
} else{
  print('Nothing done, choose quality')
}
