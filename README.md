# Test_msl_pressure_4h
#
# Tests the msl pressure for gross error
# The pressure frequency of observation is 4 times a day
# 
# The Example directory has the digitisations of a Chilean station to use as input and a .pdf about the QC of chilean records
# 
# The Script directory has 2 functions:
# read_chilean() - reads the digitisations of chilean surface records
# test_msl_pressure() - tests msl pressure for gross error limits
# Usage: 
# read_chilean()  # Choose the example file "ChileChico_Obs_1956.txt" and ignore the warning
# test_msl_pressure()
