## CAC008 Auto-suggested codes listing ==============
# If anything is slow, it's this section.  If you decide you don't want it,
# just select the code down to where IP004 starts and hit ctrl+shift+c - this
# will `comment` it and stop it from running.
# also if you’re looking for raw speed, try data.table::fread()
# as above ignore warnings about number of columns not multiple of vector
# we'll fix that later
# CAC008.xlsx
read_360_file("CAC008", 9)

CAC008_all <- CAC008_all %>%
    fill(`Dx/Proc`) %>% 
    filter(!is.na(`Visit ID`)) %>% 
    sep_cdr() %>% 
   sep_dxpx() %>% 
    select(-MRN, - `Total Codes`,  -`Admit Date`)
  
cdr_acct_rej <- CAC_008_all %>% 
  group_by(`Visit ID`) %>% 
  mutate(tot_cds_acct=n())
