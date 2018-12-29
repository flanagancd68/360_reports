{# Intro =====
# Packages required.  Uncomment line below as needed to install
# install.packages("tidyverse", "plotly")
{
library(plotly)
library(shiny)
library(readxl)
library(readr)
library(tidyverse)
library(data.table)
options(digits = 5)
}
# Collect Info from user

  qtr <- 2018.1   # enter year and quarter being analyzed.  If you want month do yyyy.mm (add leading 0)
  keep_thru <- 2018.1 # over time, some of the consolidated files will become enormous. This value will filter (i.e. delete) data less then
                         # or equal to the quarter you specify.  See help documentation.
                         # Note that this will delete data from your consolidated file. Backups are recommended
  hub <- 7## enter TOTAL number of 360 hubs/servers; no quotes

  # if you pulled this file from github you should not need to change the 
  # _w_orking _d_irectory.  If there are errors when you try to read files, 
  # uncomment and update the line below:
  #setwd("<path/to/your/data>") 
source("360_cust_funs.R")


#  Section I: Import and consolidate 360 reports
## CRITICAL: read 360_file_prep.Rmd file for pre-formatting specifications.
#  If your 360 xlsx files are not formatted EXACTLY as stated, you may receive
#  errors, or worse, results that are not accurate
#  If you DO get an error check your environment for the value of `xl_file`.
#  If it is present (it may not always be) it will tell you the folder and file
#  that was active at the time of the error.
#  Correct the file, then run the report section again to assure data integrity
## file check  Should be 23 xlsx files in each sub-folder 
## This just does a count of the files and sends a warning if there is a
## mismatch. it doesn't check for proper formatting
for (i in 1:hub) {
  path <- paste0("./data/internal/")
  file_check <- length(list.files(path, pattern = as.character(qtr)))
  assign(paste0("hub_", i), file_check)
  if (file_check < (23 * hub)) {
    file_check
    stop(paste0("Check: hub ", i, " has ", file_check, " ",qtr, " xlxs  files.  Should be at least 23 * the number of hubs."))
  }
  rm(file_check)
  rm(list = ls(pattern = "\\hub_."))
  rm(i, path)
}

# read system facility designations IMPORTANT (even if you only have one facility). See instructructions
fac_list <- read_xlsx("./data/internal/fac_list.xlsx")
# read helper data files
read_ext_file("ahrq_ccs")
read_ext_file("drg_apr")
read_ext_file("drg_ms")
read_ext_file("icd10_ccmcc")
read_ext_file("icd10_chronic")
read_ext_file("icd10_unspecified")
read_ext_file("icd10cm_desc")
read_ext_file("icd10pcs_desc")
}
{# CAC001 | auto|suggested codes precision and recall =========================
 ## read data files

  read_360_file("CAC001_ip_dx", 62)
  CAC001_ip_dx_all <- CAC001_ip_dx_all %>% 
    mutate(IpOp = "Ip", DxPx = "Dx")
  
  read_360_file("CAC001_ip_px", 62)
  CAC001_ip_px_all <- CAC001_ip_px_all %>% 
    mutate(IpOp = "Ip", DxPx = "Px")
  
  read_360_file("CAC001_op_dx", 62)
  CAC001_op_dx_all <- CAC001_op_dx_all %>% 
    mutate(IpOp = "Op", DxPx = "Dx")
  
  read_360_file("CAC001_op_px", 62)
  CAC001_op_px_all <- CAC001_op_px_all %>% 
    mutate(IpOp = "Op", DxPx = "Px")
## create consolidated file with ip+op, dx+px
  CAC001_all <- suppressMessages(full_join(CAC001_ip_dx_all, CAC001_ip_px_all))
  rm(CAC001_ip_dx_all, CAC001_ip_px_all)
  CAC001_all <- suppressMessages(full_join(CAC001_all, CAC001_op_dx_all))
  rm(CAC001_op_dx_all)
  CAC001_all <- suppressMessages(full_join(CAC001_all, CAC001_op_px_all)) %>%
    rename(Coder = "Last Reviewer/Coder") %>% 
    replace(is.na(.), 0)  %>% 
    mutate(
      Facility = str_replace(Facility, "---", "Total"),
      `% Precision` = (`% Precision` / 100), # converts to decimal e.g. 54.32 becomes 0.5432
      `% Recall` = (`% Recall` / 100)
    ) %>% # converts to decimal - doing "/100" here avoids having to do "*100" every other time a rate is calculated
    sep_cdr() %>% 
    filter(`Coder` != "Grand Totals") %>%
    select(
      Coder, coderid, Facility, Hub, `Accepted`, `All Suggested`, `All Coded`,
      `% Precision`, `% Recall`, Qtr, IpOp, DxPx
    ) # Rearrange columns.  "From CAC" number by definition is the same as "Accepted"
  rm(CAC001_op_px_all)
## from consolidatef file create facility pnr 
  fac_pnr <- CAC001_all %>%
    filter(Facility != "Total") %>%
    left_join(fac_list) %>%
    group_by(Facility, IpOp, DxPx) %>%
    mutate(
      Accepted_fac = sum(Accepted),
      All_Suggested_fac = sum(`All Suggested`),
      `All_Coded_fac` = sum(`All Coded`),
      `%_Precision_fac` = (`Accepted_fac` / `All_Suggested_fac`),
      `%_Recall_fac` = (`Accepted_fac` / `All_Coded_fac`)
    ) %>%
    ungroup() %>%
    group_by(Hub, IpOp, DxPx) %>%
    mutate(
      `All_Suggested_hub` = sum(`All Suggested`),
      Accepted_hub = sum(`Accepted`),
      `%_Precision_hub` = (`Accepted_hub` / `All_Suggested_hub`),
      `All_Coded_hub` = sum(`All Coded`),
      `%_Recall_hub` = (`Accepted_hub` / `All_Coded_hub`)
    ) %>%
    select(-`All_Suggested_hub`, -Accepted_hub, -All_Coded_hub) %>% # for anything above facility, we just want the rates.  Remove the `-` if you want to see the raw number
    ungroup() %>%
    filter(`All Suggested` > 100) %>% # exclude low outliers from coder rates
    group_by(Facility, IpOp, DxPx) %>% # compares coders across each facility
    mutate(
      CdrPrcnMax_fac = max(`% Precision`),
      CdrPrcnVar_fac = sd(`% Precision`),
      CdrRclMax_fac = max(`% Recall`),
      CdrRclVar_fac = sd(`% Recall`)
    ) %>%
    ungroup() %>%
    group_by(Hub, IpOp, DxPx) %>% # compares coders across a single hub
    mutate(
      CdrPrcnMax_hub = max(`% Precision`),
      CdrPrcnVar_hub = sd(`% Precision`),
      CdrRclMax_hub = max(`% Recall`),
      CdrRclVar_hub = sd(`% Recall`)
    ) %>%
    ungroup() %>%
    group_by(vol_pctle_2018, IpOp, DxPx) %>% # compares coders across similarly sized facilitied in terms of annual IP discharges)
    mutate(
      CdrPrcnMax_size = max(`% Precision`), # above 75%percentile  / 25-70th / below 25th
      CdrPrcnVar_size = sd(`% Precision`),
      CdrRclMax_size = max(`% Recall`),
      CdrRclVar_size = sd(`% Recall`)
    ) %>%
    ungroup() %>%
    group_by(Group, IpOp, DxPx) %>% # compares coders across our company's designated hospital groups (i.e. regions)
    mutate(
      CdrPrcnMax_grp = max(`% Precision`), # above 75%percentile  / 25-70th / below 25th
      CdrPrcnVar_grp = sd(`% Precision`),
      CdrRclMax_grp = max(`% Recall`),
      CdrRclVar_grp = sd(`% Recall`)
    ) %>%
    ungroup() %>%
    group_by(IpOp, DxPx) %>%
    mutate(
      CdrPrcnMean_all = mean(`% Precision`), # compares coders across entire company (keeping IP vs OP and Dx vs Px)
      CdrPrcnMax_all = max(`%_Precision_fac`),
      CdrPrcnVar_all = sd(`%_Precision_fac`),
      CdrRclMean_all = mean(`%_Recall_fac`),
      CdrRclMax_all = max(`%_Recall_fac`),
      CdrRclVar_all = sd(`%_Recall_fac`)
    ) %>%
    select(
      -Coder, -coderid, -`All Suggested`, -Accepted, -`% Precision`,
      -`All Coded`, -`% Recall`, -name_internal
    ) %>%
    arrange(desc(`%_Precision_fac`)) %>% 
    distinct()
## assign "lead facility" for coders who code at multiple 
  cdr_fac <- CAC001_all %>% # where a 1:1 relationship is needed
    filter(Facility != "Total") %>% # 
    group_by(coderid) %>%
    mutate(cde_prop = (`All Coded`/sum(`All Coded`))) %>% 
    arrange(desc(cde_prop)) %>%
    filter(row_number() == 1) %>%
    ungroup() %>%
    select(Coder, coderid, Facility, cde_prop) %>% join_fac() %>% mutate(Qtr=qtr)
 ## coder pnr 
  cdr_pnr <- CAC001_all %>%
    filter(Facility != "Total") %>%
    group_by(coderid, IpOp, DxPx) %>%
    mutate(
      Accepted_cdr = sum(Accepted),
      All_Suggested_cdr = sum(`All Suggested`),
      `All_Coded_cdr` = sum(`All Coded`),
      `%_Precision_cdr` = (`Accepted_cdr` / `All_Suggested_cdr`),
      `%_Recall_cdr` = (`Accepted_cdr` / `All_Coded_cdr`)
    ) %>%
    #ungroup() %>%
    select(-Facility, -Hub, -Accepted, -`All Suggested`, -`All Coded`, -`% Precision`, -`% Recall`) %>% # not sure about this
    join_cdr_fac() %>%
    rename(Lead_Faciity = Facility) %>% 
    distinct(Coder, coderid, IpOp, DxPx, .keep_all = TRUE)
  ## Save files to cons folder 
  write_cons_file(cdr_pnr)
  write_cons_file(cdr_fac)
  write_cons_file(fac_pnr)
  rm(cdr_pnr, fac_pnr, CAC001_all)
}
{# 09d Physician query listing ================================================
read_360_file("CDI09d") 
CDI09d_all <- CDI09d_all %>% 
    filter(!is.na(`Visit ID`),`Provider Response` != "Withdrawn/Not Applicable") %>% 
    rename(Coder = `Query Author__1`) %>%  
  sep_cdr() %>% 
    separate(`Queried Provider`, c("Queried Provider", "QueryMDid"), sep = "[()]", extra="drop") %>% 
    separate(`Responding Provider`, c("Responding Provider", "RespMDid"), sep = "[()]", extra="drop") %>% 
    select( -`Query Author`, -`Total Queries`, -`Facility`, -`Patient Name`, -`Admit Date`, -Gender,-MRN, -`Total Visits`) %>% 
    select(Coder, coderid, everything())  %>% 
    rename(Facility=`Facility__1`) %>% join_fac()
# high level stats by coder and facility
###  
##WHAT OTHER QUESTIONS DOES THIS REPORT GENERATE?
###
# coder level
cdr_qry <- CDI09d_all %>%
  group_by(Coder) %>%
  summarize(qry_n_cdr = n()) %>% 
  ungroup() %>% 
  mutate(qry_prptn_cdr = qry_n_cdr/sum(qry_n_cdr), qry_dstn_mean_cdr=(qry_n_cdr-mean(qry_n_cdr)), Qtr=qtr) %>% 
  arrange(desc(qry_dstn_mean_cdr)) 

write_cons_file(cdr_qry)
rm(cdr_qry)
# facility level
fac_qry <-CDI09d_all %>%
  group_by(Facility, Fac, Group) %>%
  summarize(qry_n_fac = n()) %>% 
  ungroup() %>% 
  mutate(qry_prptn_fac = qry_n_fac/sum(qry_n_fac), qry_dstn_mean_fac=qry_n_fac -mean(qry_n_fac), Qtr=qtr) %>% 
  arrange(desc(qry_dstn_mean_fac))
write_cons_file(fac_qry)
rm(fac_qry)
rm(CDI09d_all)
}
{# CAC003 auto-suggested codes precision and recall by code ===================
read_360_file("CAC003_ip_cdr", 9)
CAC003_ip_cdr_all <- CAC003_ip_cdr_all %>% 
  filter(`Dx/Proc` != "---", `All Suggested` > 24) %>% 
  replace(is.na(.), 0)  %>% 
  mutate(IpOp = "Ip") %>% sep_dxpx() %>% sep_lrcdr()

read_360_file("CAC003_op_cdr", 9)
CAC003_op_cdr_all <- CAC003_op_cdr_all %>% 
  filter(`Dx/Proc` != "---", `All Suggested` > 24) %>% replace(is.na(.), 0)  %>% 
  mutate(IpOp = "Op") %>% sep_dxpx() %>% sep_lrcdr()

read_360_file("CAC003_ip_fac", 9)
CAC003_ip_fac_all <- CAC003_ip_fac_all %>%
  filter(`Dx/Proc` != "---", `All Suggested` > 24) %>% replace(is.na(.), 0)  %>% 
  mutate(IpOp = "Ip") %>% sep_dxpx()

read_360_file("CAC003_op_fac", 9)
CAC003_op_fac_all <- CAC003_op_fac_all %>% 
  filter(`Dx/Proc` != "---", `All Suggested` > 24) %>% replace(is.na(.), 0)  %>% 
  mutate(IpOp = "Op") %>% sep_dxpx()

  fac_pnr_dxpx <- suppressMessages(full_join(CAC003_ip_fac_all, CAC003_op_fac_all)) %>%
    group_by(Facility, Code, DxPx, IpOp, Qtr, Hub)  %>% 
    sum_pnr()

  cdr_pnr_dxpx <- suppressMessages(full_join(CAC003_ip_cdr_all, CAC003_op_cdr_all)) %>%
    group_by(Coder, Code) %>%
    sum_pnr() %>% 
    group_by(Hub) %>% 
    mutate(pcn_rnk_hub = min_rank(`% Precision`))
#code level
  cde_pnr_dxpx <- fac_pnr_dxpx %>%
    group_by(Code, DxPx, IpOp, Qtr, Hub) %>%
    sum_pnr() %>%
    arrange(desc(`All Suggested`)) 
  
  cde_pnr_ccmcc <- cde_pnr_dxpx %>% 
    left_join(icd10_ccmcc) %>% 
    filter(is_ccmcc == "TRUE", IpOp=="Ip") %>% 
    select(-is_ccmcc)
  
rm(CAC003_op_fac_all, CAC003_ip_fac_all)
rm(CAC003_op_cdr_all, CAC003_ip_cdr_all)

write_cons_file(fac_pnr_dxpx)
write_cons_file(cdr_pnr_dxpx)
write_cons_file(cde_pnr_dxpx)
write_cons_file(cde_pnr_ccmcc)
}
{# CAC007 All Codes Entry Method Summary ======================================
read_CAC007("CAC007_ip_dx_cdr")
CAC007_ip_dx_cdr_all <- CAC007_ip_dx_cdr_all %>% 
  mutate(IpOp = "Ip", PxDx = "Dx") %>% 
rename(Coder = "Entry.Method") %>% 
sep_cdr() %>% join_cdr_fac()

read_CAC007("CAC007_op_dx_cdr")
CAC007_op_dx_cdr_all <- CAC007_op_dx_cdr_all %>% 
  mutate(IpOp = "Op", PxDx = "Dx") %>% 
  rename(Coder = "Entry.Method") %>% 
  sep_cdr() %>%   join_cdr_fac()

read_CAC007("CAC007_ip_px_cdr")
CAC007_ip_px_cdr_all <- CAC007_ip_px_cdr_all %>% 
  mutate(IpOp = "Ip", PxDx = "Px") %>% 
  rename(Coder = "Entry.Method") %>% 
  sep_cdr() %>% join_cdr_fac()

read_CAC007("CAC007_op_px_cdr")
CAC007_op_px_cdr_all <- CAC007_op_px_cdr_all %>% 
  mutate(IpOp = "Op", PxDx = "Px") %>% 
  rename(Coder = "Entry.Method") %>% 
  sep_cdr() %>% join_cdr_fac()

### Join all
  cdr_entry <- suppressMessages(full_join(CAC007_ip_dx_cdr_all, CAC007_op_dx_cdr_all)) %>%
    replace(is.na(.), 0)
  cdr_entry <- suppressMessages(full_join(cdr_entry, CAC007_ip_px_cdr_all)) %>%
    replace(is.na(.), 0)
  cdr_entry <- suppressMessages(full_join(cdr_entry, CAC007_op_px_cdr_all)) %>%
    replace(is.na(.), 0) %>% 
  group_by(Coder, IpOp, PxDx) %>%
  mutate(TotCodes = sum(`CAC`,`CDI`,`MAN`)) %>%
  mutate(`CAC%`= (CAC/TotCodes),
         `CDI%`=(CDI/TotCodes),
         `MAN%`=(MAN/TotCodes),
#         `OTH%`=round((OTH/(TotCodes+OTH))*100,2),
         `MANAuto` = (MAN.CRS.S+MAN.Drct.Code.S),
         `MANAuto%` = (MANAuto/MAN),
         `CAC+CDI%`=`CAC%`+`CDI%`) %>% 
  select(Coder, coderid, `CAC%`, `CDI%`, `MAN%`, TotCodes, MAN.CRS.S, MAN.Drct.Code.S, MAN, `MANAuto%`, `CAC+CDI%`, Qtr, IpOp, PxDx,Hub)
rm(CAC007_op_px_cdr_all)
rm(CAC007_ip_dx_cdr_all, CAC007_op_dx_cdr_all)
rm(CAC007_ip_px_cdr_all)
write_cons_file(cdr_entry)
### * CAC007 Facility Level
read_CAC007("CAC007_ip_dx_fac")
CAC007_ip_dx_fac_all <- CAC007_ip_dx_fac_all %>% 
  mutate(IpOp = "Ip", PxDx = "Dx") %>% 
  rename(Facility = "Entry.Method") %>%  
  join_fac()
### CAC007_op_dx_fac.xlsx
read_CAC007("CAC007_op_dx_fac")
CAC007_op_dx_fac_all <- CAC007_op_dx_fac_all %>% 
  mutate(IpOp = "Op", PxDx = "Dx") %>% 
  rename(Facility = "Entry.Method") %>% 
  join_fac()
### CAC007_ip_px_fac.xlsx
read_CAC007("CAC007_ip_px_fac")
CAC007_ip_px_fac_all <- CAC007_ip_px_fac_all %>% 
  mutate(IpOp = "Ip", PxDx = "Px") %>% 
  rename(Facility = "Entry.Method") %>% 
  join_fac()
### CAC007_op_px_fac.xlsx
read_CAC007("CAC007_op_px_fac")
CAC007_op_px_fac_all <- CAC007_op_px_fac_all %>% 
  mutate(IpOp = "Op", PxDx = "Px") %>% 
  rename(Facility = "Entry.Method") %>% 
  join_fac()
### Join all
fac_entry <- suppressMessages(full_join(CAC007_ip_dx_fac_all, CAC007_op_dx_fac_all)) %>%
  replace(is.na(.), 0)
fac_entry <- suppressMessages(full_join(fac_entry, CAC007_ip_px_fac_all)) %>%
  replace(is.na(.), 0)
fac_entry <- suppressMessages(full_join(fac_entry, CAC007_op_px_fac_all)) %>%
  replace(is.na(.), 0) %>% 
  group_by(Facility, IpOp, PxDx) %>%
  mutate(TotCodes = sum(`CAC`,`CDI`,`MAN`)) %>%
  mutate(`CAC%`= (CAC/TotCodes),
         `CDI%`=(CDI/TotCodes),
         `MAN%`=(MAN/TotCodes),
         #         `OTH%`=round((OTH/(TotCodes+OTH))*100,2),
         `MANAuto` = (MAN.CRS.S+MAN.Drct.Code.S),
         `MANAuto%` = (MANAuto/MAN),
         `CAC+CDI%`=`CAC%`+`CDI%`) %>% 
  select(Facility, `CAC%`, `CDI%`, `MAN%`, TotCodes, MAN.CRS.S, MAN.Drct.Code.S, MAN, `MANAuto%`, `CAC+CDI%`, Qtr, IpOp, PxDx)
rm(CAC007_op_px_fac_all)
rm(CAC007_ip_dx_fac_all, CAC007_op_dx_fac_all)
rm(CAC007_ip_px_fac_all)
write_cons_file(fac_entry)
}
{# IP003 HAC DRGs ==========
read_360_file("IP003_mcr", 11)
IP003_mcr_all <- IP003_mcr_all %>% 
  filter(`DRG HAC Status` != "---") %>% 
  sep_cdr() %>% 
  mutate(IpOp = "Ip", `Primary Grouper` = "MSDRG") %>% 
  join_cdr_fac() %>% 
  rename(Lead_Facility = "Facility")

cdr_hac <- IP003_mcr_all %>% 
  group_by(Coder, coderid, Qtr, IpOp, `Primary Grouper`, `DRG HAC Status`, Lead_Facility, Fac, Group) %>% 
  mutate_at(vars(matches("Case")), as.numeric) %>% 
  mutate_at(vars(matches("Reim")), as.numeric) %>% 
  summarise(tot_visits = sum(`Total Visits`),  #consolidates multiple hubs (need to keep DRG HAC Status grouping)
         pre_case_mix = weighted.mean(`Case Mix`, (`Total Visits`)),
         pre_tot_reimb = sum(`Exp Reim`),
         pre_avg_reimb = weighted.mean(`Avg Exp Reim`, `Total Visits`),
         post_case_mix = weighted.mean(`Case Mix__1`, `Total Visits`),
         post_tot_reimb = sum(`Exp Reim__1`),
         post_avg_reimb = weighted.mean(`Avg Exp Reim__1`, `Total Visits`)) %>% 
  group_by(Coder, coderid, Qtr, IpOp, `Primary Grouper`) %>%  #consolidates overall pre/post, can drop DRG HAC grouping
         mutate(pre_case_mix = weighted.mean(pre_case_mix, (tot_visits)),
         pre_tot_reimb = sum(pre_tot_reimb),
         pre_avg_reimb = weighted.mean(pre_avg_reimb, tot_visits),
         post_case_mix = weighted.mean(post_case_mix, tot_visits),
         post_tot_reimb = sum(post_tot_reimb),
         post_avg_reimb = weighted.mean(post_avg_reimb, tot_visits)) %>% 
  ungroup() %>% 
    spread(`DRG HAC Status`, `tot_visits`) %>% 
  replace(is.na(.), 0) %>% 
  rename(no_hac=`0-Not applicable, hospital is exempt or HAC criteria are not met`,
         hac_no_drg = `1-One or more HAC criteria met and Final DRG does not change`,
         hac_drg = `2-One or more HAC criteria met and Final DRG changes`) %>% 
  mutate(hac_rate_1000 = ((hac_no_drg + hac_drg)/1000)/((no_hac+hac_no_drg+hac_drg)/1000), hac_drg_rate_1000 = (hac_drg/1000)/((no_hac+hac_no_drg+hac_drg)/100))
write_cons_file(cdr_hac)
rm(IP003_mcr_all)
} ## NEEDS MORE WORK
{# IP004 Primary and Secondary DRG Listing ==============================
# See important note in 360 file prep document about secondary DRGs
# IP004.xlsx
read_360_file("IP004", 12)
IP004_all <- IP004_all %>% 
  fill(Coder, `Financial Class`) %>% 
  filter(!is.na(`Visit ID`)) %>% 
  sep_cdr() %>% 
  separate(`Prin Dx`, c("Dx"), sep = "[-]", extra="drop") %>%
  separate(`Prin Proc`, c("Px"), sep = "[-]", extra="drop") %>%
  separate(`Pt Type`, c("IpOp", "ptype"), sep="[/]", extra="drop") %>% 
  mutate(`IpOp` = str_replace(IpOp, "I", "Ip")) %>% 
  mutate(`IpOp` = str_replace(IpOp, "O", "Op")) %>% 
  mutate(Dx=str_replace(Dx, " ",""), Px=str_replace(Px, " ",""))
  
IP004_pbar <- IP004_all %>% filter(Hub %in% c(1:5)) %>% mutate(Fac=str_sub(`Visit ID`, 1, 3)) %>% select(`Visit ID`, Fac, coderid)
IP004_satx <- IP004_all %>% filter(Hub %in% c(6)) %>% mutate(Fac=str_sub(`ptype`, 1, 1)) %>% 
  mutate(Fac= str_replace(Fac, "A", "BMA")) %>% select(`Visit ID`, Fac, coderid)
  

mutate_if(IP004_all,'Hub'=6, Fac=ptype)
# needs some tidying to make variables in constant location
# Data may fail if you don't collect secondary DRG on every patient
Pt <- IP004_all %>% 
  select (Coder, coderid,`Visit ID`, `Disch Date`, LOS, IpOp, ptype, Fac, `Disch Disposition`, `Dx`, `Px`)
DRG <- IP004_all %>%
  select(`Visit ID`, DRG, Wgt, `SOI/ROM`, ALOS, GLOS) %>%
  separate(DRG, c("DRG","Grouper"),sep="[()]") %>%
  separate(DRG, c("DRG"), sep="[-]") %>% 
  #change to number to match DRG1
   mutate_at(vars(matches("Wgt")), as.numeric) 
DRG1 <-IP004_all %>%
  select(`Visit ID`, DRG__1, Wgt__1, `SOI/ROM__1`, ALOS__1, GLOS__1) %>%
  separate(DRG__1, c("DRG","Grouper"),sep="[()]") %>%
  separate(DRG, c("DRG"), sep="[-]") %>%
  rename(Wgt=Wgt__1, `SOI/ROM`= "SOI/ROM__1", ALOS=ALOS__1, GLOS=GLOS__1)
# Bring Primary DRGs and Secondary DRGs into one table
DRG <- full_join(DRG, DRG1)
rm(DRG1)
#Now designate MS vs APR DRG Info - this is the equivalent of find and replace
#As long as the various state APR systems have `APR` in the desciption they will all be classified to APRDRG
  DRG$Grouper[grepl("APR", DRG$Grouper, ignore.case=FALSE)] <- "APRDRG"
  DRG$Grouper[grepl("Medicare", DRG$Grouper, ignore.case=FALSE)] <- "MSDRG"
  DRG$Grouper[grepl("MS", DRG$Grouper, ignore.case=FALSE)] <- "MSDRG"
  DRG$Grouper[grepl("TRI", DRG$Grouper, ignore.case=TRUE)] <- "MSDRG"
  
# if you have a grouper not listed you may need to add it
# The first part in green is the string you want to search for
# the last part is where you would classify it to APRDRG or MSDRG
# see missinggrouper list for possible missing groupers that need to be mapped
APR <- DRG %>%
  filter(Grouper=="APRDRG") %>%
  separate(`SOI/ROM`, c("SOI", "ROM"), sep="[/]") %>%
  rename(APRLOS=ALOS, APRGLOS=GLOS, APRRW=Wgt) %>%
  mutate(APRDRG=as.numeric(DRG),SOI=as.numeric(SOI), ROM=as.numeric(ROM), SOIROM=as.numeric(SOI)+as.numeric(ROM)) %>%
  # we count soi+rom score of 7 or 8 as an expected mortality
  # the combined score might be useful for detecting any trends with some coders coding lower overall severity than others
  select(-Grouper, -DRG)
noapr <- anti_join(APR, DRG)
MS <- DRG %>%
  mutate(MSDRG=as.numeric(DRG)) %>%
  filter(Grouper=="MSDRG") %>%
  select(-DRG, -`SOI/ROM`, -Grouper) %>%
  rename(MSLOS=ALOS, MSGLOS=GLOS, MSRW=Wgt)
noms <-anti_join(MS, DRG)
#and bring back together again so that we still have one row per pt
APRMS <- full_join(APR, MS)
Missinggrouper <- full_join(noapr, noms) #hope this is 0 but including just in case we get another grouper we need to map to APR or MS
#if so just duplicate the DRG$Grouper[grepl("...")], where ... is some distinct identifier
cdr_drg <- full_join(APRMS, Pt)
rm(noapr,DRG, Pt, APRMS, APR, MS)
write_cons_file(cdr_drg)
}
{ #IPPlus001.xlsx ========================
read_360_file("IPPlus001", 11)
IPPlus001_all <- IPPlus001_all %>% 
  filter(Coder != "---") %>% sep_cdr() 
IPPlus001_all$Grouper[grepl("APR", IPPlus001_all$Grouper, ignore.case=FALSE)] <- "APRDRG"
IPPlus001_all$Grouper[grepl("Medicare", IPPlus001_all$Grouper, ignore.case=FALSE)] <- "MSDRG"
IPPlus001_all$Grouper[grepl("MS", IPPlus001_all$Grouper, ignore.case=FALSE)] <- "MSDRG"
IPPlus001_all$Grouper[grepl("TRI", IPPlus001_all$Grouper, ignore.case=TRUE)] <- "MSDRG"
  IPPlus001_all <- IPPlus001_all %>% 
  mutate_at(vars(matches("Case")), as.numeric) %>% 
    mutate_at(vars(matches("Total")), as.numeric) %>% 
    mutate_at(vars(matches("LOS")), as.numeric) 
cdr_cmi_fc <- IPPlus001_all
cdr_cmi <- IPPlus001_all %>% 
  filter(`Financial Class` == "---") %>% 
  select(-`Financial Class`)
write_cons_file(cdr_cmi_fc)
write_cons_file(cdr_cmi)
}
{# Prod16 =====================
read_360_file("Prod016_ip",9)
read_360_file("Prod016_op",9)
Prod016_ip_all <- Prod016_ip_all %>% 
  mutate(IpOp = "Ip") %>% 
  select(- `Hold Transactions`, -`Coding Transactions`) %>% 
  sep_cdr()
Prod016_op_all <- Prod016_op_all %>% 
  mutate(IpOp = "Op") %>% 
  select(- `Hold Transactions`, -`Coding Transactions`) %>% 
  sep_cdr()
cdr_prod <- full_join(Prod016_op_all, Prod016_ip_all)
write_cons_file(cdr_prod)
rm(Prod016_op_all, Prod016_ip_all)
}
########################################################

## Benchmarks: ======
## 12/12/2018: 51 seconds including CAC008 (19.27 seconds excluding)
# Section II: Reshape Data ------------------

