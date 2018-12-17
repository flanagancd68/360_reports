# Packages required.  Uncomment line below as needed to install
# install.packages("tidyverse", "plotly")
library(plotly, tidyverse, readxl)

# Collect Info from user ------------------------------------------------------
{
  options(digits = 4) # precision of output to two decimal places only.  Change if you want more or less
  qtr <- "18Q3" # enter year and quarter being analyzed (inside the "")
  hub <- 8 ## enter TOTAL number of 360 hubs/servers; no quotes
}  
  # if you pulled this file from github you should not need to change the 
  # _w_orking _d_irectory.  If there are errors when you try to read files, 
  # uncomment and update the line below:
  #setwd("<path/to/your/data>") 

#  Section I: Import and consolidate 360 reports ----------------------------------------
## CRITICAL: read 360_file_prep.Rmd file for pre-formatting specifications. ====
#  If your 360 xlsx files are not formatted EXACTLY as stated, you may receive
#  errors, or worse, results that are not accurate
#  If you DO get an error check your environment for the value of `xlsx_file`.
#  If it is present (it may not always be) it will tell you the folder and file
#  that was active at the time of the error.
#  Correct the file, then run the report section again to assure data integrity
## file check  Should be 21 xlsx files in each sub-folder ======================
## This just does a count of the files and sends a warning if there is a
## mismatch. it doesn't check for proper formatting
for (i in 1:hub) {
  path <- paste0("./data/", i)
  file_check <- length(list.files(path, pattern = qtr))
  assign(paste0("hub_", i), file_check)
  if (file_check < 21) {
    file_check
    stop(paste0("Check: hub ", i, " has ", file_check, " ",qtr, " xlxs  files.  Should be 21."))
  }
  rm(file_check)
  rm(list = ls(pattern = "\\hub_."))
  rm(i, path)
}

# read in system facility designations
fac_list <- read_xlsx("./data/internal/fac_list.xlsx")
# read in ccmcc table
ccmcc <- read_xlsx("./data/external/19_cm_ccmcc.xlsx")

## 09d Physician query listing ================================================
## These routines will import your hub-specific file from each sub folder, do
## some minor clean up, and then consolidate into a single dataframe.  We add columns
## for the hub and quarter being evaluated.  The for loop just tells R to repeat
## the process for however many hubs you specified above under "collect user
## info".  You can ignore the warnings.  The 360 report has duplicate column
## names - we will fix these later.

for (i in 1:hub) {
  xl_file <- paste0("./data/", i, "/", qtr, "_09d.xlsx")
  CDI09d_hub <- read_excel(xl_file, col_types = cols(
    `Total Visits` = col_skip(), `Total Queries` = col_skip(), Gender=col_skip(),`Visit ID` = col_character(),`Query Author` = col_skip(), Facility = col_skip(), 
    `SOI/ROM` = col_character(), `SOI/ROM_1` = col_character(), `Patient Name`= col_skip(), `Admit Date` = col_skip(),
    MRN = col_skip()
  )) %>% 
    filter(!is.na(`Visit ID`),`Provider Response` != "Withdrawn/Not Applicable") %>% 
    rename(Coder=`Query Author_1`, Facility=Facility_1) %>% 
    separate(Coder, c("Coder", "Coderid"), sep = "[()]") %>%
    separate(`Queried Provider`, c("Queried Provider", "QueryMDid"), sep = "[()]") %>% 
    separate(`Responding Provider`, c("Responding Provider", "RespMDid"), sep = "[()]") %>% 
    select(Coder, Coderid, everything()) %>% 
    mutate(Hub = i, QTR = qtr, Fac = str_sub(`Visit ID`, 1, 3))

  if (i == 1) {
    CDI09d_all <- CDI09d_hub
    next
  }
  CDI09d_all <- full_join(CDI09d_all, CDI09d_hub)
  rm(CDI09d_hub)
}

## This next section cleans up the files a bit and finally saves to the cons folder.
{
  CDI09d_all <- CDI09d_all %>%
       left_join(fac_list) %>% # this action links the facility name in the reports with our internal facility code and regional
    # designations.  I will use FAC in this script so even if you only have one facility
    # you will want to create a fac_list.xlsx file and put it in your external folder
    select(-name_internal) # this action removes the redundant internal facility name

  write_excel_csv(CDI09d_all, paste0("./data/cons/CDI09_all_", qtr, "_.xlsx")) # this action writes a cons xlsx file with all hubs

# high level stats by coder and facility
cdr_qry <- CDI09d_all %>%
    group_by(Coder) %>%
 summarize(qry_n_cdr = n()) %>% 
  ungroup() %>% 
  mutate(qry_prptn_cdr = qry_n_cdr/sum(qry_n_cdr), qry_dstn_mean_cdr=qry_n_cdr -mean(qry_n_cdr)) %>% 
  arrange(desc(qry_dstn_mean_cdr))

fac_qry <-CDI09d_all %>%
  group_by(Facility) %>%
  summarize(qry_n_fac = n()) %>% 
  ungroup() %>% 
  mutate(qry_prptn_fac = qry_n_fac/sum(qry_n_fac), qry_dstn_mean_fac=qry_n_fac -mean(qry_n_fac)) %>% 
  arrange(desc(qry_dstn_mean_fac))
write_excel_csv(cdr_qry, paste0("./data/cons/cdr_qry", qtr, "_.xlsx")) # this action writes a cons xlsx file with all hubs
write_excel_csv(fac_qry, paste0("./data/cons/fac_qry", qtr, "_.xlsx")) # this action writes a cons xlsx file with all hubs
rm(CDI09d_all)
}

# Note - generally, the comments above will apply to all files we will import.
# To reduce clutter, I will limit comments going forward.
#
#
## CAC001 | auto|suggested codes precision and recall =========================
## Same general concept as with the CDI report.  With CAC001 we will tag the
## four separate reports as either IP or OP and then DX or PX, then
## consolidate everything together into one table - should be 12 columns
### CAC001_ip_dx.xlsx
{
  for (i in 1:hub) {
    xlsx_file <- paste0("./data/", i, "/cac001_ip_dx.xlsx")
    CAC001_ip_dx <- read_xlsx(xlsx_file, col_types = cols(
      `% Precision` = col_number(), `% Recall` = col_number(), Accepted = col_number(),
      `All Coded` = col_number(), `All Suggested` = col_number(),
      `From CAC` = col_skip()
    )) %>%
      mutate(Hub = i, QTR = qtr, PatCls = "I", DxPx = "Dx")
    if (i == 1) {
      CAC001_ip_dx_all <- CAC001_ip_dx
      next
    }
    CAC001_ip_dx_all <- full_join(CAC001_ip_dx_all, CAC001_ip_dx)
    rm(CAC001_ip_dx)
  }
  ### CAC001_ip_px.xlsx
  for (i in 1:hub) {
    xlsx_file <- paste0("./data/", i, "/cac001_ip_px.xlsx")
    CAC001_ip_px <- read_xlsx(xlsx_file, col_types = cols(
      `% Precision` = col_number(), `% Recall` = col_number(), Accepted = col_number(),
      `All Coded` = col_number(), `All Suggested` = col_number(),
      `From CAC` = col_skip()
    )) %>%
      mutate(Hub = i, QTR = qtr, PatCls = "I", DxPx = "Px") %>%
      replace(is.na(.), 0)
    if (i == 1) {
      CAC001_ip_px_all <- CAC001_ip_px
      next
    }
    CAC001_ip_px_all <- full_join(CAC001_ip_px_all, CAC001_ip_px)
    rm(CAC001_ip_px)
  }
  ### CAC001_op_dx.xlsx
  for (i in 1:hub) {
    xlsx_file <- paste0("./data/", i, "/cac001_op_dx.xlsx")
    CAC001_op_dx <- read_xlsx(xlsx_file, col_types = cols(
      `% Precision` = col_number(), `% Recall` = col_number(), Accepted = col_number(),
      `All Coded` = col_number(), `All Suggested` = col_number(),
      `From CAC` = col_skip()
    )) %>%
      mutate(Hub = i, QTR = qtr, PatCls = "O", DxPx = "Dx") %>%
      replace(is.na(.), 0)
    if (i == 1) {
      CAC001_op_dx_all <- CAC001_op_dx
      next
    }
    CAC001_op_dx_all <- full_join(CAC001_op_dx_all, CAC001_op_dx)
    rm(CAC001_op_dx)
  }
  ### CAC001_op_px.xlsx
  for (i in 1:hub) {
    xlsx_file <- paste0("./data/", i, "/cac001_op_px.xlsx")
    CAC001_op_px <- read_xlsx(xlsx_file, col_types = cols(
      `% Precision` = col_number(), `% Recall` = col_number(), Accepted = col_number(),
      `All Coded` = col_number(), `All Suggested` = col_number(),
      `From CAC` = col_skip()
    )) %>%
      mutate(Hub = i, QTR = qtr, PatCls = "O", DxPx = "Px") %>%
      replace(is.na(.), 0)
    if (i == 1) {
      CAC001_op_px_all <- CAC001_op_px
      next
    }
    CAC001_op_px_all <- full_join(CAC001_op_px_all, CAC001_op_px)
    rm(CAC001_op_px)
  }
  ### create CAC001_all
  CAC001_all <- full_join(CAC001_ip_dx_all, CAC001_ip_px_all)
  rm(CAC001_ip_dx_all, CAC001_ip_px_all)
  CAC001_all <- full_join(CAC001_all, CAC001_op_dx_all)
  rm(CAC001_op_dx_all)
  CAC001_all <- full_join(CAC001_all, CAC001_op_px_all) %>%
    mutate(
      Facility = str_replace(Facility, "---", "Total"),
      `% Precision` = (`% Precision` / 100), # converts to decimal e.g. 54.32 becomes 0.5432
      `% Recall` = (`% Recall` / 100)
    ) %>% # converts to decimal - doing "/100" here avoids having to do "*100" every other time a rate is calculated
    separate(`Last Reviewer/Coder`, c("Coder", "Coderid"), sep = "[()]") %>%
    filter(`Coder` != "Grand Totals") %>%
    select(
      Coder, Coderid, Facility, Hub, `Accepted`, `All Suggested`, `All Coded`,
      `% Precision`, `% Recall`, QTR, PatCls, DxPx
    ) # Rearrange columns.  "From CAC" number by definition is the same as "Accepted"
  rm(CAC001_op_px_all)
  write_excel_csv(CAC001_all, paste0("./data/cons/CAC001_all", qtr, "_.xlsx")) # this action writes a cons xlsx file with all hubs
}

fac_pnr <- CAC001_all %>%
  filter(Facility != "Total") %>%
  left_join(fac_list) %>%
  group_by(Facility, PatCls, DxPx) %>%
  mutate(
    Accepted_fac = sum(Accepted),
    All_Suggested_fac = sum(`All Suggested`),
    `All_Coded_fac` = sum(`All Coded`),
    `%_Precision_fac` = (`Accepted_fac` / `All_Suggested_fac`),
    `%_Recall_fac` = (`Accepted_fac` / `All_Coded_fac`)
  ) %>%
  ungroup() %>%
  group_by(Hub, PatCls, DxPx) %>%
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
  group_by(Facility, PatCls, DxPx) %>% # compares coders across each facility
  mutate(
    CdrPrcnMax_fac = max(`% Precision`),
    CdrPrcnVar_fac = sd(`% Precision`),
    CdrRclMax_fac = max(`% Recall`),
    CdrRclVar_fac = sd(`% Recall`)
  ) %>%
  ungroup() %>%
  group_by(Hub, PatCls, DxPx) %>% # compares coders across a single hub
  mutate(
    CdrPrcnMax_hub = max(`% Precision`),
    CdrPrcnVar_hub = sd(`% Precision`),
    CdrRclMax_hub = max(`% Recall`),
    CdrRclVar_hub = sd(`% Recall`)
  ) %>%
  ungroup() %>%
  group_by(vol_pctle_2018, PatCls, DxPx) %>% # compares coders across similarly sized facilitied in terms of annual IP discharges)
  mutate(
    CdrPrcnMax_size = max(`% Precision`), # above 75%percentile  / 25-70th / below 25th
    CdrPrcnVar_size = sd(`% Precision`),
    CdrRclMax_size = max(`% Recall`),
    CdrRclVar_size = sd(`% Recall`)
  ) %>%
  ungroup() %>%
  group_by(Group, PatCls, DxPx) %>% # compares coders across our company's designated hospital groups (i.e. regions)
  mutate(
    CdrPrcnMax_grp = max(`% Precision`), # above 75%percentile  / 25-70th / below 25th
    CdrPrcnVar_grp = sd(`% Precision`),
    CdrRclMax_grp = max(`% Recall`),
    CdrRclVar_grp = sd(`% Recall`)
  ) %>%
  ungroup() %>%
  group_by(PatCls, DxPx) %>%
  mutate(
    CdrPrcnMean_all = mean(`% Precision`), # compares coders across entire company (keeping IP vs OP and Dx vs Px)
    CdrPrcnMax_all = max(`%_Precision_fac`),
    CdrPrcnVar_all = sd(`%_Precision_fac`),
    CdrRclMean_all = mean(`%_Recall_fac`),
    CdrRclMax_all = max(`%_Recall_fac`),
    CdrRclVar_all = sd(`%_Recall_fac`)
  ) %>%
  select(
    -Coder, -Coderid, -`All Suggested`, -Accepted, -`% Precision`,
    -`All Coded`, -`% Recall`, -name_internal
  ) %>%
  arrange(desc(`%_Precision_fac`)) %>% 
  distinct()
# ### create CAC001_all_cdr
cdr_fac <- CAC001_all %>% # many of our coders code across facilities and across hubs.  For purposes of a 1:1 relationship between coder
  filter(Facility != "Total") %>% # and facility, this routine looks at the facility with highest coded count and ties that facility to the coder
  group_by(Coderid) %>%
  arrange(desc(`All Coded`)) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  select(Coder, Coderid, Facility)
# #
cdr_pnr <- CAC001_all %>%
  filter(Facility != "Total") %>%
  group_by(Coderid, PatCls, DxPx) %>%
  mutate(
    Accepted_cdr = sum(Accepted),
    All_Suggested_cdr = sum(`All Suggested`),
    `All_Coded_cdr` = sum(`All Coded`),
    `%_Precision_cdr` = (`Accepted_cdr` / `All_Suggested_cdr`),
    `%_Recall_cdr` = (`Accepted_cdr` / `All_Coded_cdr`)
  ) %>%
  ungroup() %>%
  select(-Facility, -Hub, -Accepted, -`All Suggested`, -`All Coded`, -`% Precision`, -`% Recall`) %>% # not sure about this
  distinct() %>%
  left_join(cdr_fac) %>%
  rename(Lead_Faciity = Facility)
{
write_excel_csv(cdr_pnr, paste0("./data/cons/cdr_pnr", qtr, "_.xlsx")) # this action writes a cons xlsx file with all hubs
write_excel_csv(fac_pnr, paste0("./data/cons/fac_pnr", qtr, "_.xlsx")) # this action writes a cons xlsx file with all hubs
rm(CAC001_all)
}
## CAC003 auto-suggested codes precision and recall by code ===================
## same concept as above.  We can grab Dx/Px from the column with the code, so we
## just need to tag each row as IP or OP and the CDR or FAC.  We will consolidate
## into one CDR table and one FAC table.
## You may safely ignore warnings about number of columns not multiple of vector
## we'll fix that later
## You should have 12 columns.
### CAC003_ip_cdr.xlsx
{
  for (i in 1:hub) {
    xlsx_file <- paste0("./data/", i, "/CAC003_ip_cdr.xlsx")
    CAC003_ip_cdr <- read_xlsx(xlsx_file, col_types = cols(
      `Last Reviewer/Coder` = col_character(), `Dx/Proc` = col_character(), `% Precision` = col_number(), `% Recall` = col_number(), Accepted = col_number(),
      `All Coded` = col_number(), `All Suggested` = col_number(),
      `From CAC` = col_skip()
    )) %>%
      mutate(Hub = i, QTR = qtr, PatCls = "I", cdrfac = "cdr") %>%
      replace(is.na(.), 0)
    if (i == 1) {
      CAC003_ip_cdr_all <- CAC003_ip_cdr
      next
    }
    CAC003_ip_cdr_all <- full_join(CAC003_ip_cdr_all, CAC003_ip_cdr)
    rm(CAC003_ip_cdr)
  }

  ### CAC003_ip_fac.xlsx
  for (i in 1:hub) {
    xlsx_file <- paste0("./data/", i, "/CAC003_ip_fac.xlsx")
    CAC003_ip_fac <- read_xlsx(xlsx_file, col_types = cols(
      `% Precision` = col_number(), `% Recall` = col_number(), Accepted = col_number(),
      `All Coded` = col_number(), `All Suggested` = col_number(),
      `From CAC` = col_skip()
    )) %>%
      mutate(Hub = i, QTR = qtr, PatCls = "I", cdrfac = "fac") %>%
      replace(is.na(.), 0)
    if (i == 1) {
      CAC003_ip_fac_all <- CAC003_ip_fac
      next
    }
    CAC003_ip_fac_all <- full_join(CAC003_ip_fac_all, CAC003_ip_fac)
    rm(CAC003_ip_fac)
  }

  ### CAC003_op_cdr.xlsx
  for (i in 1:hub) {
    xlsx_file <- paste0("./data/", i, "/CAC003_op_cdr.xlsx")
    CAC003_op_cdr <- read_xlsx(xlsx_file, col_types = cols(
      `% Precision` = col_number(), `% Recall` = col_number(), Accepted = col_number(),
      `All Coded` = col_number(), `All Suggested` = col_number(),
      `From CAC` = col_skip()
    )) %>%
      mutate(Hub = i, QTR = qtr, PatCls = "O", cdrfac = "cdr") %>%
      replace(is.na(.), 0)
    if (i == 1) {
      CAC003_op_cdr_all <- CAC003_op_cdr
      next
    }
    CAC003_op_cdr_all <- full_join(CAC003_op_cdr_all, CAC003_op_cdr)
    rm(CAC003_op_cdr)
  }

  ### CAC003_op_fac.xlsx
  for (i in 1:hub) {
    xlsx_file <- paste0("./data/", i, "/CAC003_op_fac.xlsx")
    CAC003_op_fac <- read_xlsx(xlsx_file, col_types = cols(
      `% Precision` = col_number(), `% Recall` = col_number(), Accepted = col_number(),
      `All Coded` = col_number(), `All Suggested` = col_number(),
      `From CAC` = col_skip()
    )) %>%
      mutate(Hub = i, QTR = qtr, PatCls = "O", cdrfac = "fac") %>%
      replace(is.na(.), 0)
    if (i == 1) {
      CAC003_op_fac_all <- CAC003_op_fac
      next
    }
    CAC003_op_fac_all <- full_join(CAC003_op_fac_all, CAC003_op_fac)
    rm(CAC003_op_fac)
  }
}
### Now combine into one fac file and one cdr file

  fac_pnr_dxpx <- full_join(CAC003_ip_fac_all, CAC003_op_fac_all) %>%
    filter(`All Suggested` > 0, `Dx/Proc` != "---") %>%
    separate(`Dx/Proc`, c("DxPx", "Code"), sep = "[:]") %>%
    separate(Code, c("Code"), sep = "[-]") %>%
    filter(DxPx != "Admit Dx") %>%
    mutate(DxPx = str_replace(DxPx, "ICD Px", "Px")) %>%
    mutate(DxPx = str_replace(DxPx, "HCPCS Px", "CPT")) %>%
    mutate(Code = str_replace(Code, " ", "")) %>% 
    group_by(Facility, Code, DxPx, PatCls, QTR, Hub) %>%
    summarize(
      `All Suggested` = sum(`All Suggested`),
      Accepted = sum(Accepted),
      `All Coded` = sum(`All Coded`),
      `% Precision` = Accepted / `All Suggested`,
      `% Recall` = `Accepted` / `All Coded`
    )

  cdr_pnr_dxpx <- full_join(CAC003_ip_cdr_all, CAC003_op_cdr_all) %>%
    filter(`All Suggested` > 9, `Dx/Proc` != "---") %>%
    rename(Coder = `Last Reviewer/Coder`) %>%
    separate(Coder, c("Coder", "coderid"), sep = "[()]") %>%
    separate(`Dx/Proc`, c("DxPx", "Code"), sep = "[:]") %>%
    filter(DxPx != "Admit Dx") %>%
    mutate(DxPx = str_replace(DxPx, "ICD Px", "Px")) %>%
    mutate(DxPx = str_replace(DxPx, "HCPCS Px", "CPT")) %>%
    separate(Code, c("Code"), sep = "[-]") %>%
    mutate(Code = str_replace(Code, " ", "")) %>% 
    group_by(Coder, Code) %>%
    mutate(
      `All Suggested` = sum(`All Suggested`),
      Accepted = sum(Accepted),
      `All Coded` = sum(`All Coded`),
      `% Precision` = Accepted / `All Suggested`,
      `% Recall` = `Accepted` / `All Coded`
    ) %>% 
    group_by(Hub) %>% 
    mutate(pcn_rnk_hub = min_rank(`% Precision`))
 
  cde_pnr_dxpx <- fac_pnr_dxpx %>%
    group_by(Code, DxPx, PatCls, QTR, Hub) %>%
    summarize(
      `All Suggested` = sum(`All Suggested`),
      Accepted = sum(Accepted),
      `All Coded` = sum(`All Coded`),
      `% Precision` = Accepted / `All Suggested`,
      `% Recall` = `Accepted` / `All Coded`
    ) %>%
    filter(`All Suggested` > 24) %>%
    arrange(desc(`All Suggested`)) %>% 
    left_join(ccmcc)
  
  cde_pnr_ccmcc <- cde_pnr_dxpx %>% 
    filter(is_ccmcc == "TRUE") %>% 
    select(-is_ccmcc)


rm(CAC003_op_fac_all, CAC003_ip_fac_all)
rm(CAC003_op_cdr_all, CAC003_ip_cdr_all)


## CAC007 All Codes Entry Method Summary ======================================
## Same concepts as above. OK to ignore warnings about missing column names.
## Number of columns will actually vary as 360 does not report on columns that had
## no data.
### * CAC007 Coder Level ##########
### CAC007_ip_dx_cdr.xlsx
for (i in 1:hub) {
  xlsx_file <- paste0("./data/", i, "/CAC007_ip_dx_cdr.xlsx")
  CAC007_ip_dx_cdr <- read_xlsx(xlsx_file) %>%
    mutate(Hub = i, QTR = qtr, PatCls = "I", PxDx = "Dx", cdrfac = "cdr") %>%
    replace(is.na(.), 0)
  CAC007_ip_dx_cdr <- CAC007_ip_dx_cdr[grep("^X", colnames(CAC007_ip_dx_cdr), invert = TRUE)]
  if (i == 1) {
    CAC007_ip_dx_cdr_all <- CAC007_ip_dx_cdr
    next
  }
  CAC007_ip_dx_cdr_all <- full_join(CAC007_ip_dx_cdr_all, CAC007_ip_dx_cdr)
  rm(CAC007_ip_dx_cdr)
}
### CAC007_op_dx_cdr.xlsx
for (i in 1:hub) {
  xlsx_file <- paste0("./data/", i, "/CAC007_op_dx_cdr.xlsx")
  CAC007_op_dx_cdr <- read_xlsx(xlsx_file) %>%
    mutate(Hub = i, QTR = qtr, PatCls = "O", PxDx = "Dx", cdrfac = "cdr") %>%
    replace(is.na(.), 0)
  CAC007_op_dx_cdr <- CAC007_op_dx_cdr[grep("^X", colnames(CAC007_op_dx_cdr), invert = TRUE)]
  if (i == 1) {
    CAC007_op_dx_cdr_all <- CAC007_op_dx_cdr
    next
  }
  CAC007_op_dx_cdr_all <- full_join(CAC007_op_dx_cdr_all, CAC007_op_dx_cdr)
  rm(CAC007_op_dx_cdr)
}

### CAC007_ip_px_cdr.xlsx
for (i in 1:hub) {
  xlsx_file <- paste0("./data/", i, "/CAC007_ip_px_cdr.xlsx")
  CAC007_ip_px_cdr <- read_xlsx(xlsx_file) %>%
    mutate(Hub = i, QTR = qtr, PatCls = "I", PxDx = "Px", cdrfac = "cdr") %>%
    replace(is.na(.), 0)
  CAC007_ip_px_cdr <- CAC007_ip_px_cdr[grep("^X", colnames(CAC007_ip_px_cdr), invert = TRUE)]
  if (i == 1) {
    CAC007_ip_px_cdr_all <- CAC007_ip_px_cdr
    next
  }
  CAC007_ip_px_cdr_all <- full_join(CAC007_ip_px_cdr_all, CAC007_ip_px_cdr)
  rm(CAC007_ip_px_cdr)
}
### CAC007_op_px_cdr.xlsx
for (i in 1:hub) {
  xlsx_file <- paste0("./data/", i, "/CAC007_op_px_cdr.xlsx")
  CAC007_op_px_cdr <- read_xlsx(xlsx_file) %>%
    mutate(Hub = i, QTR = qtr, PatCls = "O", PxDx = "Px", cdrfac = "cdr") %>%
    replace(is.na(.), 0)
  CAC007_op_px_cdr <- CAC007_op_px_cdr[grep("^X", colnames(CAC007_op_px_cdr), invert = TRUE)]
  if (i == 1) {
    CAC007_op_px_cdr_all <- CAC007_op_px_cdr
    next
  }
  CAC007_op_px_cdr_all <- full_join(CAC007_op_px_cdr_all, CAC007_op_px_cdr) %>%
    replace(is.na(.), 0)
  rm(CAC007_op_px_cdr)
}
### Join all
{
  cdr_entry <- full_join(CAC007_ip_dx_cdr_all, CAC007_op_dx_cdr_all) %>%
    replace(is.na(.), 0)
  rm(CAC007_ip_dx_cdr_all, CAC007_op_dx_cdr_all)
  cdr_entry <- full_join(cdr_entry, CAC007_ip_px_cdr_all) %>%
    replace(is.na(.), 0)
  rm(CAC007_ip_px_cdr_all)
  cdr_entry <- full_join(cdr_entry, CAC007_op_px_cdr_all) %>%
    replace(is.na(.), 0)
  rm(CAC007_op_px_cdr_all)
}
### * CAC007 Facility Level ###########
### CAC007_ip_dx_fac.xlsx
for (i in 1:hub) {
  xlsx_file <- paste0("./data/", i, "/CAC007_ip_dx_fac.xlsx")
  CAC007_ip_dx_fac <- read_xlsx(xlsx_file) %>%
    mutate(Hub = i, QTR = qtr, PatCls = "I", PxDx = "Dx", cdrfac = "fac") %>%
    replace(is.na(.), 0)
  CAC007_ip_dx_fac <- CAC007_ip_dx_fac[grep("^X", colnames(CAC007_ip_dx_fac), invert = TRUE)]
  if (i == 1) {
    CAC007_ip_dx_fac_all <- CAC007_ip_dx_fac
    next
  }
  CAC007_ip_dx_fac_all <- full_join(CAC007_ip_dx_fac_all, CAC007_ip_dx_fac)
  rm(CAC007_ip_dx_fac)
}
### CAC007_op_dx_fac.xlsx
for (i in 1:hub) {
  xlsx_file <- paste0("./data/", i, "/CAC007_op_dx_fac.xlsx")
  CAC007_op_dx_fac <- read_xlsx(xlsx_file) %>%
    mutate(Hub = i, QTR = qtr, PatCls = "O", PxDx = "Dx", cdrfac = "fac") %>%
    replace(is.na(.), 0)
  CAC007_op_dx_fac <- CAC007_op_dx_fac[grep("^X", colnames(CAC007_op_dx_fac), invert = TRUE)]
  if (i == 1) {
    CAC007_op_dx_fac_all <- CAC007_op_dx_fac
    next
  }
  CAC007_op_dx_fac_all <- full_join(CAC007_op_dx_fac_all, CAC007_op_dx_fac)
  rm(CAC007_op_dx_fac)
}

### CAC007_ip_px_fac.xlsx
for (i in 1:hub) {
  xlsx_file <- paste0("./data/", i, "/CAC007_ip_px_fac.xlsx")
  CAC007_ip_px_fac <- read_xlsx(xlsx_file) %>%
    mutate(Hub = i, QTR = qtr, PatCls = "I", PxDx = "Px", cdrfac = "fac") %>%
    replace(is.na(.), 0)
  CAC007_ip_px_fac <- CAC007_ip_px_fac[grep("^X", colnames(CAC007_ip_px_fac), invert = TRUE)]
  if (i == 1) {
    CAC007_ip_px_fac_all <- CAC007_ip_px_fac
    next
  }
  CAC007_ip_px_fac_all <- full_join(CAC007_ip_px_fac_all, CAC007_ip_px_fac)
  rm(CAC007_ip_px_fac)
}
### CAC007_op_px_fac.xlsx
for (i in 1:hub) {
  xlsx_file <- paste0("./data/", i, "/CAC007_op_px_fac.xlsx")
  CAC007_op_px_fac <- read_xlsx(xlsx_file) %>%
    mutate(Hub = i, QTR = qtr, PatCls = "O", PxDx = "Px", cdrfac = "fac") %>%
    replace(is.na(.), 0)
  CAC007_op_px_fac <- CAC007_op_px_fac[grep("^X", colnames(CAC007_op_px_fac), invert = TRUE)]
  if (i == 1) {
    CAC007_op_px_fac_all <- CAC007_op_px_fac
    next
  }
  CAC007_op_px_fac_all <- full_join(CAC007_op_px_fac_all, CAC007_op_px_fac)
  rm(CAC007_op_px_fac)
}
### Join all
{
  fac_entry <- full_join(CAC007_ip_dx_fac_all, CAC007_op_dx_fac_all) %>%
    replace(is.na(.), 0)
  rm(CAC007_ip_dx_fac_all, CAC007_op_dx_fac_all)
  fac_entry <- full_join(fac_entry, CAC007_ip_px_fac_all) %>%
    replace(is.na(.), 0)
  rm(CAC007_ip_px_fac_all)
  fac_entry <- full_join(fac_entry, CAC007_op_px_fac_all) %>%
    replace(is.na(.), 0)
  rm(CAC007_op_px_fac_all)
}
## CAC008 Auto-suggested codes listing ==============
# If anything is slow, it's this section.  If you decide you don't want it,
# just select the code down to where IP004 starts and hit ctrl+shift+c - this
# will `comment` it and stop it from running.
# also if you’re looking for raw speed, try data.table::fread()
# as above ignore warnings about number of columns not multiple of vector
# we'll fix that later
# CAC008.xlsx
for (i in 1:hub) {
  xlsx_file <- paste0("./data/", i, "/CAC008.xlsx")
  CAC008_hub <- read_xlsx(xlsx_file, col_types = cols(`Visit ID` = col_character(), `Total Codes` = col_skip(),
                                                    MRN = col_skip(), `Patient Name`=col_skip(), `Entry Method`=col_skip(), `Admit Date`=col_skip())) %>%
    fill(`Dx/Proc`) %>% 
    filter(!is.na(`Visit ID`)) %>% 
    separate(Coder, c("Coder", "coderid"), sep = "[()]") %>%
    separate(`Dx/Proc`, c("DxPx", "Code"), sep = "[:]") %>%
    filter(DxPx != "Admit Dx") %>%
    mutate(DxPx = str_replace(DxPx, "ICD Px", "Px")) %>%
    mutate(DxPx = str_replace(DxPx, "HCPCS Px", "CPT")) %>%
    separate(Code, c("Code"), sep = "[-]") %>%
    mutate(Code = str_replace(Code, " ", ""))  %>% 
    mutate(Hub = i, QTR = qtr) 
  if (i == 1) {
    cdr_acct_rej <- CAC008_hub
    next
  }
  cdr_acct_rej <- full_join(cdr_acct_rej, CAC008_hub)
  rm(CAC008_hub)
}
cdr_acct_rej <- cdr_acct_rej %>% 
  group_by(`Visit ID`) %>% 
  mutate(tot_cds_acct=n())
## IP004 Primary and Secondary DRG Listing ==============================
# See important note in 360 file prep document about secondary DRGs
# Ignore warnings.  21 columns
# IP004.xlsx
for (i in 1:hub) {
  xlsx_file <- paste0("./data/", i, "/IP004.xlsx")
  IP004_hub <- read_xlsx(xlsx_file, col_types = cols( MRN=col_skip(), `Total Visits`=col_skip(), Name=col_skip(), `Admit Date`=col_skip(), 
    `Visit ID` = col_character(),
    `SOI/ROM` = col_character(), `SOI/ROM_1` = col_character()
  )) %>%
    fill(Coder, `Financial Class`) %>% 
    filter(!is.na(`Visit ID`)) %>% 
    separate(Coder, c("Coder", "coderid"), sep = "[()]") %>%
    separate(`Prin Dx`, c("Dx"), sep = "[-]") %>%
    separate(`Prin Proc`, c("Px"), sep = "[-]") %>%
    separate(`Pt Type`, c("pclass", "ptype"), sep="[/]") %>% 
    mutate(Hub = i, QTR = qtr,Fac=str_sub(`Visit ID`, 1, 3)) 
  if (i == 1) {
    cdr_acct <- IP004_hub
    next
  }
  cdr_acct <- full_join(cdr_acct, IP004_hub) 
  rm(IP004_hub)
}


Pt <- cdr_acct %>% 
  select (Coder, coderid,`Visit ID`, `Disch Date`, LOS, `pclass`, ptype, Fac, `Disch Disposition`, `Dx`, `Px`)
DRG <- cdr_acct %>%
  select(`Visit ID`, DRG, Wgt, `SOI/ROM`, ALOS, GLOS) %>%
  separate(DRG, c("DRG","Grouper"),sep="[()]") %>%
  separate(DRG, c("DRG"), sep="[-]")
DRG1 <-cdr_acct %>%
  select(`Visit ID`, DRG_1, Wgt_1, `SOI/ROM_1`, ALOS_1, GLOS_1) %>%
  separate(DRG_1, c("DRG","Grouper"),sep="[()]") %>%
  separate(DRG, c("DRG"), sep="[-]") %>%
  rename(Wgt=Wgt_1, `SOI/ROM`= "SOI/ROM_1", ALOS=ALOS_1, GLOS=GLOS_1)
# Bring Primary DRGs and Secondary DRGs into one table
DRG <- full_join(DRG, DRG1)
rm(DRG1)
#Now designate MS vs APR DRG Info - this is the equivalent of find and replace
#As long as the various state APR systems have `APR` in the desciption they will all be classified to APRDRG
DRG$Grouper[grepl("APR", DRG$Grouper, ignore.case=FALSE)] <- "APRDRG"
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
cdr_acct_1 <- full_join(APRMS, Pt)
rm(noapr, noms,DRG, Pt, APRMS, APR, MS)

write_excel_csv(pt_cdr_drg, file = paste0("./360allhub/pt_cdr_drg", qtr,"_",hub,".xlsx"))
## Prod016 #######################
# Prod016_ip.xlsx
for (i in 1:hub) {
  xlsx_file <- paste0("./data/", i, "/Prod016_ip.xlsx")
  Prod016_ip <- read_xlsx(xlsx_file) %>%
    mutate(Hub = i, QTR = qtr, PatCls = "I") %>%
    replace(is.na(.), 0)
  if (i == 1) {
    Prod016_ip_all <- Prod016_ip
    next
  }
  Prod016_ip_all <- full_join(Prod016_ip_all, Prod016_ip)
  rm(Prod016_ip)
}
# Prod016_op.xlsx
for (i in 1:hub) {
  xlsx_file <- paste0("./data/", i, "/Prod016_op.xlsx")
  Prod016_op <- read_xlsx(xlsx_file) %>%
    mutate(Hub = i, QTR = qtr, PatCls = "O") %>%
    replace(is.na(.), 0)
  if (i == 1) {
    Prod016_op_all <- Prod016_op
    next
  }
  Prod016_op_all <- full_join(Prod016_op_all, Prod016_op)
  rm(Prod016_op)
}
Prod016 <- full_join(Prod016_ip_all, Prod016_op_all)
rm(Prod016_ip_all, Prod016_op_all)

## Import misc external data files ###########
{
  ccmcc <- read_xlsx("./data/external/2019ccmccDXlist_nodecimal.xlsx")
  ICD10CM <- read_xlsx("./data/external/201910cmlist_nodecimal.xlsx")
  drg <- read_xlsx("./data/external/DRGGROUPS.xlsx")
  keys <- read_xlsx("./data/external/keys.xlsx")
  prterms <- read_xlsx("./data/external/prterms.xlsx")
  lowprecis <- read_xlsx("./data/external/lowprecis.xlsx")
  lowrecall <- read_xlsx("./data/external/lowrecall.xlsx")
}
## Benchmarks: ======
## 12/12/2018: 51 seconds including CAC008 (19.27 seconds excluding)
# Section II: Reshape Data ------------------
## CDI09d --------------------


## CAC001 ----------
### create CAC001_all_fac
# attempting to add summary columns
# really bad at this
# need to remove outliers but don't want to remove the individual coder data
