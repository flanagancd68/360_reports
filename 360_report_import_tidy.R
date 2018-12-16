# Packages required.  Uncomment line below as needed to install
# install.packages("tidyverse", "plotly")
library(plotly, tidyverse)

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
#  If your 360 csv files are not formatted EXACTLY as stated, you may receive
#  errors, or worse, results that are not accurate
#  If you DO get an error check your environment for the value of `csv_file`.
#  If it is present (it may not always be) it will tell you the folder and file
#  that was active at the time of the error.
#  Correct the file, then run the report section again to assure data integrity
## file check  Should be 21 CSV files in each sub-folder ======================
## This just does a count of the files and sends a warning if there is a
## mismatch. it doesn't check for proper formatting
for (i in 1:hub) {
  path <- paste0("./data/", i)
  file_check <- length(list.files(path, pattern = "*.csv"))
  assign(paste0("hub_", i), file_check)
  if (file_check < 21) {
    file_check
    stop(paste0("Check: hub ", i, " has ", file_check, " csv files.  Should be 21."))
  }
  rm(file_check)
  rm(list = ls(pattern = "\\hub_."))
  rm(i, path)
}

# read in system facility designations
fac_codes <- read_csv("./data/extdata/fac_codes.csv")
# read in ccmcc table
ccmcc <- read_csv("./data/extdata/ccmcc2019.csv")

## 09d Physician query listing ================================================
## These routines will import your hub-specific file from each sub folder, do
## some minor clean up, and then consolidate into a single dataframe.  We add columns
## for the hub and quarter being evaluated.  The for loop just tells R to repeat
## the process for however many hubs you specified above under "collect user
## info".  You can ignore the warnings.  The 360 report has duplicate column
## names - we will fix these later.

for (i in 1:hub) {
  csv_file <- paste0("./data/", i, "/09d.csv")
  CDI09d_hub <- read_csv(csv_file, col_types = cols(
    `Query Author` = col_skip(), Facility = col_skip(), `Total Visits` = col_skip(), `Total Queries` = col_skip(), `Visit ID` = col_character(),
    `SOI/ROM` = col_character(), `SOI/ROM_1` = col_character(),
    MRN = col_character()
  )) %>%
    mutate(Hub = i, QTR = qtr, Fac = str_sub(`Visit ID`, 1, 3)) %>%
    replace(is.na(.), 0)
  if (i == 1) {
    CDI09d_all <- CDI09d_hub
    next
  }
  CDI09d_all <- full_join(CDI09d_all, CDI09d_hub) %>%
    select(
      -MRN, -`Patient Name`, -`Gender`,
      -`Admit Date`
    ) %>%
    filter(`Visit ID` > 0)
  rm(CDI09d_hub)
}



## Now look at your "Environment" window.  obs = rows and variables = columns
## "CDI09d_all" should have22 columns (or 23 if you imported comments) and then however many
## rows of coder queries.  Check these numbers to see whether they make sense.
## If the number of columns is off, or you have too many/two vew observations
## you probably have a formatting issue with one of your 09d.csv files.
##
## This next section cleans up the files a bit and finally saves to the consolidated folder.
{
  CDI09d_all <- CDI09d_all %>%
    rename(Facility = Facility_1, Coder = `Query Author_1`) %>% # renames columns for standard reporting - do not change or it will break the script
    separate(Coder, c("Coder", "Coderid"), sep = "[()]") %>% # our system passes both the proper name and the logon name;
    # this action splits them into two colums.  If you don't need this
    # place a `#` sign in front of the word `separate`
    separate(`Queried Provider`, c("Queried Provider", "QueryMDid"), sep = "[()]") %>% # same as above for physician id; place a # before the word `separate` if you don't need this
    separate(`Responding Provider`, c("Responding Provider", "RespMDid"), sep = "[()]") %>% # same as above for physician id
    left_join(fac_codes) %>% # this action links the facility name in the reports with our internal facility code and regional
    # designations.  I will use FAC in this script so even if you only have one facility
    # you will want to create a fac_codes.csv file and put it in your extdata folder
    select(-name_internal) # this action removes the redundant internal facility name

  write_csv(CDI09d_all, paste0("./data/consolidated/CDI09_all_", qtr, "_.csv")) # this action writes a consolidated CSV file with all hubs

  CDI09d_cdr <- CDI09d_all %>%
    filter(`Provider Response` != "Withdrawn/Not Applicable") %>%
    group_by(Coder, Facility) %>%
    summarise(
      Queries_n = n(),
      Avg_days = mean(`Days to Respond`)
    )
  CDI09d_qry <- CDI09d_all %>%
    filter(`Provider Response` != "Withdrawn/Not Applicable") %>%
    group_by(`Query Template`, `Facility`) %>%
    summarize(Queries_n = n()) %>%
    ungroup() %>%
    group_by(`Query Template`) %>%
    mutate(`Temp_avg` = mean(Queries_n))

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
### CAC001_ip_dx.csv
{
  for (i in 1:hub) {
    csv_file <- paste0("./data/", i, "/cac001_ip_dx.csv")
    CAC001_ip_dx <- read_csv(csv_file, col_types = cols(
      `% Precision` = col_number(), `% Recall` = col_number(), Accepted = col_number(),
      `All Coded` = col_number(), `All Suggested` = col_number(),
      `From CAC` = col_skip()
    )) %>%
      mutate(Hub = i, QTR = qtr, PatCls = "I", DxPx = "Dx") %>%
      replace(is.na(.), 0)
    if (i == 1) {
      CAC001_ip_dx_all <- CAC001_ip_dx
      next
    }
    CAC001_ip_dx_all <- full_join(CAC001_ip_dx_all, CAC001_ip_dx)
    rm(CAC001_ip_dx)
  }
  ### CAC001_ip_px.csv
  for (i in 1:hub) {
    csv_file <- paste0("./data/", i, "/cac001_ip_px.csv")
    CAC001_ip_px <- read_csv(csv_file, col_types = cols(
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
  ### CAC001_op_dx.csv
  for (i in 1:hub) {
    csv_file <- paste0("./data/", i, "/cac001_op_dx.csv")
    CAC001_op_dx <- read_csv(csv_file, col_types = cols(
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
  ### CAC001_op_px.csv
  for (i in 1:hub) {
    csv_file <- paste0("./data/", i, "/cac001_op_px.csv")
    CAC001_op_px <- read_csv(csv_file, col_types = cols(
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
  write_csv(CAC001_all, paste0("./data/consolidated/CAC001_all", qtr, "_.csv")) # this action writes a consolidated CSV file with all hubs
}

CAC001_all_fac <- CAC001_all %>%
  filter(Facility != "Total") %>%
  left_join(fac_codes) %>%
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
  distinct()
### create CAC001_all_cdr
cdr_fac <- CAC001_all %>% # many of our coders code across facilities and across hubs.  For purposes of a 1:1 relationship between coder
  filter(Facility != "Total") %>% # and facility, this routine looks at the facility with highest coded count and ties that facility to the coder
  group_by(Coderid) %>%
  arrange(desc(`All Coded`)) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  select(Coder, Coderid, Facility)
#
CAC001_all_cdr <- CAC001_all %>%
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
  left_join(CAC001_all_fac) %>%
  rename(Lead_Faciity = Facility)

write_csv(CAC001_all_cdr, paste0("./data/consolidated/CAC001_all_cdr", qtr, "_.csv")) # this action writes a consolidated CSV file with all hubs
write_csv(CAC001_all_fac, paste0("./data/consolidated/CAC001_all_fac", qtr, "_.csv")) # this action writes a consolidated CSV file with all hubs

## CAC003 auto-suggested codes precision and recall by code ===================
## same concept as above.  We can grab Dx/Px from the column with the code, so we
## just need to tag each row as IP or OP and the CDR or FAC.  We will consolidate
## into one CDR table and one FAC table.
## You may safely ignore warnings about number of columns not multiple of vector
## we'll fix that later
## You should have 12 columns.
### CAC003_ip_cdr.csv
{
  for (i in 1:hub) {
    csv_file <- paste0("./data/", i, "/CAC003_ip_cdr.csv")
    CAC003_ip_cdr <- read_csv(csv_file, col_types = cols(
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

  ### CAC003_ip_fac.csv
  for (i in 1:hub) {
    csv_file <- paste0("./data/", i, "/CAC003_ip_fac.csv")
    CAC003_ip_fac <- read_csv(csv_file, col_types = cols(
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

  ### CAC003_op_cdr.csv
  for (i in 1:hub) {
    csv_file <- paste0("./data/", i, "/CAC003_op_cdr.csv")
    CAC003_op_cdr <- read_csv(csv_file, col_types = cols(
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

  ### CAC003_op_fac.csv
  for (i in 1:hub) {
    csv_file <- paste0("./data/", i, "/CAC003_op_fac.csv")
    CAC003_op_fac <- read_csv(csv_file, col_types = cols(
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
### Now combine into two files
{
  CAC003_fac <- full_join(CAC003_ip_fac_all, CAC003_op_fac_all) %>%
    filter(`All Suggested` > 0, `Dx/Proc` != "---") %>%
    separate(`Dx/Proc`, c("DxPx", "Code"), sep = "[:]") %>%
    separate(Code, c("Code"), sep = "[-]") %>%
    filter(DxPx != "Admit Dx") %>%
    mutate(DxPx = str_replace(DxPx, "ICD Px", "Px")) %>%
    mutate(DxPx = str_replace(DxPx, "HCPCS Px", "CPT")) %>%
    mutate(Code = str_replace(Code, " ", ""))

  CAC003_cdr <- full_join(CAC003_ip_cdr_all, CAC003_op_cdr_all) %>%
    filter(`All Suggested` > 0, `Dx/Proc` != "---") %>%
    rename(Coder = `Last Reviewer/Coder`) %>%
    separate(Coder, c("Coder", "id"), sep = "[()]") %>%
    separate(`Dx/Proc`, c("DxPx", "Code"), sep = "[:]") %>%
    filter(DxPx != "Admit Dx") %>%
    mutate(DxPx = str_replace(DxPx, "ICD Px", "Px")) %>%
    mutate(DxPx = str_replace(DxPx, "HCPCS Px", "CPT")) %>%
    separate(Code, c("Code"), sep = "[-]") %>%
    mutate(Code = str_replace(Code, " ", ""))
  CAC003_code_all <- CAC003_fac %>%
    group_by(Code, DxPx, PatCls, QTR) %>%
    summarize(
      `All Suggested` = sum(`All Suggested`),
      Accepted = sum(Accepted),
      `All Coded` = sum(`All Coded`),
      `% Precision` = Accepted / `All Suggested`,
      `% Recall` = `Accepted` / `All Coded`
    ) %>%
    filter(`All Suggested` > 100) %>%
    arrange(desc(`All Suggested`))

  CAC003_code_all_mcc <- CAC003_code_all %>%
    filter(DxPx == "Dx", PatCls == "I") %>%
    left_join(ccmcc) %>%
    filter(CCMCC == ("mcc"))
  top_n(desc(`All Suggested`), 20)
}
rm(CAC003_op_fac_all, CAC003_ip_fac_all)
rm(CAC003_op_cdr_all, CAC003_ip_cdr_all)


## CAC007 All Codes Entry Method Summary ======================================
## Same concepts as above. OK to ignore warnings about missing column names.
## Number of columns will actually vary as 360 does not report on columns that had
## no data.
### * CAC007 Coder Level ##########
### CAC007_ip_dx_cdr.csv
for (i in 1:hub) {
  csv_file <- paste0("./data/", i, "/CAC007_ip_dx_cdr.csv")
  CAC007_ip_dx_cdr <- read_csv(csv_file) %>%
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
### CAC007_op_dx_cdr.csv
for (i in 1:hub) {
  csv_file <- paste0("./data/", i, "/CAC007_op_dx_cdr.csv")
  CAC007_op_dx_cdr <- read_csv(csv_file) %>%
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

### CAC007_ip_px_cdr.csv
for (i in 1:hub) {
  csv_file <- paste0("./data/", i, "/CAC007_ip_px_cdr.csv")
  CAC007_ip_px_cdr <- read_csv(csv_file) %>%
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
### CAC007_op_px_cdr.csv
for (i in 1:hub) {
  csv_file <- paste0("./data/", i, "/CAC007_op_px_cdr.csv")
  CAC007_op_px_cdr <- read_csv(csv_file) %>%
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
  CAC007_cdr_all <- full_join(CAC007_ip_dx_cdr_all, CAC007_op_dx_cdr_all) %>%
    replace(is.na(.), 0)
  rm(CAC007_ip_dx_cdr_all, CAC007_op_dx_cdr_all)
  CAC007_cdr_all <- full_join(CAC007_cdr_all, CAC007_ip_px_cdr_all) %>%
    replace(is.na(.), 0)
  rm(CAC007_ip_px_cdr_all)
  CAC007_cdr_all <- full_join(CAC007_cdr_all, CAC007_op_px_cdr_all) %>%
    replace(is.na(.), 0)
  rm(CAC007_op_px_cdr_all)
}
### * CAC007 Facility Level ###########
### CAC007_ip_dx_fac.csv
for (i in 1:hub) {
  csv_file <- paste0("./data/", i, "/CAC007_ip_dx_fac.csv")
  CAC007_ip_dx_fac <- read_csv(csv_file) %>%
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
### CAC007_op_dx_fac.csv
for (i in 1:hub) {
  csv_file <- paste0("./data/", i, "/CAC007_op_dx_fac.csv")
  CAC007_op_dx_fac <- read_csv(csv_file) %>%
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

### CAC007_ip_px_fac.csv
for (i in 1:hub) {
  csv_file <- paste0("./data/", i, "/CAC007_ip_px_fac.csv")
  CAC007_ip_px_fac <- read_csv(csv_file) %>%
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
### CAC007_op_px_fac.csv
for (i in 1:hub) {
  csv_file <- paste0("./data/", i, "/CAC007_op_px_fac.csv")
  CAC007_op_px_fac <- read_csv(csv_file) %>%
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
  CAC007_fac_all <- full_join(CAC007_ip_dx_fac_all, CAC007_op_dx_fac_all) %>%
    replace(is.na(.), 0)
  rm(CAC007_ip_dx_fac_all, CAC007_op_dx_fac_all)
  CAC007_fac_all <- full_join(CAC007_fac_all, CAC007_ip_px_fac_all) %>%
    replace(is.na(.), 0)
  rm(CAC007_ip_px_fac_all)
  CAC007_fac_all <- full_join(CAC007_fac_all, CAC007_op_px_fac_all) %>%
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
# CAC008.csv
for (i in 1:hub) {
  csv_file <- paste0("./data/", i, "/CAC008.csv")
  CAC008_hub <- read_csv(csv_file, col_types = cols(`Visit ID` = col_character(), MRN = col_skip())) %>%
    mutate(Hub = i, QTR = qtr) %>%
    replace(is.na(.), 0)
  if (i == 1) {
    CAC008_all <- CAC008_hub
    next
  }
  CAC008_all <- full_join(CAC008_all, CAC008_hub) %>%
    select(
      -MRN, -`Dx/Proc`, -`Total Codes`, -`Patient Name`,
      -`Admit Date`, -`Entry Method`
    ) %>%
    filter(`Visit ID` > 0)
  rm(CAC008_hub)
}
## IP004 Primary and Secondary DRG Listing ==============================
# See important note in 360 file prep document about secondary DRGs
# Ignore warnings.  21 columns
# IP004.csv
for (i in 1:hub) {
  csv_file <- paste0("./data/", i, "/IP004.csv")
  IP004_hub <- read_csv(csv_file, col_types = cols(
    `Visit ID` = col_character(),
    `SOI/ROM` = col_character(), `SOI/ROM_1` = col_character(),
    MRN = col_character()
  )) %>%
    mutate(Hub = i, QTR = qtr) %>%
    replace(is.na(.), 0)
  if (i == 1) {
    IP004_all <- IP004_hub
    next
  }
  IP004_all <- full_join(IP004_all, IP004_hub) %>%
    select(
      -MRN, -`Coder`, -`Financial Class`, -`Total Visits`, -`Name`,
      -`Admit Date`
    ) %>%
    filter(`Visit ID` > 0)
  rm(IP004_hub)
}
## Prod016 #######################
# Prod016_ip.csv
for (i in 1:hub) {
  csv_file <- paste0("./data/", i, "/Prod016_ip.csv")
  Prod016_ip <- read_csv(csv_file) %>%
    mutate(Hub = i, QTR = qtr, PatCls = "I") %>%
    replace(is.na(.), 0)
  if (i == 1) {
    Prod016_ip_all <- Prod016_ip
    next
  }
  Prod016_ip_all <- full_join(Prod016_ip_all, Prod016_ip)
  rm(Prod016_ip)
}
# Prod016_op.csv
for (i in 1:hub) {
  csv_file <- paste0("./data/", i, "/Prod016_op.csv")
  Prod016_op <- read_csv(csv_file) %>%
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
  ccmcc <- read_csv("./data/extdata/2019ccmccDXlist_nodecimal.csv")
  ICD10CM <- read_csv("./data/extdata/201910cmlist_nodecimal.csv")
  drg <- read_csv("./data/extdata/DRGGROUPS.csv")
  keys <- read_csv("./data/extdata/keys.csv")
  prterms <- read_csv("./data/extdata/prterms.csv")
  lowprecis <- read_csv("./data/extdata/lowprecis.csv")
  lowrecall <- read_csv("./data/extdata/lowrecall.csv")
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
