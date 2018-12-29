
# Read Funtions
read_360_file <- function(rpt, skp=0) {
  for (i in 1:hub) {  
    xl_file <- paste0("./data/internal/", qtr, "_", i, "_", rpt,".xlsx")
    hub <- read_xlsx(xl_file, skip=skp) %>% mutate(Hub=i, Qtr=qtr)

    if (i == 1) {
      allhubs <- hub
      next
    }
    allhubs <- suppressMessages(full_join(allhubs, hub))
  }
  assign(paste0(rpt,"_all"), allhubs, envir = globalenv())
}  

read_CAC007 <- function(rpt) {
    for (i in 1:hub) {  
      xl_file <- paste0("./data/internal/", qtr, "_", i, "_", rpt,".xlsx")
      cnames <- read_xlsx(xl_file, skip=10, n_max=0) %>% names()
      hub <- read_xlsx(xl_file, skip=12, col_names = cnames, col_types = "guess", guess_max = 100) %>% 
        mutate(Hub=i, Qtr=qtr) %>%
        replace(is.na(.), 0) %>% 
        filter(!(`Entry Method` %in% c("Coder","Totals", "Total")))

      if (i == 1) {
        allhubs <- hub
        next
      }
      allhubs <- suppressMessages(full_join(allhubs, hub))
    }
  allhubs <- allhubs[grep("^X", colnames(allhubs), invert = TRUE)]
  names(allhubs) <- gsub(" ", ".", names(allhubs))
  names(allhubs) <- gsub(" ", ".", names(allhubs))
  names(allhubs) <- gsub("[(]", "", names(allhubs))
  names(allhubs) <- gsub("[)]", "", names(allhubs))
  names(allhubs) <- gsub("[/]", ".", names(allhubs))
  names(allhubs) <- gsub("Ana", "MAN.Ana", names(allhubs))
  names(allhubs) <- gsub("Ane", "MAN.Ane", names(allhubs))
  names(allhubs) <- gsub("Ann", "CAC.Ann", names(allhubs))
  names(allhubs) <- gsub("Aut", "CAC.Aut", names(allhubs))
  names(allhubs) <- gsub("Codes", "CAC.Codes", names(allhubs))
  names(allhubs) <- gsub("CRS", "MAN.CRS", names(allhubs))
  names(allhubs) <- gsub("Doc", "CAC.Doc", names(allhubs))
  names(allhubs) <- gsub("Drc", "MAN.Drc", names(allhubs))
  names(allhubs) <- gsub("CAC.Annot.MAN", "CAC.Annot", names(allhubs))
  names(allhubs) <- gsub("Evi", "CAC.Evi", names(allhubs))
  names(allhubs) <- gsub("Doc.CAC.Evi", "Doc.Evi", names(allhubs))
  names(allhubs) <- gsub("Int", "OTH.Int", names(allhubs))
  names(allhubs) <- gsub("REC", "OTH.REC", names(allhubs))
  names(allhubs) <- gsub("Wor", "CDI.Wor", names(allhubs))
  names(allhubs) <- gsub("Tra", "OTH.Tra", names(allhubs))
  names(allhubs) <- gsub("Und", "CAC.Und", names(allhubs))
  names(allhubs) <- gsub("Val", "CAC.Val", names(allhubs))
  allhubs <- allhubs %>% 
  mutate_at(vars(matches("CAC")), as.numeric) %>% 
    mutate_at(vars(matches("CDI")), as.numeric) %>% 
    mutate_at(vars(matches("MAN")), as.numeric) %>% 
    mutate_at(vars(matches("OTH")), as.numeric) %>% 
    replace(is.na(.), 0) %>% 
    mutate(CAC = (select(., matches("CAC"))) %>% rowSums)%>%
    mutate(CDI = (select(., matches("CDI"))) %>% rowSums) %>%
    mutate(MAN = (select(., matches("MAN"))) %>% rowSums) %>%
    mutate(OTH = (select(., matches("OTH"))) %>% rowSums)
  
    assign(paste0(rpt,"_all"), allhubs, envir = globalenv())
  }    

##
read_ext_file <- function(rpt, skp=0) {
  xl_file <- paste0("./data/external/", rpt,".xlsx")
  new_file <- read_xlsx(xl_file, skip=skp) 
  assign(paste0(rpt), new_file, envir = globalenv())
}

read_cons_file <- function(rpt) {
  xl_file <- paste0("./data/cons/", rpt,".xlsx")
  new_file <- read_xlsx(xl_file) 
  assign(paste0(rpt), new_file, envir = globalenv())
}

# Write Functions --------------
write_cons_file <- function(df) {
  df_name <- deparse(substitute(df))
  f <- paste0("./data/cons/", df_name, "_all.csv")
  if (file.exists(f)) {
    df_all <- suppressMessages(read_csv(f)) 
    filter(df_all, Qtr < keep_thru) 
    suppressMessages(left_join(df_all, df))
    write_csv(df_all,f)
  } else {
    df_all <- df
    write_csv(df_all,f)
  }
  assign(paste0(df_name,"_all"), df_all, envir = globalenv())
}
# Separate Functions
sep_dxpx <- function(df, ...) {
df <-  df %>%  separate(`Dx/Proc`, c("DxPx", "Code"), sep = "[:]", extra="drop") %>% 
  dplyr::filter(`DxPx` != "Admit Dx") %>% 
  separate(`Code`, c("Code"), sep = "[-]", extra="drop") %>%
    mutate(`DxPx` = str_replace(DxPx, "ICD Px", "Px")) %>% 
    mutate(`DxPx` = str_replace(DxPx, "HCPCS Px", "CPT")) %>% 
    mutate(`Code` = str_replace(Code, " ", "")) 
}
sep_cdr <- function(df, ...) {
df <-  df %>% separate(Coder, c("Coder", "coderid"), sep = "[()]", extra="drop")
}


sep_lrcdr <- function(df, ...) {
  df <-  df %>% 
    rename(Coder = "Last Reviewer/Coder") %>% 
    separate(Coder, c("Coder", "coderid"), sep = "[()]", extra="drop")
}


# Join functions
join_fac <- function(df){
    suppressMessages(left_join(df, fac_list)) %>% 
    select(-name_internal) %>%
    replace(is.na(.), 0)
}

join_cdr_fac <- function(df){
  suppressMessages(left_join(df, cdr_fac)) %>%
    replace(is.na(.), 0)
}

#mutate
sum_pnr <- function(df){
  mutate(df,
`All Suggested` = sum(as.numeric(`All Suggested`)),
Accepted = sum(as.numeric(Accepted)),
`All Coded` = sum(as.numeric(`All Coded`)),
`% Precision` = Accepted / `All Suggested`,
`% Recall` = `Accepted` / `All Coded`
)
}

cde_entry_stats <- function(df){
  df <- df %>% 
  mutate(TotCodes = sum(`CAC`,`CDI`,`MAN`)) %>%
    mutate(`CAC%`= round((CAC/TotCodes)*100,2),
           `CDI%`=round((CDI/TotCodes)*100,2),
           `MAN%`=round((MAN/TotCodes)*100,2),
           `MANAuto` = (MAN.CRS.S+MAN.Drct.Code.S),
           `MANAuto%` = round((MANAuto/MAN)*100,2),
           `CAC+CDI%`=round((`CAC%`+`CDI%`),2))
}

group_grouper <- function(df){
  df$Grouper[grepl("APR", df$Grouper, ignore.case=FALSE)] <- "APRDRG"
  df$Grouper[grepl("Medicare", df$Grouper, ignore.case=FALSE)] <- "MSDRG"
  df$Grouper[grepl("MS", df$Grouper, ignore.case=FALSE)] <- "MSDRG"
  df$Grouper[grepl("TRI", df$Grouper, ignore.case=TRUE)] <- "MSDRG"
}
