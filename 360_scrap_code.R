# 360 project scrap code
# # 
# 
# #  filter(Facility=="---", `Last Reviewer/Coder` != "Grand Totals") %>% 
#   #  select(-Facility) %>% 
#  #   rename(Coder=`Last Reviewer/Coder`) %>% 
# #    separate(Coder, c("Coder","id"),sep="[(]") %>%  
#   #  mutate(id=str_replace(id,"[)]",""), PrcnMean_hub=round(mean(`% Precision`),2), 
#    #        PrcnMax_hub=round(max(`% Precision`),2),
#     #       RclMean_hub=round(mean(`% Recall`),2), 
#      #      RclMax_hub=round(max(`% Recall`),2), 
#       #     PrcnVar_hub=round(sd(`% Precision`),2), 
#        #    RclVar_hub=round(sd(`% Recall`),2), 
#         #   PrcnFromGoal=`% Precision`-75, 
#          #  RclFromGoal=`% Recall`-75,
#           # Quarter=qtr)
#   
#  # assign(paste0(df_name,"_cdr"),test_cdr)
# 
# # ####### CAC001 AUTO SUGGESTED CODES PRECISION AND RECALL Coder then facility
# # ####### 
# # ####### STILL NEED THIS (through line 79)???????????????
# # ## INPATIENT DX
# # HIM.CAC001d <- read_csv("HIM.CAC001d.csv", 
# #      col_types = cols(`% Precision` = col_number(), `% Recall` = col_number(), Accepted = col_number(), 
# #      `All Coded` = col_number(), `All Suggested` = col_number(), 
# #      `From CAC` = col_number()))
# # # Because we're going to be doing standard deviations, we want to exclude low volume rows that can skew the data.  
# # # Below, we are excluding coders who are in the bottom 10th perentile of auto-suggested codes
# # HIM.CAC001d <- HIM.CAC001d %>% filter(`All Suggested` > quantile(HIM.CAC001d$`All Suggested`, .1),  `Last Reviewer/Coder` != "Grand Totals")
# # 
# # # below adds a more descriptive label for the total row, and reformats the coder and names to be consistent throughout all reports
# # 
# # HIM.CAC001d <- HIM.CAC001d %>% mutate(Facility = str_replace(Facility, "---", "Total")) %>% 
# #   rename(Coder=`Last Reviewer/Coder`) %>% 
# #   separate(Coder, c("Coder","id"),sep="[(]") %>%  mutate(id=str_replace(id,"[)]",""))
# # 
# # # here is where we add in the new columns for summary information (max, mean and standard deviation) for Precision and Recall
# # # at the facility level
# # HIM.CAC001d <- HIM.CAC001d %>% group_by(Facility) %>%  mutate(`PrcnMean_fac`=round(mean(`% Precision`),2), 
# #                                                         PrcnMax_fac=round(max(`% Precision`),2),
# #                                                         RclMean_fac=round(mean(`% Recall`),2), 
# #                                                         RclMax_fac=round(max(`% Recall`),2), 
# #                                                         PrcnVar_fac=round(sd(`% Precision`),2), 
# #                                                         RclVar_fac=round(sd(`% Recall`),2))
# # 
# # # same information as above except we ungroup to get the values across the entire hub
# # # We also want to know how far away each coder's precision and recall is from the 75% goal
# # HIM.CAC001d <- HIM.CAC001d %>% ungroup() %>% mutate(PrcnMean_hub=round(mean(`% Precision`),2), 
# #                                                        PrcnMax_hub=round(max(`% Precision`),2),
# #                                                        RclMean_hub=round(mean(`% Recall`),2), 
# #                                                        RclMax_hub=round(max(`% Recall`),2), 
# #                                                        PrcnVar_hub=round(sd(`% Precision`),2), 
# #                                                        RclVar_hub=round(sd(`% Recall`),2), 
# #                                                        PrcnFromGoal=`% Precision`-75, 
# #                                                        RclFromGoal=`% Recall`-75,
# #                                                        Quarter=qtr,
# #                                                        Hub=hub)
# # 
# # # The final table should now have 25 columns!
# # # Save file for future enterprise reporting
# # write.csv(HIM.CAC001d, file = paste0("./3mallhub/HIM.CAC001d_", qtr,"_",hub,".csv"))
# ### GET CODER LIST
# 
# HIM.CAC001d<- read_csv("HIM.CAC001d.csv", 
#                        col_types = cols(`% Precision` = col_number(), `% Recall` = col_number(), Accepted = col_number(), 
#                                         `All Coded` = col_number(), `All Suggested` = col_number(), 
#                                         `From CAC` = col_number())) %>% 
#   filter(`Last Reviewer/Coder` != "Grand Totals") 
# 
# cdr_fac <- HIM.CAC001d %>% 
#     filter(Facility != "Total") %>% 
#   group_by(Coder) %>%
#   arrange(desc(`All Coded`)) %>%
#   filter(row_number()==1) %>% 
#     ungroup() %>% 
#   select(Coder, Facility)
# 
# ### using automatic totals in 3M report, create one row per coder file based on fac=Total
# ## add new variables
# ### ################################################
# 
# HIM.CAC001d_cdr <- HIM.CAC001d %>% 
#   filter(Facility=="---") %>% 
#   select(-Facility) %>% 
#   rename(Coder=`Last Reviewer/Coder`) %>% 
#   separate(Coder, c("Coder","id"),sep="[(]") %>%  
#   mutate(id=str_replace(id,"[)]",""), PrcnMean_hub=round(mean(`% Precision`),2), 
#                                                     PrcnMax_hub=round(max(`% Precision`),2),
#                                                     RclMean_hub=round(mean(`% Recall`),2), 
#                                                     RclMax_hub=round(max(`% Recall`),2), 
#                                                     PrcnVar_hub=round(sd(`% Precision`),2), 
#                                                     RclVar_hub=round(sd(`% Recall`),2), 
#                                                     PrcnFromGoal=`% Precision`-75, 
#                                                     RclFromGoal=`% Recall`-75,
#                                                     Quarter=qtr,
#                                                     Hub=hub) %>% 
#   left_join(cdr_fac)
# # no need to save the file since we'll be joining it later
# # 
# ####### CAC001 AUTO SUGGESTED CODES PRECISION AND RECALL FACILITY no second group
# ## INPATIENT DX
# HIM.CAC001d_fac <- HIM.CAC001d 
#   HIM.CAC001d_fac <- HIM.CAC001d_fac %>% 
#     filter(Facility !="---",`Accepted` > quantile(HIM.CAC001d_fac$`Accepted`, .1)) %>% 
#   #rename(Coder=`Last Reviewer/Coder`) %>% 
#   #separate(Coder, c("Coder","id"),sep="[(]") %>%  mutate(id=str_replace(id,"[)]",""), Coder = str_replace(Coder, "---", "Total") )%>% 
#  group_by(Facility) %>%  
#     mutate(PrcnMin_fac=round(min(`% Precision`),2),PrcnMax_fac=round(max(`% Precision`),2), 
#          RclMin_fac=round(min(`% Recall`),2), RclMax_fac=round(max(`% Recall`),2), 
#          PrcnVar_fac=round(sd(`% Precision`),2), RclVar_fac=round(sd(`% Recall`),2)) %>% 
#   mutate(`All Suggested` =sum(`All Suggested`), Accepted=sum(`Accepted`), 
#          `% Precision`= round((`Accepted`/`All Suggested`)*100,2), `All Coded`=sum(`All Coded`), 
#          `From CAC`=sum(`From CAC`), `% Recall` = round((`From CAC`/`All Coded`)*100,2))  %>% 
#   ungroup() %>% 
#   mutate(PrcnMean_hub=round(mean(`% Precision`),2), PrcnMax_hub=round(max(`% Precision`),2),
#          RclMean_hub=round(mean(`% Recall`),2), RclMax_hub=round(max(`% Recall`),2), PrcnVar_hub=round(sd(`% Precision`),2), 
#          RclVar_hub=round(sd(`% Recall`),2), PrcnFromGoal=`% Precision`-75,  
#          RclFromGoal=`% Recall`-75, Quarter=qtr, Hub=hub) %>% 
#   select(-`Last Reviewer/Coder`) %>% distinct()
#   #filter(Coder == "Total") %>% 
#   #select(-Coder, -id) 
# 
# ### HIM CAC003_cdr - Autosuggested codes Precision and recall by code
# 
# HIM.CAC003_cdr <- read_csv("HIM.CAC003_cdr.csv",
#                        col_types = cols(`% Precision` = col_number(), `% Recall` = col_number(), Accepted = col_number(), 
#                                         `All Coded` = col_number(), `All Suggested` = col_number(), `From CAC` = col_number())) %>% 
#   filter(`All Suggested` >=0) %>% rename(Coder=`Last Reviewer/Coder`) %>% 
#   separate(Coder, c("Coder","id"),sep="[(]") %>%  
#   mutate(id=str_replace(id,"[)]","")) %>%  
#   separate(`Dx/Proc`, c("DxPx","Code"),sep="[:]") %>% 
#   mutate(DxPx=str_replace(DxPx,"ICD Px", "Px"),  
#          Quarter=qtr,
#          Hub=hub) %>% 
#   separate(Code, c("Code"),sep="[-]") %>% 
#   mutate(Code=str_replace(Code, " ","")) %>% 
#   left_join(ICD10CM) %>% 
#   left_join(ccmcc) %>% 
#   left_join(CoderFac) %>% 
#   replace(is.na(.),0)
# 
# write.csv(HIM.CAC003_cdr, file = paste0("./3mallhub/HIM.CAC003_cdr", qtr,"_",hub,".csv"))
# ## FACILITY
# ## 
# HIM.CAC003_fac <- read_csv("HIM.CAC003_fac.csv",
#                            col_types = cols(`% Precision` = col_number(), `% Recall` = col_number(), Accepted = col_number(), 
#                                             `All Coded` = col_number(), `All Suggested` = col_number(), `From CAC` = col_number())) %>% 
#   filter(`All Suggested` >=0) %>% 
#   separate(`Dx/Proc`, c("DxPx","Code"),sep="[:]") %>% 
#   mutate(DxPx=str_replace(DxPx,"ICD Px", "Px"),  
#          Quarter=qtr,
#          Hub=hub) %>% 
#   separate(Code, c("Code"),sep="[-]") %>% 
#   mutate(Code=str_replace(Code, " ","")) %>% 
#   left_join(ICD10CM) %>% 
#   left_join(ccmcc) %>% 
#   replace(is.na(.),0)
# 
# write.csv(HIM.CAC003_cdr, file = paste0("./3mallhub/HIM.CAC003_fac", qtr,"_",hub,".csv"))
# ### HIM CAC007d - All code entry method summary
# ##############################
# #####   CODER   ############
# ###########################
# ### This one has to be formatted a little differently in Excel first - we will use the group heading, not the 
# ### column heading.  We will drop the percentages columns and then recalculate as needed.  The column reading "entry method"
# ### is actually the coder's name so we will change that also.
# HIM.CAC007d_cdr <- read_csv("HIM.CAC007d.csv") %>%  
#   rename(Coder=`Entry Method`) %>% 
#   separate(Coder, c("Coder","id"),sep="[(]") %>%  
#   filter(Coder != "Totals") %>% 
#   mutate(id=str_replace(id,"[)]","")) %>% 
#   replace(is.na(.),0)
# HIM.CAC007d_cdr <- HIM.CAC007d_cdr[grep("^X",colnames(HIM.CAC007d_cdr), invert=TRUE)] 
# 
# # one problem here is that different CAC007 reports will have more or less columns
# # depending on whether a specific entry method has any data. Because of this, we 
# # can't assume a static format.
# # This solution cleans up and standardizes column names -
# # The nice thing is if the function doesn't find a match, 
# # it shouldn't throw an error.  dplyr would.
# # 
# names(HIM.CAC007d_cdr) <- gsub(" ", ".", names(HIM.CAC007d_cdr)) 
# names(HIM.CAC007d_cdr) <- gsub(" ", ".", names(HIM.CAC007d_cdr))
# names(HIM.CAC007d_cdr) <- gsub("[(]", "", names(HIM.CAC007d_cdr))
# names(HIM.CAC007d_cdr) <- gsub("[)]", "", names(HIM.CAC007d_cdr))
# names(HIM.CAC007d_cdr) <- gsub("[/]", ".", names(HIM.CAC007d_cdr))
# names(HIM.CAC007d_cdr) <- gsub("Ana", "MAN.Ana", names(HIM.CAC007d_cdr))
# names(HIM.CAC007d_cdr) <- gsub("Ane", "MAN.Ane", names(HIM.CAC007d_cdr))
# names(HIM.CAC007d_cdr) <- gsub("Ann", "CAC.Ann", names(HIM.CAC007d_cdr))
# names(HIM.CAC007d_cdr) <- gsub("Aut", "CAC.Aut", names(HIM.CAC007d_cdr))
# names(HIM.CAC007d_cdr) <- gsub("Codes", "CAC.Codes", names(HIM.CAC007d_cdr))
# names(HIM.CAC007d_cdr) <- gsub("CRS", "MAN.CRS", names(HIM.CAC007d_cdr))
# names(HIM.CAC007d_cdr) <- gsub("Doc", "CAC.Doc", names(HIM.CAC007d_cdr))
# names(HIM.CAC007d_cdr) <- gsub("Drc", "MAN.Drc", names(HIM.CAC007d_cdr))
# names(HIM.CAC007d_cdr) <- gsub("CAC.Annot.MAN", "CAC.Annot", names(HIM.CAC007d_cdr))
# names(HIM.CAC007d_cdr) <- gsub("Evi", "CAC.Evi", names(HIM.CAC007d_cdr))
# names(HIM.CAC007d_cdr) <- gsub("Doc.CAC.Evi", "Doc.Evi", names(HIM.CAC007d_cdr))
# names(HIM.CAC007d_cdr) <- gsub("Int", "OTH.Int", names(HIM.CAC007d_cdr))
# names(HIM.CAC007d_cdr) <- gsub("REC", "OTH.REC", names(HIM.CAC007d_cdr))
# names(HIM.CAC007d_cdr) <- gsub("Wor", "CDI.Wor", names(HIM.CAC007d_cdr))
# names(HIM.CAC007d_cdr) <- gsub("Tra", "OTH.Tra", names(HIM.CAC007d_cdr))
# names(HIM.CAC007d_cdr) <- gsub("Val", "CAC.Val", names(HIM.CAC007d_cdr))
# 
# ##Wow this was a beast to figure out
# 
# HIM.CAC007d_cdr <- HIM.CAC007d_cdr  %>% 
#   mutate(CAC = (select(., matches("CAC"))) %>% rowSums) %>% 
#   mutate(CDI = (select(., matches("CDI"))) %>% rowSums) %>% 
#   mutate(MAN = (select(., matches("MAN"))) %>% rowSums) %>% 
#   mutate(OTH = (select(., matches("OTH"))) %>% rowSums)%>% 
#   group_by(Coder) %>% 
#   mutate(TotCodes = sum(`CAC`,`CDI`,`MAN`,`OTH`)) %>% 
#   mutate(`CAC%`= round((CAC/TotCodes)*100,2), 
#          `CDI%`=round((CDI/TotCodes)*100,2), 
#          `MAN%`=round((MAN/TotCodes)*100,2), 
#          `OTH%`=round((OTH/TotCodes)*100,2), 
#          `MANAuto` = (MAN.CRS.S+MAN.Drct.Code.S),
#          `MANAuto%` = round((MANAuto/MAN)*100,2),
#          `CAC+CDI%`=round((`CAC%`+`CDI%`),2),
#          Quarter=qtr, Hub=hub) %>% 
#   select(Coder, id, `CAC%`, `CDI%`, `MAN%`, `OTH%`, TotCodes, MAN.CRS.S, MANAuto, MAN, `MANAuto%`, `CAC+CDI%`, Quarter, Hub)
# 
# HIM.CAC_cdr <- full_join(HIM.CAC001d_cdr, HIM.CAC007d_cdr) 
#   
# 
# rm(HIM.CAC001d_cdr, HIM.CAC007d_cdr)
# 
# #write.csv(HIM.CAC007d_cdr, file = paste0("./3mallhub/HIM.CAC007_cdr_", qtr,"_",hub,".csv"))
# ##############################
# ##### Facility   ############
# ##### ######################
# 
# ###  Entry Method is actually the facility's name so we will change that also.
# HIM.CAC007d_fac <- read_csv("HIM.CAC007d_fac.csv") %>%  
#   rename(Facility=`Entry Method`)  %>% 
#   filter(Facility != "Totals") %>% 
#   replace(is.na(.),0)
# HIM.CAC007d_fac <- HIM.CAC007d_fac[grep("^X",colnames(HIM.CAC007d_fac), invert=TRUE)] 
# 
# # 
# names(HIM.CAC007d_fac) <- gsub(" ", ".", names(HIM.CAC007d_fac)) 
# names(HIM.CAC007d_fac) <- gsub(" ", ".", names(HIM.CAC007d_fac))
# names(HIM.CAC007d_fac) <- gsub("[(]", "", names(HIM.CAC007d_fac))
# names(HIM.CAC007d_fac) <- gsub("[)]", "", names(HIM.CAC007d_fac))
# names(HIM.CAC007d_fac) <- gsub("[/]", ".", names(HIM.CAC007d_fac))
# names(HIM.CAC007d_fac) <- gsub("Ana", "MAN.Ana", names(HIM.CAC007d_fac))
# names(HIM.CAC007d_fac) <- gsub("Ane", "MAN.Ane", names(HIM.CAC007d_fac))
# names(HIM.CAC007d_fac) <- gsub("Ann", "CAC.Ann", names(HIM.CAC007d_fac))
# names(HIM.CAC007d_fac) <- gsub("Aut", "CAC.Aut", names(HIM.CAC007d_fac))
# names(HIM.CAC007d_fac) <- gsub("Codes", "CAC.Codes", names(HIM.CAC007d_fac))
# names(HIM.CAC007d_fac) <- gsub("CRS", "MAN.CRS", names(HIM.CAC007d_fac))
# names(HIM.CAC007d_fac) <- gsub("Doc", "CAC.Doc", names(HIM.CAC007d_fac))
# names(HIM.CAC007d_fac) <- gsub("Drc", "MAN.Drc", names(HIM.CAC007d_fac))
# names(HIM.CAC007d_fac) <- gsub("CAC.Annot.MAN", "CAC.Annot", names(HIM.CAC007d_fac))
# names(HIM.CAC007d_fac) <- gsub("Evi", "CAC.Evi", names(HIM.CAC007d_fac))
# names(HIM.CAC007d_fac) <- gsub("Doc.CAC.Evi", "Doc.Evi", names(HIM.CAC007d_fac))
# names(HIM.CAC007d_fac) <- gsub("Int", "OTH.Int", names(HIM.CAC007d_fac))
# names(HIM.CAC007d_fac) <- gsub("REC", "OTH.REC", names(HIM.CAC007d_fac))
# names(HIM.CAC007d_fac) <- gsub("Wor", "CDI.Wor", names(HIM.CAC007d_fac))
# names(HIM.CAC007d_fac) <- gsub("Tra", "OTH.Tra", names(HIM.CAC007d_fac))
# names(HIM.CAC007d_fac) <- gsub("Val", "CAC.Val", names(HIM.CAC007d_fac))
# 
# ##Wow this was a beast to figure out
# 
# HIM.CAC007d_fac <- HIM.CAC007d_fac  %>% 
#   mutate(CAC = (select(., matches("CAC"))) %>% rowSums) %>% 
#   mutate(CDI = (select(., matches("CDI"))) %>% rowSums) %>% 
#   mutate(MAN = (select(., matches("MAN"))) %>% rowSums) %>% 
#   mutate(OTH = (select(., matches("OTH"))) %>% rowSums)
# 
# HIM.CAC007d_fac <- HIM.CAC007d_fac  %>% group_by(Facility) %>% 
#   mutate(TotCodes = sum(`CAC`,`CDI`,`MAN`,`OTH`)) %>% 
#   mutate(`CAC%`= round((CAC/TotCodes)*100,2), 
#          `CDI%`=round((CDI/TotCodes)*100,2), 
#          `MAN%`=round((MAN/TotCodes)*100,2), 
#          `OTH%`=round((OTH/TotCodes)*100,2),          
#          `MANAuto` = (MAN.CRS.S+MAN.Drct.Code.S),
#          `MANAuto%` = round((MANAuto/MAN)*100,2),
#          `CAC+CDI%`=round((`CAC%`+`CDI%`),2), 
#          Quarter=qtr, Hub=hub) %>% 
#   select(Facility, `CAC%`, `CDI%`, `MAN%`, `OTH%`, TotCodes, MANAuto, MAN, `MANAuto%`, `CAC+CDI%`, Quarter, Hub)
# 
# HIM.CAC_fac <- full_join(HIM.CAC001d_fac, HIM.CAC007d_fac) 
#   rm(HIM.CAC001d_fac, HIM.CAC007d_fac)
# 
# 
# 
# ### HIM CAC008 - All Suggested codes account listing
# ### Large file size - causes 3M to crash
# ### run separate for ip accepted and ip rejected/not accepted
# ### 
# ### used for trouble shooting coders who are rejecting auto - suggested codes
# ### This one requires more pre-manipulation in Excel to match up the diagnosis code with the account number
# ### Delete rows down to the normal header row (delete the blue 'group' row this time)
# ### In cell L1 enter the following `DxPx`
# ### in cell L2 enter the following: `=if(len(A2)>0,A2,L1)`  Then fill that formula all the way down to the end.
# ### Then save the CSV: `HIM.CAC008.rej.csv` or `HIM.CAC008.acc.csv`
# ##############################
# HIM.CAC008.rej <- read_csv("HIM.CAC008.rej.csv")
# HIM.CAC008.acc <- read_csv("HIM.CAC008.acc.csv")
# 
# HIM.CAC008 <- full_join(HIM.CAC008.acc, HIM.CAC008.rej) %>%  filter(`Disch Date` > 0)  %>% 
#   separate(Coder, c("Coder","id"),sep="[(]") %>%  
#   #mutate(id=str_replace(id,"[)]","")) %>%  
#   separate(id, c("id"), sep="[)]") %>% 
# separate(`DxPx`, c("DxPx","Code"),sep="[:]") %>% 
#   mutate(DxPx=str_replace(DxPx,"ICD Px", "Px"),
#          Quarter=qtr,
#          Hub=hub) %>% 
#   separate(Code, c("Code"),sep="[-]") %>% 
#   mutate(Code=str_replace(Code, " ","")) %>% 
#   select(-`Dx/Proc`, -`Total Codes`, -MRN, -`Patient Name`, -`Admit Date`) %>% 
#   replace(is.na(.),0)
# 
# rm(HIM.CAC008.acc, HIM.CAC008.rej)
# 
# write.csv(HIM.CAC008, file = paste0("./3mallhub/HIM.CAC008_", qtr,"_",hub,".csv"))
# ###########################################################################################################################
# ### HIM.IP004 - Account listing
# ### This is actually a great report but it needs a lot of tidying
# ### Specifically the grouper APR and MS need to be in distinct columns
# 
# HIM_IP004_cdr <- read_csv("~/360_reports/6/HIM.IP004_cdr.csv") %>% 
#   replace(is.na(.),0) %>% 
#   filter(`Disch Date` > 0)  %>%  #removes 3M subtotal rows
#   select(-Coder, -`No Second Group`, -`Total Visits`, -`MRN`, -Name, -`Admit Date`) %>% #removes 3M subtotal columns and other unneeded columns - edit as desired
#   rename(Coder=Coder_1) %>% #usual coder parsing.  Comment out or remove if your coder field does not need formatting
#   separate(Coder, c("Coder","id"),sep="[()]") %>% 
#   separate(id, c("id"), sep="[)]") %>% 
#   separate(`Prin Dx`, c("Dx"),sep="[-]") %>%  # this keeps the code numbers and discards the descriptions, which accounts for the bulk of the bytes in the file size  If I find I need, can call it up from a separate CMS table.
#   separate(`Prin Proc`, c("Px"), sep="[-]") %>% 
#   separate(`Pt Type`, c("Pclass", "Ptype"), sep="[/]") %>% #in one of our hubs, the "ptype" is actually the facility campus designation
#   mutate(Dx=str_replace(Dx, " ",""), Px=str_replace(Px, " ",""), Quarter=qtr, Hub=hub, Fac=str_sub(`Visit ID`, 1, 3)) 
# 
# # in the 3M report, the primary DRG could be either APR or MS.  Same for secondary.
# # We need a way to be able to filter on JUST APR for SOI/ROM or just MS for CMI
# # There is a better way to do this I'm sure, but this way works.  
# # Note that I'm combining different version numbers of the same grouper (e.g. v38 and v39, etc)
# 
# Pt <- HIM_IP004_cdr %>% 
#   select (Coder, id,`Visit ID`, `Disch Date`, LOS, `Pclass`, Ptype, Fac, `Disch Disposition`, `Dx`, `Px`)
# DRG <- HIM_IP004_cdr %>% 
#   select(`Visit ID`, DRG, Wgt, `SOI/ROM`, ALOS, GLOS) %>% 
#   separate(DRG, c("DRG","Grouper"),sep="[()]") %>% 
#   separate(DRG, c("DRG"), sep="[-]")
# DRG1 <-HIM_IP004_cdr %>% 
#   select(`Visit ID`, DRG_1, Wgt_1, `SOI/ROM_1`, ALOS_1, GLOS_1) %>% 
#   separate(DRG_1, c("DRG","Grouper"),sep="[()]") %>% 
#   separate(DRG, c("DRG"), sep="[-]") %>% 
#   rename(Wgt=Wgt_1, `SOI/ROM`= "SOI/ROM_1", ALOS=ALOS_1, GLOS=GLOS_1)
# # Bring Primary DRGs and Secondary DRGs into one table
# DRG <- full_join(DRG, DRG1)
# rm(DRG1)
# #Now designate MS vs APR DRG Info - this is the equivalent of find and replace
# #As long as the various state APR systems have `APR` in the desciption they will all be classified to APRDRG
# DRG$Grouper[grepl("APR", DRG$Grouper, ignore.case=FALSE)] <- "APRDRG"
# DRG$Grouper[grepl("MS", DRG$Grouper, ignore.case=FALSE)] <- "MSDRG" 
# DRG$Grouper[grepl("TRI", DRG$Grouper, ignore.case=TRUE)] <- "MSDRG" 
# # if you have a grouper not listed you may need to add it
# # The first part in green is the string you want to search for
# # the last part is where you would classify it to APRDRG or MSDRG
# # see missinggrouper list for possible missing groupers that need to be mapped
# APR <- DRG %>% 
#   filter(Grouper=="APRDRG") %>% 
#   separate(`SOI/ROM`, c("SOI", "ROM"), sep="[/]") %>% 
#   rename(APRLOS=ALOS, APRGLOS=GLOS, APRRW=Wgt) %>% 
#   mutate(APRDRG=as.numeric(DRG),SOI=as.numeric(SOI), ROM=as.numeric(ROM), SOIROM=as.numeric(SOI)+as.numeric(ROM)) %>% 
#   # we count soi+rom score of 7 or 8 as an expected mortality
#   # the combined score might be useful for detecting any trends with some coders coding lower overall severity than others
#   select(-Grouper -DRG)
# noapr <- anti_join(APR, DRG)
# MS <- DRG %>% 
#   mutate(MSDRG=as.numeric(DRG)) %>% 
#   filter(Grouper=="MSDRG") %>%
#   select(-DRG, -`SOI/ROM`, -Grouper) %>% 
#   rename(MSLOS=ALOS, MSGLOS=GLOS, MSRW=Wgt)
# noms <-anti_join(MS, DRG)
# #and bring back together again so that we still have one row per pt
# APRMS <- full_join(APR, MS)
# Missinggrouper <- full_join(noapr, noms) #hope this is 0 but including just in case we get another grouper we need to map to APR or MS
# #if so just duplicate the DRG$Grouper[grepl("...")], where ... is some distinct identifier
# pt_cdr_drg <- full_join(APRMS, Pt)
# rm(noapr, noms,DRG, Pt, APRMS, APR, MS)
# 
# write.csv(pt_cdr_drg, file = paste0("./3mallhub/pt_cdr_drg", qtr,"_",hub,".csv"))
# ####### HIM.Prod016 - productivity metrics
# HIM.Prod016 <- read_csv("HIM.Prod016.csv") %>% 
# separate(Coder, c("Coder","id"),sep="[(]") %>%  
#   separate(id, c("id"), sep="[)]") %>%
#   mutate(Quarter=qtr,
#          Hub=hub)  %>% replace(is.na(.),0) %>% 
#   select(-`Hold Transactions`)
# #write.csv(HIM.Prod016, file = paste0("./3mallhub/HIM.Prod016_", qtr,"_",hub,".csv"))
# #
# #
# # join 1:1 coder tables together
# HIM.CAC_cdr <- left_join(HIM.CAC_cdr, HIM.Prod016)
# rm(HIM.Prod016)
# write.csv(HIM.CAC_cdr, file = paste0("./3mallhub/HIM.CAC_cdr_", qtr,"_",hub,".csv"))
# 
# 
# ###########################################################################################################################
# ###########################################################################################################################
# ###########################################################################################################################
# ###########################################################################################################################
# 
# 
# 
# ######################################################################################################################################
# ## Severity Check Coder
# 
# ccmccP_cdr <- HIM.CAC003  %>% 
#   filter(CCMCC >0) %>% 
#   arrange(desc(`All Suggested`))%>% 
#   select(`Coder`, `Facility`,`Code`, `Desc`, `CCMCC`, `All Suggested`, `Accepted`, `% Precision`)
# ccmccP_cdr <-  head(ccmccP_cdr,10,`All Suggested`) 
# ccmccR_cdr <- HIM.CAC003  %>% 
#   filter(CCMCC >0) %>%  
#   arrange(desc(`All Coded`)) %>% 
#   select(`Coder`, `Facility`, `Code`, `Desc`, `CCMCC`, `All Coded`, `From CAC`, `% Recall`) 
# ccmccR_cdr <- head(ccmccR_cdr,10,`All Coded`)
#   
# #Generate List of 10 accounts per coder to review to determine why the coder did not accept the a/s recommendation
# ccmccPaccts_cdr <- inner_join(HIM.CAC008, ccmccP_cdr) %>% 
#   filter(Action != "Accepted") %>% 
#   select(Facility, Coder, `Visit ID`, `Disch Date`, Code, Desc, CCMCC, Action, Quarter, Hub) %>% 
#   group_by(Coder) %>% 
#   sample_n(10) %>% 
#   ungroup()
# 
# 
# 
# 
# ## Severity Check Facility
# ccmccP_fac <- HIM.CAC003  %>% 
#   filter(CCMCC >0) %>% 
#   arrange(desc(`All Suggested`)) %>% 
#   select(`Coder`, `Facility`,`Code`, `Desc`, `CCMCC`, `All Suggested`, `Accepted`) %>% 
#   group_by(`Code`, `Desc`, `CCMCC`)%>% 
#   summarise(`All Suggested`=sum(`All Suggested`), `Accepted` = sum(`Accepted`)) %>%
#   mutate(`% Precision`=`Accepted`/`All Suggested`) %>% 
#   arrange(desc(`All Suggested`))
# ccmccP_fac$`% Precision` <-  round((ccmccP_fac$`% Precision` *100), 2)
# ccmccP_fac <- head(ccmccP_fac,10,`All Suggested`)
# 
# ccmccR_fac <- HIM.CAC003  %>% 
#   filter(CCMCC >0) %>%  
#   arrange(desc(`All Coded`)) %>% 
#   select(`Coder`, `Facility`, `Code`, `Desc`, `CCMCC`, `All Coded`, `From CAC`) %>% 
#   group_by(`Code`, `Desc`, `CCMCC`) %>% 
#   summarise(`All Coded`=sum(`All Coded`), `From CAC` = sum(`From CAC`)) %>%
#   mutate(`% Recall`=(`From CAC`/`All Coded`)) %>% 
#   arrange(desc(`All Coded`))
# ccmccR_fac$`% Recall` <-  round((ccmccR_fac$`% Recall` *100), 2)
# ccmccR_fac <- head(ccmccR_fac,10,`All Coded`)
# 
# ## Other codes - coder
# 
# otherP_cdr <- HIM.CAC003  %>% 
#   filter(CCMCC ==0, `DxPx`=="Dx") %>% 
#   arrange(desc(`All Suggested`))%>% 
#   select(`Coder`, `Facility`,`Code`, `Desc`, `CCMCC`, `All Suggested`, `Accepted`, `% Precision`)
# otherP_cdr <-  head(otherP_cdr,10,`All Suggested`) 
# 
# otherR_cdr <- HIM.CAC003  %>% 
#   filter(CCMCC == 0, `DxPx`=="Dx") %>%   
#   arrange(desc(`All Coded`)) %>% 
#   select(`Coder`, `Facility`, `Code`, `Desc`, `CCMCC`, `All Coded`, `From CAC`, `% Recall`) 
# otherR_cdr <- head(otherR_cdr,10,`All Coded`)
# 
# #Generate List of 10 accounts per coder to review to determine why the coder did not accept the a/s recommendation
# otherPaccts_cdr <- inner_join(HIM.CAC008, otherP_cdr) %>% 
#   filter(Action != "Accepted") %>% 
#   select(Facility, Coder, `Visit ID`, `Disch Date`, Code, Desc, CCMCC, Action, Quarter, Hub) %>% 
#   group_by(Coder) %>% 
#   sample_n(10) %>% 
#   ungroup()
# ## other codes - facility
# 
# otherP_fac <- HIM.CAC003 %>% 
#   inner_join(CoderFac) %>% 
#   filter(CCMCC ==0, `DxPx`=="Dx") %>% 
#   arrange(desc(`All Suggested`)) %>% 
#   select(`Coder`, `Facility`,`Code`, `Desc`, `CCMCC`, `All Suggested`, `Accepted`) %>% 
#   group_by(`Code`,`Facility`, `Desc`, `CCMCC`)%>% 
#   summarise(`All Suggested`=sum(`All Suggested`), `Accepted` = sum(`Accepted`)) %>%
#   ungroup() %>% 
#   mutate(`% Precision`=`Accepted`/`All Suggested`) %>% 
#   arrange(desc(`All Suggested`))
# otherP_fac$`% Precision` <-  round((otherP_fac$`% Precision` *100), 2)
# otherP_fac <- head(otherP_fac,10,`All Suggested`)
# 
# otherR_fac <- HIM.CAC003  %>% 
#   filter(CCMCC == 0, `DxPx`=="Dx") %>%  
#   arrange(desc(`All Coded`)) %>% 
#   select(`Coder`, `Facility`, `Code`, `Desc`, `CCMCC`, `All Coded`, `From CAC`) %>% 
#   group_by(`Code`,`Facility`, `Desc`, `CCMCC`) %>% 
#   summarise(`All Coded`=sum(`All Coded`), `From CAC` = sum(`From CAC`)) %>%
#   mutate(`% Recall`=(`From CAC`/`All Coded`)) %>% 
#   arrange(desc(`All Coded`))
# otherR_fac$`% Recall` <-  round((otherR_fac$`% Recall` *100), 2)
# otherR_fac <- head(otherR_fac,10,`All Coded`)
# 
# #Generate List of 10 accounts per facility to review to determine why the coder did not accept the a/s recommendation
# otherPaccts_fac <- inner_join(HIM.CAC008, otherP_fac) %>% 
#   filter(Action != "Accepted") %>% 
#   select(Facility, Coder, `Visit ID`, `Disch Date`, Code, Desc, CCMCC, Action, Quarter, Hub) %>% 
#   group_by(Facility) %>% 
#   sample_n(10) %>% 
#   ungroup()
# 
# 
# 
# ##############################################################################################################################
# # Coder Plots
# # 
# # PRplot <- HIM.CAC001d %>% filter(Facility != "Total") 
# # 
# # plot_ly(PRplot,x=~`% Precision`, y=~`% Recall`,type = 'scatter',
# #         text = ~Coder, color=~`Facility` )
# 
# PRplot_cdr <- plot_ly(HIM.CAC_cdr,x=~`% Precision`, y=~`% Recall`,mode='markers', type = 'scatter', size = ~`All Suggested`,
#         text = ~Coder, color=~`Facility` ) %>%   
#   layout(title= paste0("360 Hub: ", hub, " Precision and Recall Rates by Coder (RUQ (75%+) optimal) -",qtr)) %>%     
#   add_segments(x=0, xend=100, y=75, yend=75, showlegend = FALSE, line=list(color="blue")) %>% 
#   add_segments(x=75, xend=75, y=0, yend=100, showlegend = FALSE, line=list(color="blue"))
# 
# MANAutoplot_cdr <- plot_ly(HIM.CAC_cdr,x=~`MANAuto%`, y=~`MAN%`,mode='markers',type = 'scatter', size = ~TotCodes,
#                       text = ~Coder, color=~`Facility` ) %>%   
#   layout(title= paste0("360 Hub: ", hub, " Manual Code Entry% and Manual Entry of Auto-suggested Codes% - by Coder (LLQ (< 25%+) optimal) -",qtr)) %>%     
#   add_segments(x=0, xend=100, y=25, yend=25, showlegend = FALSE, line=list(color="blue")) %>% 
#   add_segments(x=25, xend=25, y=0, yend=100, showlegend = FALSE, line=list(color="blue"))
# 
# MANMINminutes <- mean(HIM.CAC_cdr$`Minutes/Visit`)
# MANMINplot_cdr <- plot_ly(HIM.CAC_cdr,x=~`MANAuto%`, y=~`Minutes/Visit`,mode='markers',type = 'scatter', size = ~`Total Minutes`,
#                            text = ~Coder, color=~`Facility` ) %>% 
#   add_markers() %>%
#   layout(scene = list(xaxis = list(title = 'Manual vs AutoSuggested %'),
#                       yaxis = list(title = 'Manual Entry of Auto-suggested Codes %'))) %>% 
#   layout(title= paste0("360 Hub: ", hub, " Minutes per Chart and Manual Entry of Auto-suggested Codes% - by Coder (LLQ optimal) -",qtr)) %>%     
#   add_segments(x=0, xend=100, y=MANMINminutes, yend=MANMINminutes, showlegend = FALSE, line=list(color="blue")) %>% 
#   add_segments(x=25, xend=25, y=0, yend=30, showlegend = FALSE, line=list(color="blue"))
# 
# MANAutoMINplot_cdr <- plot_ly(HIM.CAC_cdr,x=~`MANAuto%`, y=~`MAN%`, z=~`Minutes/Visit`, mode='markers',type = 'scatter', size = ~TotCodes,
#                            text = ~Coder, color=~`Facility` ) %>%  
#   add_markers() %>%
#   layout(scene = list(xaxis = list(title = 'Manual vs AutoSuggested %'),
#                       yaxis = list(title = 'Manual Entry of Auto-suggested Codes %'),
#                       zaxis = list(title = 'Avg Minutes per Chart'))) %>% 
#   layout(title= paste0("360 Hub: ", hub, " Manual Code Entry% and Manual Entry of Auto-suggested Codes% - by Coder (LLQ (< 25%+) optimal) -",qtr))
#  
# #=================================================================
# 
# HIM.CAC001_cut = quantile(HIM.CAC001d$`All Suggested`, .25)
# HIM.CAC001TopP <- HIM.CAC001d %>%  filter(Facility=="Total", `All Suggested` > HIM.CAC001_cut) %>%  
#   arrange(desc(`% Precision`)) %>% distinct() %>% 
#   top_n(5, `% Precision`) %>% left_join(cdr) %>% 
#   select(Coder, Fac, `% Precision`)
# 
# HIM.CAC001TopR <- HIM.CAC001d %>%  filter(Facility=="Total", `All Coded` > HIM.CAC001_cut) %>%  
#   arrange(desc(`% Recall`)) %>% distinct() %>% 
#   top_n(5, `% Recall`) %>% left_join(cdr) %>% 
#   select(Coder, Fac, `% Recall`)
# 
# HIM.CAC001BotP <- HIM.CAC001d %>%  filter(Facility=="Total", `All Suggested` > HIM.CAC001_cut)  %>%  
#   distinct() %>%  left_join(cdr) %>% filter(!is.na(Fac)) %>% 
#   group_by(Fac) %>%  mutate(`%PrecisMean`=mean(`% Precision`), `%PrecisMax`=max(`% Precision`)) %>% ungroup() %>% 
#   arrange(desc(`% Precision`)) %>% 
#   filter(`% Precision` <= 55) %>% 
#   select(Coder, Fac, `All Suggested`, `Accepted`,`% Precision`, `%PrecisMax`, `%PrecisMean`)
# 
# HIM.CAC001BotR <- HIM.CAC001d %>%  filter(Facility=="Total", `All Coded` > HIM.CAC001_cut) %>%  
#   distinct() %>%  left_join(cdr) %>% filter(!is.na(Fac)) %>% group_by(Fac) %>% 
#   mutate(`%RecallMean`=mean(`% Recall`), `%RecallMax`=max(`% Recall`)) %>%   ungroup() %>% 
#   arrange(desc(`% Recall`)) %>% 
#   filter(`% Recall` <= 55) %>% 
#   select(Coder, Fac, `All Coded`, `From CAC`,`% Recall`, `%RecallMax`, `%RecallMean`)
# 
# 
# #CAC008
# HIM.CAC008 <- read_csv("4/HIM.CAC008.csv", 
#                        col_types = cols(`Admit Date` = col_date(format = "%m/%d/%Y"), 
#                                         `Disch Date` = col_date(format = "%m/%d/%Y")))
# Precision_Review <- HIM.CAC008 %>% filter(Code %in% Precision_Codes)
# 
# #attach coder to account
# AllTenetFac <- read_csv("4/AllTenetFac.csv")
# HIM_IP004 <- read_csv("4/HIM.IP004.csv", col_types = cols(Acct = col_character()))
# HIM_IP004 <- left_join(HIM_IP004, AllTenetFac)
# 
# AllTenetDRG_Dx3_APR <- read_csv("4/AllTenetDRG_Dx3_APR.csv", 
#                                 col_types = cols(`APR` = col_number(), 
#                                                  acct = col_character(), disc_disp = col_number(), 
#                                                  disc_dt = col_date(format = "%m/%d/%Y"), 
#                                                  drg = col_number()))
# AllTenetDRG_noDx3_APR <- read_csv("4/AllTenetDRG_noDx3_APR.csv", 
#                                   col_types = cols(`APR` = col_number(), 
#                                                    disc_disp = col_number(), disc_dt = col_date(format = "%m/%d/%Y"), 
#                                                    drg = col_number(), drg_ty = col_character()))
# AllTenetDRG <- full_join(AllTenetDRG_Dx3_APR, AllTenetDRG_noDx3_APR)
# HIM_IP004_n <- inner_join(HIM_IP004, AllTenetDRG)
# 
# 
# #summarize by DRG
# coder <- HIM_IP004_n %>%   group_by(Coder, drg) %>% 
#   summarize(AvgSev=mean(SOI), AvgLOS=mean(los), TotAcct=n())
# #group_by(fac, grp, qtr) %>% 
# #  summarize(cmi=mean(drgwt), n=n())
# #  
# library(knitr, kableExtra)
# kable(lowprecis) %>% kableExtra::kable_styling(bootstrap_options = c("striped", "bordered", "hover", "condensed", full_width = F))
# 
# # } < when everything is final put the whole thing in braces.  This will stop the script on any error
# # even if in a for loop
