

## make modifications:


# TBB>=100, include OTB_DEF>=100
dis_ratio %>% filter(Fleetgroup == "TBB>=100")
matchedDataCond[["TBB>=100_year"]]
matchedDataCond[["TBB>=100_year"]] <- quo(Fleetgroup == "TBB>=100" | (gear_target == "OTB_DEF" & Fleetgroup == "OTB>=100"))

# OTB<100
data.frame(dis_ratio %>% filter(Fleetgroup == "OTB<100" & Season==1))
matchedDataCond[["OTB<100_Q1"]]
matchedDataCond[["OTB<100_Q1"]] <- quo(Season == 1 & 
                                        Fleetgroup == "OTB<100" &
                                        Country != "Sweden" & 
                                        domainCatchDis_ctr != "UK (England)_IC_1_27.4.b_OTB_CRU_70-99_0_0_all" &
                                        domainCatchDis_ctr != "Denmark_IC_1_27.3.a.20_OTB_CRU_90-119_0_0_all"
)
data.frame(dis_ratio %>% filter(Fleetgroup == "OTB<100" & Season==2))
matchedDataCond[["OTB<100_Q2"]] <- quo(Season == 2 & 
                                         Fleetgroup == "OTB<100" &
                                         Country != "Sweden"
)                                       
data.frame(dis_ratio %>% filter(Fleetgroup == "OTB<100" & Season==3))
matchedDataCond[["OTB<100_Q3"]] <- quo(Season == 3 & 
                                         Fleetgroup == "OTB<100" &
                                         Country != "Sweden" &
                                         domainCatchDis_ctr != "Netherlands_IC_3_27.4_OTB_DEF_70-99_0_0_all"
)
data.frame(dis_ratio %>% filter(Fleetgroup == "OTB<100" & Season==4))
matchedDataCond[["OTB<100_Q4"]] <- quo(Season == 4 & 
                                         Fleetgroup == "OTB<100" &
                                         Country != "Sweden" 
)


# OTB>=100
data.frame(dis_ratio %>% filter(Fleetgroup == "OTB>=100" & Season==4))
matchedDataCond[["OTB>=100_Q4"]] <- quo(Season %in% c(3,4) & 
                                          Fleetgroup == "OTB>=100"
)



length(strataCond) == length(matchedDataCond)
identical(names(strataCond), names(matchedDataCond))