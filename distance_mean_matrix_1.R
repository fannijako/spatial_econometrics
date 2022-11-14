load("d:/Users/Fanni/Desktop/Területi autokorreláció/data_shp.RData")
LoadLibrary()

#-------------------------------------------------------------------------------
cities_distance_df_1 <- cities_distance_df_1 %>% filter(!NUTS_1_1 %in% c("PT2" ,  "PT3" , 
       "FRY" ,  "IS0" , "ES7" ,  "TR1" , "TR2" ,  "TR3" ,  "TR4" ,  "TR5" ,  "TR6" , 
       "TR7" ,  "TR8" , "TR9" ,  "TRA" , "TRB" ,  "TRC" ,  "CY0" ,  "RS1" , "RS2" ,  "ME0" , 
       "AL0" ,  "MK0" , "MT0" ,  "NO0"))

cities_distance_df_1 <- cities_distance_df_1 %>% filter(!NUTS_1_2 %in% c("PT2" ,  "PT3" , 
       "FRY" ,  "IS0" , "ES7" ,  "TR1" , "TR2" ,  "TR3" ,  "TR4" ,  "TR5" ,  "TR6" , 
       "TR7" ,  "TR8" , "TR9" ,  "TRA" , "TRB" ,  "TRC" ,  "CY0" ,  "RS1" , "RS2" ,  "ME0" , 
       "AL0" ,  "MK0" , "MT0" ,  "NO0"))

cities_distance_df_1_2 <- cities_distance_df_1_2 %>% filter(!NUTS_1_1 %in% c("PT2" ,  "PT3" , 
       "FRY" ,  "IS0" , "ES7" ,  "TR1" , "TR2" ,  "TR3" ,  "TR4" ,  "TR5" ,  "TR6" , 
       "TR7" ,  "TR8" , "TR9" ,  "TRA" , "TRB" ,  "TRC" ,  "CY0" ,  "RS1" , "RS2" ,  "ME0" , 
       "AL0" ,  "MK0" , "MT0" ,  "NO0"))

cities_distance_df_1_2 <- cities_distance_df_1_2 %>% filter(!NUTS_1_2 %in% c("PT2" ,  "PT3" , 
       "FRY" ,  "IS0" , "ES7" ,  "TR1" , "TR2" ,  "TR3" ,  "TR4" ,  "TR5" ,  "TR6" , 
       "TR7" ,  "TR8" , "TR9" ,  "TRA" , "TRB" ,  "TRC" ,  "CY0" ,  "RS1" , "RS2" ,  "ME0" , 
       "AL0" ,  "MK0" , "MT0" ,  "NO0"))

cities_distance_df_2 <- cities_distance_df_2 %>% filter(!NUTS_2_1 %in%  c("FRY1" ,  "FRY2" , 
       "FRY3" ,  "FRY4" , "FRY5" ,  "IS00" ,  "ES70" ,  "PT20" ,  "PT30" ,  "NO0B",
       "TR10" ,  "TR21" , "TR22" ,   "TR31" , "TR32" ,  "TR33" , "TR41" ,   "TR42" , 
       "TR51" ,   "TR52" , "TR61" ,   "TR62" ,  "TR63" ,   "TR71" , "TR72" ,   "TR81" , 
       "TR82" ,   "TR83" ,"TR90" ,   "TRA1" ,  "TRA2" ,   "TRB1" ,  "TRB2" ,   "TRC1" , 
       "TRC2" ,   "TRC3" , "NO02" ,   "NO06" ,  "NO07" ,   "NO08" ,  "NO09" ,   "NO0A" , 
       "NO0B" ,   "MT00" ,  "CY00" ,   "RS11" ,  "RS12" ,   "RS21" , "RS22" ,   "ME00" , 
       "AL01" ,   "AL02" ,  "AL03" ,   "MK00"))

cities_distance_df_2 <- cities_distance_df_2 %>% filter(!NUTS_2_2 %in%  c("FRY1" ,  "FRY2" , 
      "FRY3" ,  "FRY4" , "FRY5" ,  "IS00" ,  "ES70" ,  "PT20" ,  "PT30" ,  "NO0B",
      "TR10" ,  "TR21" , "TR22" ,   "TR31" , "TR32" ,  "TR33" , "TR41" ,   "TR42" , 
      "TR51" ,   "TR52" , "TR61" ,   "TR62" ,  "TR63" ,   "TR71" , "TR72" ,   "TR81" , 
      "TR82" ,   "TR83" ,"TR90" ,   "TRA1" ,  "TRA2" ,   "TRB1" ,  "TRB2" ,   "TRC1" , 
      "TRC2" ,   "TRC3" , "NO02" ,   "NO06" ,  "NO07" ,   "NO08" ,  "NO09" ,   "NO0A" , 
      "NO0B" ,   "MT00" ,  "CY00" ,   "RS11" ,  "RS12" ,   "RS21" , "RS22" ,   "ME00" , 
      "AL01" ,   "AL02" ,  "AL03" ,   "MK00"))

cities_distance_df_2_2 <- cities_distance_df_2_2 %>% filter(!NUTS_2_1 %in%  c("FRY1" ,  "FRY2" , 
      "FRY3" ,  "FRY4" , "FRY5" ,  "IS00" ,  "ES70" ,  "PT20" ,  "PT30" ,  "NO0B",
      "TR10" ,  "TR21" , "TR22" ,   "TR31" , "TR32" ,  "TR33" , "TR41" ,   "TR42" , 
      "TR51" ,   "TR52" , "TR61" ,   "TR62" ,  "TR63" ,   "TR71" , "TR72" ,   "TR81" , 
      "TR82" ,   "TR83" ,"TR90" ,   "TRA1" ,  "TRA2" ,   "TRB1" ,  "TRB2" ,   "TRC1" , 
      "TRC2" ,   "TRC3" , "NO02" ,   "NO06" ,  "NO07" ,   "NO08" ,  "NO09" ,   "NO0A" , 
      "NO0B" ,   "MT00" ,  "CY00" ,   "RS11" ,  "RS12" ,   "RS21" , "RS22" ,   "ME00" , 
      "AL01" ,   "AL02" ,  "AL03" ,   "MK00"))

cities_distance_df_2_2 <- cities_distance_df_2_2 %>% filter(!NUTS_2_2 %in%  c("FRY1" ,  "FRY2" , 
      "FRY3" ,  "FRY4" , "FRY5" ,  "IS00" ,  "ES70" ,  "PT20" ,  "PT30" ,  "NO0B",
      "TR10" ,  "TR21" , "TR22" ,   "TR31" , "TR32" ,  "TR33" , "TR41" ,   "TR42" , 
      "TR51" ,   "TR52" , "TR61" ,   "TR62" ,  "TR63" ,   "TR71" , "TR72" ,   "TR81" , 
      "TR82" ,   "TR83" ,"TR90" ,   "TRA1" ,  "TRA2" ,   "TRB1" ,  "TRB2" ,   "TRC1" , 
      "TRC2" ,   "TRC3" , "NO02" ,   "NO06" ,  "NO07" ,   "NO08" ,  "NO09" ,   "NO0A" , 
      "NO0B" ,   "MT00" ,  "CY00" ,   "RS11" ,  "RS12" ,   "RS21" , "RS22" ,   "ME00" , 
      "AL01" ,   "AL02" ,  "AL03" ,   "MK00"))

cities_distance_df_3 <- cities_distance_df_3 %>% filter(!NUTS_3_1 %in% c( "ES703" ,  "ES704" ,
      "ES705" ,  "ES706" ,"ES707" ,  "ES708" , "ES709" ,  "FRY10" , "FRY20" ,  "FRY30" ,
      "FRY40" ,  "FRY50" , "IS001" ,  "IS002" ,"PT200" ,  "PT300" , "NO0B1" ,  "NO0B2",
      "TR100" ,  "TR211" , "TR212" ,  "TR213" , "TR221" ,  "TR222" , "TR310" ,  "TR321" , 
      "TR322" ,  "TR323" ,"TR331" ,  "TR332" ,  "TR333" ,  "TR334" ,  "TR411" ,  "TR412" , 
      "TR413" ,  "TR421" , "TR422" ,  "TR423" , "TR424" ,  "TR425" ,  "TR510" ,  "TR521" , 
      "TR522" ,  "TR611" , "TR612" ,  "TR613" , "TR621" ,  "TR622" , "TR631" ,  "TR632" , 
      "TR633" ,  "TR711" , "TR712" ,  "TR713" , "TR714" ,  "TR715" , "TR721" ,  "TR722" , 
      "TR723" ,  "TR811" ,  "TR812" ,  "TR813" ,  "TR821" ,  "TR822" , "TR823" ,  "TR831" , 
      "TR832" ,  "TR833" , "TR834" ,  "TR901" , "TR902" ,  "TR903" ,  "TR904" ,  "TR905" , 
      "TR906" ,  "TRA11" , "TRA12" ,  "TRA13" ,  "TRA21" ,  "TRA22" , "TRA23" ,  "TRA24" , 
      "TRB11" ,  "TRB12" ,  "TRB13" ,  "TRB14" ,  "TRB21" ,  "TRB22" , "TRB23" ,  "TRB24" , 
      "TRC11" ,  "TRC12" , "TRC13" ,  "TRC21" ,  "TRC22" ,  "TRC31" , "TRC32" ,  "TRC33" , 
      "TRC34" ,  "NO020" , "NO060" ,  "NO071" , "NO074" ,  "NO081" , "NO082" ,  "NO091" , 
      "NO092" ,  "NO0A1" , "NO0A2" ,  "NO0A3" , "NO0B1" ,  "NO0B2" , "MT001" ,  "MT002" , 
      "CY000" ,  "RS110" , "RS121" ,  "RS122" , "RS123" ,  "RS124" ,  "RS125" ,  "RS126" , 
      "RS127" ,  "RS211" ,  "RS212" ,  "RS213" , "RS214" ,  "RS215" , "RS216" ,  "RS217" , 
      "RS218" ,  "RS221" ,"RS222" ,  "RS223" ,  "RS224" ,  "RS225" , "RS226" ,  "RS227" , 
      "RS228" ,  "RS229" ,  "ME000" ,  "AL011" ,  "AL012" ,  "AL013" , "AL014" ,  "AL015" , 
      "AL021" ,  "AL022" ,  "AL031" ,  "AL032" , "AL033" ,  "AL034" ,  "AL035" ,  "MK001" , 
      "MK002" ,  "MK003" , "MK004" ,  "MK005" , "MK006" ,  "MK007" ,  "MK008"))

cities_distance_df_3 <- cities_distance_df_3 %>% filter(!NUTS_3_2 %in% c( "ES703" ,  "ES704" ,
      "ES705" ,  "ES706" ,"ES707" ,  "ES708" , "ES709" ,  "FRY10" , "FRY20" ,  "FRY30" ,
      "FRY40" ,  "FRY50" , "IS001" ,  "IS002" ,"PT200" ,  "PT300" , "NO0B1" ,  "NO0B2",
      "TR100" ,  "TR211" , "TR212" ,  "TR213" , "TR221" ,  "TR222" , "TR310" ,  "TR321" , 
      "TR322" ,  "TR323" ,"TR331" ,  "TR332" ,  "TR333" ,  "TR334" ,  "TR411" ,  "TR412" , 
      "TR413" ,  "TR421" , "TR422" ,  "TR423" , "TR424" ,  "TR425" ,  "TR510" ,  "TR521" , 
      "TR522" ,  "TR611" , "TR612" ,  "TR613" , "TR621" ,  "TR622" , "TR631" ,  "TR632" , 
      "TR633" ,  "TR711" , "TR712" ,  "TR713" , "TR714" ,  "TR715" , "TR721" ,  "TR722" , 
      "TR723" ,  "TR811" ,  "TR812" ,  "TR813" ,  "TR821" ,  "TR822" , "TR823" ,  "TR831" , 
      "TR832" ,  "TR833" , "TR834" ,  "TR901" , "TR902" ,  "TR903" ,  "TR904" ,  "TR905" , 
      "TR906" ,  "TRA11" , "TRA12" ,  "TRA13" ,  "TRA21" ,  "TRA22" , "TRA23" ,  "TRA24" , 
      "TRB11" ,  "TRB12" ,  "TRB13" ,  "TRB14" ,  "TRB21" ,  "TRB22" , "TRB23" ,  "TRB24" , 
      "TRC11" ,  "TRC12" , "TRC13" ,  "TRC21" ,  "TRC22" ,  "TRC31" , "TRC32" ,  "TRC33" , 
      "TRC34" ,  "NO020" , "NO060" ,  "NO071" , "NO074" ,  "NO081" , "NO082" ,  "NO091" , 
      "NO092" ,  "NO0A1" , "NO0A2" ,  "NO0A3" , "NO0B1" ,  "NO0B2" , "MT001" ,  "MT002" , 
                                                                          "CY000" ,  "RS110" , "RS121" ,  "RS122" , "RS123" ,  "RS124" ,  "RS125" ,  "RS126" , 
                                                                          "RS127" ,  "RS211" ,  "RS212" ,  "RS213" , "RS214" ,  "RS215" , "RS216" ,  "RS217" , 
                                                                          "RS218" ,  "RS221" ,"RS222" ,  "RS223" ,  "RS224" ,  "RS225" , "RS226" ,  "RS227" , 
                                                                          "RS228" ,  "RS229" ,  "ME000" ,  "AL011" ,  "AL012" ,  "AL013" , "AL014" ,  "AL015" , 
                                                                          "AL021" ,  "AL022" ,  "AL031" ,  "AL032" , "AL033" ,  "AL034" ,  "AL035" ,  "MK001" , 
                                                                          "MK002" ,  "MK003" , "MK004" ,  "MK005" , "MK006" ,  "MK007" ,  "MK008"))

cities_distance_df_3_2 <- cities_distance_df_3_2 %>% filter(!NUTS_3_1 %in% c( "ES703" ,  "ES704" ,
                                                                          "ES705" ,  "ES706" ,"ES707" ,  "ES708" , "ES709" ,  "FRY10" , "FRY20" ,  "FRY30" ,
                                                                          "FRY40" ,  "FRY50" , "IS001" ,  "IS002" ,"PT200" ,  "PT300" , "NO0B1" ,  "NO0B2",
                                                                          "TR100" ,  "TR211" , "TR212" ,  "TR213" , "TR221" ,  "TR222" , "TR310" ,  "TR321" , 
                                                                          "TR322" ,  "TR323" ,"TR331" ,  "TR332" ,  "TR333" ,  "TR334" ,  "TR411" ,  "TR412" , 
                                                                          "TR413" ,  "TR421" , "TR422" ,  "TR423" , "TR424" ,  "TR425" ,  "TR510" ,  "TR521" , 
                                                                          "TR522" ,  "TR611" , "TR612" ,  "TR613" , "TR621" ,  "TR622" , "TR631" ,  "TR632" , 
                                                                          "TR633" ,  "TR711" , "TR712" ,  "TR713" , "TR714" ,  "TR715" , "TR721" ,  "TR722" , 
                                                                          "TR723" ,  "TR811" ,  "TR812" ,  "TR813" ,  "TR821" ,  "TR822" , "TR823" ,  "TR831" , 
                                                                          "TR832" ,  "TR833" , "TR834" ,  "TR901" , "TR902" ,  "TR903" ,  "TR904" ,  "TR905" , 
                                                                          "TR906" ,  "TRA11" , "TRA12" ,  "TRA13" ,  "TRA21" ,  "TRA22" , "TRA23" ,  "TRA24" , 
                                                                          "TRB11" ,  "TRB12" ,  "TRB13" ,  "TRB14" ,  "TRB21" ,  "TRB22" , "TRB23" ,  "TRB24" , 
                                                                          "TRC11" ,  "TRC12" , "TRC13" ,  "TRC21" ,  "TRC22" ,  "TRC31" , "TRC32" ,  "TRC33" , 
                                                                          "TRC34" ,  "NO020" , "NO060" ,  "NO071" , "NO074" ,  "NO081" , "NO082" ,  "NO091" , 
                                                                          "NO092" ,  "NO0A1" , "NO0A2" ,  "NO0A3" , "NO0B1" ,  "NO0B2" , "MT001" ,  "MT002" , 
                                                                          "CY000" ,  "RS110" , "RS121" ,  "RS122" , "RS123" ,  "RS124" ,  "RS125" ,  "RS126" , 
                                                                          "RS127" ,  "RS211" ,  "RS212" ,  "RS213" , "RS214" ,  "RS215" , "RS216" ,  "RS217" , 
                                                                          "RS218" ,  "RS221" ,"RS222" ,  "RS223" ,  "RS224" ,  "RS225" , "RS226" ,  "RS227" , 
                                                                          "RS228" ,  "RS229" ,  "ME000" ,  "AL011" ,  "AL012" ,  "AL013" , "AL014" ,  "AL015" , 
                                                                          "AL021" ,  "AL022" ,  "AL031" ,  "AL032" , "AL033" ,  "AL034" ,  "AL035" ,  "MK001" , 
                                                                          "MK002" ,  "MK003" , "MK004" ,  "MK005" , "MK006" ,  "MK007" ,  "MK008"))

cities_distance_df_3_2 <- cities_distance_df_3_2 %>% filter(!NUTS_3_2 %in% c( "ES703" ,  "ES704" ,
                                                                          "ES705" ,  "ES706" ,"ES707" ,  "ES708" , "ES709" ,  "FRY10" , "FRY20" ,  "FRY30" ,
                                                                          "FRY40" ,  "FRY50" , "IS001" ,  "IS002" ,"PT200" ,  "PT300" , "NO0B1" ,  "NO0B2",
                                                                          "TR100" ,  "TR211" , "TR212" ,  "TR213" , "TR221" ,  "TR222" , "TR310" ,  "TR321" , 
                                                                          "TR322" ,  "TR323" ,"TR331" ,  "TR332" ,  "TR333" ,  "TR334" ,  "TR411" ,  "TR412" , 
                                                                          "TR413" ,  "TR421" , "TR422" ,  "TR423" , "TR424" ,  "TR425" ,  "TR510" ,  "TR521" , 
                                                                          "TR522" ,  "TR611" , "TR612" ,  "TR613" , "TR621" ,  "TR622" , "TR631" ,  "TR632" , 
                                                                          "TR633" ,  "TR711" , "TR712" ,  "TR713" , "TR714" ,  "TR715" , "TR721" ,  "TR722" , 
                                                                          "TR723" ,  "TR811" ,  "TR812" ,  "TR813" ,  "TR821" ,  "TR822" , "TR823" ,  "TR831" , 
                                                                          "TR832" ,  "TR833" , "TR834" ,  "TR901" , "TR902" ,  "TR903" ,  "TR904" ,  "TR905" , 
                                                                          "TR906" ,  "TRA11" , "TRA12" ,  "TRA13" ,  "TRA21" ,  "TRA22" , "TRA23" ,  "TRA24" , 
                                                                          "TRB11" ,  "TRB12" ,  "TRB13" ,  "TRB14" ,  "TRB21" ,  "TRB22" , "TRB23" ,  "TRB24" , 
                                                                          "TRC11" ,  "TRC12" , "TRC13" ,  "TRC21" ,  "TRC22" ,  "TRC31" , "TRC32" ,  "TRC33" , 
                                                                          "TRC34" ,  "NO020" , "NO060" ,  "NO071" , "NO074" ,  "NO081" , "NO082" ,  "NO091" , 
                                                                          "NO092" ,  "NO0A1" , "NO0A2" ,  "NO0A3" , "NO0B1" ,  "NO0B2" , "MT001" ,  "MT002" , 
                                                                          "CY000" ,  "RS110" , "RS121" ,  "RS122" , "RS123" ,  "RS124" ,  "RS125" ,  "RS126" , 
                                                                          "RS127" ,  "RS211" ,  "RS212" ,  "RS213" , "RS214" ,  "RS215" , "RS216" ,  "RS217" , 
                                                                          "RS218" ,  "RS221" ,"RS222" ,  "RS223" ,  "RS224" ,  "RS225" , "RS226" ,  "RS227" , 
                                                                          "RS228" ,  "RS229" ,  "ME000" ,  "AL011" ,  "AL012" ,  "AL013" , "AL014" ,  "AL015" , 
                                                                          "AL021" ,  "AL022" ,  "AL031" ,  "AL032" , "AL033" ,  "AL034" ,  "AL035" ,  "MK001" , 
                                                                          "MK002" ,  "MK003" , "MK004" ,  "MK005" , "MK006" ,  "MK007" ,  "MK008"))


#-------------------------------------------------------------------------------
codes <- NUTS_1$NUTS_ID

distance_mean_km_1 <- matrix(nrow = 100, ncol = 100)
distance_sd_km_1 <- matrix(nrow = 100, ncol = 100)
distance_mean_hour_1 <- matrix(nrow = 100, ncol = 100)
distance_sd_hour_1 <- matrix(nrow = 100, ncol = 100)

for (i in 1:100){
  code_1 <- codes[i]
  neighbor <- region_neighbor_df_1 %>% filter(NUTS_ID == code_1)
  if (neighbor$neighbor == ""){next}
  neighbor <- strsplit(neighbor$neighbor, split = "; ")[[1]]
  
  for (j in 1:100){
    code_2 <- codes[j]
    
    if (!any(code_2 == neighbor)){next}
    
    if (i > j){
      proba <- cities_distance_df_1 %>% filter((NUTS_1_1 == codes[i] & NUTS_1_2 == codes[j])
                                               | NUTS_1_2 == codes[j] & NUTS_1_1 == codes[i])
      distance_mean_km_1[i, j] <- mean(proba$km)
      distance_sd_km_1[i, j] <- sd(proba$km)
      distance_mean_hour_1[i, j] <- mean(proba$hour)
      distance_sd_hour_1[i, j] <- sd(proba$hour)
      
      distance_mean_km_1[j, i] <- mean(proba$km)
      distance_sd_km_1[j, i] <- sd(proba$km)
      distance_mean_hour_1[j, i] <- mean(proba$hour)
      distance_sd_hour_1[j, i] <- sd(proba$hour)
    }
  }
}

distance_mean_km_1[is.na(distance_mean_km_1)] <- 0
distance_sd_km_1[is.na(distance_sd_km_1)] <- 0
distance_mean_hour_1[is.na(distance_mean_hour_1)] <- 0
distance_sd_hour_1[is.na(distance_sd_hour_1)] <- 0

rm(i, j, proba, code_1, code_2, neighbor, codes)

save.image("d:/Users/Fanni/Desktop/Területi autokorreláció/data_shp.RData")