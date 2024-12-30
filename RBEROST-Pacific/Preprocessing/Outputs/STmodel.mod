#STmodel.mod
      
set comid_all :=
{'23989071','23989073','23989075','23989077','23989079','23989383','23989385','23989725','23989727','23989857','23989893','23990227','23990257','23990259','23990261','23990263','23990269','23990391','23990417','23990453','23990459','23990707','23990785','23990803','23990893','23990957','23990965','23990967','23990969'};

set comid_all_N within comid_all := {'23989383','23989385','23990707','23990965','23990967','23990969','23989071','23989073','23989075','23989077','23989079','23990227','23990257','23990259','23990261','23990263','23990269','23990785','23990957','23989725','23989727','23990391','23990417','23990453','23990459','23990893'};

set comid_all_P within comid_all := {'23989857','23989893','23990803'};

set comid_N1 within comid_all_N := {'23989383','23989385','23990707','23990965','23990967','23990969'};

set comid_N2 within comid_all_N := {'23989071','23989073','23989075','23989077','23989079','23990227','23990257','23990259','23990261','23990263','23990269','23990785','23990957'};

set comid_N3 within comid_all_N := {'23989725','23989727','23990391','23990417','23990453','23990459','23990893'};

set comid_N4 within comid_all_N := {};

set comid_P1 within comid_all_P := {};

set comid_P2 within comid_all_P := {};

set comid_P3 within comid_all_P := {};

set comid_P4 within comid_all_P := {'23989857','23989893','23990803'};

set urban_comid_all :={'23989071_URLD_C','23989071_UIDU_C','23989071_URHD_C','23989071_URMD_C','23989073_UIDU_C','23989073_URLD_C','23989073_URMD_C','23989073_URHD_C','23989075_URMD_C','23989075_URHD_C','23989075_URLD_C','23989075_UIDU_C','23989077_URHD_C','23989077_UIDU_C','23989079_URLD_C','23989079_URMD_C','23989079_URHD_C','23989079_UIDU_C','23989383_UIDU_C','23989383_URLD_C','23989383_URMD_C','23989383_URHD_C','23989385_URLD_C','23989385_URMD_C','23989385_URHD_C','23989385_UIDU_C','23989725_URHD_C','23989725_URLD_C','23989725_URMD_C','23989727_UIDU_C','23989727_URHD_C','23989727_URLD_C','23989727_URMD_C','23989857_URLD_D','23989857_URHD_D','23989857_URMD_D','23989893_URMD_D','23989893_URLD_D','23989893_UIDU_D','23989893_URHD_D','23990227_URMD_C','23990227_URLD_C','23990227_UIDU_C','23990227_URHD_C','23990257_URHD_C','23990257_URLD_C','23990257_URMD_C','23990257_UIDU_C','23990259_URHD_C','23990259_URMD_C','23990259_URLD_C','23990261_UIDU_C','23990261_URMD_C','23990261_URLD_C','23990261_URHD_C','23990263_UIDU_C','23990263_URLD_C','23990263_URMD_C','23990263_URHD_C','23990269_URHD_C','23990269_URLD_C','23990269_URMD_C','23990269_UIDU_C','23990391_URMD_C','23990391_UIDU_C','23990391_URLD_C','23990391_URHD_C','23990417_URLD_C','23990417_URHD_C','23990417_UIDU_C','23990417_URMD_C','23990453_URHD_C','23990453_URLD_C','23990453_URMD_C','23990459_URHD_C','23990459_URMD_C','23990459_URLD_C','23990707_URLD_C','23990707_UIDU_C','23990707_URHD_C','23990707_URMD_C','23990785_URHD_C','23990785_URMD_C','23990785_URLD_C','23990803_UIDU_D','23990803_URLD_D','23990803_URHD_D','23990803_URMD_D','23990893_URMD_C','23990957_URLD_C','23990965_URHD_C','23990967_UIDU_C','23990967_URHD_C','23990967_URLD_C','23990967_URMD_C'};

set matching_urban_comid :={'23989071','23989073','23989075','23989077','23989079','23989383','23989385','23989725','23989727','23989857','23989893','23990227','23990257','23990259','23990261','23990263','23990269','23990391','23990417','23990453','23990459','23990707','23990785','23990803','23990893','23990957','23990965','23990967'};

set urban_comid_all_N within urban_comid_all := {'23989071_URLD_C','23989071_UIDU_C','23989071_URHD_C','23989071_URMD_C','23989073_UIDU_C','23989073_URLD_C','23989073_URMD_C','23989073_URHD_C','23989075_URMD_C','23989075_URHD_C','23989075_URLD_C','23989075_UIDU_C','23989077_URHD_C','23989077_UIDU_C','23989079_URLD_C','23989079_URMD_C','23989079_URHD_C','23989079_UIDU_C','23989383_UIDU_C','23989383_URLD_C','23989383_URMD_C','23989383_URHD_C','23989385_URLD_C','23989385_URMD_C','23989385_URHD_C','23989385_UIDU_C','23989725_URHD_C','23989725_URLD_C','23989725_URMD_C','23989727_UIDU_C','23989727_URHD_C','23989727_URLD_C','23989727_URMD_C','23990227_URMD_C','23990227_URLD_C','23990227_UIDU_C','23990227_URHD_C','23990257_URHD_C','23990257_URLD_C','23990257_URMD_C','23990257_UIDU_C','23990259_URHD_C','23990259_URMD_C','23990259_URLD_C','23990261_UIDU_C','23990261_URMD_C','23990261_URLD_C','23990261_URHD_C','23990263_UIDU_C','23990263_URLD_C','23990263_URMD_C','23990263_URHD_C','23990269_URHD_C','23990269_URLD_C','23990269_URMD_C','23990269_UIDU_C','23990391_URMD_C','23990391_UIDU_C','23990391_URLD_C','23990391_URHD_C','23990417_URLD_C','23990417_URHD_C','23990417_UIDU_C','23990417_URMD_C','23990453_URHD_C','23990453_URLD_C','23990453_URMD_C','23990459_URHD_C','23990459_URMD_C','23990459_URLD_C','23990707_URLD_C','23990707_UIDU_C','23990707_URHD_C','23990707_URMD_C','23990785_URHD_C','23990785_URMD_C','23990785_URLD_C','23990893_URMD_C','23990957_URLD_C','23990965_URHD_C','23990967_UIDU_C','23990967_URHD_C','23990967_URLD_C','23990967_URMD_C'};

set urban_comid_all_P within urban_comid_all := {'23989857_URLD_D','23989857_URHD_D','23989857_URMD_D','23989893_URMD_D','23989893_URLD_D','23989893_UIDU_D','23989893_URHD_D','23990803_UIDU_D','23990803_URLD_D','23990803_URHD_D','23990803_URMD_D'};

set urban_comid_N1 within urban_comid_all_N := {'23989383_UIDU_C','23989383_URLD_C','23989383_URMD_C','23989383_URHD_C','23989385_URLD_C','23989385_URMD_C','23989385_URHD_C','23989385_UIDU_C','23990707_URLD_C','23990707_UIDU_C','23990707_URHD_C','23990707_URMD_C','23990965_URHD_C','23990967_UIDU_C','23990967_URHD_C','23990967_URLD_C','23990967_URMD_C'};

set urban_comid_N2 within urban_comid_all_N := {'23989071_URLD_C','23989071_UIDU_C','23989071_URHD_C','23989071_URMD_C','23989073_UIDU_C','23989073_URLD_C','23989073_URMD_C','23989073_URHD_C','23989075_URMD_C','23989075_URHD_C','23989075_URLD_C','23989075_UIDU_C','23989077_URHD_C','23989077_UIDU_C','23989079_URLD_C','23989079_URMD_C','23989079_URHD_C','23989079_UIDU_C','23990227_URMD_C','23990227_URLD_C','23990227_UIDU_C','23990227_URHD_C','23990257_URHD_C','23990257_URLD_C','23990257_URMD_C','23990257_UIDU_C','23990259_URHD_C','23990259_URMD_C','23990259_URLD_C','23990261_UIDU_C','23990261_URMD_C','23990261_URLD_C','23990261_URHD_C','23990263_UIDU_C','23990263_URLD_C','23990263_URMD_C','23990263_URHD_C','23990269_URHD_C','23990269_URLD_C','23990269_URMD_C','23990269_UIDU_C','23990785_URHD_C','23990785_URMD_C','23990785_URLD_C','23990957_URLD_C'};

set urban_comid_N3 within urban_comid_all_N := {'23989725_URHD_C','23989725_URLD_C','23989725_URMD_C','23989727_UIDU_C','23989727_URHD_C','23989727_URLD_C','23989727_URMD_C','23990391_URMD_C','23990391_UIDU_C','23990391_URLD_C','23990391_URHD_C','23990417_URLD_C','23990417_URHD_C','23990417_UIDU_C','23990417_URMD_C','23990453_URHD_C','23990453_URLD_C','23990453_URMD_C','23990459_URHD_C','23990459_URMD_C','23990459_URLD_C','23990893_URMD_C'};

set urban_comid_N4 within urban_comid_all_N := {};

set urban_comid_P1 within urban_comid_all_P := {};

set urban_comid_P2 within urban_comid_all_P := {};

set urban_comid_P3 within urban_comid_all_P := {};

set urban_comid_P4 within urban_comid_all_P := {'23989857_URLD_D','23989857_URHD_D','23989857_URMD_D','23989893_URMD_D','23989893_URLD_D','23989893_UIDU_D','23989893_URHD_D','23990803_UIDU_D','23990803_URLD_D','23990803_URHD_D','23990803_URMD_D'};

set road_comid_all :={'23989071_C','23989073_C','23989075_C','23989077_C','23989079_C','23989383_C','23989385_C','23989727_C','23989893_D','23990227_C','23990257_C','23990261_C','23990263_C','23990269_C','23990391_C','23990417_C','23990707_C','23990803_D','23990967_C'};

set matching_road_comid :={'23989071','23989073','23989075','23989077','23989079','23989383','23989385','23989727','23989893','23990227','23990257','23990261','23990263','23990269','23990391','23990417','23990707','23990803','23990967'};

set road_comid_all_N within road_comid_all := {'23989071_C','23989073_C','23989075_C','23989077_C','23989079_C','23989383_C','23989385_C','23989727_C','23990227_C','23990257_C','23990261_C','23990263_C','23990269_C','23990391_C','23990417_C','23990707_C','23990967_C'};

set road_comid_all_P within road_comid_all := {'23989893_D','23990803_D'};

set road_comid_N1 within road_comid_all_N := {'23989383_C','23989385_C','23990707_C','23990967_C'};

set road_comid_N2 within road_comid_all_N := {'23989071_C','23989073_C','23989075_C','23989077_C','23989079_C','23990227_C','23990257_C','23990261_C','23990263_C','23990269_C'};

set road_comid_N3 within road_comid_all_N := {'23989727_C','23990391_C','23990417_C'};

set road_comid_N4 within road_comid_all_N := {};

set road_comid_P1 within road_comid_all_P := {};

set road_comid_P2 within road_comid_all_P := {};

set road_comid_P3 within road_comid_all_P := {};

set road_comid_P4 within road_comid_all_P := {'23989893_D','23990803_D'};


set urban_bmp :=
{'Extended_Dry_Detention_Basin','Grass_Swale_w_D','Green_Roof','Infiltration_Basin','Infiltration_Chamber','Infiltration_Trench','Permeable_Pavement','Porous_Pavement_w_UD','Sand_Filter_w_UD','Wet_Pond'};

set urban_pervbmp within urban_bmp :=
{'Porous_Pavement_w_UD'};

set road_bmp :=
{'Extended_Dry_Detention_Basin','Grass_Swale_w_D','Infiltration_Basin','Infiltration_Chamber','Infiltration_Trench','Permeable_Pavement','Porous_Pavement_w_UD','Sand_Filter_w_UD','Wet_Pond'};

set ag_bmp :=
{'Conservation','Contour_Farming','Fert_75','Fert_90','Filterstrip','MIN_TILL','Terrace_Waterway','Terrace_Only','Waterway_Only','Grade_Stabilization','Diversion','Cover_Crops','Drainage_Management','Manure_Injection'};

set ag_tilebmp within ag_bmp :=
{'Drainage_Management'};

set graz_bmp :=
{'Offsite_Water'};

set ripbuf_bmp :=
{'Grassed_Buffer','Forested_Buffer'};

set point_bmp :=
{'C','C_F','FOURBDP_M','FOURBDP_M_C_F','MLE','MLE_C','C_M','M','SBR','SBR_C','SBR_C_F','SBR_DNF_C_F_M','SBR_DNF_M','FOURBDP_MBR_M','FOURBDP_MBR_M_C','MLE_MBR','MLE_MBR_C'};

set loads_all :=
{'point', 'urban', 'road', 'ag', 'graz'};
  
set area_sub :=
{'ag', 'graz'};
set urban_area_sub :=
{'urban'};
set road_area_sub :=
{'road'};
param urban_comid_xwalk {urban_comid_all} symbolic;
param road_comid_xwalk {road_comid_all} symbolic;
param baseloads_N1 {comid_N1, loads_all} >= 0;
param baseloads_N2 {comid_N2, loads_all} >= 0;
param baseloads_N3 {comid_N3, loads_all} >= 0;
param baseloads_N4 {comid_N4, loads_all} >= 0;
param baseloads_P1 {comid_P1, loads_all} >= 0;
param baseloads_P2 {comid_P2, loads_all} >= 0;
param baseloads_P3 {comid_P3, loads_all} >= 0;
param baseloads_P4 {comid_P4, loads_all} >= 0;
param riparianload_N1 {c in comid_N1} >= 0;
param riparianload_N2 {c in comid_N2} >= 0;
param riparianload_N3 {c in comid_N3} >= 0;
param riparianload_N4 {c in comid_N4} >= 0;
param riparianload_P1 {c in comid_P1} >= 0;
param riparianload_P2 {c in comid_P2} >= 0;
param riparianload_P3 {c in comid_P3} >= 0;
param riparianload_P4 {c in comid_P4} >= 0;

param area {comid_all, area_sub} >=0;
    
param urban_area {urban_comid_all, urban_area_sub} >=0;
param road_area {road_comid_all, road_area_sub} >=0;
param urban_bmp_implementationpotential {urban_comid_all, urban_bmp} >= 0;
param ag_bmp_implementationpotential {comid_all, ag_bmp} >= 0;
param unbuffered_banklength {comid_all, ripbuf_bmp} >= 0;
param total_banklength {comid_all} >= 0;

param ag_costs_capital {comid_all, ag_bmp};
param ag_costs_operations {comid_all, ag_bmp};
param graz_costs_capital {comid_all, graz_bmp};
param graz_costs_operations {comid_all, graz_bmp};
param point_costs_capital {comid_all, point_bmp};
param point_costs_operations {comid_all, point_bmp};
param urban_costs_capital {urban_comid_all, urban_bmp};
param urban_costs_operations {urban_comid_all, urban_bmp};
param road_costs_capital {road_comid_all, road_bmp};
param road_costs_operations {road_comid_all, road_bmp};
param ripbuf_costs_capital {comid_all, ripbuf_bmp};
param ripbuf_costs_operations {comid_all, ripbuf_bmp};

param ag_effic_N {comid_all_N, ag_bmp};
param graz_effic_N {comid_all_N, graz_bmp};
param point_effic_N {comid_all_N, point_bmp};
param urban_effic_N {urban_comid_all_N, urban_bmp};
param road_effic_N {road_comid_all_N, road_bmp};


param ag_effic_P {comid_all_P, ag_bmp};
param graz_effic_P {comid_all_P, graz_bmp};
param point_effic_P {comid_all_P, point_bmp};
param urban_effic_P {urban_comid_all_P, urban_bmp};
param road_effic_P {road_comid_all_P,road_bmp};

param riparianremoval_N1 {c in comid_N1, b in ripbuf_bmp};
param riparianremoval_N2 {c in comid_N2, b in ripbuf_bmp};
param riparianremoval_N3 {c in comid_N3, b in ripbuf_bmp};
param riparianremoval_N4 {c in comid_N4, b in ripbuf_bmp};
param riparianremoval_P1 {c in comid_P1, b in ripbuf_bmp};
param riparianremoval_P2 {c in comid_P2, b in ripbuf_bmp};
param riparianremoval_P3 {c in comid_P3, b in ripbuf_bmp};
param riparianremoval_P4 {c in comid_P4, b in ripbuf_bmp};
param loads_lim_N1 >= 0;
param loads_lim_N2 >= 0;
param loads_lim_N3 >= 0;

param loads_lim_P4 >= 0;

param other_loads_N1 >= 0;
param other_loads_N2 >= 0;
param other_loads_N3 >= 0;
param other_loads_N4 >= 0;

param other_loads_P1 >= 0;
param other_loads_P2 >= 0;
param other_loads_P3 >= 0;
param other_loads_P4 >= 0;

param agcost_frac >=0;
    
param agBMP_minarea;
param grazBMP_minarea;
param urbanBMP_minarea;
param roadBMP_minarea;

param urban_frac_min {urban_bmp} >=0, <= 1;
param urban_frac_max {u in urban_bmp} >= urban_frac_min[u], <= 1;
param road_frac_min {road_bmp} >=0, <= 1;
param road_frac_max {r in road_bmp} >= road_frac_min[r], <= 1;
param ag_frac_min {ag_bmp} >= 0, <= 1;
param ag_frac_max {a in ag_bmp} >= ag_frac_min[a], <= 1;
param graz_frac_min {graz_bmp} >= 0, <= 1;
param graz_frac_max {g in graz_bmp} >= graz_frac_min[g], <= 1;
param ripbuf_frac_min {ripbuf_bmp} >= 0, <= 1;
param ripbuf_frac_max {b in ripbuf_bmp} >= ripbuf_frac_min[b], <= 1;
  
param ps_coef {c in comid_all, p in point_bmp} := (point_costs_capital[c, p] + 
    point_costs_operations[c, p]);
param urban_coef {x in urban_comid_all, u in urban_bmp} := urban_area[x,'urban'] * 
  (urban_costs_capital[x,u] + urban_costs_operations[x,u]) ;
param road_coef {h in road_comid_all, r in road_bmp} := road_area[h,'road'] * 
  (road_costs_capital[h,r] + road_costs_operations[h,r]) ;
param ag_coef {c in comid_all, a in ag_bmp} := agcost_frac * area[c,'ag'] * 
  (ag_costs_capital[c,a] + ag_costs_operations[c,a]) ;
param graz_coef {c in comid_all, g in graz_bmp} := agcost_frac * area[c,'graz'] * 
  (graz_costs_capital[c,g] + graz_costs_operations[c,g]) ;
param ripbuf_coef {c in comid_all, b in ripbuf_bmp} := 
  agcost_frac * (ripbuf_costs_capital[c, b] + ripbuf_costs_operations[c, b]);
  
var agBMP_bin {comid_all, ag_bmp} binary;
var grazBMP_bin {comid_all, graz_bmp} binary;
var urbanBMP_bin {urban_comid_all, urban_bmp} binary;
var roadBMP_bin {road_comid_all, road_bmp} binary;

var point_dec {c in comid_all, p in point_bmp} binary :=0;
var urban_frac {x in urban_comid_all, u in urban_bmp} 
   >= urban_frac_min[u] * urban_bmp_implementationpotential[x, u]  
   <= urban_frac_max[u] * urban_bmp_implementationpotential[x, u] :=0;
var ag_frac {c in comid_all, a in ag_bmp}
   >= ag_frac_min[a] * ag_bmp_implementationpotential[c, a]  
   <= ag_frac_max[a] * ag_bmp_implementationpotential[c, a] :=0;
var road_frac {road_comid_all, r in road_bmp} >= road_frac_min[r] <= road_frac_max[r] :=0;
var graz_frac {comid_all, g in graz_bmp} >= graz_frac_min[g] <= graz_frac_max[g] :=0;
var ripbuf_length {c in comid_all, b in ripbuf_bmp} 
   >= 0 
   <= unbuffered_banklength[c, b] * ripbuf_frac_max[b] := 0;
  
minimize cost: sum {c in comid_all, p in point_bmp} (ps_coef[c, p] * point_dec[c, p]) + 
sum {x in urban_comid_all, u in urban_bmp} (urban_coef[x,u] * urban_frac[x,u]) +
sum {h in road_comid_all, r in road_bmp} (road_coef[h,r] * road_frac[h,r]) +
sum {c in comid_all, a in ag_bmp} (ag_coef[c,a] * ag_frac[c,a]) +
sum {c in comid_all, g in graz_bmp} (graz_coef[c,g] * graz_frac[c,g]) +
sum {c in comid_all, b in ripbuf_bmp} (ripbuf_coef[c, b] * ripbuf_length[c, b]);
  

subject to total_loads_N1:
other_loads_N1 + sum {c in comid_N1} (baseloads_N1[c,'ag'] * (1 - sum {a in ag_bmp}(ag_effic_N[c,a] * ag_frac[c,a]))) + 
sum {c in comid_N1} (baseloads_N1[c,'graz'] * (1 - sum {g in graz_bmp}(graz_effic_N[c,g] * graz_frac[c,g]))) + 
sum {c in comid_N1} (baseloads_N1[c,'urban'] * (1 - sum {x in urban_comid_N1, u in urban_bmp : c == urban_comid_xwalk[x]}(urban_effic_N[x,u] * urban_frac[x,u]))) + 
sum {c in comid_N1} (baseloads_N1[c,'road'] * (1 - sum {h in road_comid_N1, r in road_bmp : c == road_comid_xwalk[h]}(road_effic_N[h,r] * road_frac[h,r]))) + 
sum {c in comid_N1} (baseloads_N1[c,'point'] * (1 - sum {p in point_bmp} (point_effic_N[c,p] * point_dec[c,p]))) - 
sum {c in comid_N1, b in ripbuf_bmp} (ripbuf_length[c, b] * riparianremoval_N1[c, b]) <= loads_lim_N1; 

subject to total_loads_N2:
other_loads_N2 + sum {c in comid_N2} (baseloads_N2[c,'ag'] * (1 - sum {a in ag_bmp}(ag_effic_N[c,a] * ag_frac[c,a]))) + 
sum {c in comid_N2} (baseloads_N2[c,'graz'] * (1 - sum {g in graz_bmp}(graz_effic_N[c,g] * graz_frac[c,g]))) + 
sum {c in comid_N2} (baseloads_N2[c,'urban'] * (1 - sum {x in urban_comid_N2, u in urban_bmp : c == urban_comid_xwalk[x]}(urban_effic_N[x,u] * urban_frac[x,u]))) + 
sum {c in comid_N2} (baseloads_N2[c,'road'] * (1 - sum {h in road_comid_N2, r in road_bmp : c == road_comid_xwalk[h]}(road_effic_N[h,r] * road_frac[h,r]))) + 
sum {c in comid_N2} (baseloads_N2[c,'point'] * (1 - sum {p in point_bmp} (point_effic_N[c,p] * point_dec[c,p]))) - 
sum {c in comid_N2, b in ripbuf_bmp} (ripbuf_length[c, b] * riparianremoval_N2[c, b]) <= loads_lim_N2; 

subject to total_loads_N3:
other_loads_N3 + sum {c in comid_N3} (baseloads_N3[c,'ag'] * (1 - sum {a in ag_bmp}(ag_effic_N[c,a] * ag_frac[c,a]))) + 
sum {c in comid_N3} (baseloads_N3[c,'graz'] * (1 - sum {g in graz_bmp}(graz_effic_N[c,g] * graz_frac[c,g]))) + 
sum {c in comid_N3} (baseloads_N3[c,'urban'] * (1 - sum {x in urban_comid_N3, u in urban_bmp : c == urban_comid_xwalk[x]}(urban_effic_N[x,u] * urban_frac[x,u]))) + 
sum {c in comid_N3} (baseloads_N3[c,'road'] * (1 - sum {h in road_comid_N3, r in road_bmp : c == road_comid_xwalk[h]}(road_effic_N[h,r] * road_frac[h,r]))) + 
sum {c in comid_N3} (baseloads_N3[c,'point'] * (1 - sum {p in point_bmp} (point_effic_N[c,p] * point_dec[c,p]))) - 
sum {c in comid_N3, b in ripbuf_bmp} (ripbuf_length[c, b] * riparianremoval_N3[c, b]) <= loads_lim_N3; 

subject to total_loads_P4:
other_loads_P4 + sum {c in comid_P4} (baseloads_P4[c,'ag'] * (1 - sum {a in ag_bmp}(ag_effic_P[c,a] * ag_frac[c,a]))) + 
sum {c in comid_P4} (baseloads_P4[c,'graz'] * (1 - sum {g in graz_bmp}(graz_effic_P[c,g] * graz_frac[c,g]))) + 
sum {c in comid_P4} (baseloads_P4[c,'urban'] * (1 - sum {x in urban_comid_P4, u in urban_bmp : c == urban_comid_xwalk[x]}(urban_effic_P[x,u] * urban_frac[x,u]))) + 
sum {c in comid_P4} (baseloads_P4[c,'road'] * (1 - sum {h in road_comid_P4, r in road_bmp : c == road_comid_xwalk[h]}(road_effic_P[h,r] * road_frac[h,r]))) + 
sum {c in comid_P4} (baseloads_P4[c,'point'] * (1 - sum {p in point_bmp} (point_effic_P[c,p] * point_dec[c,p]))) - 
sum {c in comid_P4, b in ripbuf_bmp} (ripbuf_length[c, b] * riparianremoval_P4[c, b]) <= loads_lim_P4; 

subject to riparian_loads_N1 {c in comid_N1}:
sum {b in ripbuf_bmp} (ripbuf_length[c, b] * riparianremoval_N1[c, b]) <= riparianload_N1[c]; 

subject to riparian_loads_N2 {c in comid_N2}:
sum {b in ripbuf_bmp} (ripbuf_length[c, b] * riparianremoval_N2[c, b]) <= riparianload_N2[c]; 

subject to riparian_loads_N3 {c in comid_N3}:
sum {b in ripbuf_bmp} (ripbuf_length[c, b] * riparianremoval_N3[c, b]) <= riparianload_N3[c]; 

subject to riparian_loads_P4 {c in comid_P4}:
sum {b in ripbuf_bmp} (ripbuf_length[c, b] * riparianremoval_P4[c, b]) <= riparianload_P4[c]; 

subject to ag_treat_min {c in comid_all, a in ag_bmp}:
ag_frac[c,a] * area[c,'ag'] >= agBMP_minarea * agBMP_bin[c,a];

subject to graz_treat_min {c in comid_all, g in graz_bmp}:
graz_frac[c,g] * area[c,'graz'] >= grazBMP_minarea * grazBMP_bin[c,g];

subject to urban_treat_min {x in urban_comid_all, u in urban_bmp}:
urban_frac[x,u] * urban_area[x,'urban'] >= urbanBMP_minarea * urbanBMP_bin[x,u];
  
subject to ag_frac_const {c in comid_all, a in ag_bmp}:
ag_frac[c,a] <= agBMP_bin[c,a];

subject to graz_frac_const {c in comid_all, g in graz_bmp}:
graz_frac[c,g] <= grazBMP_bin[c,g];
  
subject to urban_frac_const {x in urban_comid_all, u in urban_bmp}:
urban_frac[x,u] <= urbanBMP_bin[x,u];

subject to ag_frac_limit {c in comid_all}: 
sum {a in ag_bmp} ag_frac[c,a] <= 1;
# prevents multiple BMPs being implemented on the same area

subject to graz_frac_limit {c in comid_all}: 
sum {g in graz_bmp} graz_frac[c,g] <= 1;
# prevents multiple BMPs being implemented on the same area

subject to urban_frac_limit {x in urban_comid_all}: 
sum {u in urban_bmp} urban_frac[x,u] <= 1;
# prevents multiple BMPs being implemented on the same area

subject to point_dec_limit {c in comid_all}: 
sum {p in point_bmp} point_dec[c,p] <= 1;
# limits each WWTP to one upgrade

subject to total_banks {c in comid_all}:
sum {b in ripbuf_bmp} ripbuf_length[c,b] <= max {b in ripbuf_bmp} 
   unbuffered_banklength[c,b];
  


subject to roads_and_parkinglots {x in urban_comid_all}:
sum{up in urban_pervbmp} urban_frac[x, up] <= max {up in urban_pervbmp} 
   urban_bmp_implementationpotential[x,up];


subject to tiledrains {c in comid_all}:
sum{agti in ag_tilebmp} ag_frac[c, agti] <= max {agti in ag_tilebmp}
   ag_bmp_implementationpotential[c,agti];

