#### Member Months ####
mm_sql <-
  paste0(
    "select 
    is_in_scope
  	,to_date(member_yyyy_mm,'YYYY-MM') as reporting_date
  	,count(distinct axial_member_id) as mm
  from client_production.member_inclusion_exclusion mie
  where client_id = 20
  	and to_date(member_yyyy_mm,'YYYY-MM') <= '2021-12-31'
  group by 1,2
  order by 1"
  )

mm <- dbGetQuery(redconn,mm_sql) %>%
  mutate(reporting_date = ymd(reporting_date),
         yr = year(reporting_date))

#### Ambulance Services Trend ####
sql <-
"select 
	right(a.pat_id,8) as axial_member_id
	,a.claim_id 
	,a.event_id
	,a.cms_type_of_bill_code as bill_type
	,a.cms_place_of_service_code as pos_code 
	,a.cms_place_of_service_desc as pos_desc 
	,right(a.billing_prov_id,10) as billing_npi
	,npi.prov_desc as billing_name
	,npi.prov_primary_nucc_prov_taxonomy_desc as taxonomy
	,a.document_effective_date as service_date
	,to_date(a.document_effective_date_text_yyyymm, 'YYYYMM') as reporting_date
	,a.wayspring_csl_enc_cat_tier_1 
	,a.claim_class_cat 
	,a.hcpcs_code 
	,a.hcpcs_desc 
	,a.cms_revenue_center_code as rev_code
	,a.cms_revenue_center_desc as rev_desc 
	,a.principal_dx_icd10cm_code as icd_code 
	,a.principal_dx_icd10cm_desc as icd_desc 
	,a.principal_dx_ahrq_ccs_dx_cat_tier_1_desc as icd_cat1 
	,a.principal_dx_ahrq_ccs_dx_cat_tier_2_desc as icd_cat2 
	,a.is_principal_icd10_dx_targeted_substance_overdose as is_od 
	,a.is_principal_icd10_dx_sud as is_sud 
	,a.enc_principal_discharge_dx_code as enc_icd_code 
	,a.is_enc_discharge_dx_ursa_hosp_admission_type_tier_1_medical as is_medical_admit
	,a.is_enc_discharge_dx_ursa_hosp_admission_type_tier_1_surgical as is_surgical_admit
	,a.is_enc_discharge_dx_ursa_hosp_admission_type_tier_1_maternity as is_maternity_admit
	,a.document_plan_paid_amount as paid 
	,b.is_in_scope 
	,b.is_in_scope_financial_risk 
from ursa.dm_ws_edw_fin_001 a
inner join client_production.member_inclusion_exclusion b 
	on right(a.pat_id,8) = b.axial_member_id 
  	and a.document_effective_date_text_yyyymm = left(b.member_yyyy_mm,4)+right(b.member_yyyy_mm,2) 
left join ursa.mi_ursa_core_110	npi 
	on right(a.billing_prov_id,10) = npi.prov_npi 
where (a.wayspring_csl_enc_cat_tier_1 = '[21] Ambulance Services'
		or a.ursa_service_type_tier_1_desc = 'Transport')
	and a.payor_id  like '%20'
	and a.document_effective_date <= '2021-12-31'"

amb <- dbGetQuery(redconn, sql) %>%
  mutate(across(contains('date'),ymd),
         is_rev_missing = is.na(rev_code),
         is_hcpcs_missing = is.na(hcpcs_code))


amb %>%
  group_by(reporting_date) %>%
  summarise(pct_rev = mean(!is_rev_missing),
            pct_hcpcs = mean(!is_hcpcs_missing)) %>%
  ungroup() %>%
  pivot_longer(cols = starts_with('pct')) %>%
  ggplot(aes(reporting_date,value,color=name)) +
  geom_line() +
  theme_bw() +
  scale_y_continuous('% of Claims',labels = percent_format())

amb %>%
  group_by(reporting_date,is_in_scope) %>%
  tally() %>%
  ungroup() %>%
  left_join(mm) %>%
  mutate(per_k = 1000*n/mm) %>%
  ggplot(aes(reporting_date,per_k,color = is_in_scope)) +
  theme_bw() +
  geom_line()

amb %>%
  filter(substr(wayspring_csl_enc_cat_tier_1,2,3) %in% c('21','98','13')) %>%
  group_by(reporting_date,is_in_scope,wayspring_csl_enc_cat_tier_1) %>%
  tally() %>%
  ungroup() %>%
  left_join(mm) %>%
  mutate(per_k = 1000*n/mm) %>%
  ggplot(aes(reporting_date,per_k,color = is_in_scope)) +
  facet_wrap(~wayspring_csl_enc_cat_tier_1) +
  theme_bw() +
  geom_line()


amb %>%
  mutate(is_pos_code_missing = is.na(pos_code),
         is_bill_type_code_missing = is.na(bill_type)) %>%
  group_by(reporting_date,is_bill_type_code_missing,claim_class_cat) %>%
  tally() %>%
  ungroup() %>%
  ggplot(aes(reporting_date,n,color=is_bill_type_code_missing)) +
  facet_wrap(~claim_class_cat) +
  geom_line() +
  theme_bw() +
  scale_x_date('Reporting Date',date_minor_breaks = '1 month',date_breaks = '3 months',date_labels = '%m-%y') +
  ylab('# Claims')

ambulance %>%
  filter(axial_member_id==56609075) %>%
  group_by(icd_code_1,icd_code_1_desc) %>%
  tally() %>%
  arrange(-n)

ambulance %>%
  filter(axial_member_id==56802933) %>%
  group_by(service_from_date) %>%
  summarise(n = n_distinct(axial_claim_id)) %>%
  filter(n>1)

ambulance %>%
  filter(axial_member_id==56802933 & service_from_date=='2021-10-22')

#### Drug Testing Spike ####
sql <- 
"select 
	right(a.pat_id,8) as axial_member_id
	,a.claim_id 
	,claim_id + document_effective_date + wayspring_csl_enc_cat_tier_1 as event_id
	,a.cms_type_of_bill_code as bill_type
	,a.cms_place_of_service_code as pos_code 
	,a.cms_place_of_service_desc as pos_desc 
	,right(a.billing_prov_id,10) as billing_npi
	,npi.prov_desc as billing_name
	,npi.prov_primary_nucc_prov_taxonomy_desc as taxonomy
	,a.document_effective_date as service_date
	,to_date(a.document_effective_date_text_yyyymm, 'YYYYMM') as reporting_date
	,a.wayspring_csl_enc_cat_tier_1 
	,a.claim_class_cat 
	,a.hcpcs_code 
	,a.hcpcs_desc 
	,a.cms_revenue_center_code as rev_code
	,a.cms_revenue_center_desc as rev_desc 
	,a.principal_dx_icd10cm_code as icd_code 
	,a.principal_dx_icd10cm_desc as icd_desc 
	,a.principal_dx_ahrq_ccs_dx_cat_tier_1_desc as icd_cat1 
	,a.principal_dx_ahrq_ccs_dx_cat_tier_2_desc as icd_cat2 
	,a.is_principal_icd10_dx_targeted_substance_overdose as is_od 
	,a.is_principal_icd10_dx_sud as is_sud 
	,a.enc_principal_discharge_dx_code as enc_icd_code 
	,a.is_enc_discharge_dx_ursa_hosp_admission_type_tier_1_medical as is_medical_admit
	,a.is_enc_discharge_dx_ursa_hosp_admission_type_tier_1_surgical as is_surgical_admit
	,a.is_enc_discharge_dx_ursa_hosp_admission_type_tier_1_maternity as is_maternity_admit
	,a.document_plan_paid_amount as paid 
	,b.is_in_scope 
	,b.is_in_scope_financial_risk 
from ursa.dm_ws_edw_fin_001 a
inner join client_production.member_inclusion_exclusion b 
	on right(a.pat_id,8) = b.axial_member_id 
  	and a.document_effective_date_text_yyyymm = left(b.member_yyyy_mm,4)+right(b.member_yyyy_mm,2) 
left join ursa.mi_ursa_core_110	npi 
	on right(a.billing_prov_id,10) = npi.prov_npi 
where a.hcpcs_code in ('0007U','0011U','G0431','G0434','G0477','G0479','G0480','G0481','G0482','G0483',
	'G0659','G6030','G6031','G6032','80301','80302','80303','80304','80305','80306','80307','80320',
	'80321','80322','80323','80324','80325','80326','80327','80328','80329','80330','80331','80332',
	'80333','80334','80335','80336','80337','80338','80339','80340','80341','80342','80343','80344',
	'80345','80346','80347','80348','80349','80350','80351','80352','80353','80354','80355','80356',
	'80357','80358','80359','80360','80361','80362','80363','80364','80365','80366','80367','80368',
	'80369','80370','80371','80372','80373','80374','80375','80376','80377','82542','82570','82646',
	'82649','83840','83925','83986','83992','84311')
	and a.payor_id  like '%20'
	and a.document_effective_date <= '2021-12-31'
	and a.document_effective_date >= '2021-01-01'"

drug_tests <- dbGetQuery(redconn, sql) %>%
  mutate(across(contains('date'),ymd))

drug_tests %>%
  filter(is_in_scope) %>%
  mutate(in_drug_tests = (substr(wayspring_csl_enc_cat_tier_1,2,3)=='22')) %>%
  group_by(reporting_date,is_in_scope,in_drug_tests) %>%
  tally() %>%
  ungroup() %>%
  left_join(mm) %>%
  mutate(per_k = 1000*n/mm) %>%
  ggplot(aes(reporting_date,per_k)) +
  facet_wrap(~in_drug_tests,scales = 'free_y') +
  theme_bw() +
  scale_y_continuous('# Tests / 1,000',limits = c(0,NA)) +
  geom_line()

drug_tests %>%
  filter(is_in_scope) %>%
  group_by(reporting_date,is_in_scope) %>%
  tally() %>%
  ungroup() %>%
  left_join(mm) %>%
  mutate(per_k = 1000*n/mm) %>%
  ggplot(aes(reporting_date,per_k)) +
  theme_bw() +
  scale_y_continuous('# Tests / 1,000',limits = c(0,NA)) +
  geom_line()


drug_tests %>%
  filter(reporting_date=='2021-07-01') %>%
  group_by(billing_name) %>%
  tally() %>%
  arrange(-n)

#### Hospital IP Summary ####
events %>%
  mutate(icd_desc = case_when(icd_code=='U07.1' ~ 'COVID-19',
                              TRUE ~ icd_desc)) %>%
  filter(grepl('Hospital IP',wayspring_csl_enc_cat_tier_1)) %>% 
  group_by(icd_code,icd_desc,wayspring_csl_enc_cat_tier_1) %>%
  summarise(`# Events` = n(),
            `# Members` = n_distinct(axial_member_id),
            `Total Paid` = sum(paid)) %>%
  arrange(-`Total Paid`) %>%
  mutate(`Total Paid` = dollar(`Total Paid`,1)) %>%
  rename(`ICD Code` = icd_code,
         `ICD Desc` = icd_desc,
         `Wayspring Category` = wayspring_csl_enc_cat_tier_1) %>% 
  write.csv('temp.csv',row.names = FALSE)

#### Outpatient Surgery ####
events %>%
  filter(grepl('08',wayspring_csl_enc_cat_tier_1)) %>%
  group_by(icd_code,icd_desc) %>%
  summarise(`# Encounters` = n(),
            `# Members` = n_distinct(axial_member_id),
            `Total Paid` = sum(paid),
            `Avg. Paid` = mean(paid),
            `% w/ ED Encounter` = percent(mean(is_emergency_dept_visit_parent_enc),.1)) %>%
  ungroup() %>%
  arrange(-`# Encounters`) %>%
  mutate(across(contains('Paid'),dollar,1))

#### Outpatient - SUD ####
events %>%
  filter(grepl('12',wayspring_csl_enc_cat_tier_1)) %>%
  mutate(facility_name = replace_na(facility_name, 'No Facility Indicated')) %>%
  group_by(facility_name) %>%
  summarise(`# Encounters` = n(),
            `# Members` = n_distinct(axial_member_id),
            `Total Paid` = sum(paid),
            `Avg. Paid` = mean(paid)) %>%
  ungroup() %>%
  arrange(-`# Encounters`) %>%
  mutate(across(contains('Paid'),dollar,1)) %>%
  rename(Facility = facility_name) %>%
  as.data.frame() %>%
  write.csv('temp.csv',row.names=FALSE)

events %>%
  filter(grepl('12',wayspring_csl_enc_cat_tier_1)) %>%
  summarise(`# Encounters` = n(),
            `# Members` = n_distinct(axial_member_id),
            `Total Paid` = sum(paid),
            `Avg. Paid` = mean(paid)) %>%
  ungroup() %>%
  arrange(-`# Encounters`) %>%
  mutate(across(contains('Paid'),dollar,1))

outpt_sql <-
"select 
	right(a.pat_id,8) as axial_member_id
	,a.claim_id 
	,a.event_id
	,c.event_facility_prov_npi as event_facility_npi
	,npi2.prov_desc as event_facility_desc
	,a.cms_type_of_bill_code as bill_type
	,a.cms_place_of_service_code as pos_code 
	,a.cms_place_of_service_desc as pos_desc 
	,right(a.billing_prov_id,10) as billing_npi
	,npi.prov_desc as billing_name
	,npi.prov_primary_nucc_prov_taxonomy_desc as taxonomy
	,a.document_effective_date as service_date
	,to_date(a.document_effective_date_text_yyyymm, 'YYYYMM') as reporting_date
	,a.wayspring_csl_enc_cat_tier_1 
	,a.claim_class_cat 
	,a.hcpcs_code 
	,a.hcpcs_desc 
	,a.cms_revenue_center_code as rev_code
	,a.cms_revenue_center_desc as rev_desc 
	,a.principal_dx_icd10cm_code as icd_code 
	,a.principal_dx_icd10cm_desc as icd_desc 
	,a.principal_dx_ahrq_ccs_dx_cat_tier_1_desc as icd_cat1 
	,a.principal_dx_ahrq_ccs_dx_cat_tier_2_desc as icd_cat2 
	,a.is_principal_icd10_dx_targeted_substance_overdose as is_od 
	,a.is_principal_icd10_dx_sud as is_sud 
	,a.enc_principal_discharge_dx_code as enc_icd_code 
	,a.document_plan_paid_amount as paid
from ursa.dm_ws_edw_fin_001 a
inner join client_production.member_inclusion_exclusion b 
	on right(a.pat_id,8) = b.axial_member_id 
  	and a.document_effective_date_text_yyyymm = left(b.member_yyyy_mm,4)+right(b.member_yyyy_mm,2) 
  	and b.is_in_scope_financial_risk
left join ursa.mi_ursa_core_110	npi 
	on right(a.billing_prov_id,10) = npi.prov_npi 
left join ursa.dm_ws_edw_enc_001 c 
	on a.event_id  = c.event_id
left join ursa.mi_ursa_core_110	npi2 
	on c.event_facility_prov_npi = npi2.prov_npi 
where a.wayspring_csl_enc_cat_tier_1 = '[12] SUD-Outpatient'
	and a.payor_id  like '%20'
	and a.document_effective_date >= '2021-01-01'
	and a.document_effective_date <= '2021-12-31'"

outpt <- dbGetQuery(redconn,outpt_sql) %>%
  mutate(across(contains('date'),ymd)) %>%
  mutate(billing_name = case_when(!is.na(billing_name) ~ billing_name,
                                  billing_npi %in% c(1659949501,1023686284) ~ 'CORAS WELLNESS AND BEHAVIORAL HEALTH, LLC'))

outpt %>%
  group_by(event_facility_desc,billing_npi,billing_name,hcpcs_code,hcpcs_desc) %>%
  summarise(`# Events` = n_distinct(event_id),
            `# Claim Lines` = n(),
            `Total Paid` = dollar(sum(paid),1),
            `Avg. Paid / Claim` = dollar(mean(paid))) %>%
  ungroup() %>%
  arrange(event_facility_desc,-`# Claim Lines`) %>%
  rename(`Event Facility` = event_facility_desc,
         `Billing NPI` = billing_npi,
         `Billing NPI Name` = billing_name,
         `HCPCS Code` = hcpcs_code,
         `HCPCS Desc` = hcpcs_desc) %>%
  write.csv('temp.csv',row.names = FALSE)

outpt %>%
  filter(grepl('H',hcpcs_code)) %>%
  group_by(billing_name,hcpcs_code,hcpcs_desc) %>%
  summarise(n = n(),
            avg_paid = mean(paid),
            min_paid = min(paid),
            max_paid = max(paid)) %>%
  ungroup() %>%
  mutate(hcpcs_desc = case_when(hcpcs_code=='H0005' ~ 'Alc/Drug Services: Group Counseling',
                                hcpcs_code=='H0015' ~ 'Alc/Drug Services: IOP (Trt >= 3 hours)',
                                hcpcs_code=='H0035' ~ 'Mental Health Partial Hosp. (Trt < 24 hours)')) %>%
  ggplot(aes(avg_paid,billing_name)) +
  facet_wrap(~hcpcs_code + hcpcs_desc) +
  theme_bw() +
  geom_point(aes(size = n)) +
  geom_linerange(aes(xmin=min_paid,xmax=max_paid)) +
  geom_text(aes(label = paste0(dollar(avg_paid),' (',n,')')),size=3,hjust=-.1) +
  theme(strip.text = element_text(size=8)) +
  ylab('Billing Provider') +
  scale_x_continuous('Avg. Paid / Claim Line',labels = dollar_format(),limits = c(0,555)) +
  scale_size('# Claim Lines') +
  ggtitle('Avg. Paid per Claim by Billing Provider','Linerange represents min and max values')
