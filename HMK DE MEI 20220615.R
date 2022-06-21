#test
require(RODBC)
require(dplyr) #set of commands for SQL translation into R
require(tidyr) #chaining together operations
require(dbplyr) #Specifically for translating SQL db connections
require(tibble)
require(sqldf)
require(lubridate)
require(ggplot2)
require(scales)
# require(xlsx)
require(openxlsx)
require(zoo)
require(stringr)

source('/home/dblakemore/Exasol Con.R')
source('/home/dblakemore/Redshift Con.R')
source('/home/dblakemore/Functions/WayspringBranding.R')


client_id <- 20
ursa_client_id <- 'WAYSPRING_EDW|20'
end_date <- ymd('2021-12-31')

#### Pull Data ####
# Member Months
mm_sql <-
  paste0(
  "select 
  	to_date(member_yyyy_mm,'YYYY-MM') as reporting_date
  	,is_in_scope_financial_risk as is_in_scope
  	,count(distinct axial_member_id) as mm
  from client_production.member_inclusion_exclusion mie
  where client_id = ", client_id, "
    and to_date(member_yyyy_mm,'YYYY-MM') >= '2019-01-01'
  	and to_date(member_yyyy_mm,'YYYY-MM') <= '",end_date, "'
  group by 1,2
  order by 1"
  )

mm <- dbGetQuery(redconn,mm_sql) %>%
  mutate(reporting_date = ymd(reporting_date),
         yr = year(reporting_date))

start_date <- min(mm$reporting_date)

mm_yr <-
  mm %>%
  group_by(yr,is_in_scope) %>%
  summarise(mm = sum(mm)) %>%
  ungroup()

# Cost & Utilization
cost_util_sql <-
  paste0(
  "select 
  	a.payor_id 
  	,a.payor_desc 
  	,to_date(a.document_effective_date_text_yyyymm,'YYYYMM') as reporting_date
  	,a.wayspring_csl_enc_cat_tier_1 
  	,count(distinct a.pat_id) as members
  	,count(distinct 
  	case when event_id is not null and wayspring_csl_enc_cat_tier_1 not in ('[12] SUD-Outpatient','[11] Medical MAT')
  		then event_id 
  		else claim_id + document_effective_date + wayspring_csl_enc_cat_tier_1
  	end) as events
  	,sum(a.document_plan_paid_amount) as paid 
  from ursa.dm_ws_edw_fin_001 a
  inner join client_production.member_inclusion_exclusion b 
  	on right(a.pat_id,8) = b.axial_member_id 
  	and a.document_effective_date_text_yyyymm = left(b.member_yyyy_mm,4)+right(b.member_yyyy_mm,2) 
  	and b.is_in_scope_financial_risk is true 
  	and b.is_first_month_sud is false 
  where claim_class_cat <> '[03] Pharmacy'
    and a.payor_id  = '", ursa_client_id, "'
    and a.document_effective_date >= '2019-01-01'
    and a.document_effective_date <= '", end_date, "'
  group by 1,2,3,4"
  )

cost_util <- dbGetQuery(redconn, cost_util_sql) %>%
  mutate(reporting_date = ymd(reporting_date),
         yr = year(reporting_date))

cost_util_yr_sql <-
  paste0(
  "select 
  	a.payor_id 
  	,a.payor_desc 
  	,left(a.document_effective_date_text_yyyymm,4) as yr
  	,a.wayspring_csl_enc_cat_tier_1 
  	,count(distinct a.pat_id) as members
  	,count(distinct 
  	case when event_id is not null and wayspring_csl_enc_cat_tier_1 not in ('[12] SUD-Outpatient','[11] Medical MAT')
  		then event_id 
  		else claim_id + document_effective_date + wayspring_csl_enc_cat_tier_1
  	end) as events
  	,sum(a.document_plan_paid_amount) as paid 
  from ursa.dm_ws_edw_fin_001 a
  inner join client_production.member_inclusion_exclusion b 
  	on right(a.pat_id,8) = b.axial_member_id 
  	and a.document_effective_date_text_yyyymm = left(b.member_yyyy_mm,4)+right(b.member_yyyy_mm,2) 
  	and b.is_in_scope_financial_risk is true 
  	and b.is_first_month_sud is false 
  where claim_class_cat <> '[03] Pharmacy'
    and a.payor_id  = '", ursa_client_id, "'
    and a.document_effective_date >= '2019-01-01'
    and a.document_effective_date <= '", end_date, "'
  group by 1,2,3,4"
  )

cost_util_yr <- dbGetQuery(redconn, cost_util_yr_sql)

cost_util_yr$yr <- as.numeric(cost_util_yr$yr)

cost_util_yr_total_sql <-
  paste0(
    "select 
  	a.payor_id 
  	,a.payor_desc 
  	,left(a.document_effective_date_text_yyyymm,4) as yr
  	,count(distinct a.pat_id) as members
  	,count(distinct 
  	case when event_id is not null and wayspring_csl_enc_cat_tier_1 not in ('[12] SUD-Outpatient','[11] Medical MAT')
  		then event_id 
  		else claim_id + document_effective_date + wayspring_csl_enc_cat_tier_1
  	end) as events
  	,sum(a.document_plan_paid_amount) as paid 
  from ursa.dm_ws_edw_fin_001 a
  inner join client_production.member_inclusion_exclusion b 
  	on right(a.pat_id,8) = b.axial_member_id 
  	and a.document_effective_date_text_yyyymm = left(b.member_yyyy_mm,4)+right(b.member_yyyy_mm,2) 
  	and b.is_in_scope_financial_risk is true 
  	and b.is_first_month_sud is false 
  where claim_class_cat <> '[03] Pharmacy'
    and a.payor_id  = '", ursa_client_id, "'
    and a.document_effective_date >= '2019-01-01'
    and a.document_effective_date <= '", end_date, "'
  group by 1,2,3"
  )

cost_util_yr_total <- dbGetQuery(redconn, cost_util_yr_total_sql)

cost_util_yr_total <-
  cost_util_yr_total %>%
  mutate(yr = as.numeric(yr),
         wayspring_csl_enc_cat_tier_1 = 'Total')

cost_util_yr <- bind_rows(cost_util_yr,cost_util_yr_total)

data <-
  mm %>%
  filter(is_in_scope) %>%
  select(-is_in_scope) %>%
  left_join(cost_util) %>%
  arrange(client_id,wayspring_csl_enc_cat_tier_1,reporting_date) %>%
  group_by(wayspring_csl_enc_cat_tier_1) %>%
  mutate(pmpm = paid / mm,
         per_k = 1000*events / mm,
         paid_12mths = rollapplyr(paid, 12, sum, partial=TRUE),
         events_12mths = rollapplyr(events, 12, sum, partial=TRUE),
         mm_12mths = rollapplyr(mm, 12, sum, partial=TRUE)
         ) %>%
  ungroup() %>%
  mutate(across(ends_with('12mths'), ~ifelse(reporting_date<=start_date+months(10),NA,.x)),
         pmpm_12mths = paid_12mths / mm_12mths,
         per_k_12mths = 1000*events_12mths / mm_12mths)


# Events
events_sql <-
  paste0(
    "with paid as (
    	select event_id
    		,sum(document_plan_paid_amount) as paid 
    	from ursa.dm_ws_edw_fin_001
    	where payor_id = 'WAYSPRING_EDW|20'
    	group by event_id 
    )
    select right(a.pat_id,8) as axial_member_id
     	,to_date(a.event_start_month_text_yyyymm,'YYYYMM') as reporting_date
     	,a.*
     	,c.prov_desc as facility_name
     	,paid.paid
    from ursa.dm_ws_edw_enc_001 a
    inner join client_production.member_inclusion_exclusion b 
    	on right(a.pat_id,8) = b.axial_member_id 
    	and a.event_start_month_text_yyyymm = left(b.member_yyyy_mm,4)+right(b.member_yyyy_mm,2) 
    	and b.is_in_scope_financial_risk is true 
    	and b.is_first_month_sud is false 
    left join ursa.mi_ursa_core_110 c 
    	on a.event_facility_prov_npi = c.prov_npi
    left join paid
    	on a.event_id  = paid.event_id
    where wayspring_csl_enc_cat_tier_1 not like '%Rx%'
    	and a.event_primary_payor_id = '", ursa_client_id,"'
    	and a.event_start_date between '", end_date - years(1) + days(1),"' and '",end_date,"'"
  )

events <- dbGetQuery(redconn,events_sql) %>%
  mutate(across(contains('date'),ymd)) %>%
  rename(los = event_los_in_elapsed_midnights,
         icd_code = enc_principal_discharge_dx_icd10cm_code,
         icd_desc = enc_principal_discharge_dx_icd10cm_desc)

events %>%
  filter(grepl('Maternity',wayspring_csl_enc_cat_tier_1)) %>%
  group_by(icd_code,icd_desc) %>%
  tally() %>% 
  arrange(-n)

#### Trend ####
lt <-c('Actual'='solid','Rolling 12 Mth'='dashed')

# png(paste0('pmpm_trend_client_id',client_id,'.png'), width = 1200, height = 800)
pmpm_trend <-
  data %>%
  ggplot(aes(reporting_date,pmpm)) +
  theme_bw() +
  facet_wrap(~wayspring_csl_enc_cat_tier_1, scales = 'free_y') +
  geom_line(size = .25,aes(linetype = 'Actual')) +
  geom_line(aes(y=pmpm_12mths, linetype = 'Rolling 12 Mth')) +
  scale_linetype_manual(name = 'Metric', breaks = c('Actual','Rolling 12 Mth'), values = c('solid','dashed')) +
  geom_vline(xintercept = ymd('2020-03-01'), size = .5, linetype = 'dashed') +
  scale_x_date('Event Month', date_breaks = '12 months', date_minor_breaks = '3 months',date_labels = '%Y') +
  scale_y_continuous('Amount Paid PMPM',labels = dollar_format(), limits = c(0,NA)) +
  geom_text(aes(x=ymd('2020-03-01'),y=.5,label = 'COVID'),size=3,hjust=0)


# png(paste0('util_trend_client_id',client_id,'.png'), width = 1200, height = 800)
util_trend <-
  data %>%
  ggplot(aes(reporting_date,per_k)) +
  theme_bw() +
  facet_wrap(~wayspring_csl_enc_cat_tier_1, scales = 'free_y') +
  geom_line(size = .25,aes(linetype = 'Actual')) +
  geom_line(aes(y=per_k_12mths, linetype = 'Rolling 12 Mth')) +
  geom_vline(xintercept = ymd('2020-03-01'), size = .5, linetype = 'dashed') +
  scale_linetype_manual(name = 'Metric', breaks = c('Actual','Rolling 12 Mth'), values = c('solid','dashed')) +
  scale_x_date('Event Month', date_breaks = '12 months', date_minor_breaks = '3 months',date_labels = '%Y') +
  scale_y_continuous('Events / 1,000 In-Scope Members') +
  geom_text(aes(x=ymd('2020-03-01'),y=.5,label = 'COVID'),size=3,hjust=0)


#### Annual Summary ####
# cost_util_yr %>%
#   left_join(mm_yr) %>%
#   group_by(wayspring_csl_enc_cat_tier_1) %>%
#   tally() %>%
#   filter(n!=3)

cost_util_summary <-
  cost_util_yr %>%
  left_join(mm_yr %>% filter(is_in_scope)) %>%
  group_by(yr) %>%
  mutate(paid_total = sum(ifelse(wayspring_csl_enc_cat_tier_1=='Total',paid,0))) %>%
  ungroup() %>%
  mutate(`Amount Paid PMPM` = paid / mm,
         `Events / 1,000` = 12000*events / mm,
         `% Paid` = paid / paid_total,
         # across(c('members','events'),comma,1),
         paid = paid,
  ) %>%
  arrange(wayspring_csl_enc_cat_tier_1,yr) %>%
  rename(`# Members` = members,
         `# Events` = events,
         `Spend Category` = wayspring_csl_enc_cat_tier_1,
         Paid = paid
  ) %>%
  select(-c('payor_id','payor_desc','paid_total','mm')) %>%
  pivot_wider(id_cols = `Spend Category`,
              names_from = yr,
              values_from = c('# Members','# Events','Paid','Amount Paid PMPM','Events / 1,000','% Paid')) %>%
  as.data.frame()

# cost_util_summary_sheet <- createSheet(wb, 'Cost & Util Summary')
# addDataFrame(cost_util_summary,cost_util_summary_sheet,row.names = FALSE, startRow = 2)

#### SUD Residential Stay Summary ####
sud_res <-
  events %>%
  filter(grepl('01',wayspring_csl_enc_cat_tier_1)) %>%
  mutate(facility_name = case_when(grepl('AQUILA',facility_name) ~ 'AQUILA OF DELAWARE',
                                   event_facility_prov_npi %in% c(1962072959,1023686284) ~ 'CORAS WELLNESS AND BEHAVIORAL HEALTH, LLC',
                                   TRUE ~ facility_name),
         `Services Provided` = '') %>%
  group_by(facility_name,`Services Provided`) %>%
  summarise(`# Stays` = n(),
            `# Members` = n_distinct(axial_member_id),
            `Total # Days` = sum(los),
            `Avg. LOS` = round(mean(los),1),
            `Days / Member` = round(sum(los) / n_distinct(axial_member_id),1),
            `% Residential Spend` = sum(paid) / sum(events$paid[grepl('01',events$wayspring_csl_enc_cat_tier_1)]),
            `Total Paid` = sum(paid),
            `Paid / Member` = sum(paid) / n_distinct(axial_member_id),
            `Cost / Day` = sum(paid) / sum(los)
  ) %>%
  arrange(-`Total Paid`) %>%
  ungroup() %>%
  rename(`Facility Name` = facility_name) %>%
  as.data.frame()

sud_res_total <-
  events %>%
  filter(grepl('01',wayspring_csl_enc_cat_tier_1)) %>%
  mutate(facility_name = 'Total',
         `Services Provided` = '') %>%
  group_by(facility_name,`Services Provided`) %>%
  summarise(`# Stays` = n(),
            `# Members` = n_distinct(axial_member_id),
            `Total # Days` = sum(los),
            `Avg. LOS` = round(mean(los),1),
            `Days / Member` = round(sum(los) / n_distinct(axial_member_id),1),
            `% Residential Spend` = sum(paid) / sum(events$paid[grepl('01',events$wayspring_csl_enc_cat_tier_1)]),
            `Total Paid` = sum(paid),
            `Paid / Member` = sum(paid) / n_distinct(axial_member_id),
            `Cost / Day` = sum(paid) / sum(los)
  ) %>%
  arrange(-`Total Paid`) %>%
  ungroup() %>%
  rename(`Facility Name` = facility_name) %>%
  as.data.frame()

#### Outpatient SUD ####
outpt_sud <-
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
  # mutate(across(contains('Paid'),dollar,1)) %>%
  rename(Facility = facility_name) %>%
  as.data.frame()

outpt_sud_total <-
  events %>%
  filter(grepl('12',wayspring_csl_enc_cat_tier_1)) %>%
  mutate(facility_name = 'Total') %>%
  group_by(facility_name) %>%
  summarise(`# Encounters` = n(),
            `# Members` = n_distinct(axial_member_id),
            `Total Paid` = sum(paid),
            `Avg. Paid` = mean(paid)) %>%
  ungroup() %>%
  arrange(-`# Encounters`) %>%
  # mutate(across(contains('Paid'),dollar,1)) %>%
  rename(Facility = facility_name) %>%
  as.data.frame()


outpt_sql <-
paste0(
  "select 
	right(a.billing_prov_id,10) as billing_npi
	,npi.prov_desc as billing_name
	,case when a.hcpcs_code is null then a.cms_revenue_center_code else a.hcpcs_code end as code  
	,case when a.hcpcs_code is null then a.cms_revenue_center_desc else a.hcpcs_desc end as code_desc
	,case when a.hcpcs_code is null then 'Revenue' else 'HCPCS' end as code_type
	,count(distinct a.event_id) as events 
	,count(*) as claim_lines
	,sum(a.document_plan_paid_amount) as total_paid
	,avg(a.document_plan_paid_amount) as avg_paid
	,min(a.document_plan_paid_amount) as min_paid 
	,max(a.document_plan_paid_amount) as max_paid
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
    	and a.payor_id = '", ursa_client_id,"'
    	and a.document_effective_date between '", end_date - years(1) + days(1),"' and '",end_date,"'
group by 1,2,3,4,5"
)

outpt <- dbGetQuery(redconn,outpt_sql) %>%
  mutate(billing_name = case_when(!is.na(billing_name) ~ billing_name,
                                  billing_npi %in% c(1659949501,1023686284) ~ 'CORAS WELLNESS AND BEHAVIORAL HEALTH, LLC'))

outpt_sud_plot <- 
  outpt %>%
  filter(grepl('H',code)) %>%
  ggplot(aes(avg_paid,billing_name)) +
  facet_wrap(~code + code_desc) +
  theme_bw() +
  geom_point(aes(size = claim_lines)) +
  geom_linerange(aes(xmin=min_paid,xmax=max_paid)) +
  geom_text(aes(label = paste0(dollar(avg_paid),' (',claim_lines,')')),size=3,hjust=-.1) +
  theme(strip.text = element_text(size=8)) +
  ylab('Billing Provider') +
  scale_x_continuous('Avg. Paid / Claim Line',labels = dollar_format(),limits = c(0,555)) +
  scale_size('# Claim Lines') +
  ggtitle('Avg. Paid per Claim by Billing Provider','Linerange represents min and max values')

outpt_sud_detail <-
  outpt %>%
  select(-c('min_paid','max_paid')) %>%
  rename(`Billing NPI` = billing_npi,
         `Billing NPI Name` = billing_name,
         `Code` = code,
         `Code Desc` = code_desc,
         `Code Type` = code_type,
         `# Events` = events,
         `# Claim Lines` = claim_lines,
         `Total Paid` = total_paid,
         `Avg. Paid / Claim` = avg_paid) %>%
  as.data.frame()

#### Hospital IP Summary ####
admits <-
  events %>%
  mutate(icd_desc = case_when(icd_code=='U07.1' ~ 'COVID-19',
                              TRUE ~ icd_desc)) %>%
  filter(grepl('Hospital IP',wayspring_csl_enc_cat_tier_1)) %>% 
  group_by(icd_code,icd_desc,wayspring_csl_enc_cat_tier_1) %>%
  summarise(`# Events` = n(),
            `# Members` = n_distinct(axial_member_id),
            `Total Paid` = sum(paid),
            `Paid / Admit` = mean(paid)) %>%
  arrange(-`Total Paid`) %>%
  # mutate(`Total Paid` = dollar(`Total Paid`,1)) %>%
  rename(`ICD Code` = icd_code,
         `ICD Desc` = icd_desc,
         `Wayspring Category` = wayspring_csl_enc_cat_tier_1) %>%
  as.data.frame()

admits_total <-
  events %>%
  mutate(icd_desc = case_when(icd_code=='U07.1' ~ 'COVID-19',
                              TRUE ~ icd_desc)) %>%
  filter(grepl('Hospital IP',wayspring_csl_enc_cat_tier_1)) %>% 
  mutate(icd_code = 'Total',
         icd_desc = '',
         wayspring_csl_enc_cat_tier_1 = '') %>%
  group_by(icd_code,icd_desc,wayspring_csl_enc_cat_tier_1) %>%
  summarise(`# Events` = n(),
            `# Members` = n_distinct(axial_member_id),
            `Total Paid` = sum(paid),
            `Paid / Admit` = mean(paid)) %>%
  arrange(-`Total Paid`) %>%
  # mutate(`Total Paid` = dollar(`Total Paid`,1)) %>%
  rename(`ICD Code` = icd_code,
         `ICD Desc` = icd_desc,
         `Wayspring Category` = wayspring_csl_enc_cat_tier_1) %>%
  as.data.frame()

#### Outpatient Surgery Summary ####
outpt_surg <-
  events %>%
  filter(grepl('08',wayspring_csl_enc_cat_tier_1)) %>%
  group_by(icd_code,icd_desc) %>%
  summarise(`# Encounters` = n(),
            `# Members` = n_distinct(axial_member_id),
            `Total Paid` = sum(paid),
            `Avg. Paid` = mean(paid),
            `% w/ ED Encounter` = mean(is_emergency_dept_visit_parent_enc)) %>%
  ungroup() %>%
  arrange(-`# Encounters`) %>%
  # mutate(across(contains('Paid'),dollar,1)) %>%
  rename(`ICD Code` = icd_code,
         `ICD Desc` = icd_desc) %>%
  as.data.frame()

outpt_surg_total <-
  events %>%
  mutate(icd_code = 'Total',
         icd_desc = '') %>%
  filter(grepl('08',wayspring_csl_enc_cat_tier_1)) %>%
  group_by(icd_code,icd_desc) %>%
  summarise(`# Encounters` = n(),
            `# Members` = n_distinct(axial_member_id),
            `Total Paid` = sum(paid),
            `Avg. Paid` = mean(paid),
            `% w/ ED Encounter` = mean(is_emergency_dept_visit_parent_enc)) %>%
  ungroup() %>%
  arrange(-`# Encounters`) %>%
  # mutate(across(contains('Paid'),dollar,1)) %>%
  rename(`ICD Code` = icd_code,
         `ICD Desc` = icd_desc) %>%
  as.data.frame()

#### Lab Summary ####
lab_procs <- read.csv('/home/dblakemore/SUD Home/Lab Costs/lab_procs.csv')

sql <-
paste0(
  "SELECT M.AXIAL_MEMBER_ID, M.AXIAL_CLAIM_ID, M.AXIAL_CLAIM_LINE_ID, M.AXIAL_CLAIM_ID_LINE_ID,
  	M.SERVICE_FROM_DATE, M.SERVICE_FROM_YYYY_MM, M.AMOUNT_INSURANCE_PAID AS PAID, M.AMOUNT_ALLOWED AS ALLOWED,
  	M.AXIAL_PLACE_OF_SERVICE AS POS, M.AXIAL_PLACE_OF_SERVICE_NAME AS POS_NAME, M.AXIAL_CLAIM_TYPE,
  	M.ICD_CODE_1, M.ICD_CODE_1_DESC, M.ICD_CODE_2, M.ICD_CODE_2_DESC, M.ICD_CODE_3, M.ICD_CODE_3_DESC, M.ICD_CODE_4, M.ICD_CODE_4_DESC,
  	M.PROCEDURE_CODE, M.ICD_PROCEDURE_CODE, 
  	M.SERVICING_PROVIDER_NPI, NPI.ENTITY_TYPE, TAX.CLASSIFICATION AS SERVICING_NPI_CLASS, TAX.[GROUPING] AS SERVICING_NPI_GROUP, TAX.SPECIALIZATION AS SERVICING_NPI_SPEC,
  	IFNULL(NPI.PROVIDER_ORGANIZATION_NAME_LEGAL_BUSINESS_NAME, CONCAT(NPI.PROVIDER_LAST_NAME, ', ', NPI.PROVIDER_FIRST_NAME)) AS SERVICING_NPI_NAME,
  	M.BILLING_PROVIDER_NPI,
  	IFNULL(NPI_BILL.PROVIDER_ORGANIZATION_NAME_LEGAL_BUSINESS_NAME, CONCAT(NPI_BILL.PROVIDER_LAST_NAME, ', ', NPI_BILL.PROVIDER_FIRST_NAME)) AS BILLING_NPI_NAME,
  	BILL_TAX.CLASSIFICATION AS BILLING_NPI_CLASS, BILL_TAX.[GROUPING] AS BILLING_NPI_GROUP, NPI_BILL.ENTITY_TYPE AS BILLING_NPI_TYPE
  FROM MASTER.MST_MEDICAL_CLAIMS M
  LEFT JOIN REFERENCE.ICD_DIAGNOSIS ICD 
  	ON M.ICD_CODE_1 = ICD.ICD_UNFORMATTED_CODE
  	AND M.ICD_TYPE = ICD.ICD_TYPE
  INNER JOIN MASTER.MEMBER_INCLUSION_EXCLUSION IE 
  	ON IE.AXIAL_MEMBER_ID = M.AXIAL_MEMBER_ID
  	AND IE.MEMBER_YYYY_MM = M.SERVICE_FROM_YYYY_MM
  	AND IE.IS_IN_SCOPE
  LEFT JOIN REFERENCE.NPI_REGISTRY NPI
  	ON M.SERVICING_PROVIDER_NPI = NPI.NPI 
  	AND NPI.NPI_TAXONOMY_SEQUENCE = 1
  LEFT JOIN REFERENCE.NUCC_PROVIDER_TAXONOMY TAX 
  	ON NPI.HEALTHCARE_PROVIDER_TAXONOMY_CODE = TAX.CODE
  LEFT JOIN REFERENCE.NPI_REGISTRY NPI_BILL
  	ON M.BILLING_PROVIDER_NPI = NPI_BILL.NPI 
  	AND NPI_BILL.NPI_TAXONOMY_SEQUENCE = 1
  LEFT JOIN REFERENCE.NUCC_PROVIDER_TAXONOMY BILL_TAX 
  	ON NPI_BILL.HEALTHCARE_PROVIDER_TAXONOMY_CODE = BILL_TAX.CODE
  WHERE M.SERVICE_FROM_DATE BETWEEN '", end_date - years(1) + days(1),"' and '",end_date,"'
  	AND M.CLIENT_ID = ",client_id,"
	AND M.PROCEDURE_CODE IN ('0007U','0011U','G0431','G0434','G0477','G0479',
	'G0480','G0481','G0482','G0483','G0659','G6030','G6031','G6032','80301',
	'80302','80303','80304','80305','80306','80307','80320','80321','80322',
	'80323','80324','80325','80326','80327','80328','80329','80330','80331',
	'80332','80333','80334','80335','80336','80337','80338','80339','80340',
	'80341','80342','80343','80344','80345','80346','80347','80348','80349',
	'80350','80351','80352','80353','80354','80355','80356','80357','80358',
	'80359','80360','80361','80362','80363','80364','80365','80366','80367',
	'80368','80369','80370','80371','80372','80373','80374','80375','80376',
	'80377','82542','82570','82646','82649','83840','83925','83986','83992',
	'84311')"
)

labs <- exa.readData(exaconn,sql) %>%
  rename_all(tolower) %>%
  mutate(service_from_date = ymd(service_from_date)) %>%
  left_join(lab_procs)

pres_conf <-
  labs %>%
  mutate(drug_test_type = substr(tolower(drug_test_type),1,4),
         n=1) %>%
  pivot_wider(id_cols = c(axial_member_id,service_from_date),
              names_from = drug_test_type,
              values_from = n,
              values_fn=sum,
              values_fill=0) %>%
  mutate(n_tests_day = pres + conf,
         pres_pre_conf = ifelse(conf==0,NA,ifelse(pres>0,1,0)),
         conf_pre_pres = ifelse(pres==0,NA,ifelse(conf>0,1,0)))

lab_billing_providers <-
  labs %>%
  mutate(billing_npi_name = ifelse(!is.na(billing_provider_npi), billing_npi_name,
                                   paste0('Unk: ',servicing_npi_name, ' (Servicing Prov)'))) %>%
  group_by(billing_npi_name,axial_member_id,service_from_date) %>%
  summarise(n_tests = n(),
            n_conf = sum(ifelse(drug_test_type=='Confirmatory',1,0)),
            paid_conf = sum(ifelse(drug_test_type=='Confirmatory',paid,0)),
            n_pres = sum(ifelse(drug_test_type=='Presumptive',1,0)),
            paid_pres = sum(ifelse(drug_test_type=='Presumptive',paid,0)),
            paid = sum(paid)) %>%
  ungroup() %>% 
  group_by(year(service_from_date)) %>%
  mutate(total_paid=sum(paid),
         Year = year(service_from_date)) %>% 
  ungroup() %>%
  left_join(pres_conf) %>% 
  left_join(pres_conf %>% 
              mutate(Year = year(service_from_date)) %>% 
              group_by(Year) %>% 
              summarise(annual_tests = sum(n_tests_day)) %>%
              ungroup()
  ) %>%
  group_by(Year,billing_npi_name) %>%
  summarise(`# Tests` = sum(n_tests),
            `% of Tests` = sum(n_tests)/mean(annual_tests),
            `# Members` = n_distinct(axial_member_id),
            `Total Paid` = sum(paid),
            `% of Total Paid` = sum(paid)/mean(total_paid),
            `Avg. Paid` = sum(paid)/sum(n_tests),
            `% Confirmatory` = sum(n_conf)/sum(n_tests),
            `Avg. Paid (Conf.)` = sum(paid_conf) / sum(n_conf),
            `% Presumptive` = sum(n_pres)/sum(n_tests),
            `Avg. Paid (Pres.)` = sum(paid_pres) / sum(n_pres),
            `Avg. # Tests / Visit` = mean(n_tests_day),
            `Avg. # Conf. Tests / Visit` = mean(conf),
            `% w/ Pres. Test` = mean(pres_pre_conf,na.rm = TRUE),
            `% w/ Conf. Test` = mean(conf_pre_pres,na.rm = TRUE)
  ) %>%
  ungroup() %>%
  arrange(Year,desc(`# Tests`)) %>%
  # mutate(across(starts_with('#'),comma)) %>%
  rename(`Billing Provider` = billing_npi_name) %>%
  as.data.frame()

lab_billing_providers_total <-
  labs %>%
  mutate(billing_npi_name = 'Total') %>%
  group_by(billing_npi_name,axial_member_id,service_from_date) %>%
  summarise(n_tests = n(),
            n_conf = sum(ifelse(drug_test_type=='Confirmatory',1,0)),
            paid_conf = sum(ifelse(drug_test_type=='Confirmatory',paid,0)),
            n_pres = sum(ifelse(drug_test_type=='Presumptive',1,0)),
            paid_pres = sum(ifelse(drug_test_type=='Presumptive',paid,0)),
            paid = sum(paid)) %>%
  ungroup() %>% 
  group_by(year(service_from_date)) %>%
  mutate(total_paid=sum(paid),
         Year = year(service_from_date)) %>% 
  ungroup() %>%
  left_join(pres_conf) %>% 
  left_join(pres_conf %>% 
              mutate(Year = year(service_from_date)) %>% 
              group_by(Year) %>% 
              summarise(annual_tests = sum(n_tests_day)) %>%
              ungroup()
  ) %>%
  group_by(Year,billing_npi_name) %>%
  summarise(`# Tests` = sum(n_tests),
            `% of Tests` = sum(n_tests)/mean(annual_tests),
            `# Members` = n_distinct(axial_member_id),
            `Total Paid` = sum(paid),
            `% of Total Paid` = sum(paid)/mean(total_paid),
            `Avg. Paid` = sum(paid)/sum(n_tests),
            `% Confirmatory` = sum(n_conf)/sum(n_tests),
            `Avg. Paid (Conf.)` = sum(paid_conf) / sum(n_conf),
            `% Presumptive` = sum(n_pres)/sum(n_tests),
            `Avg. Paid (Pres.)` = sum(paid_pres) / sum(n_pres),
            `Avg. # Tests / Visit` = mean(n_tests_day),
            `Avg. # Conf. Tests / Visit` = mean(conf),
            `% w/ Pres. Test` = mean(pres_pre_conf,na.rm = TRUE),
            `% w/ Conf. Test` = mean(conf_pre_pres,na.rm = TRUE)
  ) %>%
  ungroup() %>%
  arrange(Year,desc(`# Tests`)) %>%
  # mutate(across(starts_with('#'),comma)) %>%
  rename(`Billing Provider` = billing_npi_name) %>%
  as.data.frame()

cols <- c('All Tests'=colors$hex[1], 
          'Pres. Tests'=colors$hex[6], 
          'Conf. Tests'=colors$hex[7],
          'Visits' = colors$hex[3])

drug_test_trend_plot <-
  pres_conf %>%
  mutate(reporting_date=ymd(paste0(substr(service_from_date,1,7),'-01'))) %>%
  group_by(reporting_date) %>%
  summarise(n_visits=n(),
            pres = sum(pres),
            conf = sum(conf),
            tests = sum(n_tests_day)) %>%
  ungroup() %>%
  inner_join(mm %>% filter(is_in_scope)) %>%
  mutate(visits_1k = 1000*n_visits/mm,
         pres_1k = 1000*pres/mm,
         conf_1k = 1000*conf/mm,
         tests_1k= 1000*tests/mm) %>%
  ggplot(aes(reporting_date)) +
  geom_line(aes(y=tests_1k,color='All Tests')) +
  geom_line(aes(y=pres_1k,color='Pres. Tests')) +
  geom_line(aes(y=conf_1k,color='Conf. Tests')) +
  # geom_line(aes(y=visits_1k,color='Visits')) +
  scale_x_date('Reporting Date',date_breaks = '3 months', date_minor_breaks = '1 month', date_labels = '%b %y') +
  scale_color_manual('Metric',values=cols) +
  ylim(0,NA) + ylab('# Tests / 1,000') +
  ggtitle('Drug Testing Trend') +
  theme_bw()

# png('drug_test_trend_plot.png',width = 1200, height = 750)
# drug_test_trend_plot
# dev.off()


#### Ambulance Summary ####
sql <-
paste0(
  "WITH IN_SCOPE AS (
  	SELECT AXIAL_MEMBER_ID
  		,MAX(TO_DATE(MEMBER_YYYY_MM,'YYYY-MM')) AS MAX_DATE
  		,MAX(CASE WHEN MEMBER_YYYY_MM LIKE '2021%' THEN 1 ELSE 0 END) AS IN_SCOPE_2021
  	FROM MASTER.MEMBER_INCLUSION_EXCLUSION
  	WHERE IS_IN_SCOPE
  		AND CLIENT_ID = ", client_id, "
  	GROUP BY 1
  ),
  CRS AS (
  	SELECT AXIAL_MEMBER_ID
  		,A.OUTREACH_FLAG
  	FROM ZZ_DBM.SUD_HOME_OUTREACH_COHORTS_V2 A
  	INNER JOIN ZZ_DBM.VW_COTIVITI_RUNS B
  		ON A.CLIENT_ID = B.CLIENT_ID
  		AND A.COTIVITI_RUN_DATE = B.RUN_DATE
  )
  SELECT MAX_DATE AS RECENT_IN_SCOPE_DATE
  	,CRS.OUTREACH_FLAG
  	,M.AXIAL_MEMBER_ID
  	,M.AXIAL_CLAIM_ID
  	,M.AXIAL_CLAIM_LINE_ID
  	,M.SERVICE_FROM_DATE
  	,M.SERVICE_FROM_REPORTING_DATE
  	,M.AMOUNT_INSURANCE_PAID AS PAID
  	,M.ICD_CODE_1, M.ICD_CODE_1_DESC
  	,M.PROCEDURE_CODE
  	,P.LONG_DESC AS PROC_CODE_DESC
  	,M.SERVICING_PROVIDER_NPI
  	,IFNULL(NPI.PROVIDER_ORGANIZATION_NAME_LEGAL_BUSINESS_NAME, CONCAT(NPI.PROVIDER_LAST_NAME, ', ', NPI.PROVIDER_FIRST_NAME)) AS SERVICING_NPI_NAME
  	,M.BILLING_PROVIDER_NPI
  	,IFNULL(NPI_BILL.PROVIDER_ORGANIZATION_NAME_LEGAL_BUSINESS_NAME, CONCAT(NPI_BILL.PROVIDER_LAST_NAME, ', ', NPI_BILL.PROVIDER_FIRST_NAME)) AS BILLING_NPI_NAME
  FROM MASTER.MST_MEDICAL_CLAIMS M
  LEFT JOIN CRS
	  ON M.AXIAL_MEMBER_ID = CRS.AXIAL_MEMBER_ID
  INNER JOIN IN_SCOPE
  	ON IN_SCOPE.AXIAL_MEMBER_ID = M.AXIAL_MEMBER_ID
  	AND IN_SCOPE_2021 = 1
  LEFT JOIN ZZ_DBM.URSA_PROC P
	  ON M.PROCEDURE_CODE = P.HCPCS_CODE
  LEFT JOIN REFERENCE.NPI_REGISTRY NPI
    	ON M.SERVICING_PROVIDER_NPI = NPI.NPI
    	AND NPI.NPI_TAXONOMY_SEQUENCE = 1
  LEFT JOIN REFERENCE.NPI_REGISTRY NPI_BILL
  	ON M.BILLING_PROVIDER_NPI = NPI_BILL.NPI
  	AND NPI_BILL.NPI_TAXONOMY_SEQUENCE = 1
  WHERE M.PLACE_OF_SERVICE = 41
  	AND M.SERVICE_FROM_DATE BETWEEN '", end_date - years(1) + days(1),"' and '",end_date,"'
  	AND M.CLIENT_ID = ",client_id
)

ambulance <- exa.readData(exaconn, sql) %>%
  rename_all(tolower) %>%
  mutate(across(contains('date'),ymd))

ambulance$current_in_scope <- ifelse(ambulance$recent_in_scope_date==max(ambulance$recent_in_scope_date),1,0)
ambulance$outreach_flag[is.na(ambulance$outreach_flag)] <- 0

ambulance_summary <-
  ambulance %>%
  group_by(axial_member_id,outreach_flag,current_in_scope) %>%
  summarise(`# Rides` = n_distinct(axial_claim_id),
            `Total Paid` = sum(paid),
            `Paid / Ride` = dollar(sum(paid) / n_distinct(axial_claim_id),.01)) %>%
  ungroup() %>%
  arrange(-`# Rides`,-`Total Paid`) %>%
  filter(`# Rides`>=4) %>%
  rename(`Axial Member ID` = axial_member_id,
         `Flagged for Outreach?` = outreach_flag,
         `Currently In Scope?` = current_in_scope) %>%
  mutate(`Total Paid` = dollar(`Total Paid`,1)) %>%
  as.data.frame()

ambulance_detail <-
  ambulance %>%
  inner_join(ambulance_summary, by = c('axial_member_id' = 'Axial Member ID')) %>%
  select(axial_member_id,outreach_flag,current_in_scope,`# Rides`,axial_claim_id,service_from_date,servicing_npi_name,
         procedure_code,proc_code_desc,icd_code_1,icd_code_1_desc,paid) %>%
  arrange(-`# Rides`,axial_member_id,service_from_date,axial_claim_id,-paid) %>%
  as.data.frame()


#### ED Summary ####
sql <-
  "SELECT ICD.ICD_FORMATTED_CODE AS ICD_CODE
  	,ICD.IS_SUD
  	,ICD.IS_DRUG_OVERDOSE_OF_INTEREST AS IS_OD
  	,CASE WHEN IC.ICD_UNFORMATTED_CODE IS NULL THEN FALSE ELSE TRUE END AS IS_INJURY
  FROM REFERENCE.ICD_DIAGNOSIS ICD
  LEFT JOIN ZZ_DBM.INJURY_CATEGORIZATIONS IC
  	ON ICD.ICD_UNFORMATTED_CODE = IC.ICD_UNFORMATTED_CODE
  WHERE ICD.ICD_TYPE = 10"

icds <- exa.readData(exaconn, sql) %>%
  rename_all(tolower)

sql <-
  paste0(
  "select right(a.pat_id,8) as axial_member_id
  	,a.event_id
  	,a.event_start_date
  	,a.event_facility_prov_npi as facility_npi
  	,c.prov_desc as facility_name
  	,b.preventable_ed_visit_scenario_cat
  	,b.is_high_probability_preventable_ed_visit as is_avoidable
  	,b.is_high_probability_preventable_ed_visit_due_to_preventable_con
  	,b.is_high_probability_preventable_ed_visit_due_to_inappropriate_s
  	,a.is_hospital_inpat_admission_enc
  	,a.is_hospital_obs_stay_parent_enc
  	,b.principal_discharge_dx_icd10cm_code as icd_code
  	,b.principal_discharge_dx_icd10cm_desc as icd_desc
  	,e.primary_plan_paid_amount as paid
  from ursa.dm_ws_edw_enc_001 a
  left join ursa.so_ursa_phu_enc_003 b
  	on a.event_id = b.enc_id
  left join ursa.mi_ursa_core_110	c
  	on a.event_facility_prov_npi = c.prov_npi
  inner join client_production.member_inclusion_exclusion d
  	on right(a.pat_id,8) = d.axial_member_id
  	and to_date(a.event_start_month_text_yyyymm,'YYYYMM') = to_date(d.member_yyyy_mm, 'YYYY-MM')
  	and d.is_in_scope_financial_risk is true
  	and d.is_first_month_sud is false
  left join ursa.so_ursa_core_enc_013 e
  	on a.event_id = e.enc_id
  where wayspring_csl_enc_cat_tier_1 = '[10] Emergency Department'
  	and event_primary_payor_id = '",ursa_client_id,"'
  	and event_start_date between '", end_date - years(1) + days(1),"' and '",end_date,"'"
  )

ed <- dbGetQuery(redconn,sql) %>%
  left_join(icds) %>%
  mutate(across(contains('date'),ymd),
         across(c('is_sud','is_od','is_hospital_inpat_admission_enc','is_hospital_obs_stay_parent_enc'),replace_na,FALSE),
         is_avoidable = replace_na(is_avoidable,0),
         icd_desc = case_when(icd_code=='U07.1' ~ 'COVID-19',
                              icd_code=='R51.9' ~ 'Headache,unspecified',
                              icd_code=='O99.891' ~ 'Other specified diseases and conditions complicating pregnancy',
                              TRUE ~ icd_desc),
         preventable_ed_visit_scenario_cat = case_when(substr(preventable_ed_visit_scenario_cat,2,3)=='01' ~ 'Inappropriate Setting',
                                                       substr(preventable_ed_visit_scenario_cat,2,3)=='02' ~ 'Preventable Condition',
                                                       substr(preventable_ed_visit_scenario_cat,2,3)=='03' ~ 'Either Inappropriate Setting / Preventable Condition'),
         avoid_cat = case_when(is_avoidable == 1 ~ preventable_ed_visit_scenario_cat,
                               substr(icd_code,1,3) == 'F10' ~ 'Alcohol Use Disorder',
                               is_sud ~ 'Substance Use Disorder',
                               is_od ~ 'Overdose',
                               substr(icd_code,1,1)=='F' | substr(icd_code,1,6) %in% c('T14.91','R45.85') ~ 'Behavioral Health',
                               is_injury | substr(icd_code,1,1) %in% c('S','T') ~ 'Injury',
                               TRUE ~ 'Unavoidable / Unclassified')
         )

ed_summary <-
  ed %>%
  mutate(yr = year(event_start_date)) %>%
  left_join(mm_yr %>% filter(is_in_scope)) %>%
  group_by(avoid_cat) %>%
  summarise(`# Visits` = n(),
            `# Members` = n_distinct(axial_member_id),
            `Total Paid` = sum(paid),
            `Avg. Paid` = mean(paid),
            PMPM = sum(paid) / mean(mm)
            ) %>%
  arrange(-`# Visits`) %>%
  # mutate(`# Visits` = comma(`# Visits`)) %>%
  rename(`Avoidable Category` = avoid_cat) %>%
  as.data.frame()

ed_summary_total <-
  ed %>%
  mutate(yr = year(event_start_date),
         avoid_cat = 'Total') %>%
  left_join(mm_yr %>% filter(is_in_scope)) %>%
  group_by(avoid_cat) %>%
  summarise(`# Visits` = n(),
            `# Members` = n_distinct(axial_member_id),
            `Total Paid` = sum(paid),
            `Avg. Paid` = mean(paid),
            PMPM = sum(paid) / mean(mm)
  ) %>%
  # mutate(`# Visits` = comma(`# Visits`)) %>%
  rename(`Avoidable Category` = avoid_cat) %>%
  as.data.frame()

avoidable_ed <-
  ed %>%
  select(-c(starts_with('is_hospital'),'is_sud','is_od','is_injury','avoid_cat','is_avoidable',starts_with('is_high'))) %>%
  # mutate(paid = dollar(paid)) %>%
  rename(`Axial Member ID` = axial_member_id,
         `Event ID` = event_id,
         `Event Date` = event_start_date,
         `Facility NPI` = facility_npi,
         `Facility Name` = facility_name,
         `Preventable Scenario` = preventable_ed_visit_scenario_cat,
         `Principle ICD Code` = icd_code,
         `ICD Code Desc` = icd_desc,
         Paid = paid
         ) %>%
  as.data.frame()


#### Medical Pharmacy ####
sql <-
  paste0(
  "WITH X AS (
  	SELECT MAX(MEMBER_YYYY_MM) AS MAX_MONTH
  	FROM MASTER.MEMBER_INCLUSION_EXCLUSION
  	WHERE CLIENT_ID = ", client_id,"
  ),
  IE AS (
  	SELECT *
  	FROM MASTER.MEMBER_INCLUSION_EXCLUSION IE
  	INNER JOIN X
  		ON IE.MEMBER_YYYY_MM = X.MAX_MONTH
  	WHERE IE.IS_IN_SCOPE
  		AND CLIENT_ID = ", client_id,"
  )
  SELECT M.AXIAL_MEMBER_ID
    ,M.SERVICE_FROM_DATE AS [DATE]
    ,NPI.NPI
    ,CASE WHEN NPI.PROVIDER_ORGANIZATION_NAME_LEGAL_BUSINESS_NAME IS NULL
    	THEN CONCAT(NPI.PROVIDER_LAST_NAME,', ', NPI.PROVIDER_FIRST_NAME)
    	ELSE NPI.PROVIDER_ORGANIZATION_NAME_LEGAL_BUSINESS_NAME
    END AS PROVIDER_NAME
  	,M.PLACE_OF_SERVICE AS POS
  	,M.PLACE_OF_SERVICE_NAME AS POS_NAME
  	,M.PROCEDURE_CODE AS PROC_CODE
  	,P.LONG_DESC AS PROC_CODE_DESC
  	,M.NDC
  	,M.ICD_CODE_1
  	,M.ICD_CODE_1_DESC
  	--,M.AMOUNT_ALLOWED
  	,M.AMOUNT_TOTAL_PAID
  	,M.AMOUNT_INSURANCE_PAID
  	,M.UNITS
  	,ASP.PAYMENT_LIMIT AS ASP_UNIT_COST
  	,M.UNITS*ASP.PAYMENT_LIMIT AS ASP_COST
  	,M.AMOUNT_TOTAL_PAID / (M.UNITS*ASP.PAYMENT_LIMIT) AS PCT_ASP
  FROM MASTER.MST_MEDICAL_CLAIMS M
  LEFT JOIN REFERENCE.NPI_REGISTRY NPI
  	ON M.SERVICING_PROVIDER_NPI = NPI.NPI
  	AND NPI.NPI_TAXONOMY_SEQUENCE = 1
  LEFT JOIN ZZ_DBM.URSA_PROC P
  	ON M.PROCEDURE_CODE = P.HCPCS_CODE
  INNER JOIN IE
  	ON M.AXIAL_MEMBER_ID = IE.AXIAL_MEMBER_ID
  	AND M.SERVICE_FROM_REPORTING_DATE BETWEEN ADD_MONTHS(TO_DATE(MEMBER_YYYY_MM,'YYYY-MM'),-11)
  		AND TO_DATE(IE.MEMBER_YYYY_MM,'YYYY-MM')
  LEFT JOIN ZZ_DBM.CMS_ASP ASP
  	ON M.PROCEDURE_CODE = ASP.PROCEDURE_CODE
  	AND M.SERVICE_FROM_DATE BETWEEN ASP.START_DATE AND ASP.END_DATE
  WHERE (M.AMOUNT_ALLOWED>=500 OR M.AMOUNT_TOTAL_PAID>=500)
  	AND (ASP.PROCEDURE_CODE IS NOT NULL OR LEFT(M.PROCEDURE_CODE,1) IN ('J','Q'))
    AND M.PROCEDURE_CODE NOT IN ('J7297','J7298','J7307','J7300')
    AND M.PLACE_OF_SERVICE NOT IN ('21')"
  )

med_pharm <- exa.readData(exaconn,sql) %>%
  rename_all(tolower) %>%
  mutate(date = ymd(date))

med_pharm$proc_code_desc[med_pharm$proc_code=='J0791' & med_pharm$proc_code_desc==''] <- 'Injection, crizanlizumab-tmca, 5 mg'
med_pharm$proc_code_desc[med_pharm$proc_code=='Q5119' & med_pharm$proc_code_desc==''] <- 'Injection, rituximab-pvvr, biosimilar, (ruxience), 10 mg'
med_pharm$proc_code_desc[med_pharm$proc_code=='Q5106' & med_pharm$proc_code_desc==''] <- 'Injection, rituximab-pvvr, biosimilar, (ruxience), 10 mg'

med_pharm_plot <-
  med_pharm %>%
  filter(!is.na(pct_asp) & pct_asp<=250) %>%
  ggplot(aes(pos_name,pct_asp,color=pos_name)) +
  theme_bw() +
  geom_jitter() +
  geom_hline(yintercept = 1, color = 'dark grey') +
  scale_y_continuous('% ASP',label = percent_format()) +
  annotate('text',x=0.55,y=-2.75,size=3,label='100% ASP') +
  xlab('Place of Service') +
  theme(legend.position = 'none') +
  ggtitle('% of ASP by Place of Service','Excluding outliers >250,000%')

med_pharm_summary <-
  med_pharm %>%
  filter(pct_asp>=1.2) %>%
  group_by(proc_code,proc_code_desc,provider_name) %>%
  summarise(`# Visits` = n(),
            `# Members` = n_distinct(axial_member_id),
            `Savings Opportunity` = sum(amount_total_paid - asp_cost),
            `Savings / Visit` = sum(amount_total_paid - asp_cost)/n(),
            `% of ASP` = sum(amount_total_paid)/sum(asp_cost)) %>%
  arrange(-`Savings Opportunity`) %>%
  # mutate(`Savings Opportunity` = dollar(`Savings Opportunity`)) %>%
  rename(`Proc Code` = proc_code,
         `Proc Code Desc` = proc_code_desc,
         Provider = provider_name) %>%
  as.data.frame()

med_pharm_summary_total <-
  med_pharm %>%
  filter(pct_asp>=1.2) %>%
  mutate(proc_code = 'Total',
         proc_code_desc = '',
         provider_name = '') %>%
  group_by(proc_code,proc_code_desc,provider_name) %>%
  summarise(`# Visits` = n(),
            `# Members` = n_distinct(axial_member_id),
            `Savings Opportunity` = sum(amount_total_paid - asp_cost),
            `Savings / Visit` = sum(amount_total_paid - asp_cost)/n(),
            `% of ASP` = sum(amount_total_paid)/sum(asp_cost)) %>%
  arrange(-`Savings Opportunity`) %>%
  # mutate(`Savings Opportunity` = dollar(`Savings Opportunity`)) %>%
  rename(`Proc Code` = proc_code,
         `Proc Code Desc` = proc_code_desc,
         Provider = provider_name) %>%
  as.data.frame()

med_pharm_detail <-
  med_pharm %>%
  inner_join(
    med_pharm %>%
    filter(pct_asp>=1.2) %>%
    select(axial_member_id,proc_code) %>%
    distinct()
  ) %>%
  arrange(axial_member_id,proc_code,date) %>%
  mutate(`Savings Opportunity` = ifelse(amount_insurance_paid<asp_cost,0,amount_insurance_paid-asp_cost),
         # across(c(starts_with('amount'),'asp_cost'),dollar),
         asp_unit_cost = asp_unit_cost,
         pct_asp = pct_asp) %>%
  rename(`Axial Member ID` = axial_member_id,
         `Service Date` = date,
         NPI = npi,
         `Provider Name` = provider_name,
         POS = pos,
         `POS Name` = pos_name,
         `Proc Code` = proc_code,
         `Proc Code Desc` = proc_code_desc,
         NDC = ndc,
         `ICD Code` = icd_code_1,
         `ICD Desc` = icd_code_1_desc,
         `Allowed` = amount_total_paid,
         `Paid` = amount_insurance_paid,
         Units = units,
         `ASP Unit Cost` = asp_unit_cost,
         `ASP Cost` = asp_cost,
         `% of ASP` = pct_asp) %>%
  as.data.frame()


# # Unknown
# ignore_unknown <- 1
# stopifnot((ignore_unknown==1| nrow(med_pharm %>% filter(is.na(asp_unit_cost)))==0))
# 
# # Botox
# botox <- 0
# 
# if(botox==1 & nrow(med_pharm %>% filter(proc_code=='J0585'))>0){
#   
#   botox_summary <-
#     med_pharm %>% 
#     filter(proc_code=='J0585') %>%
#     group_by(provider_name,icd_code_1,icd_code_1_desc) %>%
#     summarise(`# Visits` = n(),
#               `# Members` = n_distinct(axial_member_id),
#               `Savings Opportunity` = sum(max(amount_total_paid - asp_cost,0)),
#               `Total Paid` = dollar(sum(amount_total_paid)),
#               `% of ASP` = percent(sum(amount_total_paid)/sum(asp_cost),.1)) %>%
#     rename(`Provider` = provider_name,
#            `ICD Code` = icd_code_1,
#            `ICD Desc` = icd_code_1_desc) %>%
#     as.data.frame()
#   
#   botox_summary_total <-
#     med_pharm %>% 
#     filter(proc_code=='J0585') %>%
#     mutate(provider_name = 'Total',
#            icd_code_1 = '',
#            icd_code_1_desc = '') %>%
#     group_by(provider_name,icd_code_1,icd_code_1_desc) %>%
#     summarise(`# Visits` = n(),
#               `# Members` = n_distinct(axial_member_id),
#               `Savings Opportunity` = sum(max(amount_total_paid - asp_cost,0)),
#               `Total Paid` = dollar(sum(amount_total_paid)),
#               `% of ASP` = percent(sum(amount_total_paid)/sum(asp_cost),.1)) %>%
#     rename(`Provider` = provider_name,
#            `ICD Code` = icd_code_1,
#            `ICD Desc` = icd_code_1_desc) %>%
#     as.data.frame()
#   
#   botox_sheet <- createSheet(wb, 'Botox Utilization')
#   addDataFrame(bind_rows(botox_summary_total,botox_summary),botox_sheet,row.names = FALSE)
# }
#   
#### Imaging ####
sql <-
  paste0(
  "select
  	case when b.is_hospital_inpat_admission_enc = 1 then 'Admission'
  		when b.is_clinician_office_visit_enc = 1 then 'Office'
  		when b.is_emergency_dept_visit_parent_enc = 1 then 'ED'
  		when b.is_hospital_obs_stay_parent_enc = 1 then 'Observation'
  		when b.is_snf_enc = 1 then 'SNF'
  		when b.is_ambulatory_surgical_center_enc = 1 then 'ASC'
  		when b.is_hospital_outpat_surgery_enc = 1 then 'Outpt. Surgery'
  		when b.is_other_hospital_outpat_enc = 1 then 'Other Outpt'
  		else 'Other'
  	end as encounter_category
  	,right(a.pat_id,8) as axial_member_id
  	,mie.is_in_scope
  	,a.document_id
  	,a.hcpcs_code
  	,a.hcpcs_desc
  	,d.ahrq_ccs_single_level_proc_cat_desc  as hcpcs_cat
  	,a.principal_dx_icd10cm_code as icd_code
  	,a.principal_dx_icd10cm_desc as icd_desc
  	,a.document_plan_paid_amount as paid
  	,right(a.billing_prov_id,10) as billing_npi
  	,c.prov_desc as billing_desc
  from ursa.dm_ws_edw_fin_001 a
  inner join client_production.member_inclusion_exclusion mie
  	on right(a.pat_id,8) = mie.axial_member_id
  	and to_date(a.document_effective_date_text_yyyymm,'YYYYMM') = to_date(mie.member_yyyy_mm,'YYYY-MM')
  left join ursa.so_ursa_core_enc_013 b
  	on a.event_id = b.enc_id
  left join ursa.mi_ursa_core_110	c
  	on right(a.billing_prov_id,10) = c.prov_npi
  left join ursa.mi_ursa_core_101 d
  	on a.hcpcs_code  = d.hcpcs_code
  where a.payor_id = '", ursa_client_id,"'
  	and a.document_effective_date between '", end_date - years(1) + days(1),"' and '",end_date,"'
  	and a.hcpcs_code between '70010' and '79999'"
  )

imaging <- dbGetQuery(redconn, sql) %>%
  mutate(across(contains('date'),ymd),
         hcpcs_cat = case_when(!is.na(hcpcs_cat) ~ hcpcs_cat,
                               hcpcs_code %in% (71045 + seq(0,3)) ~ 'Routine chest X-ray',
                               hcpcs_code=='71271' ~ 'CT scan chest',
                               hcpcs_code %in% c('74018','74019','74021','77047','77049') ~ 'Other diagnostic radiology and related techniques',
                               hcpcs_code %in% c('74221','74248') ~ 'Upper gastrointestinal X-ray',
                               hcpcs_code == '76981' ~ 'Other diagnostic ultrasound',
                               hcpcs_code %in% c('78830','78831','78832') ~ 'Radioisotope scan and function studies',
                               hcpcs_code %in% c('78431','78433','78434') ~ 'Diagnostic nuclear medicine procedures'),
         hcpcs_desc = ifelse(!is.na(hcpcs_desc),hcpcs_desc,paste0(hcpcs_code,': ',hcpcs_cat)))

imaging_summary <-
  imaging %>%
  group_by(is_in_scope) %>%
  mutate(total_paid = sum(paid),
         total_tests = n()) %>%
  ungroup() %>%
  group_by(is_in_scope,encounter_category) %>%
  summarise(`# Tests` = n(),
            `% of Tests` = n() / mean(total_tests),
            `Total Paid` = sum(paid),
            `Avg. Paid` = mean(paid),
            `% of Paid` = sum(paid) / mean(total_paid)) %>%
  ungroup() %>%
  left_join(mm %>%
              filter(reporting_date>=end_date - years(1) + days(1) & end_date - years(1) + days(1)<=end_date) %>%
              group_by(is_in_scope) %>%
              summarise(mm = sum(mm)) %>%
              ungroup()
            ) %>%
  mutate(`# Tests / 1,000` = 12000*`# Tests`/mm) %>%
  select(-mm) %>%
  pivot_wider(id_cols = encounter_category,
              names_from = is_in_scope,
              values_from = c(contains('Tests'),contains('Paid')),
              names_sort = TRUE) %>%
  rename(`Encounter Category` = encounter_category) %>%
  as.data.frame()

imaging_summary_total <-
  imaging %>%
  group_by(is_in_scope) %>%
  mutate(total_paid = sum(paid),
         total_tests = n()) %>%
  ungroup() %>%
  mutate(encounter_category = 'Total') %>%
  group_by(is_in_scope,encounter_category) %>%
  summarise(`# Tests` = n(),
            `% of Tests` = n() / mean(total_tests),
            `Total Paid` = sum(paid),
            `Avg. Paid` = mean(paid),
            `% of Paid` = sum(paid) / mean(total_paid)) %>%
  ungroup() %>%
  left_join(mm %>%
              filter(reporting_date>=end_date - years(1) + days(1) & end_date - years(1) + days(1)<=end_date) %>%
              group_by(is_in_scope) %>%
              summarise(mm = sum(mm)) %>%
              ungroup()
  ) %>%
  mutate(`# Tests / 1,000` = 12000*`# Tests`/mm) %>%
  select(-mm) %>%
  pivot_wider(id_cols = encounter_category,
              names_from = is_in_scope,
              values_from = c(contains('Tests'),contains('Paid')),
              names_sort = TRUE) %>%
  rename(`Encounter Category` = encounter_category) %>%
  as.data.frame()

# imaging %>%
#   group_by(hcpcs_cat) %>%
#   mutate(n_in_scope = sum(is_in_scope),
#          is_in_scope = ifelse(is_in_scope,'In Scope','Not In Scope')) %>%
#   ungroup() %>%
#   group_by(is_in_scope,hcpcs_cat) %>%
#   mutate(n_total=n()) %>%
#   ungroup() %>%
#   filter(n_in_scope>10) %>%
#   group_by(hcpcs_cat,is_in_scope,encounter_category) %>%
#   summarise(avg_paid = mean(paid),
#             pct = n()/mean(n_total)) %>%
#   ungroup() %>%
#   ggplot(aes(is_in_scope,avg_paid,size=pct,color = encounter_category)) +
#   theme_bw() +
#   facet_wrap(~hcpcs_cat,scales = 'free_y') +
#   geom_jitter(width=.1) +
#   scale_y_continuous('Avg. Paid',labels = dollar_format()) +
#   xlab('In Scope?')

imaging_details <-
  imaging %>%
  group_by(is_in_scope) %>%
  mutate(total_paid = sum(paid),
         total_tests = n()) %>%
  ungroup() %>%
  group_by(encounter_category) %>%
  mutate(total_paid_billing = sum(paid)) %>%
  ungroup() %>%
  group_by(is_in_scope,encounter_category,total_paid_billing, hcpcs_desc,hcpcs_cat) %>%
  summarise(`# Tests` = n(),
            `% of Tests` = n() / mean(total_tests),
            `Total Paid` = sum(paid),
            `Avg. Paid` = mean(paid),
            `% of Paid` = sum(paid) / mean(total_paid)) %>%
  ungroup() %>%
  left_join(mm %>%
              filter(reporting_date>=end_date - years(1) + days(1) & end_date - years(1) + days(1)<=end_date) %>%
              group_by(is_in_scope) %>%
              summarise(mm = sum(mm)) %>%
              ungroup()
  ) %>%
  mutate(`# Tests / 1,000` = 12000*`# Tests`/mm) %>%
  select(-mm) %>%
  pivot_wider(id_cols = c(encounter_category,hcpcs_cat,hcpcs_desc),
              names_from = is_in_scope,
              values_from = c(contains('Tests'),contains('Paid'),total_paid_billing),
              names_sort = TRUE) %>%
  arrange(-total_paid_billing_TRUE,encounter_category,-`Total Paid_TRUE`) %>%
  # mutate(across(c(contains('Total Paid')),dollar)) %>%
  select(-c('total_paid_billing_TRUE','total_paid_billing_FALSE')) %>%
  rename(`Encounter Category` = encounter_category,
         `HCPCS Category` = hcpcs_cat,
         `Proc Code Description` = hcpcs_desc) %>%
  as.data.frame()


