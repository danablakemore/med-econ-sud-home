setwd('/home/dblakemore/SUD Home/MEI/')
wb <- createWorkbook()
fn <- 'HMK DE MEI Summary Analysis 2019-2021 v2.xlsx'

s1 <- createStyle(textDecoration=c("bold"),
                  fgFill = 'lightgrey',
                  halign = 'center',
                  border = 'bottom')


#### PMPM Trend ####
addWorksheet(wb, 'PMPM Trend',gridLines = FALSE)
print(pmpm_trend)
insertPlot(wb, 1, width = 15, height = 8, fileType = "png", units = "in")

#### Util Trend ####
addWorksheet(wb, 'Utilization Trend',gridLines = FALSE)
print(util_trend)
insertPlot(wb, 2, width = 15, height = 8, fileType = "png", units = "in")

#### Cost Util Summary ####
sheet_name <- 'Cost and Util Summary'
if(sum(wb$sheet_names==sheet_name)>0){removeWorksheet(wb,sheet_name)}
addWorksheet(wb, sheet_name,gridLines = FALSE)
yrs <- length(unique(cost_util$yr))

# Top Row
for(i in seq(2,ncol(cost_util_summary),by=yrs)){
  temp <- seq(i,i+(yrs-1),1)
  mergeCells(wb,sheet_name,rows = 1, cols = temp)
  writeData(wb,sheet_name,startRow = 1, startCol = i,
            x = str_sub(names(cost_util_summary[,-1]),start = 1L, end = -6L)[i])
  print(temp)
}
addStyle(wb, sheet_name,rows = 1, cols = 1:ncol(cost_util_summary),
         style = createStyle(textDecoration=c("bold"),
                             fgFill = 'lightgrey',
                             halign = 'center',
                             border = 'TopBottomLeftRight '))

# Second Row
temp <- c('Spend Category', as.integer(str_sub(names(cost_util_summary[,-1]),-4L)))
for(i in 1:ncol(cost_util_summary)){
  writeData(wb,sheet_name,startRow = 2,startCol = i,x = temp[i])
}
rm(temp)
addStyle(wb, sheet_name,rows = 2,cols = 1:ncol(cost_util_summary),
         style = createStyle(textDecoration=c("bold"),
                             fgFill = 'lightgrey',
                             halign = 'center',
                             border = 'bottom'))
addStyle(wb, sheet_name,rows = 2,cols = 1,stack = TRUE,
         style = createStyle(halign = 'left'))

# Add Data
writeData(wb, sheet = sheet_name, x = cost_util_summary,
               startRow = 3, startCol = 1,
               colNames = FALSE, rowNames = FALSE)

# Style Totals
addStyle(wb, sheet_name,rows = nrow(cost_util_summary)+2, cols = 1:ncol(cost_util_summary),
         style = createStyle(border = 'TopBottom',textDecoration = 'bold',fgFill = 'lightgrey'))

# Add section dividers
addStyle(wb, sheet_name, rows = 2:(nrow(cost_util_summary)+2),
         cols = seq(1,ncol(cost_util_summary),by=yrs),
         gridExpand = TRUE,
         stack = TRUE,
         style = createStyle(border = 'right'))

# Style 'Paid' Columns
# temp <- 1:ncol(cost_util_summary)
addStyle(wb,sheet_name,
         rows = 3:(nrow(cost_util_summary)+2),
         cols = which(grepl('Paid_',names(cost_util_summary)) & !grepl('%',names(cost_util_summary))),
         gridExpand = TRUE,
         stack = TRUE,
         createStyle(numFmt = '$0,0'))

# Style 'PMPM' Columns
addStyle(wb,sheet_name,
         rows = 3:(nrow(cost_util_summary)+2),
         cols = which(grepl('PMPM',names(cost_util_summary))),
         gridExpand = TRUE,
         stack = TRUE,
         createStyle(numFmt = 'CURRENCY'))

# Style 'Events / 1,000' Columns
addStyle(wb,sheet_name,
         rows = 3:(nrow(cost_util_summary)+2),
         cols = which(grepl('1,000',names(cost_util_summary))),
         gridExpand = TRUE,
         stack = TRUE,
         createStyle(numFmt = '0,0.0'))

# Style '% Paid' Columns
addStyle(wb,sheet_name,
         rows = 3:(nrow(cost_util_summary)+2),
         cols = which(grepl('%',names(cost_util_summary))),
         gridExpand = TRUE,
         stack = TRUE,
         createStyle(numFmt = '0.0%'))

# Style # Columns
addStyle(wb,sheet_name,
         rows = 3:(nrow(cost_util_summary)+2),
         cols = which(grepl('#',names(cost_util_summary))),
         gridExpand = TRUE,
         stack = TRUE,
         createStyle(numFmt = '0,0'))

# Autofit Columns
setColWidths(wb,sheet_name,widths = 'auto',cols = 1:ncol(cost_util_summary))

#### SUD Residential ####
sheet_name <- 'SUD Residential Stay Summary'
if(sum(wb$sheet_names==sheet_name)>0){removeWorksheet(wb,sheet_name)}

addWorksheet(wb, sheet_name)
writeData(wb,x=bind_rows(sud_res_total,sud_res),sheet_name,
          rowNames = FALSE,
          headerStyle = s1)
addStyle(wb,sheet_name,rows = 1, cols = 1, stack = TRUE, style = createStyle(halign = 'left'))

# Style Totals
addStyle(wb,sheet_name,
         rows = 2, 
         cols = 1:ncol(sud_res), 
         stack = TRUE, 
         style = createStyle(textDecoration = c('bold'),
                             border = 'bottom'))

# Format # Columns
addStyle(wb,sheet_name,
         rows = 2:(nrow(sud_res)+2),
         cols = which(grepl('#',names(sud_res))),
         gridExpand = TRUE,
         stack = TRUE,
         style = createStyle(numFmt = '#,##0;-#,##0;-'))

# Format Days Columns
addStyle(wb,sheet_name,
         rows = 2:(nrow(sud_res)+2),
         cols = which(names(sud_res) %in% c('Avg. LOS','Days / Member')),
         gridExpand = TRUE,
         stack = TRUE,
         style = createStyle(numFmt = '#,##0.0;-#,##0.0;-'))

# Format % Columns
addStyle(wb,sheet_name,
         rows = 2:(nrow(sud_res)+2),
         cols = which(grepl('%',names(sud_res))),
         gridExpand = TRUE,
         stack = TRUE,
         style = createStyle(numFmt = '#,##0.0%;-#,##0.0%;-%'))

# Format Total Paid Column
addStyle(wb,sheet_name,
         rows = 2:(nrow(sud_res)+2),
         cols = which(grepl('Total Paid',colnames(sud_res))),
         gridExpand = TRUE,
         stack = TRUE,
         style = createStyle(numFmt = '$#,##0;-$#,##0;-'))

# Format $ Columns
addStyle(wb,sheet_name,
         rows = 2:(nrow(sud_res)+2),
         cols = which(names(sud_res) %in% c('Paid / Member','Cost / Day')),
         gridExpand = TRUE,
         stack = TRUE,
         style = createStyle(numFmt = 'CURRENCY'))

# Autofit Columns
setColWidths(wb,sheet_name,widths = 'auto',cols = 1:ncol(cost_util_summary))

#### SUD Outpatient ####
sheet_name <- 'SUD Outpatient'
if(sum(wb$sheet_names==sheet_name)>0){removeWorksheet(wb,sheet_name)}

addWorksheet(wb,sheet_name)
writeData(wb,sheet_name,bind_rows(outpt_sud_total,outpt_sud),
          rowNames = FALSE,
          headerStyle = s1)

# Style Totals
addStyle(wb,sheet_name,
         rows = 2,
         cols = 1:ncol(outpt_sud),
         gridExpand = TRUE,
         stack = TRUE,
         style = createStyle(textDecoration = c('bold'),
                             border = 'bottom'))

# Style Commas
addStyle(wb,sheet_name,
         rows = 2:(nrow(outpt_sud)+2),
         cols = which(grepl('#',colnames(outpt_sud))),
         gridExpand = TRUE,
         stack = TRUE,
         style = createStyle(numFmt = '#,##0;-#,##0;-'))

# Style Total Paid
addStyle(wb,sheet_name,
         rows = 2:(nrow(outpt_sud)+2),
         cols = which(grepl('Total Paid',colnames(outpt_sud))),
         gridExpand = TRUE,
         stack = TRUE,
         style = createStyle(numFmt = '$#,##0;-$#,##0;-'))

# Style Avg. Paid
addStyle(wb,sheet_name,
         rows = 2:(nrow(outpt_sud)+2),
         cols = which(colnames(outpt_sud)=='Avg. Paid'),
         gridExpand = TRUE,
         stack = TRUE,
         style = createStyle(numFmt = 'CURRENCY'))

setColWidths(wb,sheet_name,widths = 'auto',cols = 1:ncol(outpt_sud))

#### Outpatient SUD Detail ####
sheet_name <- 'SUD Outpatient Details'
if(sum(wb$sheet_names==sheet_name)>0){removeWorksheet(wb,sheet_name)}

addWorksheet(wb,sheet_name)
writeData(wb,sheet_name,outpt_sud_detail,
          withFilter = openxlsx_getOp("withFilter", TRUE),
          headerStyle = s1)

# Style Commas
addStyle(wb,sheet_name,
         rows = 2:(nrow(outpt_sud_detail)+1),
         cols = which(grepl('#',colnames(outpt_sud_detail))),
         gridExpand = TRUE,
         stack = TRUE,
         style = createStyle(numFmt = '#,##0;-#,##0;-'))

# Style $
addStyle(wb,sheet_name,
         rows = 2:(nrow(outpt_sud_detail)+1),
         cols = which(grepl('Paid',colnames(outpt_sud_detail))),
         gridExpand = TRUE,
         stack = TRUE,
         style = createStyle(numFmt = 'CURRENCY'))

# Add SUD Outpatient Plot
print(outpt_sud_plot)
insertPlot(wb,sheet_name,width = 15, height = 8, startRow = 25)

setColWidths(wb,sheet_name,cols = 1:nrow(outpt_sud_detail), widths = 'auto')

#### Hospital IP Summary ####
sheet_name <- 'Hospital IP Summary'
if(sum(wb$sheet_names==sheet_name)>0){removeWorksheet(wb,sheet_name)}

addWorksheet(wb, sheet_name)
writeData(wb, sheet_name, bind_rows(admits_total,admits),headerStyle = s1)

# Style Totals
addStyle(wb,sheet_name,
         rows = 2,
         cols = 1:ncol(admits),
         gridExpand = TRUE,
         stack = TRUE,
         style = createStyle(textDecoration = c('bold'),
                             border = 'bottom'))

# Style Commas
addStyle(wb,sheet_name,
         rows = 2:(nrow(admits)+2),
         cols = which(grepl('#',colnames(admits))),
         gridExpand = TRUE,
         stack = TRUE,
         style = createStyle(numFmt = '#,##0;-#,##0;-'))

# Style Total Paid
addStyle(wb,sheet_name,
         rows = 2:(nrow(admits)+2),
         cols = which(grepl('Total Paid',colnames(admits))),
         gridExpand = TRUE,
         stack = TRUE,
         style = createStyle(numFmt = '$#,##0;-$#,##0;-'))

# Style Avg Paid
addStyle(wb,sheet_name,
         rows = 2:(nrow(admits)+2),
         cols = which(colnames(admits)=='Paid / Admit'),
         gridExpand = TRUE,
         stack = TRUE,
         style = createStyle(numFmt = 'CURRENCY'))

setColWidths(wb,sheet_name,widths = 'auto',cols = which(colnames(admits)!='ICD Desc'))
setColWidths(wb,sheet_name,widths = 70,cols = which(colnames(admits)=='ICD Desc'))

#### Outpatient Surgery ####
sheet_name <- 'Outpatient Surgery'
if(sum(wb$sheet_names==sheet_name)>0){removeWorksheet(wb,sheet_name)}

addWorksheet(wb, sheet_name)
writeData(wb, sheet_name, bind_rows(outpt_surg_total,outpt_surg),headerStyle = s1)

# Style Totals
addStyle(wb,sheet_name,
         rows = 2,
         cols = 1:ncol(outpt_surg),
         gridExpand = TRUE,
         stack = TRUE,
         style = createStyle(textDecoration = c('bold'),
                             border = 'bottom'))

# Style Commas
addStyle(wb,sheet_name,
         rows = 2,
         cols = which(grepl('#',colnames(outpt_surg))),
         gridExpand = TRUE,
         stack = TRUE,
         style = createStyle(numFmt = 'COMMA'))

# Style Total Paid
addStyle(wb,sheet_name,
         rows = 2,
         cols = which(grepl('Total Paid',colnames(outpt_surg))),
         gridExpand = TRUE,
         stack = TRUE,
         style = createStyle(numFmt = '$#,##0;-$#,##0;-'))

# Style Avg Paid
addStyle(wb,sheet_name,
         rows = 2:(nrow(admits)+2),
         cols = which(colnames(outpt_surg)=='Avg. Paid'),
         gridExpand = TRUE,
         stack = TRUE,
         style = createStyle(numFmt = 'CURRENCY'))

# Style %
addStyle(wb,sheet_name,
         rows = 2:(nrow(admits)+2),
         cols = which(grepl('%',colnames(outpt_surg))),
         gridExpand = TRUE,
         stack = TRUE,
         style = createStyle(numFmt = '0.0%'))

setColWidths(wb,sheet_name,widths = 'auto',cols = which(colnames(outpt_surg)!='ICD Desc'))
setColWidths(wb,sheet_name,widths = 70,cols = which(colnames(outpt_surg)=='ICD Desc'))

#### Drug Tests ####
sheet_name <- 'Drug Tests'
if(sum(wb$sheet_names==sheet_name)>0){removeWorksheet(wb,sheet_name)}

addWorksheet(wb, sheet_name)
writeData(wb, sheet_name, bind_rows(lab_billing_providers_total,lab_billing_providers),
          headerStyle = s1)

# Style Totals
addStyle(wb,sheet_name,
         rows = 2,
         cols = 1:ncol(lab_billing_providers_total),
         gridExpand = TRUE,
         stack = TRUE,
         style = createStyle(textDecoration = c('bold'),
                             border = 'bottom'))

# Style Total Paid
addStyle(wb,sheet_name,
         rows = 2:(nrow(lab_billing_providers)+2),
         cols = which(grepl('Total Paid',colnames(lab_billing_providers))),
         gridExpand = TRUE,
         stack = TRUE,
         style = createStyle(numFmt = '$#,##0;-$#,##0;-'))

# Style %
addStyle(wb,sheet_name,
         rows = 2:(nrow(lab_billing_providers)+2),
         cols = which(grepl('%',colnames(lab_billing_providers))),
         gridExpand = TRUE,
         stack = TRUE,
         style = createStyle(numFmt = '#,##0.0%;-#,##0.0%;-'))

# Style # Tests / Members
addStyle(wb,sheet_name,
         rows = 2:(nrow(lab_billing_providers)+2),
         cols = which(colnames(lab_billing_providers) %in% c('# Tests','# Members')),
         gridExpand = TRUE,
         stack = TRUE,
         style = createStyle(numFmt = '#,##0;-#,##0;-'))

# Style Avg. Paid
addStyle(wb,sheet_name,
         rows = 2:(nrow(lab_billing_providers)+2),
         cols = which(grepl('Avg. Paid',colnames(lab_billing_providers))),
         gridExpand = TRUE,
         stack = TRUE,
         style = createStyle(numFmt = '$#,##0.00;-$#,##0.00;-'))

# Style Avg. #
addStyle(wb,sheet_name,
         rows = 2:(nrow(lab_billing_providers)+2),
         cols = which(grepl('Avg. #',colnames(lab_billing_providers))),
         gridExpand = TRUE,
         stack = TRUE,
         style = createStyle(numFmt = '#,##0.0;-#,##0.0;-'))

setColWidths(wb,sheet_name,widths = 'auto',cols = which(colnames(lab_billing_providers)!='Billing Provider'))
setColWidths(wb,sheet_name,widths = 45,cols = which(colnames(lab_billing_providers)=='Billing Provider'))

#### Drug Test Trend ####
sheet_name <- 'Drug Testing Trend'
if(sum(wb$sheet_names==sheet_name)>0){removeWorksheet(wb,sheet_name)}

addWorksheet(wb, sheet_name)
print(drug_test_trend_plot)
insertPlot(wb, sheet_name, width = 15, height = 8, fileType = "png", units = "in")

#### High Utilizers: Ambulances ####
sheet_name <- 'Ambulance Summary'
if(sum(wb$sheet_names==sheet_name)>0){removeWorksheet(wb,sheet_name)}
addWorksheet(wb, sheet_name)

# Ambulance Summary
writeData(wb,sheet_name,ambulance_summary, startRow = 2, headerStyle = s1)
writeData(wb,sheet_name,x='Frequent Ambulance Users (4+ Rides)',startRow = 1, startCol = 1)
mergeCells(wb,sheet_name,cols=1:ncol(ambulance_summary),rows = 1)
addStyle(wb,sheet_name,style = s1, rows = 1, cols = 1:ncol(ambulance_summary))

#Ambulance Detail
writeData(wb,sheet_name,ambulance_detail, startRow = 2, startCol = ncol(ambulance_summary)+2, 
          headerStyle = s1, withFilter = openxlsx_getOp("withFilter", TRUE))
writeData(wb,sheet_name,x='Ambulance Ride Details',
          startRow = 1,
          startCol = (ncol(ambulance_summary)+2))
mergeCells(wb,sheet_name,
           cols=(ncol(ambulance_summary)+2):((ncol(ambulance_summary)+1)+ncol(ambulance_detail)),
           rows = 1)
addStyle(wb,sheet_name,style = s1, 
         rows = 1, 
         cols = (ncol(ambulance_summary)+2):((ncol(ambulance_summary)+1)+ncol(ambulance_detail)))

# Format Total Paid Column
addStyle(wb,sheet_name,
         rows = 3:(nrow(ambulance_summary)+3),
         cols = c(which(grepl('Total Paid',colnames(ambulance_summary))),
                  which(grepl('paid',colnames(ambulance_detail))) + ncol(ambulance_summary)+1),
         gridExpand = TRUE,
         stack = TRUE,
         style = createStyle(numFmt = '$#,##0;-$#,##0;-'))

# Format Paid / Risk
addStyle(wb,sheet_name,
         rows = 3:(nrow(ambulance_summary)+3),
         cols = which(grepl('Paid / Ride',colnames(ambulance_summary))),
         gridExpand = TRUE,
         stack = TRUE,
         style = createStyle(numFmt = '$#,##0.00;-$#,##0.00;-'))

setColWidths(wb,sheet_name,cols = 1:((ncol(ambulance_summary)+1)+ncol(ambulance_detail)),widths = 'auto')

#### ED Visits ####
sheet_name <- 'ED Summary'
if(sum(wb$sheet_names==sheet_name)>0){removeWorksheet(wb,sheet_name)}
addWorksheet(wb, sheet_name)

writeData(wb,sheet_name,bind_rows(ed_summary_total,ed_summary),headerStyle = s1)

# Style Totals
addStyle(wb,sheet_name,
         rows = 2,
         cols = 1:ncol(ed_summary),
         gridExpand = TRUE,
         stack = TRUE,
         style = createStyle(textDecoration = c('bold'),
                             border = 'bottom'))


# Format Total Paid Column
addStyle(wb,sheet_name,
         rows = 2:(nrow(ed_summary)+2),
         cols = which(grepl('Total Paid',colnames(ed_summary))),
         gridExpand = TRUE,
         stack = TRUE,
         style = createStyle(numFmt = '$#,##0;-$#,##0;-'))

# Format PMPM / Avg. Paid
addStyle(wb,sheet_name,
         rows = 2:(nrow(ed_summary)+2),
         cols = which(colnames(ed_summary) %in% c('Avg. Paid','PMPM')),
         gridExpand = TRUE,
         stack = TRUE,
         style = createStyle(numFmt = '$#,##0.00;-$#,##0.00;-'))

# Format # Columns
addStyle(wb,sheet_name,
         rows = 2:(nrow(ed_summary)+2),
         cols = which(grepl('#',colnames(ed_summary))),
         gridExpand = TRUE,
         stack = TRUE,
         style = createStyle(numFmt = '#,##0;-#,##0;-'))

setColWidths(wb,sheet_name,cols = 1:((ncol(ambulance_summary)+1)+ncol(ambulance_detail)),widths = 'auto')

#### Avoidable ED Visit Details ####
sheet_name <- 'Avoidable ED Detail'
if(sum(wb$sheet_names==sheet_name)>0){removeWorksheet(wb,sheet_name)}
addWorksheet(wb, sheet_name)

writeData(wb,sheet_name,avoidable_ed,headerStyle = s1, withFilter = openxlsx_getOp("withFilter", TRUE))

# Format Paid
addStyle(wb,sheet_name,
         rows = 2:(nrow(ed_summary)+2),
         cols = which(colnames(avoidable_ed) %in% c('Paid')),
         gridExpand = TRUE,
         stack = TRUE,
         style = createStyle(numFmt = '$#,##0;-$#,##0;-'))

setColWidths(wb,sheet_name,cols = 1:ncol(avoidable_ed),widths = 'auto')
setColWidths(wb,sheet_name,
             cols = which(colnames(avoidable_ed) %in% c('Facility Name','Preventable Scenario','ICD Code Desc')))

#### Medical Pharmacy ####
sheet_name <- 'Medical Pharmacy Summary'
if(sum(wb$sheet_names==sheet_name)>0){removeWorksheet(wb,sheet_name)}
addWorksheet(wb, sheet_name)

writeData(wb,sheet_name,bind_rows(med_pharm_summary_total,med_pharm_summary),
          withFilter = openxlsx_getOp('withFilter',TRUE),headerStyle = s1)

print(med_pharm_plot)
insertPlot(wb,sheet_name,height=4.5,width=8,startRow = 3, startCol = ncol(med_pharm_summary)+2)

# Style Totals
addStyle(wb, sheet_name,rows = 2, cols = 1:ncol(med_pharm_summary),
         style = createStyle(border = 'bottom',textDecoration = 'bold'))

# Format # Columns
addStyle(wb,sheet_name,
         rows = 2:(nrow(med_pharm_summary)+2),
         cols = which(grepl('#',colnames(med_pharm_summary))),
         gridExpand = TRUE,
         stack = TRUE,
         style = createStyle(numFmt = '#,##0;-#,##0;-'))

# Format Savings Column
addStyle(wb,sheet_name,
         rows = 2:(nrow(med_pharm_summary)+2),
         cols = which(grepl('Savings',colnames(med_pharm_summary))),
         gridExpand = TRUE,
         stack = TRUE,
         style = createStyle(numFmt = '$#,##0;-$#,##0;-'))

# Format % Column
addStyle(wb,sheet_name,
         rows = 2:(nrow(med_pharm_summary)+2),
         cols = which(grepl('%',colnames(med_pharm_summary))),
         gridExpand = TRUE,
         stack = TRUE,
         style = createStyle(numFmt = '0.0%;-0.0%;-'))

# Add Notes Column
writeData(wb,sheet_name,c('Notes'),startCol = ncol(med_pharm_summary)+1)
addStyle(wb,sheet_name,rows=1, cols=ncol(med_pharm_summary)+1,style = s1)

setColWidths(wb,sheet_name,cols = 1:ncol(med_pharm_summary),widths = 'auto')
setColWidths(wb,sheet_name,cols = ncol(med_pharm_summary)+1,widths = 40)
setColWidths(wb,sheet_name,cols = which(colnames(med_pharm_summary)=='Proc Code Desc'),widths = 50)

#### Medical Pharmacy Detail
sheet_name <- 'Medical Pharmacy Detail'
if(sum(wb$sheet_names==sheet_name)>0){removeWorksheet(wb,sheet_name)}
addWorksheet(wb, sheet_name)

writeData(wb,sheet_name,med_pharm_detail,headerStyle = s1,
          withFilter = openxlsx_getOp('withFilter',TRUE))

# Format $ Columns
addStyle(wb,sheet_name,
         rows = 2:(nrow(med_pharm_detail)+2),
         cols = which(colnames(med_pharm_detail) %in% c('Allowed','Paid','ASP Cost','Savings Opportunity')),
         gridExpand = TRUE,
         stack = TRUE,
         style = createStyle(numFmt = '$#,##0.00;-$#,##0.00;-'))

# Format ASP Unit Column
addStyle(wb,sheet_name,
         rows = 2:(nrow(med_pharm_detail)+2),
         cols = which(colnames(med_pharm_detail) %in% c('ASP Unit Cost')),
         gridExpand = TRUE,
         stack = TRUE,
         style = createStyle(numFmt = '$#,##0.000;-$#,##0.000;-'))

# Format % Column
addStyle(wb,sheet_name,
         rows = 2:(nrow(med_pharm_detail)+2),
         cols = which(grepl('%',colnames(med_pharm_detail))),
         gridExpand = TRUE,
         stack = TRUE,
         style = createStyle(numFmt = '0.0%;-0.0%;-'))


setColWidths(wb,sheet_name,cols = 1:ncol(med_pharm_detail),widths = 'auto')
setColWidths(wb,sheet_name,cols = which(colnames(med_pharm_detail)=='Proc Code Desc'),widths = 50)

#### Imaging ####
sheet_name <- 'Imaging Summary'
if(sum(wb$sheet_names==sheet_name)>0){removeWorksheet(wb,sheet_name)}
addWorksheet(wb, sheet_name)

writeData(wb,sheet_name,bind_rows(imaging_summary,imaging_summary_total),headerStyle = s1,startRow = 2)
addStyle(wb,sheet_name,
         rows = nrow(imaging_summary)+3,
         cols = 1:ncol(imaging_summary),
         gridExpand = TRUE,
         style = createStyle(textDecoration = c('bold'),
                             fgFill = 'lightgrey',
                             border = 'TopBottom'))

temp <- c('Encounter Category',sapply(str_split(names(imaging_summary),'_')[-1],function(x)ifelse(x,'In Scope','Not In Scope'))[2,])
for(i in 1:length(temp)){
  writeData(wb,sheet_name,
            c(temp[i]),
            startRow = 2,
            startCol = i)
  addStyle(wb,sheet_name,style=s1,rows = 2, cols = i,stack = TRUE)
}



for(i in seq(2,ncol(imaging_summary),by=2)){
  temp <- seq(i,i+1,1)
  mergeCells(wb,sheet_name,rows = 1, cols = temp)
  writeData(wb,sheet_name,startRow = 1, startCol = i,
            x = sapply(str_split(names(imaging_summary)[-1],'_'),identity)[1,][i])
  print(temp)
}
addStyle(wb,sheet_name,rows=1,cols=1:ncol(imaging_summary),stack = TRUE,
         style = createStyle(textDecoration=c("bold"),
                             fgFill = 'lightgrey',
                             halign = 'center',
                             border = 'TopBottomLeftRight '))

addStyle(wb,sheet_name,rows = 2:(nrow(imaging_summary)+3),cols = seq(1,ncol(imaging_summary),2),
         gridExpand = TRUE,style = createStyle(border = 'right'),stack = TRUE)

addStyle(wb,sheet_name,rows=nrow(imaging_summary)+3,cols=ncol(imaging_summary),stack = TRUE,
         style = createStyle(textDecoration = c('bold'),
                             fgFill = 'lightgrey',
                             border = 'TopBottom'))

# Style # Columns
addStyle(wb,sheet_name,
         rows = 3:(nrow(imaging_summary)+3),
         cols = which(grepl('#',names(imaging_summary))),
         gridExpand = TRUE,
         stack = TRUE,
         style = createStyle(numFmt = '#,##0;-#,##0;-'))

# Style $ Columns
addStyle(wb,sheet_name,
         rows = 3:(nrow(imaging_summary)+3),
         cols = which(grepl('%',names(imaging_summary))),
         gridExpand = TRUE,
         stack = TRUE,
         style = createStyle(numFmt = '0.0%;-$0.0%;-'))

# Style Total Paid
addStyle(wb,sheet_name,
         rows = 3:(nrow(imaging_summary)+3),
         cols = which(grepl('Total Paid',names(imaging_summary))),
         gridExpand = TRUE,
         stack = TRUE,
         style = createStyle(numFmt = '$#,##0;-$#,##0;-'))

# Style Avg Paid
addStyle(wb,sheet_name,
         rows = 3:(nrow(imaging_summary)+3),
         cols = which(grepl('Avg. Paid',names(imaging_summary))),
         gridExpand = TRUE,
         stack = TRUE,
         style = createStyle(numFmt = '$#,##0.00;-$#,##0.00;-'))

setColWidths(wb,sheet_name,cols = 1:ncol(imaging_summary),widths = 'auto')

#### Imaging Details ####
sheet_name <- 'Imaging Details by POS'
if(sum(wb$sheet_names==sheet_name)>0){removeWorksheet(wb,sheet_name)}
addWorksheet(wb, sheet_name)

writeData(wb,sheet_name,imaging_details,headerStyle = s1,
          withFilter = openxlsx_getOp("withFilter", FALSE))

# Style # Columns
addStyle(wb,sheet_name,
         rows = 2:(nrow(imaging_details)+1),
         cols = which(grepl('#',names(imaging_details))),
         gridExpand = TRUE,
         stack = TRUE,
         style = createStyle(numFmt = '#,##0;-#,##0;-'))

# Style $ Columns
addStyle(wb,sheet_name,
         rows = 2:(nrow(imaging_details)+1),
         cols = which(grepl('%',names(imaging_details))),
         gridExpand = TRUE,
         stack = TRUE,
         style = createStyle(numFmt = '0.0%;-$0.0%;-'))

# Style Total Paid
addStyle(wb,sheet_name,
         rows = 2:(nrow(imaging_details)+1),
         cols = which(grepl('Total Paid',names(imaging_details))),
         gridExpand = TRUE,
         stack = TRUE,
         style = createStyle(numFmt = '$#,##0;-$#,##0;-'))

# Style Total Paid
addStyle(wb,sheet_name,
         rows = 2:(nrow(imaging_details)+1),
         cols = which(grepl('Avg. Paid',names(imaging_details))),
         gridExpand = TRUE,
         stack = TRUE,
         style = createStyle(numFmt = '$#,##0.00;-$#,##0.00;-'))


setColWidths(wb,sheet_name,cols = 1:ncol(imaging_summary),widths = 'auto')
setColWidths(wb,sheet_name,cols=which(colnames(imaging_details) %in% c('Proc Code Description','HCPCS Category')),widths=c(40,40))

saveWorkbook(wb,fn,overwrite = TRUE,returnValue = TRUE)
