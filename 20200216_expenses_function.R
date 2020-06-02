cat(paste0("\n\n\n\n----------------------------------------------------------------------"))
cat(paste0("\nRun the script as Rscript 20200216_expenses_function.R mmm yyyy nnnn \nwhere mmm is month abbreviation and yyyy is year in full\nE.g Rscript 20200216_expenses_function.R Sep 2015\n"))
cat(paste0("----------------------------------------------------------------------\n\n\n"))

inputs = commandArgs(trailingOnly = TRUE)

SavingsExpenses = function(filename = "20200201_daily_expenses.txt",
                           output_file = "20200201_daily_expenses_out.txt",
                           year_and_month = 202005,
                           total_monthly_expenses = TRUE, 
                           total_expenses = TRUE){
  
  # Read income file 
  income_dat = read.delim("update_income.txt", sep = ",", header = FALSE, stringsAsFactors = FALSE)
  
  # read the file 
  data = read.csv(filename, stringsAsFactors = FALSE)

  # Take the date
  dates = data$date
  
  # Take time 
  time = as.character(data$time)
  time_splits = strsplit(time, "\\.")
  
  # Some handy custom functions to take care of hour and minute
  add0_b4 = function(x) ifelse(nchar(x) < 2, paste0("0", x), x)
  add0_af = function(x) ifelse(nchar(x) < 2, paste0(x, "0"), x)
  whatNA = function(x) ifelse(is.na(x), "00", add0_af(x))
  
  # Get the hour
  hour = add0_b4(unlist(lapply(time_splits, function(x) x[1])))
  
  # Get minute 
  minute = whatNA(unlist(lapply(time_splits, function(x) x[2])))
  
  # Hour minute
  hrmin = paste0(hour, ":", minute)
  
  # Date and time 
  nice_dtime = strptime(paste0(dates, " ", hrmin), format = "%d.%m.%Y %H:%M", tz = "EAT")
  
  # Our final table
  final_table = data.frame(time = nice_dtime, desc = data$description, amt = data$amount)
  
  # Order the table by date and time
  final_table = final_table[order(final_table$time), ]
  
  # Take year and month from table to mimick request 
  year_month_from_table = format(final_table$time, "%Y%m")
  
  # Get row indices corresponding to year and month of request
  if(nchar(year_and_month) < 6){
    stop(paste0("format of year_and_month should be yyyymm"))
  } else {
    
    # month table
    month_table = final_table[grepl(as.character(year_and_month), year_month_from_table), ]
    
    # What is the month 
    mon = unique(format(month_table$time, "%B"))
    year = unique(format(month_table$time, "%Y"))
    
    # monthly_expenses 
    monthly_expenses = sum(month_table$amt)
    
    # income mons 
    income_mons = format(as.Date(income_dat$V1, "%Y%b%d"), "%m")
    
    # Which mon is requested 
    mon_req = substr(year_and_month, 5, 6)
    
    # Income for month requested
    income4mon_requested = income_dat[which(income_mons == mon_req), 2]
    
    # Monthly savings
    monthly_savings = income4mon_requested - monthly_expenses
    
    # Months paid 
    months_paid = unique(format(final_table$time, "%Y-%m"))
    
    # No. of months paid 
    nmon = length(income_dat$V1)
    
    # Total income 
    total_income = sum(income_dat$V2)
    
    # Total savings 
    total_savings = total_income - sum(final_table$amt) 
    
    # Total expenses 
    total_expenses = sum(final_table$amt)
    
    #cat(paste0("\nAverage Total Income so far = Eur ", format(total_income, big.mark = ","), "\n"))
    cat(paste0("\nAverage Total Expenses since 1st February 2020 = Eur ", format(total_expenses, big.mark = ","), "\n"))
    cat(paste0("\nMonthly income for the month of ", mon, " ",  year, " = Eur ", format(income4mon_requested, big.mark = ","), "\n"))
    cat(paste0("\nMonthly expenses for the month of ", mon, " ",  year, " = Eur ", format(monthly_expenses, big.mark = ","), "\n"))
    cat(paste0("\nMonthly savings for the month of ", mon, " ",  year, " = Eur ", format(monthly_savings, big.mark = ","), "\n"))
    cat(paste0("\nTotal Savings for the last ", nmon, " months = Eur ", format(total_savings, big.mark = ","), "\n"))
    
    # Save output file 
    table2save = data.frame(time = format(final_table[, 1], "%Y-%b-%m %H:%M"), final_table[, c(2, 3)])
    write.table(x = table2save, file = output_file, sep = ",", row.names = FALSE, quote = FALSE)
    
    }
  
  
}
# Some functions to handle the month and year
yearnum = function(year){
  out = tryCatch(
    {
      as.numeric(year)  
    },
    error = function(cond){
      return("error")
    },
    warning = function(cond){
      return("warning")
    }
    
  )
  return(out)
}

monfun = function(month){
  
  out = tryCatch(
    {
      as.Date(paste0("2020", month, "01"), "%Y%b%d")
    },
    warning = function(cond){
      return("warning")
    }
  )
  return(out)
}


if(length(inputs) != 2) {
  cat(paste0("==========================================================================================\n"))
  cat(paste0("No of inputs not valid. Two inputs are expected. Start with month as bbb and Year as yyyy.\nSystem shall now use current month and Year\n"))
  cat(paste0("==========================================================================================\n\n\n"))
  current_month = format(Sys.Date(), "%b")
  current_year = format(Sys.Date(), "%Y")
} else {
  
  # Take the first input  
  input_1 = inputs[1]
  
  # Check if it is a valid month 
  mon_check = monfun(input_1)

  if(is.na(mon_check)){
    cat(paste0("==============================================================================================="))
    cat(paste0("\nHinweis: First input is not a valid month in the format bbb. Shall use the current month instead\n"))
    cat(paste0("===============================================================================================\n\n\n"))
    current_month =  format(Sys.Date(), "%b")
  } else {
      current_month = input_1
  }
  
  # Take the second input 
  input_2 = inputs[2]
  
  # Check if it is a valid year 
  year_check = yearnum(input_2)
  if(year_check == "warning"){
    cat(paste0("==============================================================================================="))
    cat(paste0("\nHinweis: Second input is not a valid year in the form yyyy. Shall use the current year instead\n"))
    cat(paste0("===============================================================================================\n\n\n"))
    current_year = format(Sys.Date(), "%Y")
  } else {
    current_year = input_2  
    }
  
}

income_data = read.delim("update_income.txt", sep = ",", header = FALSE, stringsAsFactors = FALSE)
valid_dates = format(as.Date(income_data$V1, format = "%Y%b%d"), "%Y%m")
year_and_month = format(as.Date(paste0(current_year, current_month, "01"), format = "%Y%b%d"), "%Y%m")
if(year_and_month < min(valid_dates) | year_and_month > max(valid_dates)){
  cat(paste0("No results. Time requested outside valid period\n"))
  } else {
    SavingsExpenses(year_and_month = year_and_month)
  }
