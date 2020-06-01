cat(paste0("\n\nRun the script as Rscript 20200216_expenses_function.R mmm yyyy nnnn \nwhere mmm is month abbreviation, yyyy is year in full and nnnn is salary in 4 digits\nE.g Rscript 20200216_expenses_function.R Sep 2015 3400\n\n"))

inputs = commandArgs(trailingOnly = TRUE)

SavingsExpenses = function(filename = "20200201_daily_expenses.txt",
                           output_file = "20200201_daily_expenses_out.txt",
                           income = 2150,  # Income for this month
                           av_salary = 2245,
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
    mon = unique(format(month_table$time, "%b"))
    year = unique(format(month_table$time, "%Y"))
    
    # monthly_expenses 
    monthly_expenses = sum(month_table$amt)
    
    # Monthly savings 
    monthly_savings = income - monthly_expenses
    
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
    cat(paste0("\nAverage Total Expenses so far = Eur ", format(total_expenses, big.mark = ","), "\n"))
    #cat(paste0("\nMonthly income for the month of ", mon, ", ",  year, " = Eur ", format(income, big.mark = ","), "\n"))
    cat(paste0("\nMonthly expenses for the month of ", mon, ", ",  year, " = Eur ", format(monthly_expenses, big.mark = ","), "\n"))
    cat(paste0("\nMonthly savings for the month of ", mon, ", ",  year, " = Eur ", format(monthly_savings, big.mark = ","), "\n"))
    cat(paste0("\nTotal Savings for the last ", nmon, " months = Eur ", format(total_savings, big.mark = ","), "\n"))
    
    # Save output file 
    table2save = data.frame(time = format(final_table[, 1], "%Y-%b-%m %H:%M"), final_table[, c(2, 3)])
    write.table(x = table2save, file = output_file, sep = ",", row.names = FALSE, quote = FALSE)
    
    }
  
  
}


if(length(inputs) > 0) {
  year_and_month = as.numeric(format(as.Date(paste0(inputs[1], inputs[2], "01"), format = "%b%Y%d"), "%Y%m"))
  income = as.numeric(inputs[3])
  SavingsExpenses(year_and_month = year_and_month, income = income)
  } else {
    SavingsExpenses()
    }


