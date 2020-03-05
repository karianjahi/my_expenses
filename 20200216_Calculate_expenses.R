filename = "/home/karianjahi/Downloads/20200201_daily_expenses.txt"
outfilename = "/home/karianjahi/Downloads/20200201_daily_expenses_out.txt"
data = read.delim(filename, sep = ",", stringsAsFactors = FALSE)
add0 = function(x) ifelse(nchar(x) < 2, paste0("0", x), x)
add0After = function(x) ifelse(nchar(x) < 2, paste0(x, "0"), x)
dtime_char = unlist(lapply(format(data$time, digits = 4), function(x) gsub(" ", "", x)))
data$time = unlist(lapply(strsplit(dtime_char, "\\."), function(x) paste0(add0(x[1]), ".", add0After(x[2]))))

# Create a date time
date_time = format(strptime(paste0(data$date, " ", data$time), format = "%d.%m.%Y %H.%M"), format = "%Y-%b-%d %H:%M")
data = data.frame(cbind(time = date_time, data[, 3:ncol(data)]))
names(data) = c("date/time", names(data)[2:3])
data = data[order(strptime(data$`date/time`, "%Y-%b-%d %H:%M"), decreasing = TRUE), ]

salary = 2530
write.table(x = data, file = outfilename, sep = ",", quote = FALSE, col.names = TRUE, row.names = FALSE)

# Aggregate by day 
library(xts)
data.xts = xts(data$amount, strptime(data$`date/time`, "%Y-%b-%d %H:%M"))
daysum = unlist(lapply(split(data$amount, format(index(data.xts), "%Y-%b-%d")), sum))
daysum_df = data.frame(date = names(daysum), amount = unlist(daysum));rownames(daysum_df) = 1:nrow(daysum_df)


my_list = NULL
my_list$salary = salary
total_expenses = sum(data$amount) ; my_list$total_expenses = total_expenses
amount_remaining = salary - total_expenses ;my_list$Savings = amount_remaining
food_table = data[grepl("Food", data$description), ] ; my_list$food_table = food_table
food_expenses = sum(food_table$amount) ; my_list$food_expenses = food_expenses
Transport_table = data[grepl("Transport", data$description), ] ; my_list$transport_table = Transport_table
transport_expenses = sum(Transport_table$amount) ; my_list$transport_expenses = transport_expenses
avoidable_table = data[-c(grep("Rent", data$description), grep("Food", data$description)), ] ; my_list$avoidable_table = avoidable_table
avoidable_expenses = sum(avoidable_table$amount) ; my_list$avoidable_expenses = avoidable_expenses
lean_table = data[c(grep("Rent", data$description), grep("Food", data$description)), ]
lean_expenses = sum(lean_table$amount) ; my_list$lean_expenses_rent_and_food = lean_expenses
my_list$daily_expenses = daysum_df

# Focus on current month and year
current_month = format(Sys.Date(), "%Y-%b")
data = data[grepl(current_month, data$`date/time`), ]
mon_list = NULL
mon_list$current_month_year = current_month
mon_list$salary = salary
total_expenses = sum(data$amount) ; mon_list$total_expenses = total_expenses
amount_remaining = salary - total_expenses ;mon_list$Savings = amount_remaining
food_table = data[grepl("Food", data$description), ] ; mon_list$food_table = food_table
food_expenses = sum(food_table$amount) ; mon_list$food_expenses = food_expenses
Transport_table = data[grepl("Transport", data$description), ] ; mon_list$transport_table = Transport_table
transport_expenses = sum(Transport_table$amount) ; mon_list$transport_expenses = transport_expenses
avoidable_table = data[-c(grep("Rent", data$description), grep("Food", data$description)), ] ; mon_list$avoidable_table = avoidable_table
avoidable_expenses = sum(avoidable_table$amount) ; mon_list$avoidable_expenses = avoidable_expenses
lean_table = data[c(grep("Rent", data$description), grep("Food", data$description)), ]
lean_expenses = sum(lean_table$amount) ; mon_list$lean_expenses_rent_and_food = lean_expenses
