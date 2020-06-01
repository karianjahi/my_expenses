filename = "20200201_daily_expenses.txt"
outfilename = "20200201_daily_expenses_out.txt"
data = read.delim(filename, sep = ",", stringsAsFactors = FALSE)
add0 = function(x) ifelse(nchar(x) < 2, paste0("0", x), x)
add0After = function(x) ifelse(nchar(x) < 2, paste0(x, "0"), x)
dtime_char = unlist(lapply(format(data$time, digits = 4), function(x) gsub(" ", "", x)))
data$time = unlist(lapply(strsplit(dtime_char, "\\."), function(x) paste0(add0(x[1]), ".", add0After(x[2]))))

# Create a date time
date_time = format(strptime(paste0(data$date, " ", data$time), format = "%d.%m.%Y %H.%M"), format = "%Y-%b-%d %H:%M")
data = data.frame(cbind(time = date_time, data[, 3:ncol(data)]))
names(data) = c("date/time", names(data)[2:3])
data = data[order(strptime(data$`date/time`, "%Y-%b-%d %H:%M"), decreasing = FALSE), ]
data_untouched = data

# Get first date and last date 
seqMon = function(x) seq(x[1], x[2], by = "month")
salary_months_length = length(seqMon((as.Date(paste0(format((function(x) c(x[1], x[length(x)]))(sort(strptime(data$`date/time`, "%Y-%b-%d %H:%M"))), "%Y-%m"), "-01")) - 1))) + 1

salary = 2245 * salary_months_length
write.table(x = data, file = outfilename, sep = ",", quote = FALSE, col.names = TRUE, row.names = FALSE)

# Aggregate by day 
library(xts)
data.xts = xts(data$amount, strptime(data$`date/time`, "%Y-%b-%d %H:%M"))
daysum = unlist(lapply(split(data$amount, format(index(data.xts), "%Y-%b-%d")), sum))
daysum_df = data.frame(date = names(daysum), amount = unlist(daysum));rownames(daysum_df) = 1:nrow(daysum_df)


my_list = NULL
my_list$salary = salary
total_expenses = sum(data$amount) ; my_list$total_expenses = total_expenses
amount_remaining = salary - total_expenses ;my_list$Savings = paste0("After ", salary_months_length, " months of salary, you saved only Eur ", amount_remaining)
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
salary = 2150 + 250
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

# Healthy living
health_file = "20200519_health_food_record.txt"
health_data = read.csv(health_file, stringsAsFactors = FALSE)

# Milk
milk = gsub(" ", "", health_data$Milk)
health_data$milk_count = ifelse(milk == "No", 0, 1)

# Sugar
sugar = gsub(" ", "", health_data$Sugared_drink)
health_data$sugar_count = ifelse(sugar == "No", 0, 1)

# Bread
bread = gsub(" ", "", health_data$Bread)
health_data$bread_count = ifelse(bread == "No", 0, 1)

# Jog
jog = gsub(" ", "", health_data$daily_jog)
health_data$jog_count = ifelse(jog == "No", 0, 1)

cat(paste0("\n\n\n\n\n\nNo of days = ", nrow(health_data), "   \nmilk intake = ", sum(health_data$milk_count), " days\n", "sugar intake = ", sum(health_data$sugar_count), " days\n", "bread intake = ", sum(health_data$bread_count), " days\n", "jog intake = ", sum(health_data$jog_count), " days\n"))



