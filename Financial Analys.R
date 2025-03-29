#Data
#daramad
revenue <- c(14574.49, 7606.46, 8611.41, 9175.41, 8058.65, 8105.44, 11496.28, 9766.09, 10305.32, 14379.96, 10713.97, 15433.50)
#hazine
expenses <- c(12051.82, 5695.07, 12319.20, 12089.72, 8658.57, 840.20, 3285.73, 5821.12, 6976.93, 16618.61, 10054.37, 3803.96)

#Calculate Profit(sood) As The Differences Between Revenue And Expenses
profit <- revenue - expenses
profit

#Calculate Tax As 30% Of Profit And Round To 2 Decimal Points
tax <- round(0.30 * profit, digits = 2)
tax 

#Calculate Profit Remaining After Tax Is Deducted
profit.after.tax <- profit - tax
profit.after.tax

#Calculate The Profit Margin(hashie sood) As Profit After Tax Over Revenue
#Round To 2 Decimal Points, Then Multiply By 100 To Get %
profit.margin <- round(profit.after.tax / revenue, 2) * 100
profit.margin

#Calculate The Mean Profit After Tax For The 12 Months (sood khales saliane)
mean_pat <- mean(profit.after.tax)
mean_pat

# Names of Months
months <- c("January", "February", "March", "April", "May", "June",
            "July", "August", "September", "October", "November", "December")


#Find The Months With Above-Mean Profit After Tax
good.months <- profit.after.tax > mean_pat

# Get Month Names
good.month.names <- months[good.months]

# Print
good.month.names

#Bad Months Are The Opposite Of Good Months !
bad.months <- !good.months
bad.months.names <- months[bad.months]
# Print
bad.months.names


#The Best Month Is Where Profit After Tax Was Equal To The Maximum
best.month <- profit.after.tax == max(profit.after.tax)
best.month.names <- months[best.month]
# Print
best.month.names

#The Worst Month Is Where Profit After Tax Was Equal To The Minimum
worst.month <- profit.after.tax == min(profit.after.tax)
worst.month.names <- months[worst.month]
# Print
worst.month.names

#Convert All Calculations To Units Of One Thousand Dollars 
revenue.1000 <- round(revenue / 1000, 0)
expenses.1000 <- round(expenses / 1000, 0)
profit.1000 <- round(profit / 1000, 0)
profit.after.tax.1000 <- round(profit.after.tax / 1000, 0)

#Print Results
revenue.1000
expenses.1000
profit.1000
profit.after.tax.1000
profit.margin
good.months
bad.months
best.month
worst.month

#visualising


#Bar.Plot
df <- data.frame(
  Month = factor(months, levels = months),
  ProfitAfterTax = profit.after.tax,
  GoodMonth = good.months
)

ggplot(df, aes(x = Month, y = ProfitAfterTax, fill = GoodMonth)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("red", "green"), labels = c("Bad", "Good")) +
  labs(title = "Profit After Tax per Month", y = "Profit After Tax ($)", fill = "Month Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Line.Plot
df_line <- data.frame(
  Month = factor(months, levels = months),
  Revenue = revenue,
  Expenses = expenses,
  ProfitAfterTax = profit.after.tax
)


#Bar.Plot
df_margin <- data.frame(
  Month = factor(months, levels = months),
  ProfitMargin = profit.margin
)

ggplot(df_margin, aes(x = Month, y = ProfitMargin)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_hline(yintercept = mean(df_margin$ProfitMargin), linetype = "dashed", color = "red") +
  labs(title = "Profit Margin (%) per Month", y = "Profit Margin (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))