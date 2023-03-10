#----------------- Run this for the first time install R ----------------------
#mypackages <- c( #'readxl','xlsx',
#  'data.table','reshape',
#  'chron','lubridate','purrr','dplyr', 
#  'cluster','factoextra','tidyr','philentropy', 
#  'stringr', 'ggplot2', 'patchwork')
#install.packages(mypackages)
#------------------------------------------------------------------------------

# 0) Install and read the data and ETL

library(data.table)
library(dplyr)
library(lubridate)
library(ggplot2)
library(patchwork)

# Sample dataframe with date and sales value columns
df <- fread('Tool_Output.csv' )
df <- na.omit(df)
# Convert date column to datetime format
df$Date <- as.Date(df$Date, format = "%m/%d/%Y %H:%M:%S")

df <- df %>% group_by(Date, WithinRange, Group) %>% summarise(SuccessMetric = sum(SuccessMetric)) %>% arrange(Date)

# Split the dataframe to two group of control and test
df$test_group <- ifelse(df$Group == 'Test', 1, 0)
df1 <- df[df$test_group == 1,]
df2 <- df[df$test_group == 0,]

# Define the start date for the test
for_loop <- df1[df1$WithinRange == 'Post', 'Date']
start_date <- min(for_loop$Date)
df1$pre_post <- ifelse(df1$Date >= start_date, "Pre", "Post")
df2$pre_post <- ifelse(df2$Date >= start_date, "Pre", "Post")

# Split the Test data into two groups based on the test start date
test_pre <- df1[df1$Date < start_date,]
test_pos <- df1[df1$Date >= start_date,]

# Split the control data into two groups based on the test start date
cont_pre <- df2[df2$Date < start_date,]
cont_pos <- df2[df2$Date >= start_date,]


# 1) Draw graphs of both test and control sets. add the date of test as a v-line
# ---------------------- PLOT ----------------
test_message <- ''
control_message <- ''

if (nrow(test_pre) == 0) test_message <- '*Test Groups Pre-Test is empty'
if (nrow(test_pos) == 0) test_message <- '*Test Groups Post-Test is empty'
if (nrow(cont_pre) == 0) control_message <- '*Control Groups Pre-Test is empty'
if (nrow(cont_pos) == 0) control_message <- '*Control Groups Post-Test is empty'

jpeg(filename = "Plot_of_Input.jpg", pointsize =24, width=20, height=20, units="cm", res=300, quality=600, bg = "white", restoreConsole = TRUE)
# Line plot with multiple groups
p1 <- ggplot(data=df1, aes(x=Date, y=SuccessMetric, group=pre_post), size=1) +
  geom_line(aes(color=pre_post))+
#  geom_point(aes(color=pre_post)) +
  scale_color_manual(name = "", 
                     values = c("Pre" = "blue", "Post" = "red")) +
  labs(title = "Test", subtitle = test_message) +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_vline(xintercept = as.numeric(start_date), linetype = "dashed", color = "red")

p2 <- ggplot(data=df2, aes(x=Date, y=SuccessMetric, group=pre_post), size=1) +
  geom_line(aes(color=pre_post))+
#  geom_point(aes(color=pre_post)) +
  scale_color_manual(name = "", 
                     values = c("Pre" = "blue", "Post" = "red")) +
  labs(title = "Control", subtitle = control_message) +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_vline(xintercept = as.numeric(start_date), linetype = "dashed", color = "red")

combined_plot <- p1 + p2 + plot_layout(nrow = 2) 
combined_plot
dev.off()

# perform difference-in-differences analysis
test_pre_mean <- mean(test_pre$SuccessMetric)
test_pos_mean <- mean(test_pos$SuccessMetric)
cont_pre_mean <- mean(cont_pre$SuccessMetric)
cont_pos_mean <- mean(cont_pos$SuccessMetric)

# numeric change on mean after test
did <- (test_pos_mean - test_pre_mean) - (cont_pos_mean - cont_pre_mean)
did

# Change of rate after test
did_rate <- did / test_pre_mean
did_rate

error_message <- 'No Error Message'

if(is.nan(did)){
  if (nrow(test_pre) == 0) error_message <- 'Test Groups Pre-Test is empty'
  if (nrow(test_pos) == 0) error_message <- 'Test Groups Post-Test is empty'
  if (nrow(cont_pre) == 0) error_message <- 'Control Groups Pre-Test is empty'
  if (nrow(cont_pos) == 0) error_message <- 'Control Groups Post-Test is empty'
}

cat(sprintf("Numeric change = %.4f,\nChange Rate Post test = %.2f%%\n", did, did_rate * 100))

# Perform two-sample t-test
test_pre_mean <- mean(test_pre$SuccessMetric)
test_pos_mean <- mean(test_pos$SuccessMetric)

t_test_result <- t.test(test_pre$SuccessMetric, test_pos$SuccessMetric)
p_value_origin <- t_test_result$p.value

if (p_value_origin <= 0.05){
  p_value_meaning_text <- 'Test is Statisticaly Significant'
} else {
  p_value_meaning_text <- 'Test is Not Statisticaly Significant'
}


print(paste0("Original t-test result P value = ", format(p_value_origin, digits=4)))
p_output <- print(p_value_origin, digits=4)

output <- data.frame(p_value = c(p_value_origin),
                     p_value_meaning = c(p_value_meaning_text),
                     Lift_Perc_over_Control_Group = c(did_rate),
                     Error = c(error_message)
)
print(output)

write.csv(output, "statistical_significance_output.csv", row.names=FALSE)
