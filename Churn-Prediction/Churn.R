#---- LIBRARIES
library(tidyverse)
library(DataExplorer)
library(h2o)
library(skimr)
library(ggplot2)
library(plotly)
library(scales)
library(tidyquant)
library(highcharter)
#---- 
df <- read.csv("Data/BankChurners.csv")

# Dropping last 2 columns
df <- df %>% select(-Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_1,
                    -Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_2,
                    -CLIENTNUM)


# Introductory plot
df %>% plot_intro()

# Plotting numerical values
df %>% plot_histogram()

# Plotting factors
df %>% plot_bar()

# Plot missing values
df %>% plot_missing()
# no missing values


# Skimming
skim(df)
# Skim through numerical columns
skim(df %>% select_if(is.numeric))
# Skim through strings
skim(df %>% select_if(is.character))

# Recode Target Variable
df <- df %>% 
  mutate(Attrition_Flag = recode(Attrition_Flag, "Attrited Customer" = 1,
                                 "Existing Customer" = 0))

# Columns to be turned into factors
cols <- c("Dependent_count", "Total_Relationship_Count", 
          "Contacts_Count_12_mon", "Months_Inactive_12_mon",
          "Total_Relationship_Count", "Attrition_Flag",
          "Education_Level", "Marital_Status", 
          "Income_Category", "Card_Category",
          "Gender")
# To Factor
df <- df %>% mutate(across(cols,factor))

str(df)

# Releveling factors
df$Education_Level <- fct_relevel(df$Education_Level, "Unknown", "Uneducated", "High School", "College", "Graduate", "Post-Graduate", "Doctorate")
df$Marital_Status <- fct_relevel(df$Marital_Status, "Unknown", "Single", "Married", "Divorced")
df$Income_Category <- fct_relevel(df$Income_Category, "Unknown", "Less than $40K", "$40K - $60K", "$60K - $80K", "$80K - $120K", "$120K +")
df$Card_Category <- fct_relevel(df$Card_Category, "Blue", "Silver", "Gold", "Platinum")
df$Attrition_Flag <- fct_relevel(df$Attrition_Flag, "0", "1")

# Checking number of occurences of each value
df %>% select(Education_Level) %>% group_by(Education_Level) %>% count()
df %>% select(Marital_Status) %>% group_by(Marital_Status) %>% count()
df %>% select(Income_Category) %>% group_by(Income_Category) %>% count()


# VISUALIZATION ----

# Doughnut Chart for Attrition


data <- df %>% 
  select(Attrition_Flag) %>%
  group_by(Attrition_Flag) %>%
  count() %>% 
  rename(count = n)

# Compute percentages
data$fraction = data$count / sum(data$count)

# Compute the cumulative percentages (top of each rectangle)
data$ymax = cumsum(data$fraction)

# Compute the bottom of each rectangle
data$ymin = c(0, head(data$ymax, n=-1))

# Make the plot
ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Attrition_Flag)) +
  geom_rect() +
  coord_polar(theta="y") + 
  xlim(c(2, 4)) + theme_tq() 

# Attrition Pie Chart
ggplot(df, aes(x = "", fill = Attrition_Flag)) + 
  geom_bar(width = 1) +
  theme(axis.line = element_blank(), 
        plot.title = element_text(hjust=0.5)) + 
  labs(fill="Attrition", 
       x=NULL, 
       y=NULL, 
       title="Pie Chart of Attrition") + 
  coord_polar(theta = "y", start=0) + 
  theme_tq()

# Grouped Bar Chart for Gender

ggplotly(ggplot(df, aes(fill=Attrition_Flag, x=Gender)) +
  geom_bar(position="dodge", stat="count") + theme_tq())


# Age Distribution

ggplotly(ggplot(aes(x=Customer_Age), data=df) + 
           geom_histogram(fill='#0099FF', stat="count") + 
           ggtitle('Customer Age Distribution') + theme_tq())


# Relationship between Total Relationship Count and Attrition?

ggplotly(df %>% 
  select(Total_Relationship_Count,Attrition_Flag) %>%
  group_by(Total_Relationship_Count,Attrition_Flag) %>% 
    count(Total_Relationship_Count, Attrition_Flag) %>%
  ggplot(aes(Total_Relationship_Count)) + 
    geom_bar(aes(y=n, fill= Attrition_Flag), stat = "identity") + 
    theme_tq())

# Same as above, with facets


ggplotly(ggplot(df, aes(x=Attrition_Flag, fill=Attrition_Flag)) +
           geom_bar(position="dodge", stat="count") +
           facet_wrap(~Total_Relationship_Count, scales="free") +
           theme_tq())

# Churn rate percentage per gender?

ggplotly(df %>% 
           select(Gender, Attrition_Flag) %>%
           group_by(Gender) %>% 
           mutate(Attrition_Flag = as.numeric(as.character(Attrition_Flag))) %>%
           summarise(churn_percent = mean(Attrition_Flag)) %>%
           ggplot(aes(x=Gender, y=churn_percent)) +
           geom_bar(stat="identity") + 
           theme_tq())


# Relationship between Months on book and Churn percentage?

ggplotly(df %>% 
  select(Months_on_book, Attrition_Flag) %>%
  group_by(Months_on_book) %>% 
  mutate(Attrition_Flag = as.numeric(as.character(Attrition_Flag))) %>%
  summarise(churn_percent = mean(Attrition_Flag)) %>%
  ggplot(aes(x=Months_on_book, y=churn_percent)) +
  geom_point() +
  geom_smooth(se=F) +
  theme_tq())

# No obvious relationship. Try with years instead of months?                                                                              

df %>% select(Months_on_book) %>% group_by(Months_on_book) %>% count() %>% view()

# Attrition per card type?

ggplotly(ggplot(df, aes(x=Attrition_Flag, fill=Attrition_Flag)) +
           geom_bar(position="dodge", stat="count") +
           facet_wrap(~Card_Category, scales="free") +
           theme_tq())


# H2O ----

# Initialize H2O JVM
h2o.init()
# Split data into Train/Validation/Test Sets
df_h2o <- as.h2o(df)

split_h2o <- h2o.splitFrame(df_h2o, c(0.7, 0.15), seed = 12 )

train_h2o <- h2o.assign(split_h2o[[1]], "train" ) # 70%
valid_h2o <- h2o.assign(split_h2o[[2]], "valid" ) # 15%
test_h2o  <- h2o.assign(split_h2o[[3]], "test" )  # 15%

# Set names for h2o
y <- "Attrition_Flag"
x <- setdiff(names(train_h2o), y)

# Run the automated machine learning 
automl_models_h2o <- h2o.automl(
  x = x, 
  y = y,
  training_frame    = train_h2o,
  leaderboard_frame = valid_h2o,
  max_runtime_secs  = 600
)

leaderboard <- h2o.get_leaderboard(automl_models_h2o)
leaderboard


model_ids <- as.vector(automl_models_h2o@leaderboard$model_id)
model <- h2o.getModel(model_ids[1])

pred <- h2o.predict(model, newdata= test_h2o)

h2o.confusionMatrix(model,test_h2o)

#          0   1    Error      Rate
# 0      1259  18 0.014096  =18/1277
# 1        18 219 0.075949   =18/237
# Totals 1277 237 0.023778  =36/1514


# Variable importance of the best model

h2o.varimp(model) %>%
  top_n(20) %>% 
  hchart('bar', hcaes(x = variable, y = scaled_importance)) %>%  
  hc_xAxis(title = list(text = ""))%>% 
  hc_yAxis(title = list(text = "Scaled Importance"), max=1) %>% 
  hc_title(text = "Variable Importance") %>%
  hc_add_theme(hc_theme_elementary())



