

# ติดตั้งแพ็คเกจที่จำเป็น (รันครั้งเดียว)
install.packages("readr")
install.packages("dplyr")
install.packages("psych")
install.packages("knitr")

# โหลดไลบรารี
library(readr)
library(dplyr)
library(knitr)

# โหลดข้อมูล
data <- read_csv("bank1.csv")

# เลือกเฉพาะคอลัมน์ตัวเลข
numeric_data <- data %>% select_if(is.numeric)

# สร้าง Summary Statistic ด้วยฟังก์ชัน apply + quantile
summary_stats <- numeric_data %>%
  summarise(across(everything(), list(
    count = ~sum(!is.na(.)),
    mean = ~mean(., na.rm = TRUE),
    std = ~sd(., na.rm = TRUE),
    min = ~min(., na.rm = TRUE),
    `25%` = ~quantile(., 0.25, na.rm = TRUE),
    `50%` = ~quantile(., 0.50, na.rm = TRUE),
    `75%` = ~quantile(., 0.75, na.rm = TRUE),
    max = ~max(., na.rm = TRUE)
  ), .names = "{.col}_{.fn}"))

# แปลงให้อยู่ในรูปตาราง (transposed)
summary_stats_t <- as.data.frame(t(summary_stats))
colnames(summary_stats_t) <- "Value"
summary_stats_t$Feature <- rownames(summary_stats_t)
summary_stats_t <- summary_stats_t %>%
  tidyr::separate(Feature, into = c("Feature", "Metric"), sep = "_") %>%
  tidyr::pivot_wider(names_from = Metric, values_from = Value)

# แสดงตาราง
kable(summary_stats_t, digits = 2, caption = "Summary Statistics ของข้อมูล bank1.csv")



# ติดตั้งแพ็คเกจ (ถ้ายังไม่ได้ติดตั้ง)
install.packages("readr")
install.packages("dplyr")
install.packages("corrplot")  # สำหรับแสดงภาพ

# โหลดไลบรารี
library(readr)
library(dplyr)
library(corrplot)

# อ่านข้อมูล
data <- read_csv("bank1.csv")

# เลือกเฉพาะคอลัมน์เชิงตัวเลข (numeric)
numeric_data <- data %>% select_if(is.numeric)

# คำนวณค่าสหสัมพันธ์ (Correlation Matrix)
cor_matrix <- cor(numeric_data, use = "complete.obs", method = "pearson")

# แสดง Correlation Matrix ด้วย corrplot
corrplot(cor_matrix, method = "color", type = "upper",
         tl.col = "black", tl.srt = 20, addCoef.col = "black")

# แสดงตารางค่าสัมประสิทธิ์สหสัมพันธ์
print(round(cor_matrix, 2))

# คำนวณ Correlation matrix
cor_matrix <- cor(numeric_data, use = "complete.obs")

# เรียงความสัมพันธ์กับตัวแปร 'Exited'
cor_exited <- sort(cor_matrix[,"Exited"], decreasing = TRUE)

# แสดงผลลัพธ์แบบเรียง
print(round(cor_exited, 2))

str(data)        # แสดงประเภทของแต่ละคอลัมน์
glimpse(data)    # แบบย่อและอ่านง่าย (จาก dplyr)

# สรุปจำนวนค่าว่างทั้งหมดในแต่ละคอลัมน์
colSums(is.na(data))