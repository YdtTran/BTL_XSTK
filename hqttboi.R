library(caret)
# cố định cách chọn các dòng ngẫu nhiên
set.seed(111)
# chọn dữ liệu cho train với biến mục tiêu là 'Memory_Bandwidth'
train_index <- createDataPartition(main_df$Memory_Bandwidth, p = 0.8, 
                                   list = FALSE)
# tạo training_set bằng các dòng được chọn
training_set <- main_df[train_index, ]
# tạo testing_set bằng các dòng còn lại
testing_set <- main_df[-train_index, ]
# tạo mô hình hồi quy tuyến tính
model<-lm(Memory_Bandwidth ~ Process + Memory + Memory_Speed + 
            Memory_Bus + L2_Cache, training_set)
summary((model))