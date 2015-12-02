cat("write temporary data frames into csv files\n")
write.csv(train.num.select, "trian_num_select.csv", row.names = F)
write.csv(test.num.select, "test_num_select.csv", row.names = F)
write.csv(train.weeks, "train_weeks.csv", row.names = F)
write.csv(test.weeks, "test_weeks.csv", row.names = F)
write.csv(train.char, "train_char.csv", row.names = F)
write.csv(test.char, "test_char.csv", row.names = F)


cat("reading data, including train and test\n")
path <- "/Users/fujun/Desktop/DataMining/"
train.num.select <- read_csv(paste0(path, "train_num_select.csv", collapse = ""))
test.num.select <- read_csv(paste0(path, "test_num_select.csv", collapse = ""))
train.weeks <- read_csv(paste0(path, "train_weeks.csv", collapse=""))
test.weeks <- read_csv(paste0(path, "test_weeks.csv", collapse=""))
train.char <- read_csv(paste0(path, "train_char.csv", collapse=""))
test.char <- read_csv(paste0(path, "test_char.csv", collapse=""))
y <- read.csv("y.csv")