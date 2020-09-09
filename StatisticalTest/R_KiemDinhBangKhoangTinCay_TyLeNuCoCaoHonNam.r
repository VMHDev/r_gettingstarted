#Giả sử ta muốn khảo sát tỉ lệ giới tính trong một tổng thể (population) là tất cả cư dân của một thành phố. 
#Từ tổng thể, ta thu thập được một mẫu dữ liệu ngẫu nhiên (random sample) gồm 100 người trong đó có 60 nữ và 40 nam. 
#Dựa vào mẫu dữ liệu này, ta kiểm định nghi vấn “tỉ lệ nữ của thành phố là cao hơn nam” với mức ý nghĩa (significance level) 5%.
################################################################################################################################
# Gọi P là tỉ lệ nữ trong dân cư của thành phố, p_hat là tỉ lệ nữ trong mẫu dữ liệu.
# Ta không biết tham số (parameter) p là bao nhiêu, nhưng một cách “hợp lý”, ta có thể dùng thống kê (statistic) p_hat để ước lượng p. Tức là, ta đoán rằng
# p = p_hat
# Như vậy, vì p = p_hat > 0.5, nên ta có thể nhận định là “tỉ lệ nữ của thành phố cao hơn nam”.
# Tuy nhiên, ta không “tin cậy” lắm vào nhận định này vì tùy theo mẫu dữ liệu thu thập mà ta có kết quả ước lượng khác nhau. 
# Để tính đến sự biến động của p_hat theo mẫu dữ liệu cỡ n = 100 có thể thu thập từ tổng thể, ta thực hiện kiểm định giả thuyết:
# H0: p = p_hat = 0.5
# H1: p > 0.5
# với mức ý nghĩa anpha = 0.05.
################################################################################################################################
#Giữ giá trị cố định khi giả lập dữ liệu
set.seed(700); 

#Mức ý nghĩa 5%
anpha <- 0.05; 

#Giả lập dữ liêu với 60 giá trị "1" và 40 giá trị "0" (Giả lập số lượng)
n <- 100
sample1 <- c(rep(1, 60), rep(0, 40))

#Tính trung bình
(p_hat <- mean(sample1))

###################################Khoảnng Tin Cậy###############################################
#...................................Bootstrap................................................
#Tạo phân phối bootsrap rồi tính độ lệch chuuẩn vẽ biểu đồ hist
#replace=TRUE: Lấy có hoàn lại
boot_dist <- replicate(10000, mean(sample(sample1, n, replace=TRUE)))
hist(boot_dist, breaks = 40); sd(boot_dist)

#Xây dựng khoảng tin cậy 95%
(confint <- quantile(boot_dist, c(anpha, 1), names=FALSE))

#....................................CongThuc................................................
#Tính Sai số chuẩn
se <- sqrt(p_hat*(1 - p_hat)/n); se

#Tính phân phối chuẩn
z <- qnorm(1 - anpha)

#Xác định khoảng tin cậy 95%
(confint <- c(p_hat - z*se, 1))

#####################################Kiểm Định###############################################
#.....................................Bằng Khỏang Tin Cậy..........................................
# Code này viết lại code trên
n <- 100; p0 <- 0.5; p_hat <- 0.6; anpha <- 0.05
sample1 <- c(rep(1, 60), rep(0, 40))
boot_dist <- replicate(10000, mean(sample(sample1, n, replace=TRUE)))
(confint <- quantile(boot_dist, c(anpha, 1), names = FALSE))
!(confint[1] <= p0 && p0 <= confint[2])
# Dựa trên khoảng tin cậy ở trên là [0.52, 0.81]
# Mà p0 = 0.5
# => Nằm ngoài khoảng tin cậy 
# => Bác bỏ H0 chấp nhận H1 => Tỉ lệ nữ của lớn hơn nam.
#.......................................Bằng công thức ...........................................
#Kích thước mãu
n <- 100

# P theo H0
p0 <- 0.5

# p thep mẫu
p_hat <- 0.6 

# Mức ý nghĩa 5%
anpha <- 0.05

# Tính sai số chuẩn theo công thức p
(se <- sqrt(p0*(1 - p0)/n))

#Tính phân phối mẫu
(z <- (p_hat - p0)/se)

#Tính p_value
(p_value <- 1 - pnorm(z))

#Tình giá trị tới hạn z(1-anpha)
(crit_val <- qnorm(1 - anpha))

# Kiểm tra p_value
# p_value < anpha => Bác bỏ H0 chấp nhận H1
p_value < anpha;  #(1)

# Kiểm tra giá trị tới hạn z(1-anpha)
# z(1-anpha) < z => Bác bỏ H0 chấp nhận H1
crit_val < z      #(2)

#(1) ra TRUE => Bác bỏ H0 chấp nhận H1 => Tỉ lệ nữ của lớn hơn nam.

#(2) ra TRUE => Bác bỏ H0 chấp nhận H1 => Tỉ lệ nữ của lớn hơn nam.

#..........................................Bằng Hàm .............................................
prop.test(60, 100, p = 0.5, conf.level = 1 - anpha, alternative = "greater")
#=> p-value = 0.02872
#=> Khoảng tin cậy 95%: [0.51, 1]

#....................................Bằng Randomization code 1 .......................................
#Giả lập dữ liệu với 50 giá trị "1" và 50 giá trị "0" (Giả lập số lượng)
sample0 <- c(rep(1, 50), rep(0, 50)); (p0 <- mean(sample0)) [1]

#Tạo phân phối Randomization rồi tính độ lệch chuuẩn vẽ biểu đồ hist
#replace=TRUE: Lấy có hoàn lại
rand_dist <- replicate(10000, mean(sample(sample0, n, replace=TRUE)))
hist(rand_dist, breaks = 40); sd(rand_dist)

#Tính p-value
(pvalue <- sum(rand_dist >= p_hat)/length(rand_dist))

#....................................Bằng Randomization code 2...............................................
n <- 100
p0 <- 0.5
p_hat <- 0.6
anpha <- 0.05
nullsample <- c(rep(1, 50), rep(0, 50))     #mẫu dữ liệu tương ứng với H0
stat <- function(data)                      #thống kê cần tính
{
  return(sum(data)/length(data))            #tỉ lệ mẫu
}

# Hàm randomization
randomization <- function(B)
{
  return(replicate(B, stat(sample(nullsample, n, replace = TRUE))))
}

# Tạo phân phối randomization
rand_dist <- randomization(10000)

# Tính p_value
(p_value <- sum(rand_dist >= p_hat)/length(rand_dist))

# Tính giá trị tới hạn z(1-anpha)
(crit_val <- quantile(rand_dist, 1 - anpha))

# Kiểm tra p_value
# p_value < anpha => Bác bỏ H0 chấp nhận H1
p_value < anpha; 

# Kiểm tra giá trị tới hạn z(1-anpha)
# z(1-anpha) < z => Bác bỏ H0 chấp nhận H1
crit_val < p_hat
