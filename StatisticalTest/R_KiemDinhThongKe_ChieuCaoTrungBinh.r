#Giả sử ta muốn khảo sát chiều cao trung bình của một tổng thể là tất cả cư dân của một thành
#phố. Từ tổng thể, ta thu thập được một mẫu dữ liệu ngẫu nhiên gồm chiều cao của 40 người như
#bảng dưới. Dựa vào mẫu dữ liệu này, ta kiểm định nghi vấn “chiều cao trung bình của cư dân
#thành phố là 1.60 mét” với mức ý nghĩa 5%.
#Bảng dữ liệu chiều cao 40 người (đơn vị mét)
#1.56, 1.47, 1.59, 1.65, 1.62, 1.78, 1.69, 1.49, 1.92, 1.55
#1.65, 1.52, 1.65, 1.60, 1.71, 1.48, 1.69, 1.65, 1.59, 1.74
#1.70, 1.61, 1.58, 1.65, 1.75, 1.65, 1.46, 1.53, 1.59, 1.62
#1.60, 1.55, 1.57, 1.46, 1.57, 1.63, 1.46, 1.68, 1.53, 1.48
################################################################################################################################
#Gọi mu là chiều cao trung bình của cư dân thành phố, x_bar là chiều cao trung bình của n = 40 người trong mẫu dữ liệu,
#Ta không biết tham số mu là bao nhiêu, nhưng một cách “hợp lý”, ta có thể dùng thống kê x_bar để ước lượng mu. Tức là, ta đoán rằng
#Như vậy, vì mu = x_bar != 1.60, nên ta có thể nhận định là “chiều cao trung bình của cư dân thành phố không là 1.60 mét”. 
#Tuy nhiên, ta không “tin cậy” lắm vào nhận định này vì tùy theo mẫu dữ liệu thu thập mà ta có kết quả ước lượng khác nhau. 
#Để tính đến sự biến động của x_bar theo mẫu dữ liệu cỡ n = 40 có thể thu thập từ tổng thể, ta thực hiện kiểm định giả thuyết:
#  H0: mu = mu0 = 1.6
#  H1: mu != 1.6
#với mức ý nghĩa anpha = 0.05.
################################################################################################################################
sample1 <- c(1.56, 1.47, 1.59, 1.65, 1.62, 1.78, 1.69, 1.49, 1.92, 1.55,
             1.65, 1.52, 1.65, 1.60, 1.71, 1.48, 1.69, 1.65, 1.59, 1.74,
             1.70, 1.61, 1.58, 1.65, 1.75, 1.65, 1.46, 1.53, 1.59, 1.62,
             1.60, 1.55, 1.57, 1.46, 1.57, 1.63, 1.46, 1.68, 1.53, 1.48)
n <- length(sample1)
mu0 <- 1.6
anpha <- 0.05

#.....................................Bằng Khỏang Tin Cậy..........................................
boot_dist <- replicate(10000, mean(sample(sample1, n, replace = TRUE)))
(confint <- quantile(boot_dist, c(anpha/2, 1-anpha/2), names = FALSE))

!(confint[1] <= mu0 && mu0 <= confint[2])

#.......................................Bằng công thức ...........................................
#Tính trung bình mẫu
(x_bar <- mean(sample1))                      #1.60675

#Tính sai số chuẩn
(se <- sd(sample1)/sqrt(n))                   #0.01557606

#Tính phân phối student t
(t <- (x_bar - mu0)/se)                       #0.4333572

#Tính p_value
(p_value <- 2*(1 - pt(t, df = n - 1)))        #0.6671428

#Tình giá trị tới hạn z(1-anpha)
(crit_val <- qt(1 - anpha/2, df = n - 1))     #2.022691

# Kiểm tra p_value
# p_value < anpha => Bác bỏ H0 chấp nhận H1
p_value < anpha;                              #0.6671428 < 0.5

# Kiểm tra giá trị tới hạn z(1-anpha)
# z(1-anpha) < z => Bác bỏ H0 chấp nhận H1
crit_val < t                                  #2.022691 < 0.4333572

#(1) ra FALSE => Không bác bỏ H0 => Không chấp nhận H1 => Như vậy, với mức ý nghĩa 5%, chiều cao trung bình của cư dân thành phố là 1.60 mét.

#(2) ra FALSE => Không bác bỏ H0 => Không chấp nhận H1 => Như vậy, với mức ý nghĩa 5%, chiều cao trung bình của cư dân thành phố là 1.60 mét.

#..........................................Bằng Hàm .............................................
t.test(sample1, mu = mu0, conf.level = 1 - anpha)
#=> p-value = 0.6671
#=> Khoảng tin cậy 95%: [0.51, 1]

#....................................Bằng Randomization...............................................
nullsample <- sample1 - (x_bar - mu0) #mẫu dữ liệu tương ứng với H0
mean(nullsample)
1.6
stat <- function(data) #thống kê cần tính
{
  return(mean(data)) #trung bình mẫu
}

# Hàm randomization
randomization <- function(B)
{
  return(replicate(B, stat(sample(nullsample, n, replace = TRUE))))
}

# Tạo phân phối randomization
rand_dist <- randomization(10000)

# Tính p_value
(p_value <- sum(abs(rand_dist-mu0) >= abs(x_bar-mu0))/length(rand_dist))

# Tính giá trị tới hạn z(1-anpha)
(crit_val <- quantile(rand_dist, 1 - anpha/2))

# Kiểm tra p_value
# p_value < anpha => Bác bỏ H0 chấp nhận H1
p_value < anpha; 

# Kiểm tra giá trị tới hạn z(1-anpha)
# z(1-anpha) < z => Bác bỏ H0 chấp nhận H1
abs(crit_val - mu0) < abs(x_bar - mu0)
