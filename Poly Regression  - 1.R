Input = ("
Length  Clutch
284      3  
290      2  
290      7
290      7  
298     11  
299     12
302     10  
306      8  
306      8
309      9  
310     10  
311     13
317      7  
317      9  
320      6
323     13
334      2  
334      8
")
Data = read.table(textConnection(Input), header = TRUE)
Data$Length = as.numeric(Data$Length)

y_linear = lm(Clutch~Length, data = Data)
y_linear
summary(y_linear)
length2 = Data$Length * Data$Length
length2
y2_linear = lm(Clutch~Length + length2, data = Data)
summary(y2_linear)

length3 = length2 * Data$Length
y3_linear = lm(Clutch~Length + length2+ length3, data = Data)
summary(y3_linear)
summary(y3_linear)$r.squared

y4_linear = lm(Clutch~Length + length2+ length3 +Data$Length^4, data = Data)
summary(y4_linear)

y5_linear = lm(Clutch~Data$Length + length2+ length3 +Data$Length^4 + Data$Length^5, data = Data)
summary(y5_linear)
len2 = Data$Length * Data$Length
len2
len2_1 = Data$Length^2
len2_1

y2_linear_1 = lm(Clutch~ Data$Length + length2+ length3 +Data$Length^4 + Data$Length^5, data = Data)



