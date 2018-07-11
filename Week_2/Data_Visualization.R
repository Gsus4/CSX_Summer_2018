library("ggplot2")
#中西部人口統計數據
midwest
#單變數：類別型
ggplot(data = midwest, aes(x = category)) + geom_bar(fill = "steelblue", colour = "black")
#單變數：連續型
ggplot(data = midwest, aes(x = popdensity)) + geom_histogram(binwidth = 500, fill = "steelblue")
#連續 vs 連續
ggplot(data = midwest, aes(x = popdensity, y=poppovertyknown)) + geom_point()
#雙變數：離散 vs 連續
ggplot(midwest, aes(x=category, y=poppovertyknown)) + geom_boxplot()
