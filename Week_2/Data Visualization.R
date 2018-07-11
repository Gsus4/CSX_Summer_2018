library("ggplot2")
midwest

ggplot(data = midwest, aes(x = category)) +
  geom_bar(fill = "steelblue", colour = "black")

ggplot(data = midwest, aes(x = popdensity)) +
  geom_histogram(binwidth = 500, fill = "steelblue")

ggplot(data = midwest, aes(x = category)) +
  geom_bar(fill = "steelblue", colour = "black")

ggplot(data = midwest, aes(x = popdensity)) +
  geom_histogram(binwidth = 500, fill = "steelblue")

ggplot(data = midwest, aes(x = popdensity, y=poppovertyknown)) +
  geom_point()

ggplot(midwest, aes(x=category, y=poppovertyknown)) +
  geom_boxplot()
