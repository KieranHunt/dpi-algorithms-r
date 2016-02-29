library('stats')
library('reshape2')
library('ggplot2')

print("Frequency plot of rule length")

source("read-json-data.R")
source("fte-theme.R")

rules <- c("time","person","year", "way", "day", "thing", "man", "world", "life", "hand", "part", "child", "eye", "woman", "place", "work", "week", "case", "point", "government", "google", "facebook", "youtube", "baidu", "yahoo", "amazon", "wikipedia", "qq", "twitter", "taobao", "live", "sina", "linkedin", "weibo", "ebay", "yandex", "hao123", "vk", "bing", "msn");

max_length <- 0

lengths <- c()

for (i in 1:length(rules)) {
    rule <- rules[i]
    rule_length <- nchar(rule)

    lengths <- c(lengths, rule_length)
}

data_frame <- data.frame(lengths);
names(data_frame) <- c("length")

data_frame

plot <- ggplot(data_frame)
plot <- plot + geom_density(aes(x = data_frame[["length"]], fill = "red", color = "red", alpha = 0.5))
plot <- plot + fte_theme()
plot <- plot + theme(legend.position="none")
plot <- plot + labs(x = "Rule length", y = "Frequency", title = "Rule length frequency")

ggsave("graphs/rule_length_frequency.png", dpi = 1200, width = 8, height = 6, type = "cairo")
