library("tidyverse")
library("dplyr")
library("janitor")
library("lubridate")
library("readxl")
library("openxlsx")
library("pivottabler")

df <- read_excel("Type_sales.xlsx")
df <- rename_with(df,tolower)
df <- clean_names(df)
df$total=as.double(df$total/100000)
str(df)
df <- filter(df,type!="NA")
df$fraction <- df$total / sum(df$total)

# Compute the cumulative percentages (top of each rectangle)
df$ymax <- cumsum(df$fraction)

# Compute the bottom of each rectangle
df$ymin <- c(0, head(df$ymax, n=-1))

# Compute label position
df$labelPosition <- (df$ymax + df$ymin) / 2

# Compute a good label
df$label <- paste0(df$type)

# Make the plot
ggplot(df, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=type)) +
  geom_rect() +
  geom_text( x=2, aes(y=labelPosition, label=label, color=type), size=6) + # x here controls label position (inner / outer)
  scale_fill_brewer(palette="Dark2") +
  scale_color_brewer(palette="Dark2") +
  coord_polar(theta="y") +
  xlim(c(-1, 4)) +
  theme_void() +
  theme(legend.position = "none")


