library("tidyverse")
library("reshape2")
library("dplyr")
library("plotly")

load("./../Data/finaldata.RData")
common_theme = theme(
    plot.title = element_text(face = "bold", size = 20),
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 10, colour = "black")
)

## Net Worth V/S Rank with years
##  Start
gfg_data <- data.frame()
for (i in c(2011:2019, 2021)){
    gfg_data <- rbind(
        gfg_data,
        data.frame(
            Net.Worth = tables[[i]]$`Net Worth (in billion $US)`[1:500],
            Rank = 1:500,
            Year = rep(i, 500 - 1 + 1)
        )
    )
}

gfg_data$Year <- factor(gfg_data$Year)

gfg_plot <- ggplot(gfg_data) +
    geom_line(
        mapping = aes(x = Rank, y = Net.Worth, color = Year)
    ) +
    labs(
        title = "Net Worth v/s Rank with Time",
        y = "Net Worth"
    ) +
    common_theme

gfg_plot

## End

## Distribution of Age with Time Boxplot
## Start
gfg_data <- data.frame()
for (i in c(2011:2019, 2021)){
    gfg_data <- rbind(
        gfg_data,
        data.frame(
            Year = rep(i, 500),
            Age = tables[[i]]$Age
        )
    )
}

gfg_data$Year <- factor(gfg_data$Year)

gfg_plot <- ggplot(gfg_data) +
    geom_boxplot(
        mapping = aes(x = Year, y = Age, fill = Year),
        lwd = 1
    ) +
    labs(
        title = "Boxplot of Age v/s Year"
    ) +
    common_theme +
    theme(legend.position = "none")

gfg_plot
## End


## Distribution of Age with Time Histogram
## Start
gfg_plot <- ggplot(data.frame(tables[[2011]])) +
    geom_histogram(
        mapping = aes(x = Age),
        color = "white",
        binwidth = 5
    ) +
    labs(
        title = "Histogram of Age",
        y = "Number of Billionaires"
    ) +
    common_theme

gfg_plot
## End


## Wealth in Sources of Income
## Start
gfg_data <- data.frame(tables[[2011]]) %>%
    group_by(`Source.of.Wealth`) %>%
    summarise(total.net.worth = sum(`Net.Worth..in.billion..US.`)) %>%
    arrange(total.net.worth)

gfg_data$Source.of.Wealth <- factor(
    gfg_data$Source.of.Wealth,
    levels = as.character(gfg_data$Source.of.Wealth)
)

gfg_plot <- ggplot(gfg_data, aes(x = total.net.worth, y = Source.of.Wealth)) +
    geom_col(
        orientation = "y"
    ) +
    geom_text(
        mapping = aes(label = total.net.worth),
        hjust = -0.2,
    ) +
    labs(
        title = "Total Net Worth",
        x = "Total Net Worth",
        y = "Source of Wealth"
    ) +
    xlim(0, 75 + max(gfg_data$total.net.worth)) +
    common_theme

gfg_plot
## End


## Distribution of Net Worth with Source of Income Boxplot
## Start
gfg_plot <- ggplot(data.frame(tables[[2011]])) +
    geom_boxplot(
        mapping = aes(x = `Source.of.Wealth`, y = `Net.Worth..in.billion..US.`)
    ) +
    labs(
        title = "Boxplot of Net Worth v/s Source of Wealth",
        x = "Source of Wealth",
        y = "Net Worth in Billion US$"
    ) +
    ylim(0, 30) +
    common_theme +
    theme(
        legend.position = "none",
        axis.text.x = element_text(angle = 90)
    )

gfg_plot
## End


## Network, Age, and Source of Wealth
## Start
gfg_plot <- ggplot(data.frame(tables[[2011]])) +
    geom_point(
        mapping = aes(x = Age, y = Net.Worth..in.billion..US., color = Source.of.Wealth)
    ) +
    scale_color_discrete(name = "Source of Wealth") +
    ylim(0, 30) +
    labs(
        title = "Scatter Plot",
        x = "Age",
        y = "Net Worth (in billion $US)"
    ) +
    common_theme

gfg_plot
## End


## Change in number of billionaires for each country with time
## Start
gfg_data <- data.frame(Country = unique(tables[[2011]]$Citizenship))

for (i in c(2011:2019, 2021)){
    nation_count <- data.frame(table(tables[[i]]$Citizenship))
    colnames(nation_count)[colnames(nation_count) == "Freq"] <- as.character(i)
    gfg_data <- merge(
        x = gfg_data,
        y = nation_count,
        by.x = "Country",
        by.y = "Var1",
        all.x = TRUE
    )
}

gfg_data[is.na(gfg_data)] <- 0

gfg_data <- cbind(gfg_data["Country"], stack(gfg_data[as.character(c(2011:2019, 2021))])) %>%
    mutate(
        `Year` = `ind`,
        `Count` = `values`,
        .keep = "unused"
    ) %>%
    arrange(`Country`, `Year`)

gfg_data$Year <- as.integer(as.character(gfg_data$Year))

gfg_plot <- ggplot(data.frame(gfg_data)) +
    geom_line(
        mapping = aes(x = Year, y = Count, color = Country)
    ) +
    labs(
        title = "Count v/s Time for Different Countries",
        x = "Year",
        y = "Number of Billionaires"
    ) +
    common_theme

gfg_plot

## End


## Map using plotly



## Dataframe for map
gfg_data <- data.frame(country_codes)

nation_count <- data.frame(table(tables[[2011]]$Citizenship))

gfg_data <- merge(
    x = gfg_data,
    y = nation_count,
    by.x = "Country",
    by.y = "Var1",
    all.x = TRUE
)

gfg_data[is.na(gfg_data)] <- 0

gfg_data[gfg_data$Country %in% "Russia", "Freq"] <- sum(gfg_data[gfg_data$Code %in% "RUS", "Freq"])
gfg_data <- filter(gfg_data, Country != "Russian Federation")

fig <- plot_ly(
    data = gfg_data,
    type = "choropleth",
    locations = ~Code,
    z = ~Freq,
    text = ~Country,
    colorscale = "Reds"
    ) %>%
    colorbar(
        title = "Number of Billionaires"
    ) %>%
    layout(
        title = list(
            text = paste("Country Heat Map:", as.character(2011)),
            x = 0,
            font = list(color = "black")
        ),
        font = list(size = 15)
    )

fig

