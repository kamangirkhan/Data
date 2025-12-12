---
title: "ABS Transactions Analysis — Fully Annotated"
author: "Arash Nateghian"
output: 
  html_document: 
    keep_md: true
---

## 1) Load Libraries & Dataset


``` r
# Core data and wrangling
library(dplyr)       # verbs: filter, select, group_by, summarise, mutate
```

```
## Warning: package 'dplyr' was built under R version 4.5.1
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

``` r
library(tidyverse)   # brings ggplot2, readr, tidyr, etc.
```

```
## Warning: package 'tidyverse' was built under R version 4.5.1
```

```
## Warning: package 'ggplot2' was built under R version 4.5.1
```

```
## Warning: package 'tibble' was built under R version 4.5.1
```

```
## Warning: package 'tidyr' was built under R version 4.5.1
```

```
## Warning: package 'readr' was built under R version 4.5.1
```

```
## Warning: package 'purrr' was built under R version 4.5.1
```

```
## Warning: package 'stringr' was built under R version 4.5.1
```

```
## Warning: package 'forcats' was built under R version 4.5.1
```

```
## Warning: package 'lubridate' was built under R version 4.5.1
```

```
## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
## ✔ forcats   1.0.0     ✔ readr     2.1.5
## ✔ ggplot2   3.5.2     ✔ stringr   1.5.1
## ✔ lubridate 1.9.4     ✔ tibble    3.3.0
## ✔ purrr     1.1.0     ✔ tidyr     1.3.1
```

```
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## ✖ dplyr::filter() masks stats::filter()
## ✖ dplyr::lag()    masks stats::lag()
## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
```

``` r
library(stringr)     # robust string/regex helpers
library(lubridate)   # date parsing, wday/month/year helpers
library(ggplot2)     # plots
library(purrr)       # functional map/reduce style helpers
library(arules)      # market basket analysis (Apriori)
```

```
## Warning: package 'arules' was built under R version 4.5.1
```

```
## Loading required package: Matrix
## 
## Attaching package: 'Matrix'
## 
## The following objects are masked from 'package:tidyr':
## 
##     expand, pack, unpack
## 
## 
## Attaching package: 'arules'
## 
## The following object is masked from 'package:dplyr':
## 
##     recode
## 
## The following objects are masked from 'package:base':
## 
##     abbreviate, write
```


``` r
transactions <- read.csv(
  "TransactionData.csv",
  header = TRUE,
  sep = ",",
  encoding = "UTF-8",
  quote = "\""
)
```


## 2) Clean and normalize text columns


``` r
transactions <- transactions |>
  mutate(
    CLASSIFICATIONDEPARTMENT = trimws(CLASSIFICATIONDEPARTMENT),
    STORENAME = trimws(STORENAME),
    STORENAME = ifelse(STORENAME == "White Oak", "White Oak Town Center", STORENAME),
    SIZE = ifelse(SIZE == "5LTR", "5L", SIZE),
    SIZE = ifelse(SIZE == "3LTR", "3L", SIZE),
    SIZE = ifelse(SIZE == "1LTR", "1L", SIZE),
    TRANSDATE = as.Date(TRANSDATE, format = "%Y-%m-%d"),
    YEAR = year(TRANSDATE),
    DESCRIPTION = DESCRIPTION %>% str_trim() %>% str_squish() %>% str_to_upper(),
    FEE = NETAMOUNT / LINEQTY
  )
```


## 3) Fix inconsistent pack unit labels


``` r
transactions <- transactions %>%
  mutate(
    PACKUNIT = case_when(
      grepl("^[0-9]+$", PACKUNIT) & PACKUNIT == "1" ~ "Btl",
      grepl("^[0-9]+$", PACKUNIT) ~ paste0(PACKUNIT, "pk"),
      PACKUNIT == "08pk" ~ "8pk",
      PACKUNIT == "06pk" ~ "6pk",
      PACKUNIT == "05pk" ~ "5pk",
      PACKUNIT == "04pk" ~ "4pk",
      PACKUNIT == "03pk" ~ "3pk",
      PACKUNIT == "02pk" ~ "2pk",
      PACKUNIT == "01pk" ~ "Btl",
      TRUE ~ PACKUNIT
    )
  )
```

## 4) Fix missing BEER classification type


``` r
transactions$CLASSIFICATIONTYPE <- ifelse(
  (transactions$CLASSIFICATIONTYPE == "" |
     transactions$CLASSIFICATIONTYPE == "NULL" |
     is.na(transactions$CLASSIFICATIONTYPE)) &
    transactions$CLASSIFICATIONDEPARTMENT == "BEER",
  transactions$CLASSIFICATIONDEPARTMENT,
  transactions$CLASSIFICATIONTYPE
)
```


## 5)  Filter out unwanted rows and handle missing/invalid data


``` r
transactionsFS <- transactions |>
  filter(
    STORENAME != "Westwood",
  )
```


## 6)  Filter out unwanted rows and handle missing/invalid data


``` r
transactions <- transactions |>
  filter(
    STORENAME != "Westwood",
    CLASSIFICATIONDEPARTMENT != "DONATIONS",
    CLASSIFICATIONDEPARTMENT != "NON INVENTORY",
    SIZE != "UNIT",
    (SIZE != "" & !is.na(SIZE)),
    (TAGDESC != "NULL" & !is.na(TAGDESC)),
    (TAGDESC == "STOCK")
  )
```


## 7) Standardize Product Descriptions


``` r
# Identify items with conflicting descriptions

item_desc_conflicts <- transactions %>%
  select(ITEMID, DESCRIPTION) %>%
  distinct() %>%
  group_by(ITEMID) %>%
  filter(n_distinct(DESCRIPTION) > 1) %>%
  arrange(ITEMID)


# Automated cleaning method

transactions_clean <- transactions %>%
  group_by(ITEMID) %>%
  mutate(
    has_size = str_detect(DESCRIPTION, "\\b[0-9.]+(ML|L|OZ|Z)\\b"),
    DESCRIPTION = if (any(has_size)) DESCRIPTION[has_size][1] else DESCRIPTION
  ) %>%
  ungroup() %>%
  select(-has_size)
  
transactions <- transactions_clean

# Manual override rules for edge cases 

transactions <- transactions %>%
  mutate(
    DESCRIPTION = case_when(
      DESCRIPTION == "BOWMANS PET VODKA" ~ "BOWMANS PET VODKA 200ML",
      DESCRIPTION == "NEW AMSTERDAM APPLE" ~ "NEW AMSTERDAM APPLE - 50ML",
      DESCRIPTION == "NEW AMSTERDAM PEACH" ~ "NEW AMSTERDAM PEACH - 50ML",
      DESCRIPTION == "NEW AMSTERDAM- PASSION F" ~ "NEW AMSTERDAM- PASSION F - 50ML",
      DESCRIPTION == "NEW AMSTERDAM GRAPEFRUIT" ~ "NEW AMSTERDAM GRAPEFRUIT - 50ML",
      DESCRIPTION == "L MARCA PRO -ROSE" ~ "L MARCA PRO -ROSE - 750ML",
      DESCRIPTION == "MR BOSTON TRIPLE SEC" ~ "MR BOSTON TRIPLE SEC - 1LTR",
      DESCRIPTION == "MR BOSTON PEACH SCHNAPPS 1" ~ "MR BOSTON PEACH SCHNAPPS 1 - 1L",
      
      TRUE ~ DESCRIPTION
    )
  )
```


## 8) Product Mix & Size Performance


``` r
# Sales & quantity by package size

transactions %>%
  group_by(SIZE) %>%
  summarise(
    TotalSales = sum(NETAMOUNT, na.rm = TRUE),
    TotalQty   = sum(LINEQTY,  na.rm = TRUE)
  ) %>%
  arrange(desc(TotalSales))
```

```
## # A tibble: 36 × 3
##    SIZE  TotalSales TotalQty
##    <chr>      <dbl>    <dbl>
##  1 750ML 131020386.  6039756
##  2 1.75L  43902274.  1444640
##  3 12.0Z  13699709.   844580
##  4 1L     11097585.   480518
##  5 375ML   8352566.   811572
##  6 1.5L    5794310.   423448
##  7 50ML    4438538.  2836377
##  8 3L      3481740.   169073
##  9 355ML   2340103.   148296
## 10 200ML   2331356.   350634
## # ℹ 26 more rows
```

``` r
# Sales by category type (e.g., VODKA, PROSECCO, etc.)

transactions %>%
  group_by(CLASSIFICATIONTYPE) %>%
  summarise(TotalSales = sum(NETAMOUNT, na.rm = TRUE))
```

```
## # A tibble: 102 × 2
##    CLASSIFICATIONTYPE TotalSales
##    <chr>                   <dbl>
##  1 ALES                 2070524.
##  2 APERITIF              115210.
##  3 BAROLO                 40554.
##  4 BEAUJOLAIS             52585.
##  5 BEAUJOLAIS VILLAGE     87642.
##  6 BIANCO                  1649.
##  7 BLANC                  28187.
##  8 BLUSH                 148868.
##  9 BOURBON             17299433.
## 10 BRANDY               1947305.
## # ℹ 92 more rows
```


## 9) High-Value Transactions (> $200)


``` r
# Count & sales of high-value receipts per store

top_high_value_stores <- transactionsFS %>%
  filter(NETAMOUNT > 200) %>%
  group_by(STORENAME) %>%
  summarise(
    HighValueCount      = n(),
    TotalHighValueSales = sum(NETAMOUNT),
    .groups = "drop"
  ) %>%
  arrange(desc(HighValueCount))

# % of receipts that are > $200 per store

store_high_value_share <- transactionsFS %>%
  group_by(STORENAME) %>%
  summarise(
    TotalTransactions      = n(),
    HighValueTransactions  = sum(NETAMOUNT > 200, na.rm = TRUE),
    HighValueShare         = round(100 * HighValueTransactions / TotalTransactions, 2),
    .groups = "drop"
  ) %>%
  arrange(desc(HighValueShare))
```


## 10)


``` r
NOlicensee_transactions <- transactions %>%
  filter(CUSTACCOUNT != "Licensee")
```


## 11) Bottle / Pack Size Popularity


``` r
transactionsnew <- transactions %>%
  mutate(UnitType = ifelse(CLASSIFICATIONDEPARTMENT == "BEER", PACKUNIT, SIZE))

size_counts <- transactionsnew %>%
  group_by(CLASSIFICATIONDEPARTMENT, UnitType) %>%
  summarise(Count = n(), .groups = "drop") %>%
  arrange(desc(Count))
```

``` r
ggplot(size_counts, 
       aes(x = UnitType, y = Count, size = Count, color = CLASSIFICATIONDEPARTMENT)) +
  geom_point(alpha = 0.7) +
  scale_size(range = c(3, 12)) +
  labs(
    title = "Counts of Product Sizes & Beer Packs by Department",
    x = "Unit Type (Bottle Size or Pack Size)",
    y = "Count"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
```

![](ABS_Transactions_Analysis_Annotated_files/figure-html/unnamed-chunk-13-1.png)<!-- -->


## 12) Category Mix by Store (Pivot)


``` r
# Receipt counts by department x store

dept_by_store <- transactions %>%
  group_by(STORENAME, CLASSIFICATIONDEPARTMENT) %>%
  summarise(Count = n(), .groups = "drop") %>%
  pivot_wider(names_from = CLASSIFICATIONDEPARTMENT, values_from = Count, values_fill = 0)


# Sales by department x store

dept_sales_by_store <- transactions %>%
  group_by(STORENAME, CLASSIFICATIONDEPARTMENT) %>%
  summarise(TotalSales = sum(NETAMOUNT, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = CLASSIFICATIONDEPARTMENT, values_from = TotalSales, values_fill = 0)
```


## 13) Weekday / Weekend / Holiday Sales


``` r
transactionsFS$TRANSDATE <- as.Date(transactionsFS$TRANSDATE)

ny_range  <- seq(as.Date("2023-12-21"), as.Date("2023-12-24"), by="day")
ny_range1 <- seq(as.Date("2024-12-21"), as.Date("2024-12-24"), by="day")
ny_range2 <- seq(as.Date("2023-12-29"), as.Date("2024-01-02"), by="day")
ny_range3 <- seq(as.Date("2024-12-29"), as.Date("2025-01-02"), by="day")

us_holidays <- as.Date(c(
  "2023-07-04","2023-09-04","2023-11-23","2023-12-25",
  ny_range, ny_range1,
  "2024-05-27","2024-07-04","2024-09-02","2024-11-28","2024-12-25",
  ny_range2, ny_range3,
  "2025-05-26"
))

transactionsFS <- transactionsFS %>%
  mutate(
    DayOfWeek = wday(TRANSDATE, label = TRUE),
    DayType   = case_when(
      TRANSDATE %in% us_holidays         ~ "Holiday",
      DayOfWeek %in% c("Sat","Sun")      ~ "Weekend",
      TRUE                               ~ "Weekday"
    )
  )

# Aggregate daily by DayType

daily_sales <- transactionsFS %>%
  group_by(TRANSDATE, DayType) %>%
  summarise(DailySales = sum(NETAMOUNT, na.rm = TRUE), .groups = "drop")

# Mean per DayType

avg_totals <- daily_sales %>%
  group_by(DayType) %>%
  summarise(AverageDailySales = mean(DailySales), .groups = "drop")
```

``` r
ggplot(avg_totals, aes(x = DayType, y = AverageDailySales, fill = DayType)) +
  geom_col(width = 0.6) +
  labs(
    title = "Average Total Sales by Day Type",
    x = "Day Category",
    y = "Average Total Sales ($)"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("steelblue", "darkorange", "darkred"))
```

![](ABS_Transactions_Analysis_Annotated_files/figure-html/unnamed-chunk-16-1.png)<!-- -->


## 14) Daily Sales — FY23-24 vs FY24-25


``` r
daily_sales <- transactionsFS %>%
  group_by(TRANSDATE) %>%
  summarise(TotalSales = sum(NETAMOUNT, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    Day   = day(TRANSDATE),
    Month = month(TRANSDATE, label = TRUE, abbr = TRUE),
    Year  = year(TRANSDATE)
  )

fy23_24 <- daily_sales %>% filter(TRANSDATE >= as.Date("2023-07-01") & TRANSDATE <= as.Date("2024-06-30"))
fy24_25 <- daily_sales %>% filter(TRANSDATE >= as.Date("2024-07-01") & TRANSDATE <= as.Date("2025-06-30"))

fy23_24_aligned <- fy23_24 %>% mutate(FirstOfMonth=floor_date(TRANSDATE,"month"),
                                      WeekdayOffset=wday(FirstOfMonth,week_start=1)-1,
                                      DayAligned=Day+WeekdayOffset)

fy24_25_aligned <- fy24_25 %>% mutate(FirstOfMonth=floor_date(TRANSDATE,"month"),
                                      WeekdayOffset=wday(FirstOfMonth,week_start=1)-1,
                                      DayAligned=Day+WeekdayOffset)
```

``` r
weekday_labels <- rep(c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"), length.out = 40)

ggplot(fy23_24_aligned, aes(x = DayAligned, y = TotalSales, color = Month, group = Month)) +
  geom_line(size = 1.1) +
  labs(
    title = "Daily Sales by Month — FY 2023–2024 (Aligned to Weekday Start)",
    x = "Aligned Calendar Day",
    y = "Sales ($)"
  ) +
  scale_x_continuous(
    breaks = seq(1, 40, by = 1),
    labels = weekday_labels
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8))
```

```
## Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
## ℹ Please use `linewidth` instead.
## This warning is displayed once every 8 hours.
## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
## generated.
```

![](ABS_Transactions_Analysis_Annotated_files/figure-html/unnamed-chunk-18-1.png)<!-- -->

``` r
ggplot(fy24_25_aligned, aes(x = DayAligned, y = TotalSales, color = Month, group = Month)) +
  geom_line(size = 1.1) +
  labs(
    title = "Daily Sales by Month — FY 2024–2025 (Aligned to Weekday Start)",
    x = "Aligned Calendar Day",
    y = "Sales ($)"
  ) +
  scale_x_continuous(
    breaks = seq(1, 40, by = 1),
    labels = weekday_labels
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8))
```

![](ABS_Transactions_Analysis_Annotated_files/figure-html/unnamed-chunk-19-1.png)<!-- -->

## 15) Weekday Sales Ranking


``` r
weekday_sales <- transactionsFS %>%
  mutate(Weekday = wday(TRANSDATE, label = TRUE, abbr = TRUE)) %>%
  group_by(Weekday) %>%
  summarise(TotalSales = sum(NETAMOUNT, na.rm = TRUE), .groups = "drop") %>%
  arrange(TotalSales)
```

``` r
ggplot(weekday_sales, aes(x = reorder(Weekday, TotalSales), y = TotalSales)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = scales::dollar_format()(round(TotalSales,0))),
            vjust = -0.5, size = 3.5) +
  labs(
    title = "Total Sales by Weekday",
    x = "Weekday", y = "Total Sales ($)"
  ) +
  theme_minimal()
```

![](ABS_Transactions_Analysis_Annotated_files/figure-html/unnamed-chunk-21-1.png)<!-- -->


## 16) Monthly Sales — Compare Fiscal Years


``` r
transactionsFS <- transactionsFS %>%
  mutate(YearMonth = format(TRANSDATE, "%Y-%m"))

fy23_24_monthlyFS <- transactionsFS %>%
  filter(TRANSDATE >= "2023-07-01" & TRANSDATE <= "2024-06-30") %>%
  group_by(YearMonth) %>%
  summarise(TotalSales = sum(NETAMOUNT), .groups = "drop") %>%
  arrange(YearMonth) %>%
  mutate(FiscalYear="FY23-24", MonthIndex=row_number())

fy24_25_monthlyFS <- transactionsFS %>%
  filter(TRANSDATE >= "2024-07-01" & TRANSDATE <= "2025-06-30") %>%
  group_by(YearMonth) %>%
  summarise(TotalSales = sum(NETAMOUNT), .groups = "drop") %>%
  arrange(YearMonth) %>%
  mutate(FiscalYear="FY24-25", MonthIndex=row_number())

combined_monthlyFS <- rbind(fy23_24_monthlyFS, fy24_25_monthlyFS)
```

``` r
ggplot(combined_monthlyFS, aes(x = MonthIndex, y = TotalSales, color = FiscalYear, group = FiscalYear)) +
  geom_line(linewidth = 1.3) +
  geom_point(size = 3) +
  scale_x_continuous(
    breaks = 1:12,
    labels = c("Jul","Aug","Sep","Oct","Nov","Dec","Jan","Feb","Mar","Apr","May","Jun")
  ) +
  scale_color_manual(values = c("FY23-24"="steelblue", "FY24-25"="darkorange")) +
  labs(
    title = "Monthly Total Sales Comparison: FY23–24 vs FY24–25",
    x = "Month", y = "Total Sales ($)", color = "Fiscal Year"
  ) +
  theme_minimal()
```

![](ABS_Transactions_Analysis_Annotated_files/figure-html/unnamed-chunk-23-1.png)<!-- -->

## 17) Store Summary — Sales, Transactions, Baskets (FY24-25)


``` r
fy24_25 <- NOlicensee_transactions %>%
  filter(TRANSDATE >= "2024-07-01" & TRANSDATE <= "2025-06-30")

store_fy24_25_summary <- fy24_25 %>%
  group_by(STORENAME) %>%
  summarise(
    TotalSales        = sum(NETAMOUNT, na.rm=TRUE),
    TotalTransactions = n(),
    .groups = "drop"
  )

baskets_fy24_25 <- fy24_25 %>%
  group_by(STORENAME) %>%
  summarise(TotalBaskets = n_distinct(TRANSACTIONID), .groups = "drop")

store_fy24_25_final <- store_fy24_25_summary %>%
  left_join(baskets_fy24_25, by="STORENAME") %>%
  arrange(STORENAME)
```


## 18) Add Square Footage & Efficiency Metrics


``` r
store_sqft <- readr::read_csv("Designation.csv") # expect columns: STORENAME, SquareFootage
```

```
## Rows: 27 Columns: 6
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (4): STORENAME, Current Designation, SY 2023, SY 2024
## dbl (2): STORE, SquareFootage
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

``` r
store_fy24_25_final <- store_fy24_25_final %>%
  left_join(store_sqft %>% select(STORENAME, SquareFootage), by="STORENAME") %>%
  mutate(
    TotalSalesPerSqFt        = TotalSales        / SquareFootage,
    TotalTransactionsPerSqFt = TotalTransactions / SquareFootage,
    TotalBasketsPerSqFt      = TotalBaskets      / SquareFootage
  ) %>%
  arrange(STORENAME)

traffic_median <- median(store_fy24_25_final$TotalTransactionsPerSqFt, na.rm = TRUE)
sales_median   <- median(store_fy24_25_final$TotalSalesPerSqFt,        na.rm = TRUE)
basket_median  <- median(store_fy24_25_final$TotalBasketsPerSqFt,      na.rm = TRUE)
```


## 19) Basket Structure — Avg Basket $ and Items


``` r
basket_metrics <- fy24_25 %>%
  group_by(STORENAME, TRANSACTIONID) %>%
  summarise(
    BasketTotal = sum(NETAMOUNT, na.rm = TRUE),
    Items       = sum(LINEQTY,  na.rm = TRUE),
    .groups = "drop"
  )

basket_summary <- basket_metrics %>%
  group_by(STORENAME) %>%
  summarise(
    AvgBasketValue    = mean(BasketTotal, na.rm = TRUE),
    AvgItemsPerBasket = mean(Items,       na.rm = TRUE),
    .groups = "drop"
  )

store_fy24_25_final <- store_fy24_25_final %>%
  left_join(basket_summary, by = "STORENAME") %>%
  arrange(STORENAME)
```


## 20) Quadrant Classification: Sales, Traffic & Basket Density


``` r
store_fy24_25_quad <- store_fy24_25_final %>%
  mutate(
    Quad_Sales_Traffic = case_when(
      TotalSalesPerSqFt >= sales_median  & TotalTransactionsPerSqFt >= traffic_median ~ "High Spend & High Traffic",
      TotalSalesPerSqFt >= sales_median  & TotalTransactionsPerSqFt <  traffic_median ~ "High Spend & Low Traffic",
      TotalSalesPerSqFt <  sales_median  & TotalTransactionsPerSqFt >= traffic_median ~ "Low Spend & High Traffic",
      TRUE ~ "Low Spend & Low Traffic"
    ),
    Quad_Sales_Baskets = case_when(
      TotalSalesPerSqFt >= sales_median  & TotalBasketsPerSqFt >= basket_median ~ "High Spend & High Baskets",
      TotalSalesPerSqFt >= sales_median  & TotalBasketsPerSqFt <  basket_median ~ "High Spend & Low Baskets",
      TotalSalesPerSqFt <  sales_median  & TotalBasketsPerSqFt >= basket_median ~ "Low Spend & High Baskets",
      TRUE ~ "Low Spend & Low Baskets"
    ),
    Quad_Traffic_Baskets = case_when(
      TotalTransactionsPerSqFt >= traffic_median & TotalBasketsPerSqFt >= basket_median ~ "High Traffic & High Baskets",
      TotalTransactionsPerSqFt >= traffic_median & TotalBasketsPerSqFt <  basket_median ~ "High Traffic & Low Baskets",
      TotalTransactionsPerSqFt <  traffic_median & TotalBasketsPerSqFt >= basket_median ~ "Low Traffic & High Baskets",
      TRUE ~ "Low Traffic & Low Baskets"
    )
  )
```


``` r
# Sales vs Traffic

ggplot(store_fy24_25_quad, aes(x = TotalTransactionsPerSqFt,
                                y = TotalSalesPerSqFt,
                                color = Quad_Sales_Traffic,
                                label = STORENAME)) +
  geom_point(size = 4) +
  geom_text(vjust = -0.6, size = 3) +
  geom_vline(xintercept = traffic_median, linetype = "dashed") +
  geom_hline(yintercept = sales_median, linetype = "dashed") +
  labs(
    title = "Quadrant: Sales Efficiency vs Traffic Efficiency",
    x = "Transactions per SqFt",
    y = "Sales per SqFt",
    color = "Quadrant"
  ) +
  theme_minimal()
```

![](ABS_Transactions_Analysis_Annotated_files/figure-html/unnamed-chunk-28-1.png)<!-- -->


``` r
# Sales vs Basket

ggplot(store_fy24_25_quad, aes(x = TotalBasketsPerSqFt,
                                y = TotalSalesPerSqFt,
                                color = Quad_Sales_Baskets,
                                label = STORENAME)) +
  geom_point(size = 4) +
  geom_text(vjust = -0.6, size = 3) +
  geom_vline(xintercept = basket_median, linetype = "dashed") +
  geom_hline(yintercept = sales_median, linetype = "dashed") +
  labs(
    title = "Quadrant: Sales Efficiency vs Basket Density",
    x = "Baskets per SqFt",
    y = "Sales per SqFt",
    color = "Quadrant"
  ) +
  theme_minimal()
```

![](ABS_Transactions_Analysis_Annotated_files/figure-html/unnamed-chunk-29-1.png)<!-- -->


``` r
# Traffic vs Basket

ggplot(store_fy24_25_quad, aes(x = TotalBasketsPerSqFt,
                                y = TotalTransactionsPerSqFt,
                                color = Quad_Traffic_Baskets,
                                label = STORENAME)) +
  geom_point(size = 4) +
  geom_text(vjust = -0.6, size = 3) +
  geom_vline(xintercept = basket_median, linetype = "dashed") +
  geom_hline(yintercept = traffic_median, linetype = "dashed") +
  labs(
    title = "Quadrant: Traffic Efficiency vs Basket Density",
    x = "Baskets per SqFt",
    y = "Transactions per SqFt",
    color = "Quadrant"
  ) +
  theme_minimal()
```

![](ABS_Transactions_Analysis_Annotated_files/figure-html/unnamed-chunk-30-1.png)<!-- -->

## 21) Demographics vs Store Performance



``` r
demographics <- read.csv(
  "Demographics.csv",
  header = TRUE, sep = ",", encoding = "UTF-8", quote = "\""
)

demographics_selected <- demographics %>%
  select(STORENAME, SquareFootage, TotalPopulation, White, Black, Hispanic,
         X25YearsOld, PovertyLevel, TwicePovertyLevel)

store_fy24_25_summary <- store_fy24_25_summary %>%
  left_join(demographics_selected, by = "STORENAME")

# 15a) Size vs Sales

cor(store_fy24_25_summary$TotalSales, store_fy24_25_summary$SquareFootage, use = "complete.obs")
```

```
## [1] 0.3013263
```

``` r
summary(lm(TotalSales ~ SquareFootage, data = store_fy24_25_summary))
```

```
## 
## Call:
## lm(formula = TotalSales ~ SquareFootage, data = store_fy24_25_summary)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -2981169 -1129424  -121124   753618  3678365 
## 
## Coefficients:
##                Estimate Std. Error t value Pr(>|t|)
## (Intercept)   2069162.5  1222312.0   1.693    0.103
## SquareFootage     341.5      216.1   1.580    0.127
## 
## Residual standard error: 1701000 on 25 degrees of freedom
## Multiple R-squared:  0.0908,	Adjusted R-squared:  0.05443 
## F-statistic: 2.497 on 1 and 25 DF,  p-value: 0.1267
```

``` r
# 15b) Population vs Sales

cor(store_fy24_25_summary$TotalSales, store_fy24_25_summary$TotalPopulation, use = "complete.obs")
```

```
## [1] 0.03887317
```

``` r
summary(lm(TotalSales ~ TotalPopulation, data = store_fy24_25_summary))
```

```
## 
## Call:
## lm(formula = TotalSales ~ TotalPopulation, data = store_fy24_25_summary)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -3249825 -1464570  -303586   979487  3475108 
## 
## Coefficients:
##                  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)     3.791e+06  7.911e+05   4.793 6.38e-05 ***
## TotalPopulation 5.401e+00  2.777e+01   0.195    0.847    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1783000 on 25 degrees of freedom
## Multiple R-squared:  0.001511,	Adjusted R-squared:  -0.03843 
## F-statistic: 0.03784 on 1 and 25 DF,  p-value: 0.8473
```

``` r
# 15c) 25+ vs Sales

cor(store_fy24_25_summary$TotalSales, store_fy24_25_summary$X25YearsOld, use = "complete.obs")
```

```
## [1] 0.06280109
```

``` r
summary(lm(TotalSales ~ X25YearsOld, data = store_fy24_25_summary))
```

```
## 
## Call:
## lm(formula = TotalSales ~ X25YearsOld, data = store_fy24_25_summary)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -3187023 -1429426  -268457   978659  3432242 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 3.708e+06  7.841e+05   4.729 7.52e-05 ***
## X25YearsOld 1.240e+01  3.943e+01   0.315    0.756    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1781000 on 25 degrees of freedom
## Multiple R-squared:  0.003944,	Adjusted R-squared:  -0.0359 
## F-statistic: 0.09899 on 1 and 25 DF,  p-value: 0.7557
```

``` r
# 15d) Poverty vs Sales

cor(store_fy24_25_summary$TotalSales, store_fy24_25_summary$PovertyLevel, use = "complete.obs")
```

```
## [1] -0.1767873
```

``` r
summary(lm(TotalSales ~ PovertyLevel, data = store_fy24_25_summary))
```

```
## 
## Call:
## lm(formula = TotalSales ~ PovertyLevel, data = store_fy24_25_summary)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -3654714 -1384601     -740   902742  3761843 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  4296918.3   530306.7   8.103 1.86e-08 ***
## PovertyLevel    -173.0      192.6  -0.898    0.378    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1756000 on 25 degrees of freedom
## Multiple R-squared:  0.03125,	Adjusted R-squared:  -0.007496 
## F-statistic: 0.8066 on 1 and 25 DF,  p-value: 0.3777
```

``` r
ggplot(store_fy24_25_summary,
       aes(x = SquareFootage, y = TotalSales)) +
  geom_point(size = 3, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "darkred") +
  labs(
    title = "Total Sales vs Store Square Footage",
    x = "Store Square Footage",
    y = "Total Sales ($)"
  ) +
  theme_minimal()
```

```
## `geom_smooth()` using formula = 'y ~ x'
```

![](ABS_Transactions_Analysis_Annotated_files/figure-html/unnamed-chunk-32-1.png)<!-- -->

``` r
ggplot(store_fy24_25_summary,
       aes(x = TotalPopulation, y = TotalSales)) +
  geom_point(size = 3, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "darkred") +
  labs(
    title = "Total Sales vs Local Population",
    x = "Total Population in Store Trade Area",
    y = "Total 2024 Sales ($)"
  ) +
  theme_minimal()
```

```
## `geom_smooth()` using formula = 'y ~ x'
```

![](ABS_Transactions_Analysis_Annotated_files/figure-html/unnamed-chunk-33-1.png)<!-- -->

``` r
ggplot(store_fy24_25_summary,
       aes(x = X25YearsOld, y = TotalSales)) +
  geom_point(size = 3, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "darkred") +
  labs(
    title = "Total Sales vs Population Age 25+",
    x = "Population Age 25+",
    y = "Total Sales (2024)"
  ) +
  theme_minimal()
```

```
## `geom_smooth()` using formula = 'y ~ x'
```

![](ABS_Transactions_Analysis_Annotated_files/figure-html/unnamed-chunk-34-1.png)<!-- -->

``` r
ggplot(store_fy24_25_summary,
       aes(x = PovertyLevel, y = TotalSales)) +
  geom_point(size = 3, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "darkred") +
  labs(
    title = "Total Sales vs Population Below Poverty Level",
    x = "Residents Below Poverty Level",
    y = "Total Sales (2024)"
  ) +
  theme_minimal()
```

```
## `geom_smooth()` using formula = 'y ~ x'
```

![](ABS_Transactions_Analysis_Annotated_files/figure-html/unnamed-chunk-35-1.png)<!-- -->

## 23) Top/Bottom SKUs — Store & District


``` r
# Scope: FY24-25 window

fy24_25_FS <- transactionsFS %>%
  filter(TRANSDATE >= "2024-07-01" & TRANSDATE <= "2025-06-30")

# Top 5 by qty per store

top5_items_per_store <- fy24_25_FS %>%
  group_by(STORENAME, ITEMID, DESCRIPTION) %>%
  summarise(
    total_qty = sum(TOTALQTY, na.rm = TRUE),
    total_sales = sum(NETAMOUNT, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  group_by(STORENAME) %>%
  slice_max(total_qty, n = 5, with_ties = FALSE) %>%
  arrange(STORENAME, desc(total_qty))
```

```
## `summarise()` has grouped output by 'STORENAME', 'ITEMID'. You can override
## using the `.groups` argument.
```

``` r
# Top 30 overall by qty

top30_items_all_stores <- fy24_25_FS %>%
  group_by(ITEMID, DESCRIPTION) %>%
  summarise(
    total_qty   = sum(TOTALQTY,  na.rm = TRUE),
    total_sales = sum(NETAMOUNT, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  slice_max(total_qty, n = 30, with_ties = FALSE) %>%
  arrange(desc(total_qty))

# Bottom 5 by qty per store (stock only)

bottom5_items_per_stores <- fy24_25_FS %>%
  filter(ITEMTAG == "ST") %>% 
  group_by(STORENAME, ITEMID, DESCRIPTION) %>%
  summarise(
    total_qty = sum(TOTALQTY, na.rm = TRUE),
    total_sales = sum(NETAMOUNT, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  group_by(STORENAME) %>%
  slice_min(total_qty, n = 5, with_ties = FALSE) %>%
  arrange(STORENAME, total_qty)
```

```
## `summarise()` has grouped output by 'STORENAME', 'ITEMID'. You can override
## using the `.groups` argument.
```

``` r
# Bottom 30 overall (stock only)

bottom30_items_all_stores <- fy24_25_FS %>%
  filter(ITEMTAG == "ST") %>%
  group_by(ITEMID, DESCRIPTION) %>%
  summarise(
    total_qty   = sum(TOTALQTY,  na.rm = TRUE),
    total_sales = sum(NETAMOUNT, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  slice_min(total_qty, n = 30, with_ties = FALSE) %>%
  arrange(total_qty)
```

``` r
ggplot(top5_items_per_store,
       aes(x = reorder(DESCRIPTION, total_qty),
           y = total_qty)) +
  geom_col(fill = "#2E86C1") +
  coord_flip() +
  facet_wrap(~ STORENAME, scales = "free_y") +
  labs(
    title = "Top 5 Selling ItemIDs per Store (FY24-25)",
    x = "Product",
    y = "Total Quantity Sold"
  ) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  theme_minimal(base_size = 6)
```

![](ABS_Transactions_Analysis_Annotated_files/figure-html/unnamed-chunk-37-1.png)<!-- -->

``` r
ggplot(top30_items_all_stores,
       aes(x = reorder(DESCRIPTION, total_qty),
           y = total_qty)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Top 5 Selling Items Across All Stores (FY24-25)",
    x = "Product",
    y = "Total Quantity Sold"
  ) +
  theme_minimal(base_size = 12)
```

![](ABS_Transactions_Analysis_Annotated_files/figure-html/unnamed-chunk-38-1.png)<!-- -->

``` r
ggplot(bottom5_items_per_stores,
       aes(x = reorder(DESCRIPTION, total_qty),
           y = total_qty)) +
  geom_col(fill = "#2E86C1") +
  coord_flip() +
  facet_wrap(~ STORENAME, scales = "free_y") +
  labs(
    title = "Bottom 5 Selling ItemIDs per Store (FY24-25)",
    x = "Product",
    y = "Total Quantity Sold"
  ) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  theme_minimal(base_size = 6)
```

![](ABS_Transactions_Analysis_Annotated_files/figure-html/unnamed-chunk-39-1.png)<!-- -->

``` r
ggplot(bottom30_items_all_stores,
       aes(x = reorder(DESCRIPTION, total_qty),
           y = total_qty)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Bottom 30 Selling Items Across All Stores (FY24-25)",
    x = "Product",
    y = "Total Quantity Sold"
  ) +
  theme_minimal(base_size = 12)
```

![](ABS_Transactions_Analysis_Annotated_files/figure-html/unnamed-chunk-40-1.png)<!-- -->

## Data Processing Flowchart
```
Clean CSV (TransactionData_Clean.csv)
   ↓
Load & Validate
   ↓
Size/Mix/Category KPIs
   ↓
Weekday/Weekend/Holiday Effects
   ↓
Time Series FY vs FY
   ↓
Store KPIs + SqFt Efficiency
   ↓
Demographics Correlations
   ↓
MBA (Apriori) — Cross-Sell
   ↓
Top/Bottom SKU Reports
```

