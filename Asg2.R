#library(tidyverse)

#Q1
filter(mtcars, gear==4)

#Q2a
filter(mtcars, gear==4, cyl==6)

#Q2b
count(filter(mtcars, gear==4, cyl==6))

#Q3
arrange(mtcars, wt)

#Q4
#library(nycflights13)
filter(flights, month %in% c(1, 11), arr_delay <= 10)

#Q5
count(filter(flights, month==5))

#Q6a
ggplot(filter(flights, month==2), aes(x=dep_delay))+
  geom_histogram(binwidth=10)

#6b
dep_100 = filter(flights, month==2, dep_delay < 100)
ggplot(filter(flights, month==2), aes(x=dep_delay))+
  geom_histogram(binwidth=5)

#Q7a 
jan1_1 = filter(flights, month == 1, day == 1, dep_delay < 180)
ggplot(jan1_1, aes(x = dep_delay, y = arr_delay))+
  geom_point()

#Q7b
jan1_2 = filter(flights, month == 1, day == 1, dep_delay < 10)
ggplot(jan1_2, aes(x = dep_delay, y = arr_delay))+
  geom_point()

#Q7c 
# graphA shows a stronger trend than graphB as it shows stronger positive slope/correlation among the arrival and departure delay whereas graphB appears to have a weaker correlation with more spreadout points.

#Q7d 
jan1_1 = filter(flights, month == 1, day == 1, dep_delay < 180)
jan1_2 = filter(flights, month == 1, day == 1, dep_delay < 10)
cor(x = jan1_1$dep_delay, y = jan1_1$arr_delay, use = "complete.obs")
cor(x = jan1_2$dep_delay, y = jan1_2$arr_delay, use = "complete.obs")
# these results make sense as they show that raphA has a high correlation, which was suspected, while the second graph does not.

#Q8
plot_decay = function(which_month, which_day, lower_range, upper_range) {
  jan30 = filter(flights, month == which_month, day == which_day, lower_range <= dep_delay, dep_delay <= upper_range)
  ggplot(jan30, aes(x = dep_delay, y = arr_delay))+
    geom_point()
}
plot_decay(which_month=1, which_day=30, lower_range=-5, upper_range = 30)

#Q9
ggplot(flights, aes(x = month))+
  geom_bar()

#Q10
by_day = group_by(flights, month, day)
avg_delay = summarize(by_day, avg_delay=mean(arr_delay, na.rm = TRUE))$avg_delay
ggplot(mapping=aes(x=1:365, y=avg_delay)) + 
  geom_line()


