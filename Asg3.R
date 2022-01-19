#Q1a
ggplot(data = mpg)
# Running this creates a blank plot in the plot box

#Q1b
ggplot(mapping = aes(x = mpg$drv, y = mpg$class)) +
  geom_point()
# This is not useful as a scatterplot should not be used to for two categorical types of data

#Q2a
# In this code, the points are not blue as when a colour in defined within aes(), it is the defined column label, thus "colour" would be the word of the colour as compared to setting the points to that colour
??geom_point
#Q2b
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), colour = "blue")

#Q3
delay = flights %>%
  group_by(origin) %>%
  summarize(avg_arr_delay = mean(arr_delay, na.rm = TRUE), avg_dep_delay = mean(dep_delay, na.rm = TRUE)) %>%
  ungroup()
delay_data = tibble(origin = unlist(c(delay[,1], delay[,1])),
                    avg_delay = unlist (c(delay[,2], delay[,3])),
                    Type = rep(c("arrival", "departure"), each = 3))
ggplot(delay_data, aes(x = origin, y = avg_delay, fill = Type))+
  geom_col(position = "dodge")+
  labs( title = "Average Arrival Delay and Departure Delay by Origins", 
        y = "Average Delay",
        x = "Origin")

#Q4a
avg_price = diamonds %>%
  group_by(cut) %>%
  summarize(price = mean(price))
#These results do not seem reasonable as the cost does not increase with the quality of the cut. Fair is the second most expensive while Ideal is the cheapest.

#Q4b
ggplot(diamonds, aes(x = price)) +
  geom_histogram(position = "identity") +
  facet_grid(cut ~ ., scales = "free_y")
#I would expect to see that fair cuts are generally more expensive than ideal cut diamonds considering the mean is larger

#Q4c
diamond_carat = filter(diamonds, carat <= 1 & carat >= 0.9)
ggplot(diamond_carat, aes(x = price)) +
  geom_histogram(position = "identity") +
  facet_grid(cut ~ ., scales = "free_y")

#Q5
ideal_cut = filter(diamonds, cut == "Ideal", clarity == "VS2")
ggplot(ideal_cut, aes(x = carat, y = price, color = color)) +
  geom_point(size = 0.9)
# The majority of diamond carats are around 0.5, 0.67, 1, 1.5, and 2, with few not being these discrete numbers 
# As carat increases so does the price, in an exponential matter

#Q6
# se refers to whether or not the confidence interval will be shown around the smooth regression line

#Q7
ggplot() +
  geom_point(mpg, mapping = aes(x = displ, y = hwy)) +
  geom_smooth(mpg, mapping = aes(x=displ, y = hwy))

#Q8
ggplot() +
  geom_point(mpg, mapping = aes(x = displ, y = hwy)) +
  geom_smooth(mpg, mapping = aes(x = displ, y = hwy, group = drv), se = FALSE)

#Q9
ggplot() +
  geom_point(mpg, mapping = aes(x = displ, y = hwy)) +
  geom_smooth(data = filter(mpg, drv =="r"), mapping = aes(x = displ, y = hwy), se = FALSE)

#Q10a
ggplot() +
  geom_point(mpg, mapping = aes(x = displ, y = hwy, color = drv)) +
  geom_smooth(mpg, mapping = aes(x = displ, y = hwy, color = drv), se = FALSE)

#Q10b
ggplot() +
  geom_point(mpg, mapping = aes(x = displ, y = hwy, color = drv)) +
  geom_smooth(mpg, mapping = aes(x = displ, y = hwy), se = FALSE)

