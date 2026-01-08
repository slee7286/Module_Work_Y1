# Name: Evangelos Dimitriou


# Install the packages if necessary
# install.packages("tidyverse")
# install.packages("palmerpenguins")

# install.packages(c("tidyverse", "palmerpenguins"))

# My name is Evangelos

library(tidyverse)
library(palmerpenguins)

View(penguins)
view(penguins)

# Chapter 2

# Basic math calculation
1 / 200 * 30
(59 + 73 + 2) / 3
sin(pi / 2)

# Create new object
x <- 3 * 4
x = 3*4 # avoid
x

# Combine multiple elements
# create a vector of primes 
primes <- c(2, 3, 5, 7, 11, 13)
primes

# Basic arithmetic on vectors is applied to every element of the vector
# multiply primes by 2
primes * 2
primes - 1

primes <- c(primes, 15, 17)
primes
x

z <- c(primes, x)

this_is_a_really_long_name <- 2.5

primes
primes <- primes[-1]

r_rocks <- 2^3

r_rock
R_rocks

# Calling functions
seq(from = 1, to = 10, by = 0.5)
seq(1, 10)

x <- "hello world"
x
x <- "hello"

# Exercises

# 1

my_variable <- 10
my_varıable
my_variable

# 2 
library(tidyverse)

# This is not correct
ggplot(dTA = mpg) + 
  geom_point(maping = aes(x = displ y = hwy)) +
  geom_smooth(method = "lm")

# Correct version
# Not working
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  geom_smooth(method = "lm")

ggplot(data = mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  geom_smooth(mapping = aes(x = displ, y = hwy), method = "lm")


# 3
# Option+Shift+K/Alt+Shift+K gives you all the shortcuts

# 4 

x <- 3*4
x
3*4

my_bar_plot <- ggplot(mpg, aes(x = class)) +
  geom_bar()
my_scatter_plot <- ggplot(mpg, aes(x = cty, y = hwy)) +
  geom_point()
ggsave(filename = "mpg-plot.png", plot = my_bar_plot)

# Where is it saved?
# The files are saved on the working directory
getwd()


# Chapter 4

install.packages("nycflights13")
library(nycflights13)

# Names
View(flights)

# Strive for:
short_flights <- flights |> filter(air_time < 60)
short_flights
short_flights <- flights |> filter(air_time < 60)




# Avoid:
SHORTFLIGHTS <- flights |> filter(air_time < 60)


# Spaces

a <- 1 
b <- 2
d <- 2

# Strive for

z <- (a + b)^2 / d
z
# Avoid
z<-( a + b ) ^ 2/d
z

x <- seq(1,10)

# Strive for
mean(x, na.rm = TRUE)

# Avoid
mean (x ,na.rm=TRUE)

# -----------------------------------------------------------------------------

flights

flights |> 
  mutate(
    speed      = distance / air_time,
    dep_hour   = dep_time %/% 100,
    dep_minute = dep_time %%  100
  )

# With description


# Add new variables to the flights dataset
flights |> 
  mutate(
    # Calculate average speed (miles per minute)
    # You can multiply by 60 later to get miles per hour if desired
    speed = distance / air_time,
    
    # Extract the departure hour from dep_time
    # Example: if dep_time = 915, then 915 %/% 100 = 9
    dep_hour = dep_time %/% 100,
    
    # Extract the departure minute from dep_time
    # Example: if dep_time = 915, then 915 %% 100 = 15
    dep_minute = dep_time %% 100
  )
View(flights)
flights$carrier
flights2 <- flights |> 
  mutate(
    # Calculate average speed (miles per minute)
    # You can multiply by 60 later to get miles per hour if desired
    speed = distance / air_time,
    
    # Extract the departure hour from dep_time
    # Example: if dep_time = 915, then 915 %/% 100 = 9
    dep_hour = dep_time %/% 100,
    
    # Extract the departure minute from dep_time
    # Example: if dep_time = 915, then 915 %% 100 = 15
    dep_minute = dep_time %% 100
  )

View(flights2)
# -----------------------------------------------------------------------------

# Pipes |>

# The |> operator is the "pipe" in base R.
# It takes the output of one expression and passes it as the first argument
# to the next function, making code easier to read.
#
# Example:
#   x |> f()          is the same as   f(x)
#   x |> f(y)         is the same as   f(x, y)
#
# This allows you to write a sequence of data transformations
# step-by-step, from top to bottom, instead of nesting functions.
#
# Example:
#   flights |>
#     filter(dest == "IAH") |>
#     group_by(month) |>
#     summarise(n = n())
#
# This is equivalent to:
#   summarise(group_by(filter(flights, dest == "IAH"), month), n = n())


# Strive for 
flights |>  
  filter(!is.na(arr_delay), !is.na(tailnum)) |> 
  count(dest)

# Avoid
flights|>filter(!is.na(arr_delay), !is.na(tailnum))|>count(dest)

# Strive for
flights |>  
  group_by(tailnum) |> 
  summarize(
    delay = mean(arr_delay, na.rm = TRUE),
    n = n()
  )

# Avoid
flights |>
  group_by(
    tailnum
  ) |> 
  summarize(delay = mean(arr_delay, na.rm = TRUE), n = n())



# Strive for 
flights |>  
  group_by(tailnum) |> 
  summarize(
    delay = mean(arr_delay, na.rm = TRUE),
    n = n()
  )

# Avoid
flights|>
  group_by(tailnum) |> 
  summarize(
    delay = mean(arr_delay, na.rm = TRUE), 
    n = n()
  )

# Avoid
flights|>
  group_by(tailnum) |> 
  summarize(
    delay = mean(arr_delay, na.rm = TRUE), 
    n = n()
  )


# -----------------------------------------------------------------------------

# ggplot2

flights |> 
  group_by(month) |> 
  summarize(
    delay = mean(arr_delay, na.rm = TRUE)
  ) |> 
  ggplot(aes(x = month, y = delay)) +
  geom_point() + 
  geom_line()


flights |> 
  group_by(dest) |> 
  summarize(
    distance = mean(distance),
    speed = mean(distance / air_time, na.rm = TRUE)
  ) |> 
  ggplot(aes(x = distance, y = speed)) +
  geom_smooth(
    method = "loess",
    span = 0.5,
    se = FALSE, 
    color = "white", 
    linewidth = 4
  ) +
  geom_point()

# With description

# ---------------------------------------------
# 1st plot: Average arrival delay per month
# ---------------------------------------------
flights |> 
  # Group the data by month
  group_by(month) |> 
  # Calculate summary statistics for each group
  summarize(
    # Compute mean arrival delay, ignoring missing values
    delay = mean(arr_delay, na.rm = TRUE)
  ) |> 
  # Create a ggplot object with month on x-axis and delay on y-axis
  ggplot(aes(x = month, y = delay)) +
  # Add points for each month
  geom_point() + 
  # Connect the points with a line
  geom_line()


# ---------------------------------------------
# 2nd plot: Relationship between distance and average speed per destination
# ---------------------------------------------
flights |> 
  # Group the data by destination airport
  group_by(dest) |> 
  # Calculate summary statistics for each destination
  summarize(
    # Compute mean distance of flights to that destination
    distance = mean(distance),
    # Compute mean speed (distance / air_time), ignoring missing values
    speed = mean(distance / air_time, na.rm = TRUE)
  ) |> 
  # Create a ggplot object with distance on x-axis and speed on y-axis
  ggplot(aes(x = distance, y = speed)) +
  # Add a smooth trend line using loess method
  geom_smooth(
    method = "loess",    # Loess smoothing (local regression)
    span = 0.5,          # Controls smoothness (smaller = more wiggly)
    se = FALSE,          # Do not display confidence interval
    color = "white",     # Line color
    linewidth = 4        # Thickness of the line
  ) +
  # Add points for each destination
  geom_point()


# -----------------------------------------------------------------------------


# The following is not easy to read

flights|>filter(dest=="IAH")|>group_by(year,month,day)|>summarize(n=n(),
                                                                  delay=mean(arr_delay,na.rm=TRUE))|>filter(n>10)

flights|>filter(carrier=="UA",dest%in%c("IAH","HOU"),sched_dep_time>
                  0900,sched_arr_time<2000)|>group_by(flight)|>summarize(delay=mean(
                    arr_delay,na.rm=TRUE),cancelled=sum(is.na(arr_delay)),n=n())|>filter(n>10)

# This is better

# Flights arriving to IAH with at least 10 flights per day
# ✅ Clean and readable version
flights |>
  # Keep only flights to IAH
  filter(dest == "IAH") |>
  # Group data by year, month, and day
  group_by(year, month, day) |>
  # Compute summary statistics for each group
  summarise(
    n = n(),                                     # Number of flights per day
    delay = mean(arr_delay, na.rm = TRUE)       # Average arrival delay
  ) |>
  # Keep only days with more than 10 flights
  filter(n > 10)


# UA flights to IAH or HOU between 9am and 8pm,
# summarised by flight number, with at least 10 flights
flights |>
  # Filter for UA flights to IAH or HOU between 9am and 8pm
  filter(
    carrier == "UA",
    dest %in% c("IAH", "HOU"),
    sched_dep_time > 900,
    sched_arr_time < 2000
  ) |>
  # Group by flight number
  group_by(flight) |>
  # Compute summary statistics for each flight
  summarise(
    delay = mean(arr_delay, na.rm = TRUE),        # Average arrival delay
    cancelled = sum(is.na(arr_delay)),            # Number of cancelled flights
    n = n()                                       # Total number of flights
  ) |>
  # Keep only flights with more than 10 occurrences
  filter(n > 10)









# Penguins dataset

#-----------------------------------------------------------
# PLOT 1: Distribution of Penguin Body Mass
#-----------------------------------------------------------
# This plot shows how the body mass of penguins is distributed
# across the three species (Adelie, Chinstrap, Gentoo).
# It helps students understand how to visualise distributions
# using histograms and compare groups using colour and fill.
#-----------------------------------------------------------penguins |>
  filter(!is.na(body_mass_g)) |>                  # Keep only rows where body_mass_g is not missing
  ggplot(aes(x = body_mass_g, fill = species)) +  # Create ggplot: x-axis is body mass, fill by species
  geom_histogram(bins = 30, alpha = 0.7, color = "white") +  # Histogram with 30 bins, semi-transparent fill
  labs(
    title = "Distribution of Penguin Body Mass",              # Add plot title
    subtitle = "Different species have distinct body mass distributions",  # Add subtitle
    x = "Body mass (grams)",                                  # Label for x-axis
    y = "Number of penguins",                                 # Label for y-axis
    fill = "Species"                                          # Legend title
  ) +
  theme_minimal()      


#-----------------------------------------------------------
# PLOT 2: Relationship Between Bill Length and Bill Depth
#-----------------------------------------------------------
# This scatter plot shows how bill length and bill depth vary
# among penguin species. Each point is a penguin, and colour
# indicates species. The regression lines (method = "lm")
# show linear trends per species, helping students see patterns
# and learn how to fit trend lines using geom_smooth().
#-----------------------------------------------------------
penguins |>
  filter(!is.na(bill_length_mm), !is.na(bill_depth_mm)) |>     # Remove missing values for both variables
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm, color = species)) +  # Scatter plot: color by species
  geom_point(size = 2, alpha = 0.8) +                          # Add points with slight transparency
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +      # Add linear regression lines per species
  labs(
    title = "Bill Length vs Bill Depth by Species",            # Plot title
    subtitle = "Linear trend lines highlight species differences", # Subtitle
    x = "Bill length (mm)",                                    # X-axis label
    y = "Bill depth (mm)",                                     # Y-axis label
    color = "Species"                                          # Legend title
  ) +
  theme_classic()                                              # Use a classic theme for contrast


#-----------------------------------------------------------
# PLOT 3: Average Penguin Body Mass per Island and Species
#-----------------------------------------------------------
# This grouped bar plot shows the mean body mass for each species
# across the three islands. It demonstrates how to summarise data
# using group_by() and summarise(), and how to visualise grouped
# summaries with geom_col() and position = "dodge".
#-----------------------------------------------------------
penguins |>
  filter(!is.na(body_mass_g)) |>                     # Exclude missing values
  group_by(island, species) |>                       # Group data by island and species
  summarise(mean_mass = mean(body_mass_g), .groups = "drop") |>  # Compute average body mass
  ggplot(aes(x = island, y = mean_mass, fill = species)) +       # Create grouped bar plot
  geom_col(position = "dodge") +                     # Side-by-side bars for each species
  labs(
    title = "Average Penguin Body Mass per Island and Species",  # Plot title
    x = "Island",                                   # X-axis label
    y = "Average body mass (grams)",                # Y-axis label
    fill = "Species"                                # Legend title
  ) +
  theme_minimal() +                                 # Minimal theme for clarity
  theme(axis.text.x = element_text(angle = 15, hjust = 1))  # Slightly tilt x-axis labels for readability
