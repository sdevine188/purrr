library(purrr)

# https://gist.github.com/jennybc/e7da3b1be68be611a16ea64f573537ee

big_df <- mtcars %>% 
        select(cyl, mpg, disp) %>% 
        arrange(cyl) %>% 
        slice(17:22) %>% 
        rename(ID = cyl)
big_df

complex_function <- function(ID, data) {
        tibble(ID = ID, n = nrow(data), half = ID / 2)
}

big_df %>% 
        group_by(ID) %>%
        nest() %>%
        pmap_dfr(complex_function)

# note that pmap_dfr automatically unnests the input for you, so unnest() in the function is unnecessary
complex_function2 <- function(ID, data) {
        # data2 <- data %>% unnest()
        data2 <- data
        data2 <- data2 %>% mutate(new_col = "yes")
        data2
}

big_df %>% 
        group_by(ID) %>%
        nest() %>%
        pmap_dfr(complex_function2)

# use ... to absorb other column names so they don't need to be listed as explicit arguments to function
complex_function2 <- function(data, ...) {
        # data2 <- data %>% unnest()
        data2 <- data
        data2 <- data2 %>% mutate(new_col = "yes")
        data2
}

big_df %>% 
        group_by(ID) %>%
        nest() %>%
        pmap_dfr(complex_function2)


############################################################


# testing w tidy eval

# note if you unnest the tibble with group_by var and data, you get df that retains group_by var 
big_df %>% 
        group_by(ID) %>%
        nest() %>% slice(1) %>% unnest()

# note if you save just the data and then unnest it, you no longer have the group_by var
data <- big_df %>% 
        group_by(ID) %>%
        nest() %>% slice(1) %>% select(data)
data
data %>% unnest()

specific_var <- "mpg"
complex_function2 <- function(data, ...) {
        # data2 <- data %>% unnest()
        specific_var_sym <- sym(specific_var)
        data2 <- data
        data2 <- data2 %>% mutate(new_col = "yes") %>% select(!!specific_var_sym, new_col)
        data2
}

big_df %>% 
        group_by(ID) %>%
        nest() %>%
        pmap_dfr(complex_function2, .id = "grouping_var")

# renaming grouping variable so we can access it inside function using tidy eval
complex_function2 <- function(grouping_var, data) {
        specific_var_sym <- sym(specific_var)
        data %>% mutate(grouping_variable = grouping_var, n = nrow(data), half = grouping_var/2, 
                        specific_var2 = !!specific_var_sym + 1)
}

specific_var <- "mpg"
grouping_var_sym <- sym("ID")

big_df %>% rename(grouping_var = !!grouping_var_sym) %>%
        group_by(grouping_var) %>%
        nest() %>%
        pmap_dfr(.l = ., .f = complex_function2, .id = "originated_from_which_grouped_df")


################################################


# simple iteration over list like a for loop using map
starwars %>% map(.x = 1:5, .f = ~ rep("test", times = 3))
starwars %>% map(.x = 1:5, .f = ~ rep("test", times = 3)) %>% unlist()


####################################################


# use map2 to creat tibble with tidy eval assigning the variable names and values
var_names <- c("new_var1", "new_var2")
values <- list(vector1 = c(1, 2, 3, 4), vector2 = c(5, 6, 7, 8))

create_tbl <- function(.x, .y, ...){
        var_name_sym <- sym(.x)
        tibble(!!var_name_sym := .y)
}
map2(.x = var_names, .y = values, .f = create_tbl)
map2(.x = var_names, .y = values, .f = create_tbl) %>% bind_cols(.)


#################################################


# split, apply, combine with pmap
complex_function3 <- function(cyl, data) {
        tibble(id = cyl, n = nrow(data), half = cyl / 2)
}

mtcars %>% group_by(cyl) %>% nest() %>% pmap_dfr(., .f = complex_function3)
mtcars %>% group_by(cyl) %>% nest()
mtcars %>% group_by(cyl) %>% nest() %>% slice(1) %>% unnest()


# retaining the original dataframe
complex_function3.5 <- function(cyl, data) {
        data %>% mutate(id = cyl, n = nrow(data), half = cyl/2)
}
mtcars %>% group_by(cyl) %>% nest() %>% pmap_dfr(., .f = complex_function3.5) %>% data.frame()


################################################


# random vignettes for tibble

df <- tibble(
        x = 1:3,
        y = c("a", "d,e,f", "g,h")
)
df

df %>% mutate(y = strsplit(y, ",")) %>% unnest(y)
glimpse(df %>% mutate(y = strsplit(y, ",")))

df %>% mutate(y = strsplit(y, ",")) %>% unnest()


df <- tibble(
        a = list(c("a", "b"), "c"),
        b = list(1:2, 3),
        c = c(11, 22)
)
df
df %>% unnest()
df %>% unnest(a, .preserve = b)

df <- tibble(
        x = 1:2,
        y = list(a = 1, b = 3:4),
        z = list(set1 = 2, set2 = 5:6),
        w = list(name1 = 7, name2 = 9)
)
df
df %>% unnest()
df %>% unnest(., .preserve = c(y, w))
unnest(df, .preserve = c(y, z), .id = "name")


df <- tibble(
        a = list(c("a", "b"), "c"),
        b = list(1:2, 3),
        d = list(1:2, 3:4),
        c = c(11, 22)
)
df
df %>% unnest()
df %>% unnest(a, b)


df <- tibble(
        x = 1:2,
        y = list(
                tibble(z = 1),
                tibble(z = 3:4)
        )
)
df
df %>% unnest()

df <- tibble(x = 1:2, y = tibble(v1 = starwars$homeworld, v2 = starwars$species, v3 = starwars$name), 
             z = tibble(v1 = mtcars$mpg, v2 = mtcars$cyl))