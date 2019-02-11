library(tidyverse)
library(purrr)
library(dplyr)
library(rlang)
library(tidyr)


# create new variable using mutate computed by mapping function to an existing variable
# by default, map will apply function to all variables when passed a tbl
# obviously you don't need to map a simple function adding one, but this can handle more complex functions
starwars %>% mutate(height2 = unlist(map(.x = .$height, .f = function(.x, ...) {.x + 1} ))) %>% select(height, height2)
starwars %>% mutate(height2 = unlist(map(.x = .$height, .f = ~ .x + 1))) %>% select(height, height2)


############################################################################


# just showing that you can provide the iterator .x as a named argument using map

# create add_one function
add_one <- function(value) {
        
        return(value + 1)
}

map_dfr(.x = starwars %>% select(mass) %>% head(), .f = add_one)
map_dfr(.x = starwars %>% select(mass) %>% head(), .f = ~ add_one(value = .x))
starwars %>% select(mass) %>% head()


###############################################################################


# show that if you want access to all variables in a function used in pmap,
# the best way is to just pass pmap a nested tbl, which enters function as "data", and has all variables accessible
# you can't automatically access unnames variables from an unnested tbl via having passed the dots

# create add_mass_and_height function
add_mass_and_height_w_error <- function(mass, height, ...) {
        
        # add mass and height
        return(tibble(mass = mass, height = height, eye_color = eye_color, sum = mass + height))
}

pmap_dfr(.l = starwars %>% head(), .f = add_mass_and_height_w_error)


##############


# create nested_add_mass_and_height function
nested_add_mass_and_height <- function(data, ...) {
        
        # add mass and height
        return(tibble(mass = data %>% pull(mass), height = data %>% pull(height),
                      sum = data %>% mutate(sum = mass + height) %>% pull(sum)))
        
}

starwars %>% head() %>% nest() %>% pmap_dfr(.l = ., .f = nested_add_mass_and_height)


###############


# also can explicitly pass named variables you wanted, but it seems less flexible than passing the nested tbl

# create add_mass_and_height function
add_mass_and_height <- function(mass, height, ...) {
        
        # add mass and height
        return(tibble(mass = mass, height = height, sum = mass + height))
}

pmap_dfr(.l = starwars %>% head(), .f = add_mass_and_height)


############################################################################


# build a function that operates differently depending on the data used as an input
# data can be input as argument using a string (and so does not need to be hard-coded)
# map this function, with the data_input as an argument
# the list that map iterates over is not part of the data, but instead a seperate list (variable names in this case)

# split starwars into two datasets
starwars1 <- starwars %>% slice(1:50) %>% mutate(starwars1_only_var = runif(n = nrow(.), min = 0, max = 100))
glimpse(starwars1)

starwars2 <- starwars %>% slice(51:nrow(.)) %>% mutate(starwars2_only_var = runif(n = nrow(.), min = 0, max = 100))
glimpse(starwars2)

# create list of variables to iterate over
var_list <- c("starwars1_only_var", "starwars2_only_var")

# create strings to reference datasets
data_input1 <- "starwars1"
data_input2 <- "starwars2"

# create function 
get_avg_of_var_depending_on_data_input <- function(var, data_input) {
        
        # get data
        data <- eval(parse(text = data_input))
        
        # get current_var_name_sym
        current_var_name <- var
        current_var_name_sym <- sym(current_var_name)
        
        # check if data contains current_var_name, and if so get avg for current_var_name
        if(current_var_name %in% names(data)) {
                data %>% summarize(avg = mean(!!current_var_name_sym))
        }
}

# apply get_avg_of_var_depending_on_data_input function
# note you can name the iterator .x in the function
map_dfr(.x = var_list, .f = ~ get_avg_of_var_depending_on_data_input(var = .x, data_input = data_input1))
map_dfr(.x = var_list, .f = ~ get_avg_of_var_depending_on_data_input(var = .x, data_input = data_input2))


############################################################################


# https://gist.github.com/jennybc/e7da3b1be68be611a16ea64f573537ee

# row operations on a dataframe, 
# similar functionality to mutate, but could include more advanced functions

test_tbl <- tibble(var1 = c(1, 2, 3), var2 = c(4, 5, 6), var3 = c(7, 8, 9), 
                   var4 = c("123, 456, 789", "234, 567, 890", "345, 678, 901"))
test_tbl

combine_var1_and_var2 <- function(var1, var2, ...){
        var1 + var2
}


pmap(.l = test_tbl, .f = combine_var1_and_var2)
test_tbl %>% pmap(.l = ., .f = combine_var1_and_var2)
test_tbl %>% pmap(.l = ., .f = function(var1, var2, ...) { var1 + var2 })
test_tbl %>% pmap(.l = ., .f = combine_var1_and_var2) %>% unlist() %>% tibble(combined_var = .)


###################################################


# create a counter, or other placeholder variable, in purrr using the <<- assignement operator to push 
# <<- pushes assignment up one parent environment
# this is useful in purrr functions, since a variable assigned in a loop will not retain value in subsequent iterations
# instead, the variable will always re-initialize each iteration at its value in the parent environment

# initialize counter outside function
counter <- 0
# rm(counter)

# create update_counter function
update_counter <- function(...) {
        
        # update counter
        # using normal assignment operator will result in counter always = 1, because it initializes each loop = 0
        # counter <- counter + 1
        counter <<- counter + 1
        counter
}

# call function
map(.x = 10:15, .f = update_counter)


###################################################


# use pmap to iterate through rows of tibble, updating a placeholder value based on most recent cell value

# for silly example, say i want to add variable for what "team" starwars characters are on
# and that the team is decided by human captains; team number = human's height
# so every human record is the captain for all records until the next human
starwars

# need to define current_team outside of the function, so that it can be overwritten and retained during loops
current_team <- NA
# rm(current_team)

# create assign_team function
assign_team <- function(name, species, height, ...) {

        # overwrite current_team for each new Human captain
        # note use of <<- assignment operator to push up one parent environment
        if(!is.na(species) & species == "Human") {
                current_team <<- height
        }

        # create output tibble showing each character and their team
        output <- tibble(name = name, species = species) %>% mutate(team = current_team)
        output
        
}

# call assign_team function
starwars_w_teams <- pmap_dfr(.l = starwars, .f = assign_team)
starwars_w_teams


###########################################################################


# use pmap with groups using nested tbls to conduct operations
# you may be able to use group_by/summarize, or mutate/case_when to accomplish same thing
# but it seems useful to have the ability to fully control the grouped tbls, instead of 
# being reliant on handling them through summarize or mutate

# also added a progress counter showing percentage of groups completed
# using preserve_order_for_group_indices() function in the assorted_helper_scripts folder
# since group_indices by default orders groups alphabetically instead of using order of appearance in data
# create preserve_order_for_group_indices function
preserve_order_for_group_indices <- function(x) {
        match(x, unique(x))
}

# get toy data
tbl <- starwars %>% select(name, mass, species)

# as a nonsensical example, say i want to collapse all the records where species = droid
# into a single record, with their mass summed
# this would take some thought using summarize / mutate, but is straightforward with nest/pmap

# create collapse_droid_records function
collapse_droid_records <- function(species, data, ...) {
        
        # get pct_groups_completed
        pct_groups_completed <- (data %>% slice(1) %>% pull(group_index) / 
                data %>% slice(1) %>% pull(group_index_count)) * 100
        
        # print pct progress
        print(str_c(round(pct_groups_completed, digits = 2), "% completed"))
        
        # get current_species
        # could also just reference species instead of grouping_var
        current_species <- data %>% distinct(grouping_var) %>% pull(grouping_var)
        
        # collapse droid records according to any arbitrary rules
       if(current_species == "Droid" & !is.na(current_species)) {

               name <- "skynet"
               mass <- data %>% summarize(mass_sum = sum(mass, na.rm = TRUE)) %>% pull(mass_sum)
               grouping_var <- data %>% distinct(grouping_var) %>% pull(grouping_var)

               # combine and return output
               output <- tibble(name = name, mass = mass, grouping_var = grouping_var)
               return(output)

       } else {
               # otherwise if species/grouping_var != droid, just return original tbl
               return(data)
       }
}

tbl %>% mutate(grouping_var = species) %>%
        mutate(group_index = group_indices(., grouping_var) %>% preserve_order_for_group_indices(),
               group_index_count = n_distinct(group_index)) %>% group_by(species) %>% nest() %>% 
        pmap_dfr(.l = ., .f = collapse_droid_records) %>%
        rename(species = grouping_var) %>% filter(species %in% c("Human", "Droid")) %>% data.frame()


#######################################################


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


# use pmap inside mutate to generate multiple duplicate rows
params <- expand.grid(param_a = c(2, 4, 6)
                      ,param_b = c(3, 6, 9)
                      ,param_c = c(50, 100)
                      ,param_d = c(1, 0))
params

as.tbl(params) %>% mutate(test_var = pmap(., function(param_a, param_b, ...){ rep(5, times = param_a) * param_b })) %>% unnest()


#################################################


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


############################################################


# map a function within a mapped function, passing a variable created from outer function into the inner function
test1 <- c("test nevada", "test texas", "test alaska", "test maine", "test florida")
test2 <- c("nevada", "alaska", "alabama")

inner_function <- function(.x, .y){
        current_test1 <- .x
        current_test2 <- .y
        str_detect(string = current_test1, pattern = regex(current_test2, ignore_case = TRUE))
}

outer_function <- function(.x) {
        current_test1 <- .x
        map2(.x = current_test1, .y = test2, .f = inner_function)
}

map(.x = test1, .f = outer_function)


###################################################
################################################
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