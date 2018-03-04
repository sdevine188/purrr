big_df <- mtcars %>% 
        select(cyl, mpg, disp) %>% 
        arrange(cyl) %>% 
        slice(17:22) %>% 
        rename(ID = cyl)
big_df

complex_function <- function(ID, data) {
        tibble(ID = ID, n = nrow(data), half = ID / 2)
}

complex_func2 <- function(ID, data) {
        tibble(
                ID = ID,
                half = ID / 2,
                N = nrow(data)
        )
}

big_df %>% 
        group_by(ID) %>%
        nest() %>%
        pmap_dfr(complex_function)

################################################


# split, apply, combine with pmap
complex_function3 <- function(cyl, data) {
        tibble(id = cyl, n = nrow(data), half = cyl / 2)
}

mtcars %>% group_by(cyl) %>% nest() %>% pmap_dfr(., .f = complex_function3)
mtcars %>% group_by(cyl) %>% nest()
mtcars %>% group_by(cyl) %>% nest() %>% slice(1) %>% unnest()


############################################################33


# pmap with tidy eval
# basic test
test <- function(id_var, df) {
        id_var_sym <- sym(id_var)
        df %>% select(!!id_var_sym)
}
test(id_var = "cyl", df = mtcars)


# test on mtcars
complex_function4 <- function(id_var, data) {
        tibble(id = id_var, n = nrow(data), half = id_var / 2)
}

mtcars_4cyl <- mtcars %>% filter(cyl == 4)
complex_function4(id_var = 6, data = mtcars_4cyl)


# test using tidy eval
complex_function4 <- function(id_var, data) {
        tibble(id = id_var, n = nrow(data), half = id_var / 2)
}

id_var_sym <- sym("cyl")
mtcars %>% mutate(id_var = !!id_var_sym) %>% group_by(id_var) %>% nest() %>% pmap_dfr(., .f = complex_function4)

id_var_sym <- sym("mpg")
mtcars %>% mutate(id_var = !!id_var_sym) %>% group_by(id_var) %>% nest() %>% pmap_dfr(., .f = complex_function4)







################################################













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
