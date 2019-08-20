require(tidyverse)

# car_to_pol <- function(car_coord){
#   x = car_coord[1]
#   y = car_coord[2]
#   r = sqrt(x^2 + y^2)
#   theta = atan2(y, x)
#   return(c(r = r, theta = theta))
#   
# }
# 
# pol_to_car <- function(pol_coord){
#   r = pol_coord[1]
#   theta = pol_coord[2]
#   x = r * cos(theta)
#   y = r * sin(theta)
#   return(c(x = x, y = y))
#   }
# 
# test_point <- c(10, 5)
# car_to_pol(test)
# pol_to_car(car_to_pol(test))
# 
# test_set_size = 100
# test_set <- tibble(x = runif(test_set_size, -1, 1),
#                    y = runif(test_set_size, -1, 1),
#                    r = sqrt(x ^ 2 + y ^ 2),
#                    theta = atan2(y, x))
# 
# test_set %>%
#   ggplot(aes(x = x, y = y)) + 
#   geom_point()
# 
# test_set %>%
#   ggplot(aes(x = theta, y = r)) + 
#   geom_point()
# 
# d_cat <- function(p_1, p_2){
#   
#   sqrt((p_1[1] - p_2[1])^2 + (p_1[2] - p_2[2])^2)
#   
#   }
# 
# d_pol <- function(p_1, p_2){
#   
#   sqrt(p_1[1]^2 + p_2[1]^2 - 2 * p_1[1] * p_2[1] * cos(p_1[2] - p_2[2]))
#   
#   }
# 
# test_point_2 <- c(8, 6)
# 
# d_cat(test_point, test_point_2)
# 
# 
# d_pol(car_to_pol(test_point), car_to_pol(test_point_2))

# let's create an "encircling" set using our polar representation.

require(magrittr)

# encircling_set <- test_set %$%
#   tibble(r = max(r), theta = unique(theta)) %>%
#   mutate(x = r * cos(theta),
#          y = r * sin(theta))
# universal_set <- test_set %>%
#   mutate(set = 'actual') %>%
#   bind_rows(encircling_set %>%
#               mutate(set = 'encircle')) %>%
#   group_by(set) %>%
#   mutate(set_point_id = seq_along(set)) %>%
#   ungroup()
# 
#  universal_set %>%
#   ggplot() +
#   geom_point(aes(x = x, y = y, color = set))
# 
# boundary_points <- universal_set[,c(1,2)] %>%
#   dist() %>%
#   as.matrix() %>%
#   .[which(universal_set$set == 'encircle'), which(universal_set$set == 'actual')] %>%
#   apply(X = ., MARGIN = 1, FUN = which.min) %>%
#   as.numeric() 
# 
# universal_set2 <- universal_set %>%
#   mutate(set = if_else(set == 'actual' & set_point_id %in% boundary_points, 'boundary', set))
# 
# universal_set2 %>%
#   ggplot() +
#   geom_point(aes(x = x, y = y, color = set))
# 
# # a different approach
# 
# test_df <- expand.grid(actual_point = unique(universal_set$set_point_id[universal_set$set == 'actual']), encircling_point = unique(universal_set$set_point_id[universal_set$set == 'encircle'])) %>%
#   left_join(universal_set %>% 
#               filter(set == 'actual') %>%
#               select(x, y, set_point_id), by = c('actual_point' = 'set_point_id')) %>%
#   left_join(universal_set %>% 
#               filter(set == 'encircle') %>%
#               select(x, y, set_point_id), by = c('actual_point' = 'set_point_id')) %>%
#   mutate(d = sqrt((x.x - x.y)^2 + (y.x - y.y)^2)) %>%
#   group_by(encircling_point) %>%
#   filter(d == min(d)) %>%
#   ungroup() %>%
#   select(set_point_id = actual_point, x = x.x, y = y.x) %>%
#   mutate(set = 'boundary')
# 
# universal_set %>%
#   filter(!(set = 'actual' & set_point_id %in% test_df$set_point_id)) %>%
#   bind_rows(test_df) %>%
#   ggplot() +
#   geom_point(aes(x = x, y = y, color = set))

n_points <- 1000
set_of_points <- tibble(x = runif(n = n_points, 0, 1),
                        y = runif(n = n_points, 0, 1),
                        rho = sqrt(x^2 + y^2),
                        theta = atan2(y, x),
                        point_id = 1:n_points)

ggplot(set_of_points) +
  geom_point(aes(x = x, y = y))

ggplot(set_of_points) +
  geom_point(aes(x = theta, y = rho))

set_of_circling_points <- set_of_points %$%
  tibble(rho = max(rho), 
         theta = seq(0,2*pi,.01),
         x = rho * cos(theta),
         y = rho* sin(theta),
         point_id = 1:length(rho))

ggplot() +
  geom_point(data = set_of_points, aes(x = x, y = y)) +
  geom_point(data = set_of_circling_points, aes(x = x, y = y), color = 'red')

ggplot() +
  geom_point(data = set_of_points, aes(x = theta, y = rho)) +
  geom_point(data = set_of_circling_points, aes(x = theta, y = rho), color = 'red') 

combined_points <- bind_rows(set_of_points %>%
                               mutate(set = 'original_points'), 
                             set_of_circling_points %>%
                               mutate(set = 'circle_points'))

boundary_points <- combined_points %>%
  select(x, y) %>%
  dist() %>%
  as.matrix() %>%
  .[which(combined_points$set == 'circle_points'), which(combined_points$set == 'original_points')] %>%
  apply(X = ., MARGIN = 1, FUN = which.min) %>%
  as.numeric() %>%
  unique()

combined_points %>%
  filter(set == 'original_points') %>%
  ggplot() +
  geom_point(aes(x = x, y = y, color = point_id %in% boundary_points)) +
  geom_path(data = combined_points %>%
                 filter(set == 'original_points' & point_id %in% boundary_points) %>%
                 arrange(rho, theta), 
               aes(x = x, y = y))



