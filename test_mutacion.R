source("mutacion.R")

test_pop = rbind( c(1,1,1,1,1),
                  c(2,2,2,2,2),
                  c(3,3,3,3,3),
                  c(4,4,4,4,4),
                  c(5,5,5,5,5))
test_pop = list(c(1,1,1,1,1),
                c(2,2,2,2,2),
                c(3,3,3,3,3),
                c(4,4,4,4,4),
                c(5,5,5,5,5))
mutated_pop <- mutacion(test_pop, 1, 5, 1, 5, -2, 2, 4)
print(mutated_pop)