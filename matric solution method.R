install.packages("lpSolveAPI")


#question 2
library(lpSolveAPI)

lp_question2 <- make.lp(9, 9)

lp.control(lp_question2, sense= "maximize")

set.objfn(lp_question2, c(25,	10,	5, 22,	7,	2,	25,	10,	5))

set.row( lp_question2,1 , c(1, 1, 1),indices= c (1, 2, 3))
set.row( lp_question2,2 , c(1, 1, 1),indices= c (4, 5, 6))
set.row( lp_question2,3 , c(1, 1, 1),indices= c (7, 8, 9))
set.row( lp_question2,4 ,c(0.45, -0.55, -0.55),indices= c (1, 2, 3))
set.row( lp_question2,5 ,c(0.55, -0.45, -0.45),indices= c (4, 5, 6))
set.row( lp_question2,6 ,c(0.7, -0.3, -0.3),indices= c (7, 8, 9))
set.row( lp_question2,7 ,c(-0.3, 0.7, -0.3),indices= c (1, 2, 3))
set.row( lp_question2,8 ,c(-0.4, 0.6, -0.4),indices= c (4, 5, 6))
set.row( lp_question2,9 ,c(-0.5, 0.5, -0.5),indices= c (7, 8, 9))
set.rhs( lp_question2, c(3300, 3600,	4000, 0,	0,	0,	0, 0,	0))
set.constr.type(lp_question2, c("<=","<=","<=",">=",">=",">=",">=",">=",">="))

set.type(lp_question2, c(1:9) , "real")

set.bounds(lp_question2, lower = rep(0, 9), upper = rep(Inf, 9))

solve(lp_question2)
optimal_profit <- get.objective(lp_question2)
optimal_value <- get.variables(lp_question2)

#result
optimal_profit
optimal_value
sum( optimal_value[c(1, 2, 3)])
sum( optimal_value[c(4, 5, 6)])
sum( optimal_value[c(7, 8, 9)])


#Question 3
library(lpSolveAPI)

#Player1

lp_question3 <- make.lp(0,7) #7 constain and 0 variable

lp.control(lp_question3,sense="maximize")  

set.objfn(lp_question3,c(0,0,0,0,0,0,1))

#add matrix of player 1
add.constraint(lp_question3, c(0, 0, 40, 0, 0, -40, 1), "<=" ,0)
add.constraint(lp_question3, c(0, 0, 0, 40, -40, 0, 1), "<=" ,0)
add.constraint(lp_question3, c(-40, 0, 0, 0, 0, 40, 1), "<=" ,0)
add.constraint(lp_question3, c(0, -40, 0, 0, 40, 0, 1), "<=" ,0)
add.constraint(lp_question3, c(0, 40, 0, -40, 0, 0, 1), "<=" ,0)
add.constraint(lp_question3, c(40, 0, -40, 0, 0, 0, 1), "<=" ,0)
add.constraint(lp_question3, c(1, 1, 1, 1, 1, 1, 0), "=", 1)

set.bounds(lp_question3, lower = c(0,0,0,0,0,0, -Inf))

Row_Q3 <- c("Row1", "Row2", "Row3", "Row4", "Row5", "Row6", "Row7")

Col_Q3 <- c("x1","x2","x3","x4","x5","x6","v")

dimnames(lp_question3) <- list(Row_Q3, Col_Q3)

#result of player 1
lp_question3
solve(lp_question3) 
get.objective(lp_question3)
get.variables(lp_question3)
get.constraints(lp_question3)

#Player2

lp_question3 <- make.lp(0,7)

lp.control(lp_question3,sense="minimize")  

set.objfn(lp_question3, c(0,0,0,0,0,0,1))

#add matrix of player 2
add.constraint(lp_question3, c(0, 0, -40, 0, 0, 40, 1), ">=" ,0)
add.constraint(lp_question3, c(0, 0, 0, -40, 40, 0, 1), ">=" ,0)
add.constraint(lp_question3, c(40, 0, 0, 0, 0, -40, 1), ">=" ,0)
add.constraint(lp_question3, c(0, 40, 0, 0, -40, 0, 1), ">=" ,0)
add.constraint(lp_question3, c(0, -40, 0, 40, 0, 0, 1), ">=" ,0)
add.constraint(lp_question3, c(-40, 0, 40, 0, 0, 0, 1), ">=" ,0)
add.constraint(lp_question3, c(1, 1, 1, 1, 1, 1, 0), "=", 1)

set.bounds(lp_question3, lower = c(0, 0, 0, 0, 0, 0, -Inf))

Row_Q3 <- c("Row1","Row2","Row3","Row4","Row5","Row6","Row7")

Col_Q3 <- c("y1","y2","y3","y4","y5","y6","v")

dimnames(lp_question3) <- list(Row_Q3, Col_Q3)

#result of player 2
lp_question3
solve(lp_question3) 
get.objective(lp_question3)
get.variables(lp_question3)
get.constraints(lp_question3)