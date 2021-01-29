# Finding shortest path
# Ko Byeongmin
# Jan 26 2021

# functions

greedy <- function(costdata) { # calculates optimal path with greedy algorithm
    
    num.row <- dim(costdata)[1]
    num.col <- dim(costdata)[2]
    
    path = rbind(c(1,1))
    
    i <- 1
    j <- 1
    
    while (i < num.row & j < num.col) {
        if (costdata[i+1, j] > costdata[i, j+1]) {
            j <- j+1
            path = rbind(path, c(i, j))
        } else {
            i <- i+1
            path = rbind(path, c(i, j))
        }
    }
    
    if (i == num.row) {
        while (j < num.col) {
            j <- j+1
            path = rbind(path, c(i, j))
        }
    }
    
    if (j == num.col) {
        while (i < num.row) {
            i <- i+1
            path = rbind(path, c(i, j))
        }
    }
    
    totalexpense = 0
    
    for (i in 1:dim(path)[1]) {
        totalexpense = totalexpense + costdata[path[i, 1], path[i, 2]]
    }
    
    return(list("grid" = costdata, "path" = path, "totalexpense" = totalexpense))
    
}

expensecalc <- function (costdata) { # calculates optimal path recursively
    
    num.row <- dim(costdata)[1]
    num.col <- dim(costdata)[2]
    expense <- matrix(
        nrow = num.row,
        ncol = num.col,
        data = 0
    )

    for (i in 1:(num.col-1)) {
        expense[num.row, (num.col-i)] =
            costdata[num.row, (num.col-i)] +
            expense[num.row, (num.col-i+1)]
    }
    
    for (i in 1:(num.row-1)) {
        expense[(num.row-i), num.col] =
            costdata[(num.row-i), num.col] +
            expense[(num.row-i+1), num.col]
    }
    
    for (j in 1:(num.col-1)) {
        for (i in 1:(num.row-1)) {
            if (expense[(num.row-i), (num.col-j+1)] >= expense[(num.row-i+1), (num.col-j)]) {
                expense[(num.row-i), (num.col-j)] = expense[(num.row-i+1), (num.col-j)] +
                    costdata[(num.row-i), (num.col-j)]
            } else {
                expense[(num.row-i), (num.col-j)] = expense[(num.row-i), (num.col-j+1)] +
                    costdata[(num.row-i), (num.col-j)]
            }
        }
    }

    path = rbind(c(1,1))
    
    i <- 1
    j <- 1
    
    while (i < num.row & j < num.col) {
        if (expense[i+1, j] > expense[i, j+1]) {
            j <- j+1
            path = rbind(path, c(i, j))
        } else {
            i <- i+1
            path = rbind(path, c(i, j))
        }
    }
    
    if (i == num.row) {
        while (j < num.col) {
            j <- j+1
            path = rbind(path, c(i, j))
        }
    }
    
    if (j == num.col) {
        while (i < num.row) {
            i <- i+1
            path = rbind(path, c(i, j))
        }
    }
    
    totalexpense = 0
    
    for (i in 1:dim(path)[1]) {
        totalexpense = totalexpense + costdata[path[i, 1], path[i, 2]]
    }
    
    return(list("grid" = costdata, "path" = path, "totalexpense" = totalexpense))
    
}

## showcase

costdata <- matrix(
    data = c(2, 5 ,3, 8, 6,
    4, 2, 9, 4, 1,
    5, 3, 2, 6, 9,
    0, 3, 8, 5, 0),
    nrow = 4,
    ncol = 5,
    byrow = T
)

print(costdata)

myway = expensecalc(costdata)
myway$path
myway$totalexpense
myanotherway = greedy(costdata)
myanotherway$path
myanotherway$totalexpense

## performance review

testcost <- function(nrow, ncol, times) {
        
    diff = 0
   
     for (trial in 1:times) {
        
        costdata <- matrix(
            data = sample(x = 0:15, size = nrow*ncol, replace = TRUE),
            nrow = nrow,
            ncol = ncol
        )
        
        diff = c(diff, (greedy(costdata)$totalexpense - expensecalc(costdata)$totalexpense))
    }

    return(diff[-1])
}

mytest = testcost(10, 12, 1000)
plot(mytest, type = "l",
     main = "Differnce of costs: how bad is the greedy algorithm?",
     sub = "Done on 10 by 12 Grid. Costs sampled from 0 to 15",
     xlab = "Trial", 
     ylab = "Difference in costs")
abline(h = mean(mytest), col = "red")
mean(mytest)