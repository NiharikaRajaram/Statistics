# setting up 10000 runs
runs <- 10000

# defining coordinates of the square
# points are randomly generated within this region for uniform distribution
square_x = runif(runs, min=0, max=1)
square_y = runif(runs, min=0, max=1)

# defining the condition which all the points inside the circle should satisfy
circle_points = (square_x-0.5)^2 + (square_y-0.5)^2 <= (0.5)^2

# as per explanation of pi
pi = 4 * sum(circle_points) / runs

print(pi)

# plot the square and circle
plot(square_x, square_y,
     pch='.',
     asp=1,
     xlab='x',
     ylab='y',
     col=ifelse(circle_points, "red", "green"),
     main = "Estimating pi using Monte Carlo"
     )
