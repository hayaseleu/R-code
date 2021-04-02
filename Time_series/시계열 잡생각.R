  x <- rnorm(100,0 , 1 )

ts.plot(x)

test(x)

jarque.bera.test(x)

hist(x)

x <- rpois(100, 10)

ts.plot(x)

diff(x)

test(x)

jarque.bera.test(x)

hist(x)


rbi