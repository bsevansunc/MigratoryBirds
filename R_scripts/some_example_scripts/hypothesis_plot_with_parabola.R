# I used this script to draw hypotheses for my
# AOU presentation

# Create function for parabola (using the logistic
# formula for density-dependent growth here):

parabola = function(r,k,n) r*n*((k-n)/k)

# Define values for r, k, and n

r = .3
k = 100
n = 1:100

# Plot the relationship (making axes blank):

plot(parabola(r,k,n)~n, type = 'l', lwd = 3, 
     ylim = c(-1,8),
     xaxt = 'n', yaxt = 'n',bty = 'n',
     xlab  = '',ylab = '')

# Add the axes, note that "expression(phi)" draws the 
# symbol phi:

title(ylab = expression(phi), line = 0.5, cex.lab = 3)
title(xlab = '% Impervious surface', line = 1, cex.lab = 2)

# Add a box (L-shaped) around the plot: 

box (bty = 'l', lwd =3)

# Add a text (with no box):

legend('topright','H1', bty = 'n', cex = 2)

# As above, but this time with a negative linear 
# relationship:

plot(y~n, type = 'l', lwd = 3, ylim = c(-1,8),
     xaxt = 'n', yaxt = 'n',bty = 'n',
     xlab  = '',ylab = '')
  title(ylab = expression(phi), line = 0.5, cex.lab = 3)
  title(xlab = '% Impervious surface', line = 1, cex.lab = 2)
  box (bty = 'l', lwd =3)
  legend('topright','H2', bty = 'n', cex = 2)
