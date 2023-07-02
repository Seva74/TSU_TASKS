import matplotlib.pyplot as plt
import numpy as np

# Define the equations of the lines
def line1(x):
    return (7 - 2 * x) / 3

def line2(x):
    return (-2 + 4 * x) / 2

# Generate x values
x = np.linspace(-5, 5, 100)

# Plot the lines
plt.plot(x, line1(x), label='2x + 3y = 7')
plt.plot(x, line2(x), label='-4x + 2y = -2')

# Set plot title and labels
plt.title('Intersection of Two Lines')
plt.xlabel('x')
plt.ylabel('y')

# Add legend
plt.legend()

# Show the plot
plt.grid(True)
plt.show()
