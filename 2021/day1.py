# read in the input as list
import numpy as np
# offset the list by 1
#x = np.loadtxt("2021/input/day1_example.txt")
x = np.loadtxt("2021/input/day1_part1.txt")
x = np.insert(x,[0],np.nan)
inc = 0
for ind,val in enumerate(x):
    if ind != 0:
        inc += (val - x[ind - 1] > 0)
        
print(inc)

#part2
y = np.insert(x,[0],[np.nan,np.nan])

inc = 0
prev_slide = np.nan
for ind,val in enumerate(y):
    slide_sum = val + y[ind - 1] + y[ind - 2]
    if not np.isnan(slide_sum):
        if slide_sum > prev_slide:
            inc += 1
        prev_slide = slide_sum
        
print(inc)