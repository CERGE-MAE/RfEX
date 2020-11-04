# Task 1:
# BRIEFLY explain the output of the following commands.
# Is there anything interesting about about it?
# What do these two structures have in common?
# (You can provide additional examples to support you explanation)
c(1, 5, "6", Sys.time(), T, NA)
matrix(c(1, 5, "6", Sys.time(), T, NA), 2, 3)
# The first command creates vector, while the second one creates a matrix. Both
# structures can only contain one type of variable, the additional
# information stored in matrix is the shape of the structure
# (matrix is basically a vector that remembers how it should fold into the given dimensions).
# The interesting process happening at the background is class/type coercion, 
# since there is a character (highest order type), all other values are coerced into
# characters. In case of Sys.time(), this coercion is, as follows:
# POSIXct -> numeric (number of seconds from 1970-01-01 00:00 ETC) -> character;
# which is the reason why there is a huge number instead of date-time.
# NA values are never coerced to anything.

#-------------------------------------------------------------------------------

# Task 2:
# Using predefined vectors bellow, test whether;
#  1.) Any words in the same position are the same in all 3 vectors.
#  2.) Any words are same in at least 2 vectors.
#  3.) Any word from v1 is the same with at only one other vector (not both!!!).
v1 = c("R", "is", "Amazing")
v2 = c("R", "is", "good")
v3 = c("Python", 'is', 'fine')

# 1.)
v1 == v2 & v2 == v3
# 2.)
v1 == v2 | v2 == v3 | v3 == v1
# 3.)
(v1 == v2 & v1 != v3) | (v1 == v3 & v1 != v2)

#-------------------------------------------------------------------------------

# Task 3;
# Using predefined structures bellow, output:
# 1.) 3rd position of vector v4
# 2.) 5th row of 2nd columnd of matrix m1
# 3.) 'y1' column from dataframe in the 'ans' position of list l1
#         - do not use numbers for slicing in this case
# 4.) 5th row of the 'drat' column from dataframe df1
#         - without using the column name as reference
#         - do not just hardcode the column position
v4 = c("A", "C", "B", "D", "G", "F")
m1 = matrix(seq(1,24,1), 6, 4)
l1 = list(a = mtcars, b = chickwts, ans = anscombe)
df1 = mtcars

# 1.)
v4[3]
# 2.)
m1[5,2]
# 3.)
l1[["ans"]]["y1"]
# 4.)
df1[names(df1) == "drat"][5,]

#-------------------------------------------------------------------------------

# Task 4:
# From df1 (defined in previous task), choose the following cells:
#   filter cars with 4 cylinders and 1 carb and nonzero am having drat higher than or equal to 4
#   select mpg, disp and wt columns
library(dplyr)
df1 %>% 
    filter(cyl == 4, carb == 1, am != 0, drat >= 4) %>%
    select(mpg, disp, wt)

#-------------------------------------------------------------------------------

# Task 5:
# 1.) For the factor f1 defined bellow:
#   1.1) change the levels (based on the first letter of the word) into:
#         pre-school, elementary, high, grammar, university, NotApplicable
#   1.2) order them according to highest achieved education
# 2.) Cast factor f2 into numeric keeping the original values.
f1 = factor(sample(c("P", "E", "H", "G", "U", "NA"), 50, replace = T))
f2 = factor(sample(1:10, 30, replace = T))

# 1.1)
levels(f1) <- c("elementary", "grammar", "high", "NotApplicable", "pre-school", "university")
# 1.2)
ordered(f1, levels = c("pre-school", "elementary","high","grammar", "university", "NotApplicable"))
# 2.)
as.numeric(as.character(f2))
