# Open File that contains data.
# This is so it scales to problem 67
with open("tri.data") as f:
  # Read file
  c = f.readlines()
# List comprehension
# strip newline characters off of each line "\n"
# after striping newlines, split line up using spaces as delimiters
content = [str.split(x.rstrip()) for x in c]

# transform each item of each row to int
content = map((lambda row:
  map((lambda foo:
    int(float(foo))),
    row)),
  content)

# maxrow is for code readability later
maxRow = len(content)
# newTri is the new triangle of added values
newTri = []
# append the bottom row of the triangle, because there are no operations on it
newTri.append(content[len(content)-1])

for x in range(0,maxRow-1):
  newTri.append([])
  currentRow = 14-x
  for num in range(0,len(content[currentRow-1])):
    newTri[x+1].append(max(content[currentRow-1][num] + newTri[x][num], content[currentRow-1][num] + newTri[x][num+1]))

# Print the last row of newTri, which is our max-sum
print(newTri[len(newTri)-1])


