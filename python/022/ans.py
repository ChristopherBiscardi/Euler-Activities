with open("names.txt") as f:
  c = f.readlines()

#make file a sorted list
content = c[0].split(",")
content = [x.strip('"').lower() for x in content]
content.sort()
# values will hold the values for each name.
# could be inserted into the original array
values = []
#convert strings to lists of chars
content = [list(x) for x in content]
#for all words
for i in range(0,len(content)):
  #calculate value and append to values list
  values.append((i+1) * sum([ord(x) - 96 for x in content[i]]))

#print sum of values to stdout
print(sum(values))

