
# coding: utf-8

# In[206]:

import re
import datetime
import matplotlib.pyplot as plt
# %matplotlib inline 


# The data is hosted here: https://snap.stanford.edu/data/amazon-meta.html
# This is an example of the reviews data. It can be applied to any online store or seller.

f = open('amazon-meta.txt', 'rU')

"""
Find how often people review an item. 
Longer period between reviews means that the item becames less popular.
Better promotion will increase sells, i.e. reviews are more often appear.
This is an example for an item with 500 reviews.
"""

review = []
for line in f:   
    if re.search(r'reviews: total: 500 ', line):
#         print line,
#         review.append(line)
        nextLine = line
        while not nextLine in ['\n', '\r\n']:
#             print nextLine,
            review.append(nextLine)
            nextLine = f.next()
              
        break

dates = []
for i in range(len(review)):
    match = re.search(r'\w+-\w+-\w+', review[i])
    if match:
        trueDate = datetime.datetime.strptime(match.group(), "%Y-%m-%d")
        date = trueDate.date()
        dates.append(date)

datesDiff = []
for i in range(len(dates)-1):
    diff = abs((dates[i+1] - dates[0]).days)
    datesDiff.append(diff)
plt.plot(datesDiff, range(len(datesDiff)))
plt.title('Trend of item reviews')
plt.xlabel('Review date (from the first review) in days')
plt.ylabel('Number of reviews')
# plt.show()
plt.savefig('ReviewsTrend.png', dpi=300)



"""
Now find average number of reviews per every group.
Less number means less popular group, therefore it needs more promotions.
This is an example for groups of items only. It can be applied to categories, genres, etc.
"""

bookRev = 0
musicRev = 0
dvdRev = 0
videoRev = 0

# Numbers are from the description
numBook = 393561
numDVD = 19828
numMusic= 103144
numVideo = 26132

for line in f: 
    matchBook = re.search(r'\s+group:\sBook\s+', line)
    matchMusic = re.search(r'\s+group:\sMusic\s+', line)
    matchDVD = re.search(r'\s+group:\sDVD\s+', line)
    matchVideo = re.search(r'\s+group:\sVideo\s+', line)

    if matchBook:
        nextLine = line
        matchRev = re.search(r'reviews:\stotal:\s(\w+)\s.', nextLine)
        while not matchRev:
            nextLine = f.next() 
            matchRev = re.search(r'reviews:\stotal:\s(\w+)\s', nextLine)
            if matchRev:
                bookRev = bookRev + int(matchRev.group(1))
    
    if matchMusic:
        nextLine = line
        matchRev = re.search(r'reviews:\stotal:\s(\w+)\s.', nextLine)
        while not matchRev:
            nextLine = f.next() 
            matchRev = re.search(r'reviews:\stotal:\s(\w+)\s', nextLine)
            if matchRev:
                musicRev = musicRev + int(matchRev.group(1))
                
    if matchDVD:
        nextLine = line
        matchRev = re.search(r'reviews:\stotal:\s(\w+)\s.', nextLine)
        while not matchRev:
            nextLine = f.next() 
            matchRev = re.search(r'reviews:\stotal:\s(\w+)\s', nextLine)
            if matchRev:
                dvdRev = dvdRev + int(matchRev.group(1))
                
    if matchVideo:
        nextLine = line
        matchRev = re.search(r'reviews:\stotal:\s(\w+)\s.', nextLine)
        while not matchRev:
            nextLine = f.next() 
            matchRev = re.search(r'reviews:\stotal:\s(\w+)\s', nextLine)
            if matchRev:
                videoRev = videoRev + int(matchRev.group(1))
    
bb = bookRev / numBook
mm = musicRev / numMusic
dvd = dvdRev / numDVD
vv = videoRev / numVideo
x= [bb, mm, dvd, vv]


width = 1
plt.bar(range(4), x, width)
plt.title('Plot of avegage number of reviews per group')
plt.xlabel('Books, Music CDs, DVDs, Videos')
plt.ylabel('Reviews per item')
# plt.show()
plt.savefig('ReviewsPerGroup.png', dpi=300)


f.close()


# In[ ]:



