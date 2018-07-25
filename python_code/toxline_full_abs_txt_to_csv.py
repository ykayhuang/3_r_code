# -*- coding: utf-8 -*-
"""
Created on Mon Jun 25 16:57:09 2018

@author: YK Huang
"""
#C:\Users\YK Huang\Downloads -> into the low ones
with open("/Users/YK Huang/Downloads/toxnet (9).txt", "r") as ins:
    array = []
    for line in ins:
        array.append(line)

      
        # Python code to count the number of occurrences
def countX(lst, x):
    return lst.count(x)

def lsrs(x):
    return x.rstrip().lstrip()

#loop setting
a=array
l = range(countX(array,"TITLE:\n"))
#loop start
i = 0 # 設定控制變數
while i < countX(array,"TITLE:\n"):
    ftiina=a.index("TITLE:\n")
    b=a[ftiina+1:]
    if b.index("AUTHORS:\n") <5:
        fahinb=b.index("AUTHORS:\n")
    else:
        fahinb=5
    c=b[:fahinb-1]
    c2=map(lsrs,c)
    d= ' '.join(c2)
    a=b
    l[i]=d
    i = i+1 # 調整控制變數值
#loop ends

#write a csv file
import csv
with open("/Users/YK Huang/Downloads/toxnet(9).csv", "w") as output:
    writer = csv.writer(output, lineterminator='\n')
    for val in l:
        writer.writerow([val])    