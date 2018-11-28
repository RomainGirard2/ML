# -*- coding: utf-8 -*-
"""
Created on Fri Sep 28 09:35:29 2018

@author: user
"""

import numpy as np
from collections import Counter

"""
1. Divide the data in two groups: training and test examples.
2. Parse both the training and test examples to generate both the spam and ham data sets.
3. Generate a dictionary from the training data.
4. Extract features from both the training data and test data.
5. Implement the Naive Bayes from scratch, fit the respective models to the training data.
6. Make predictions for the test data.
7. Measure the spam-filtering performance for each approach through the confusion matrix.
8. Discuss your results.
"""

NumberOfLine = 0
i = 0
j = 0
k = 0
z = 0

trainLine = []
testLine = []

dictionary = []

""" 1. Divide the data in two groups: training and test examples. """

fichier = open("messages.txt","r")
for line in fichier :
    NumberOfLine += 1
fichier.close()


fichier = open("messages.txt","r")
for line in fichier :
    if i < (NumberOfLine/5)*4 :
        trainLine.append(line)
        i = i+1
    else:
        testLine.append(line)
        i = i+1
fichier.close()

""" 2. Parse both the training and test examples to generate both the spam and ham data sets. """

train_labels = np.zeros(int((NumberOfLine/5)*4))
for mess in trainLine :
    if mess[0:4] == "spam":
        train_labels[j] = 1
    j = j+1

test_labels = np.zeros(int(NumberOfLine/5))
for mess in testLine :
    if mess[0:4] == "spam":
        test_labels[k] = 1
    k = k+1

""" 3. Generate a dictionary from the training data. """

all_words = []
for line in trainLine :
    words = line.split()
    all_words += words
all_words = [i for i in all_words if i != "spam"]
all_words = [i for i in all_words if i != "ham"]
dictionary = Counter(all_words)
dictionary = dictionary.most_common(3000)

""" 4. Extract features from both the training data and test data. """

docID = 0
features_matrix1 = np.zeros((int((NumberOfLine/5)*4), 3000))
for line in trainLine :
    words = line.split()
    for word in words :
        wordID = 0
        for i,d in enumerate(dictionary) :
            if d[0] == word :
                wordID = i
                features_matrix1[docID,wordID] = 1
    docID = docID + 1

docID = 0
features_matrix2 = np.zeros((int(NumberOfLine/5), 3000))
for line in testLine :
    words = line.split()
    for word in words :
        wordID = 0
        for i,d in enumerate(dictionary) :
            if d[0] == word :
                wordID = i
                features_matrix2[docID,wordID] = 1
    docID = docID + 1

"""5. Implement the Naive Bayes from scratch, fit the respective models to the training data."""

nbspam=np.sum(train_labels) 
nbham=np.sum(1-train_labels) 
spamPHI = (np.matmul(train_labels.transpose(),features_matrix1)+1)/(nbspam+2) #(1,3000) P(X(n)=1|Y=1)
hamPHI = (np.matmul((1-train_labels).transpose(),features_matrix1)+1)/(nbham+2) #(1,3000) P(X(n)=1|Y=0)
phitot=(nbspam+1)/(nbspam+nbham+2) #P(Y=1)

"""6. Make predictions for the test data."""
result = np.ones((1000,2))
result[:,0] = result[:,0]*(1-phitot)
result[:,1] = result[:,1]*(phitot)
for i in range(1000):
    for j in range(3000):
        if features_matrix2[i,j] == 1:    
            result[i,0]=hamPHI[j]*result[i,0]
            result[i,1]=spamPHI[j]*result[i,1]
            
pred = np.zeros((1000))
for i in range(1000):
    if result[i,0]<result[i,1]:
        pred[i]=1
        
""" 7. Measure the spam-filtering performance for each approach through the confusion matrix. """
matrice_confusion = np.zeros((2,2))
for i in range(1000):
    if (test_labels[i] == 1 and pred[i] == 1):
        matrice_confusion[1,1] += 1
    elif (test_labels[i] == 0 and pred[i] == 0):
        matrice_confusion[0,0] += 1
    elif (test_labels[i] == 1 and pred[i] == 0):
        matrice_confusion[0,1] += 1
    elif (test_labels[i] == 0 and pred[i] == 1):
        matrice_confusion[1,0] += 1