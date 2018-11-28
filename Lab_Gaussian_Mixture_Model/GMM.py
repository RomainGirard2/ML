# -*- coding: utf-8 -*-

import random
import math
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.colors import LogNorm

"""def prob(sigma,mu,phi,x):
    (1 / (((2 * math.pi)) * math.sqrt(np.absolute(np.linalg.det(sigma))))) * math.exp(
                (-0.5) * np.transpose(x - mu).dot(np.linalg.inv(sigma)).dot(x - mu) * phi)"""
    
def weights(X_train, mu, sigma, phi):
    weight = np.zeros((len(X_train), 2))
    for i in range(len(X_train)):
        for j in range(2):
            denom = 0
            num = (1 / (((2 * math.pi)) * math.sqrt(np.absolute(np.linalg.det(sigma[j]))))) * math.exp(
                (-0.5) * np.transpose(X_train[i] - mu[j]).dot(np.linalg.inv(sigma[j])).dot(X_train[i] - mu[j]) * phi[j])
            for l in range(2):
                denom += (1 / (((2 * math.pi)) * math.sqrt(np.absolute(np.linalg.det(sigma[l]))))) * math.exp(
                    (-0.5) * np.transpose(X_train[i] - mu[l]).dot(np.linalg.inv(sigma[l])).dot(X_train[i] - mu[l]) * phi[l])
            weight[i, j] = num / denom
    return weight

def llborn(X_train, mu, sigma, phi):
    weight = weights(X_train, mu, sigma, phi)
    ll = np.zeros((len(X_train), 1))
    for i in range(len(X_train)):
        suml = 0
        for j in range(2):
            num = (1 / (((2 * math.pi)) * math.sqrt(np.absolute(np.linalg.det(sigma[j]))))) * math.exp(
                (-0.5) * np.transpose(X_train[i] - mu[j]).dot(np.linalg.inv(sigma[j])).dot(X_train[i] - mu[j]) * phi[j])
            if weight[i,j] != 0:
                suml += weight[i, j] * np.log(num/weight[i, j])
        ll[i] = suml
    return ll


m_samples = 300

# generate random sample, two components
np.random.seed(0)
# generate spherical data centered on (20, 20)
shifted_gaussian = np.random.randn(m_samples,2) + np.array([20, 20])
# generate zero centered stretched Gaussian data
C = np.array([[0., -0.7], [3.5, .7]])
stretched_gaussian = np.dot(np.random.randn(m_samples,2), C)
# concatenate the two datasets into the final training set
X_train = np.vstack([shifted_gaussian, stretched_gaussian])

#mu random initialization
mu = np.array([random.choice(X_train), random.choice(X_train)])

#Sigma random initialization
sigma = np.array([np.random.randn(2, 2), np.random.randn(2, 2)])

#Phi random initialization
phi = [0.2, 0.8]

#lower bound random initialization
ll_old = 0
ll_current = 1

nb_iter = 0
lowerbound_log = []

while(np.abs(ll_current-ll_old)>0.00001):
    ll_old = ll_current
    
    # Expectation step
    w = weights(X_train, mu, sigma, phi)

    # Maximization step
    
    #compute phi
    for j in range(2):
        phi[j] = (1/(len(X_train))) * np.sum(w[:, j])
    
    #compute mu
    for j in range(2):
        num = 0
        denom = 0
        for i in range(len(X_train)):
            num += (w[i, j] * (X_train[i]))
            denom += w[i, j]
        mu[j] = num/denom

    #compute sigma
    for j in range(2):
        num = 0
        denom = 0
        for i in range(len(X_train)):
            num += w[i, j] * np.outer(X_train[i] - mu[j], X_train[i] - mu[j])
            denom += w[i, j]
        sigma[j] = num / denom

    # Likelihood
    ll_current = np.sum(llborn(X_train, mu, sigma, phi))
    nb_iter += 1
    lowerbound_log.append(ll_current)

x = np.linspace(-20., 30.)
y = np.linspace(-20., 40.)
X, Y = np.meshgrid(x, y)
XX = np.array([X.ravel(), Y.ravel()]).T

Z = -llborn(XX, mu, sigma, phi) 

Z = Z.reshape(X.shape)
CS = plt.contour(X, Y, Z, norm=LogNorm(vmin=1.0, vmax=1000.0), levels=np.logspace(0, 3, 10))
CB = plt.colorbar(CS, shrink=0.8, extend='both')
plt.scatter(X_train[:, 0], X_train[:, 1], .8)

plt.title('Negative log-likelihood predicted by a GMM')
plt.axis('tight')
plt.show()