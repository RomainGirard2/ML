import numpy as np

# solving algorithm
def solving(A, B, N, T, x):
    alpha = np.zeros((N, T))
    alpha[1][0] = 1

    for t in range(1, T):
        for j in range(N):
            sumalpha = 0
            for i in range(N):
                sumalpha = sumalpha + alpha[i][t - 1] * A[i][j]
            alpha[j][t] = B[j][x[t-1]] * sumalpha
    return alpha

# Initialisation
A = np.array([[1, 0, 0, 0], [0.2, 0.3, 0.1, 0.4], [0.2, 0.5, 0.2, 0.1], [0.7, 0.1, 0.1, 0.1]])
B = np.array([[1, 0, 0, 0, 0], [0, 0.3, 0.4, 0.1, 0.2], [0, 0.1, 0.1, 0.7, 0.1], [0, 0.5, 0.2, 0.1, 0.2]])
N = 4
T = 5
x = [1, 3, 2, 0]

# Solving
alpha = solving(A, B, N, T, x)

# Decoding
path = np.argmax(alpha, axis=0)