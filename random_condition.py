import numpy as np
import matplotlib.pyplot as plt
b=np.zeros([1024,256])
for i in range(256):
        b[:,i]=np.linspace(i,i+100,1024)
print(np.shape(b))

base=0.2+np.sin(1./6.*b)
print(np.shape(base))
a=base+(2*np.random.random([1024,256])-1.0)
#print(a)
plt.imshow(a)
plt.show()
np.savetxt('random_condition.dat',a)
