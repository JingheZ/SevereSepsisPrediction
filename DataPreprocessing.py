from multiprocessing import Process, Queue
import numpy as np
import sys
dfile = sys.argv[-1]
data = np.genfromtxt(dfile,dtype=int)

def agent(data, i, q):
	zero = np.array([[0,1,1,0],[1,0,0,1],[1,0,0,1],[1,0,0,1],[1,0,0,1],[1,0,0,1],[0,1,1,0]])
	one = np.array([[0,0,1,0],[0,1,1,0],[0,0,1,0],[0,0,1,0],[0,0,1,0],[0,0,1,0],[0,1,1,1]])
	two = np.array([[1,1,1,0],[0,0,0,1],[0,0,0,1],[0,1,1,0],[1,0,0,0],[1,0,0,0],[1,1,1,1]])
	three = np.array([[1,1,1,0],[0,0,0,1],[0,0,0,1],[0,1,1,0],[0,0,0,1],[0,0,0,1],[1,1,1,0]])
	four = np.array([[0,0,1,0],[1,0,1,0],[1,0,1,0],[1,1,1,1],[0,0,1,0],[0,0,1,0],[0,0,1,0]])
	five = np.array([[1,1,1,1],[1,0,0,0],[1,0,0,0],[0,1,1,0],[0,0,0,1],[0,0,0,1],[1,1,1,0]])
	six = np.array([[0,1,1,0],[1,0,0,0],[1,0,0,0],[1,1,1,0],[1,0,0,1],[1,0,0,1],[0,1,1,0]])
	seven = np.array([[1,1,1,1],[1,0,0,1],[0,0,0,1],[0,0,0,1],[0,0,0,1],[0,0,0,1],[0,0,0,1]])
	eight = np.array([[0,1,1,0],[1,0,0,1],[1,0,0,1],[0,1,1,0],[1,0,0,1],[1,0,0,1],[0,1,1,0]])
	nine = np.array([[0,1,1,0],[1,0,0,1],[1,0,0,1],[0,1,1,1],[0,0,0,1],[0,0,0,1],[0,1,1,0]])
	archive = np.array([zero, one, two, three, four, five, six, seven, eight, nine])
	
	t = []
	for j in range(10):
		dif = archive[j][i] - data[i]
		t0 = 0
		for m in range(4):
			if dif[m] == 0:
				t0 += 1		
		t.append(t0)
	max_t = max(t)
	ind_max = []
	for i in range(10): 
		if t[i] == max_t:
			ind_max.append(i)
	q.put(ind_max)


queue = Queue()

p0 = Process(target = agent,args = (data,0, queue))
p1 = Process(target = agent,args = (data,1, queue))
p2 = Process(target = agent,args = (data,2, queue))
p3 = Process(target = agent,args = (data,3, queue))
p4 = Process(target = agent,args = (data,4, queue))
p5 = Process(target = agent,args = (data,5, queue))
p6 = Process(target = agent,args = (data,6, queue))

p0.start()
p1.start()
p2.start()
p3.start()
p4.start()
p5.start()
p6.start()

p0.join()
p1.join()
p2.join()
p3.join()
p4.join()
p5.join()
p6.join()

	
results = []
for i in range(7):
	results.append(queue.get(True))

results1 = []
for i in range(7):
    for j in range(len(results[i])):
        results1.append(results[i][j])
        
counts = np.bincount(results1)
digit = np.argmax(counts) 
      
result = str("The most likely digit is %s" %digit)	
f = open("result.txt", "wb")
f.write(result)
f.close()


	
		
		
		
		
		
 
		
		
		
		
	


