import sys

def convert(s):
    str1 = ""
    return(str1.join(s))

def corona(state,arrivals,aps,n,m,time):
    if(state[n][m]!= 'X'):
        if(time < arrivals[n-1][m-1] or arrivals[n-1][m-1]==0):
            arrivals[n-1][m-1] = time
            if(state[n][m]=='A'):
                for (k,l) in aps:
                    if ((k,l)!=(n,m)):
                        if(time+5 < arrivals[k-1][l-1] or arrivals[k-1][l-1]==0):
                            arrivals[k-1][l-1] = time+5
                            virusspread(state,arrivals,aps,k,l,time+7)
            virusspread(state,arrivals,aps,n,m,time+2)
            
def virusspread(state,arrivals,aps,n,m,time):
    corona(state,arrivals,aps,n+1,m,time)
    corona(state,arrivals,aps,n,m-1,time)
    corona(state,arrivals,aps,n,m+1,time)
    corona(state,arrivals,aps,n-1,m,time)              
        
def safeblock(state,arrivals,n,m,time,Move,path):
    if(path==[]):
        if(state[n][m]!= 'X' and state[n][m]!= 'A' and state[n][m]!= 'W'):
            if(time < arrivals[n-1][m-1]):
               safepath(state,arrivals,n,m,time+1,Move,path)
                

def safepath(state,arrivals,n,m,time,Move,path):
    if(state[n][m]!='T' and path==[]):
        safeblock(state,arrivals,n+1,m,time,'D',path)
        safeblock(state,arrivals,n,m-1,time,'L',path)
        safeblock(state,arrivals,n,m+1,time,'R',path)
        safeblock(state,arrivals,n-1,m,time,'U',path)
    if(path!=[] or state[n][m]=='T'):
        if(Move!=''):
           path.insert(0,Move)
                
        

"Start State"
filename = sys.argv[1]
startstate = []
with open(filename) as inp:
   for line in inp:
      startstate.append(['X']+ list(line.rstrip()) + ['X'])

N = len(startstate)
M = len(startstate[0])-2

"Creating upper-lower X lines so that no error occurs"
lims=['X' for x in range(M+2)]
startstate.append(lims)
startstate.insert(0,lims)

"Initializing the list with time until virus reaches each place"
contagion = []
for i in range(N):
    contagion.append([0]*(M))

"Find location of Wuhan"
(wline,wcol) = next((i, startstate.index('W'))
            for i, startstate in enumerate(startstate)
            if 'W' in startstate) 

"Find locations of airports"
aps = [(i, startstate.index('A'))
       for i, startstate in enumerate(startstate)
       if 'A' in startstate]

"Mark times corona reaches each location"
virusspread(startstate,contagion,aps,wline,wcol,2)

"Find location of Sotiris"
(sline,scol) = next((i, startstate.index('S'))
            for i, startstate in enumerate(startstate)
            if 'S' in startstate)
path = []

Move = ''

safepath(startstate,contagion,sline,scol,1,Move,path)

if(path==[]):
    print("IMPOSSIBLE")
else:
    print(len(path))
    print(convert(path))
    
