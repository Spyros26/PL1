import sys
import queue

def compat(item,hder):
    if((item=='A' and hder=='U') or (item=='U' and hder=='A') or (item=='C' and hder=='G') or (item=='G' and hder=='C')):
        return True
    else:
        return False

def compid(item):
    if(item=='A'):
        return 'U'
    elif(item=='U'):
        return 'A'
    elif(item=='C'):
        return 'G'
    else:
        return 'C'

                    
def letter(item,code,head,tail,comp,ender,slist):
    if((item==head and comp==0) or (compat(item,head) and comp==1) or (not(item in ender) and comp==0) or (not(compid(item) in ender) and comp==1)):
        code1 = code + 'p'
        if(item==head and comp==0):
            head1 = item
            ender1 = ender
        elif(compat(item,head) and comp==1):
            head1 = compid(item)
            ender1 = ender
        elif(not(item in ender) and comp==0):
            head1 = item
            ender1 = ender + item
        elif(not(compid(item) in ender) and comp==1):
            head1 = compid(item)
            ender1 = ender + compid(item)
        obj1 = (code1,head1,tail,comp,ender1)
        slist.put(obj1)
        
    if((compat(item,head) and comp==0) or (item==head and comp==1) or (not(compid(item) in ender) and comp==0) or (not(item in ender) and comp==1)):
        code2 = code + 'cp'
        if(compat(item,head) and comp==0):
            head2 = head
            comp2 = 1
            ender2 = ender
        elif(item==head and comp==1):
            head2 = head
            comp2 = 0
            ender2 = ender
        elif(not(compid(item) in ender) and comp==0):    
            head2 = compid(item)
            comp2 = 1
            ender2 = ender + compid(item)
        elif(not(item in ender) and comp==1):
            head2 = item
            comp2 = 0
            ender2 = ender + item
        obj2 = (code2,head2,tail,comp2,ender2)
        slist.put(obj2)
        
    if((item==tail and comp==0) or (compat(item,tail) and comp==1) or (not(item in ender) and comp==0) or (not(compid(item) in ender) and comp==1)):
        code3 = code + 'rp'
        if(item==tail and comp==0):
            head3 = item
            tail3 = head
            ender3 = ender
        elif(compat(item,tail) and comp==1):
            head3 = compid(item)
            tail3 = head
            ender3 = ender
        elif(not(item in ender) and comp==0):
            head3 = item
            tail3 = head
            ender3 = ender + item
        elif(not(compid(item) in ender) and comp==1):
            head3 = compid(item)
            tail3 = head
            ender3 = ender + compid(item)
        obj3 = (code3,head3,tail3,comp,ender3)
        slist.put(obj3)
        
    if((item==tail and comp==1) or (compat(item,tail) and comp==0) or (not(compid(item) in ender) and comp==0) or (not(item in ender) and comp==1)):
        code4 = code + 'crp'
        if(item==tail and comp==1):
            head4 = item
            tail4 = head
            comp4 = 0
            ender4 = ender
        elif(compat(item,tail) and comp==0):
            head4 = compid(item)
            tail4 = head
            comp4 = 1
            ender4 = ender
        elif(not(item in ender) and comp==1):
            head4 = item
            tail4 = head
            comp4 = 0
            ender4 = ender + item
        elif(not(compid(item) in ender) and comp==0): 
            head4 = compid(item)
            tail4 = head
            comp4 = 1
            ender4 = ender + compid(item)
        obj4 = (code4,head4,tail4,comp4,ender4)
        slist.put(obj4)
    

"start"
starter=queue.LifoQueue(0)
filename = sys.argv[1]
with open(filename) as inp:
    N = [int(x) for x in next(inp).split()]
    for line in inp:
        for ch in line:
            starter.put(ch)
        dead = starter.get()
        item = starter.get()
        code = 'p'
        ender = item
        head = item
        tail = item
        comp = 0
        obj = (code,head,tail,comp,ender)
        flist = queue.Queue(0)
        flist.put(obj)
        slist = queue.Queue(0)
        n=1
       
        while(starter.empty()==False):
            item3 = starter.get()
            while(flist.empty()==False):
                take = flist.get()
                letter(item3,take[0],take[1],take[2],take[3],take[4],slist)
            while(slist.empty()==False):
                item4 = slist.get()
                flist.put(item4)
       
        final = flist.get()         
        while(flist.empty()==False):
            n = flist.get()
            if(len(n[0]) < len(final[0])):
                final = n
            elif(len(n[0])==len(final[0])):
                if(n[0]<final[0]):
                    final = n
                
        print(final[0])
        print("\n")    
        
