ID:1,2,3,4.
Pid: p1,p2.
Name: n1,n2.
G:10,20,30,40.

*In(ID,Pid,Name,G)
1,p1,n1,10
2,p1,n1,20
3,p2,n2,30
4,p2,n2,40
.
*Lt(G,G)
10,20
10,30
10,40
20,30
20,40
30,40
.
invent_1(Pid,G)
.
Out(Pid,G,G)
p1,10,20
p2,30,40
.
