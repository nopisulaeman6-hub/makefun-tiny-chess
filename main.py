import json,os,urllib.request as u
V=[0,100,320,330,500,900,0]
D=lambda s:[(ord(c)-107)*5 for c in s]
K='aceecgklelmneknoeknoekmncgkkacee'
M=[0,1,2,3,3,2,1,0]
T=[D('kkkkuuuummoqllmpkkkoljiklmmgkkkk'),D(K),D(K),D('kkkklmmmjkkkjkkkjkkkjkkkjkkkkkkl'),D('giijikkkiklljkllkkllillliklkgiij'),D('eccaeccaeccaeccageecigggookkoqmk')]
KD=[1,-1,16,-16,15,17,-15,-17]
ND=[14,18,31,33,-14,-18,-31,-33]
DD=[ND,KD[4:],KD[:4],KD,KD]
E=[-1];N=[0];B=150000;R=range(128)
def ev(b):return sum((V[p]+T[p-1][(7-(q>>4))*4+M[q&7]]if p>0 else-V[-p]-T[-p-1][(q>>4)*4+M[q&7]])for q in R for p in[b[q]]if p and not q&8)
def atk(b,q,s):
 for d in(15,17):
  t=q-d*s
  if not t&136 and b[t]==s:return 1
 for d in ND+KD:
  t=q+d
  if not t&136 and b[t]==(2 if d in ND else 6)*s:return 1
 for i,d in enumerate(KD):
  t=q+d
  while not t&136:
   p=b[t]
   if p:
    if p in(s*(3+(i<4)),s*5):return 1
    break
   t+=d
 return 0
def gen(b,s,o=0):
 m=[]
 def a(t):m.append((f,t,5 if t>>4 in(0,7)else 0))
 for f in R:
  if f&8:continue
  p=b[f]
  if not p or p*s<0:continue
  x=abs(p)
  if x==1:
   d=16*s;t=f+d
   if not(t&136 or b[t]):
    if not o:a(t)
    if f>>4==1+5*(s<0):
     z=f+2*d
     if not b[z] and not o:m.append((f,z,0))
   for c in(d-1,d+1):
    t=f+c
    if t&136:continue
    if b[t]*s<0 and b[t]!=-6*s:a(t)
    elif t==E[0]:m.append((f,t,0))
  else:
   for d in DD[x-2]:
    t=f+d
    while not t&136:
     if b[t]:
      if b[t]*s<0 and b[t]!=-6*s:m.append((f,t,0))
      break
     if not o:m.append((f,t,0))
     if x in(2,6):break
     t+=d
 return m
def og(b,s,o=0):return sorted(gen(b,s,o),key=lambda m:b[m[1]]*s)
def cr(b,f,t,p,r):
 if abs(p)==6 and abs(t-f)==2:x,y=(t+1,t-1)if t>f else(t-2,t+1);x,y=(y,x)if r else(x,y);b[y]=b[x];b[x]=0
def do(b,m,s):
 f,t,pr=m;p=b[f];c=b[t];o=E[0];ec=-1
 b[t]=p;b[f]=0
 if abs(p)==1:
  if t==o:ec=t-16*s;b[ec]=0
  E[0]=f+16*s if abs(t-f)==32 else-1
  if pr:b[t]=pr*s
 else:E[0]=-1
 cr(b,f,t,p,0)
 return(c,o,ec)
def un(b,m,z,s):
 f,t,pr=m;c,o,ec=z;p=b[t]
 b[f]=p if not pr else s;b[t]=c
 if ec>=0:b[ec]=-s
 cr(b,f,t,p,1)
 E[0]=o
def kp(b,s):return b.index(6*s)
H={}
def sr(b,s,d,al,be,pl,Y=()):
 N[0]+=1
 if N[0]>B:raise TimeoutError
 k=tuple(b),s
 if k in Y:return 0
 if d==0 and pl<4 and atk(b,kp(b,s),-s):d=1
 if d==0:
  st=ev(b)*s
  if st>=be:return be
  if st>al:al=st
  for m in og(b,s,1):
   z=do(b,m,s);sc=-sr(b,-s,0,-be,-al,pl+1);un(b,m,z,s)
   if sc>=be:return be
   al=max(al,sc)
  return al
 e=H.get(k)
 if e and e[0]>=d:
  if e[2]==0:return e[1]
  if e[2]==1:al=max(al,e[1])
  else:be=min(be,e[1])
  if al>=be:return e[1]
 a0=al;b0=be
 if d>2 and not atk(b,kp(b,s),-s) and any(p*s in(2,3,4,5)for p in b):
  o=E[0];E[0]=-1;sc=-sr(b,-s,d-3,-be,-be+1,pl+1);E[0]=o
  if sc>=be:return be
 bs=-1e9;lg=0
 ms=og(b,s)
 if e and e[3] in ms:ms.remove(e[3]);ms.insert(0,e[3])
 for m in ms:
  z=do(b,m,s)
  if atk(b,kp(b,s),-s):un(b,m,z,s);continue
  lg+=1;sc=-sr(b,-s,d-1,-be,-al,pl+1,Y+(k,));un(b,m,z,s)
  if sc>bs:bs=sc;bm=m
  al=max(al,sc)
  if al>=be:break
 if not lg:return-9e3+pl if atk(b,kp(b,s),-s)else 0
 H[k]=(d,bs,(bs>=b0)-(bs<=a0),bm)
 return bs
def sq(n):return(ord(n[1])-49)*16+ord(n[0])-97
def pm(x):return(sq(x[:2]),sq(x[2:4]),5-'qrbn'.index(x[4])if len(x)>4 else 0)
def fen(f):
 b=[0]*128;r=7;c=0
 for ch in f:
  if ch=='/':r-=1;c=0
  elif ch.isdigit():c+=int(ch)
  else:b[r*16+c]=('PNBRQK'.index(ch.upper())+1)*(ch.isupper()*2-1);c+=1
 return b
def best(b,s,lg):
 N[0]=0;bm=lg[0];k=tuple(b),s
 if len(H)>1e5:H.clear()
 try:
  for d in range(1,99):
   al=-1e9;cur=None
   for x in lg:
    m=pm(x);z=do(b,m,s)
    if atk(b,kp(b,s),-s):un(b,m,z,s);continue
    sc=-sr(b,-s,d-1,-1e9,-al,1,(k,));un(b,m,z,s)
    if sc>al:al=sc;cur=x
   if cur:bm=cur;lg.remove(cur);lg.insert(0,cur)
 except TimeoutError:pass
 return bm
U=os.getenv("MAKEFUN_ENV_URL")
def post(p,y):return json.load(u.urlopen(U+p,json.dumps(y).encode()))
while(g:=post("/game",{}))["status"]!="done":
 while g["status"]=="your_move":
  f=g["fen"].split();b=fen(f[0]);s=2*(f[1]=='w')-1;E[0]=sq(f[3])if f[3]!='-'else-1
  g=post("/move",{"game":g["game"],"move":best(b,s,g["legal_moves"])})
