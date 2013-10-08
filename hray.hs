import Data.Bits    -- hray > hray.ppm
import qualified Data.ByteString as B
z=zipWith;k=map;(&)=z(+);v%c=z(*)v[c,c,
 c];a#b=sum$z(*)a b;o v=v%(1/sqrt(v#v))
[x,y,z]*^[a,b,c]=[y*c-z*b,z*a-x*c,x*b-
 y*a];u(y,r)=k(\x->[-x,0,-y-4])[
 fromIntegral x|x<-[0..19],testBit r x]
b::[Int];b=[229774,279057,16897,16910,
 16912,20369,526,512,16896];j=concat$k
 u$zip[1..]b;q x d=a x d$i x d
a x d(m,t,n)|m==0=[0.7,0.6,1]%((1-(
 d!!2))**4)|m==2=[p,p,p]&((q h r)%0.5)|
 True=let z=k ceiling$h%0.2;s=b*0.2+0.1
 in if mod(z!!0+z!!1)2>0 then[3,1,1]%s
  else[3,3,3]%s where
 h=x&(d%t);l=o$[9,9,16]&(h%(-1))
 r=d&(n%((n#d)*(-2)));g=l#n;f(e,_,_)=e
 b=if g<0||(f$i h l)/=0 then 0 else g
 p=if b>0 then(l#r)**99 else 0
h r d z=if l>0&&s>0.01then(2,s,o$p&(d%s
 ))else(2,1e7,[]) where
 p=r&z;b=p#d;l=b*b-(p#p)+1;s= -b-sqrt l
it a b=if p a<=p b then a else b
m x=foldl1 it x;i r d=m$c:if p>0.01then
 [(1,p,[0,0,1])]else[(0,1e6,[])]
 where p= -(r!!2)/(d!!2);c=m$k(h r d)j
p(_,e,_)=e;tt= -1;e' a b c(x,y)t=
 let r=[17,16,8]&t;
     d=o$(t%tt)&(((a%x)&(b%y)&c)%16)in
 (q r d)%3.5;e a b c t=let z=[-28,0,28]
 in(foldl1(&)[(e' a b c t((a%v)&(b%w)))
 |v<-z,w<-z])%7;main=
 do putStr"P6 512 512 255 "
    B.putStr$B.pack$k round p where
 g=o[-6,-16,0];a=(o$[0,0,1]*^g)%0.002
 b=(o$(g*^a))%0.002;c=g&((a&b)%(-256))
 w=[511,510..0];p=concat$k(e a b c)[
  (x,y)|y<-w,x<-w]
