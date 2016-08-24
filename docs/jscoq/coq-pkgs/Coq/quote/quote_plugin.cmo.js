(function(a){"use strict";var
C=140,q="plugins/quote/g_quote.ml4",aC="in",B=250,ax="quote_plugin",r=124,aw="$k",av="[",o=246,aB="$c",V="Extension: cannot occur",s="quote",aA="]",au="$lc",u="Quote",U="$f",at="plugins/quote/quote.ml",az=136,ay="using",n=a.jsoo_runtime,as=n.caml_check_bound,b=n.caml_new_string,A=n.caml_obj_tag,ae=n.caml_register_global,T=n.caml_wrap_exception;function
e(a,b){return a.length==1?a(b):n.caml_call_gen(a,[b])}function
g(a,b,c){return a.length==2?a(b,c):n.caml_call_gen(a,[b,c])}function
j(a,b,c,d){return a.length==3?a(b,c,d):n.caml_call_gen(a,[b,c,d])}function
ad(a,b,c,d,e){return a.length==4?a(b,c,d,e):n.caml_call_gen(a,[b,c,d,e])}var
c=n.caml_get_global_data(),S=b(ax),h=c.Term,f=c.Util,w=c.Tacmach,k=c.Assert_failure,L=c.Proofview,af=c.Tactics,X=c.Evd,M=c.Global,aj=c.Constr_matching,i=c.Names,D=c.Termops,W=c.Pp,N=c.Errors,ai=c.Coqlib,x=c.CamlinternalLazy,ah=c.Option,ag=c.Patternops,t=c.Pervasives,d=c.Constrarg,p=c.Loc,m=c.Genarg,l=c.Tacinterp,aq=c.Mltop,a1=[0,b(at),473,11],a0=[0,b(at),458,13],aY=b("invalid inversion scheme for quote"),aX=[0,b("Coq"),[0,b(s),[0,b(u),0]]],aW=b("M"),aV=b("Quote: not a simple fixpoint"),aT=b("End_idx"),aU=[0,b(u),0],aR=b("Left_idx"),aS=[0,b(u),0],aP=b("Right_idx"),aQ=[0,b(u),0],aN=b("varmap_find"),aO=[0,b(u),0],aL=b("Node_vm"),aM=[0,b(u),0],aJ=b("Empty_vm"),aK=[0,b(u),0],aH=b(s),aI=b(u),bs=b(aw),bY=[0,b(q),1,0],bt=[0,b(ay)],bu=b(aB),bX=[0,b(q),1,0],bv=[0,b(aC)],bw=[0,b(aA)],bx=b(au),bW=[0,b(q),1,0],by=[0,b(av)],bz=b(U),bV=[0,b(q),1,0],bA=[0,b(s)],bB=b(aw),bU=[0,b(q),1,0],bC=[0,b(ay)],bD=b(aB),bT=[0,b(q),1,0],bE=[0,b(aC)],bF=b(U),bS=[0,b(q),1,0],bG=[0,b(s)],bH=[0,[0,b(aA)],0],bI=b(au),bR=[0,b(q),1,0],bJ=[0,b(av)],bK=b(U),bQ=[0,b(q),1,0],bL=[0,b(s)],bM=b(U),bP=[0,b(q),1,0],bN=[0,b(s)],bO=b(s),bn=b(V),bl=b(V),bj=b(V),bh=b(V),a7=b(ax),a8=b("cont"),a9=b("x"),bq=b(s),a$=b(": "),ba=b(s),bc=b("Exception in tactic extend "),aD=c.Not_found,aG=c.Environ,aE=c.Reductionops,aF=c.Hashtbl,a3=c.Tacentries,a4=c.Geninterp,a2=c.Tacenv,a5=c.Feedback,a6=c.Array;function
v(a,b){return j(ai[4],aI,[0,aH,a],b)}var
E=[o,function(a){return v(aK,aJ)}],F=[o,function(a){return v(aM,aL)}],G=[o,function(a){return v(aO,aN)}],H=[o,function(a){return v(aQ,aP)}],I=[o,function(a){return v(aS,aR)}],J=[o,function(a){return v(aU,aT)}],O=e(f[20][1],[0,h[141]]);function
y(a){return e(N[6],aV)}function
Y(a){var
b=e(h[99],a);return e(h[C],b)}function
ak(a){var
b=e(i[1][7],a);return n.caml_int_of_string(j(f[15][4],b,1,n.caml_ml_string_length(b)-1|0))}function
al(a){var
b=e(t[20],a),c=g(t[16],aW,b);return e(i[1][5],c)}function
P(a,b,c){var
d=e(h[C],a);if(11===d[0]){var
i=d[1],j=i[1];if(0===j[2]){var
k=function(a){return e(h[114],c-a|0)},l=g(f[19][2],c,k),m=[0,e(h[131],[0,[0,[0,j[1],0],b+1|0],i[2]]),l];return e(h[r],m)}}return y(0)}function
am(a,b){function
k(a){var
d=a;for(;;){var
c=e(h[C],d);switch(c[0]){case
5:var
d=c[1];continue;case
9:var
l=c[2],i=c[1];if(e(h[1],i))if(e(h[29],i)===b){var
o=e(f[19][38],l);return[11,[0,al(e(h[29],o))]]}var
p=g(f[19][15],k,l),q=X[16],r=e(M[2],0);return[4,j(ag[7],r,q,i),p];default:var
m=X[16],n=e(M[2],0);return j(ag[7],n,m,d)}}}return k(a)}function
Z(a,b,c){try{var
X=e(h[41],a),p=X}catch(f){f=T(f);if(f!==h[28])throw f;var
p=y(0)}var
F=e(M[2],0),q=Y(g(aG[59],F,p));if(14===q[0]){var
r=q[1],s=r[1];if(1===s[1].length-1)if(0===s[2]){var
i=r[2];if(1===i[1].length-1)if(1===i[2].length-1){var
t=i[3];if(1===t.length-1){var
u=e(h[80],t[1]),d=u[1],H=e(f[17][1],d),I=e(L[62][5],c),J=e(w[49][4],c),K=j(aE[82],0,I,J),k=Y(u[2]);if(13===k[0]){var
v=k[2],l=[0,0],m=[0,0],n=[0,0],N=k[4],Q=function(a,b){var
p=e(h[80],b),c=p[2],i=e(f[17][1],p[1]);if(e(h[1],c))if(1===e(h[29],c)){n[1]=[0,P(e(f[17][3],d)[2],a,i)];return 0}var
q=e(h[39],c),r=q[2];if(r){var
s=r[2];if(s){var
j=s[2];if(j){var
k=j[2];if(k)if(!k[2])if(e(h[1],j[1]))if(e(h[1],k[1])){var
t=A(G),w=B===t?G[1]:o===t?e(x[2],G):G;if(g(K,q[1],w)){m[1]=[0,P(e(f[17][3],d)[2],a,i)];return 0}}}}}var
u=l[1],v=am(c,(H+i|0)+1|0);l[1]=[0,[0,P(e(f[17][3],d)[2],a,i),v],u];return 0};g(f[19][14],Q,N);var
z=e(ah[3],n[1]),R=z?e(ah[3],m[1]):z;if(R)y(0);var
E=e(h[C],v),S=7===E[0]?e(D[54],E[3]):v,U=n[1],V=j(f[17][16],O[4],b,O[1]),W=m[1];return[0,e(f[17][6],l[1]),W,S,V,U]}return y(0)}}}}return y(0)}function
Q(c,b){var
d=b;for(;;){var
i=g(O[3],d,c);if(i)return i;var
a=e(h[C],d);switch(a[0]){case
5:var
d=a[1];continue;case
9:var
j=Q(c,a[1]);if(j){var
k=a[2],l=function(a){return Q(c,a)};return g(f[19][30],l,k)}return j;default:return 0}}}function
an(c,b){var
g=c.length-1,a=A(F),l=g>>>1|0,i=B===a?F[1]:o===a?e(x[2],F):F,j=A(E),k=[0,b],m=B===j?E[1]:o===j?e(x[2],E):E,d=e(h[r],[0,m,k]);function
f(a){if(g<a)return d;if(l<a){var
j=a-1|0,m=[0,i,[0,b,as(c,j)[j+1],d,d]];return e(h[r],m)}var
n=f((2*a|0)+1|0),k=a-1|0,o=f(2*a|0),p=[0,i,[0,b,as(c,k)[k+1],o,n]];return e(h[r],p)}return f(1)}function
ao(a){function
b(a){return 1===a?0:[0,1===(a%2|0)?1:0,b(a>>>1|0)]}var
c=A(J),d=B===c?J[1]:o===c?e(x[2],J):J,g=b(a),i=e(f[17][6],g);function
k(a,b){var
g=[0,b];if(a)var
c=A(H),i=B===c?H[1]:o===c?e(x[2],H):H,d=i;else
var
f=A(I),j=B===f?I[1]:o===f?e(x[2],I):I,d=j;return e(h[r],[0,d,g])}return j(f[17][16],k,i,d)}function
_(i,b,c){var
a=b;for(;;){var
k=j(w[36],i,a,c);if(k)return k;var
d=e(h[C],a);switch(d[0]){case
5:var
a=d[1];continue;case
9:var
l=d[2],m=function(a){return _(i,a,c)};return g(f[19][28],m,l);default:return 0}}}function
ap(d,b){function
e(a,b){if(b){var
f=b[2],c=b[1];return g(h[az],a,c)?b:_(d,a,c)?[0,a,[0,c,f]]:[0,c,e(a,f)]}return[0,a,0]}if(b){var
a=ap(d,b[2]);return e(b[1],a)}return b}var
R=e(aF[18],[0,h[az],h[151]]);function
$(c,b){e(ai[11],aX);var
k=e(R[1],17),h=[0,0],l=[0,1];function
m(b){function
a(a){var
d=a;for(;;){if(d){var
n=d[1];try{var
s=n[2],t=X[16],u=e(M[2],0),v=ad(aj[3],u,t,s,b),w=e(i[1][10][17],v),x=function(a){var
b=m(a[2]);return[0,ak(a[1]),b]},y=g(f[17][12],x,w),z=g(D[53],y,n[1]);return z}catch(f){f=T(f);if(f===aj[1]){var
d=d[2];continue}throw f}}var
o=c[2];if(o){var
p=c[5];if(p)if(Q(c[4],b))return g(D[53],[0,[0,1,b],0],p[1]);try{var
C=g(R[7],k,b);return C}catch(f){f=T(f);if(f===aD){var
A=o[1],B=[0,[0,1,ao(l[1])],0],q=g(D[53],B,A);l[1]++;h[1]=[0,b,h[1]];j(R[5],k,b,q);return q}throw f}}var
r=c[5];if(r)return g(D[53],[0,[0,1,b],0],r[1]);var
E=e(W[1],aY);return j(N[3],0,0,E)}}return a(c[1])}var
a=g(f[17][12],m,b),d=c[3],n=e(f[17][6],h[1]);return[0,a,an(e(f[19][12],n),d)]}function
aZ(a,b){var
c=[0,function(c){var
d=g(w[49][2],a,c);function
o(a){return g(w[49][2],a,c)}var
l=Z(d,g(f[17][12],o,b),c),m=$(l,[0,e(L[62][3],c),0]),i=m[1];if(i)if(!i[2]){var
n=i[1];if(l[2]){var
p=e(h[r],[0,d,[0,m[2],n]]);return j(af[3],0,p,2)}var
q=e(h[r],[0,d,[0,n]]);return j(af[3],0,q,2)}throw[0,k,a0]}];return e(L[62][9],c)}var
z=[0,v,E,F,G,H,I,J,O,y,Y,ak,al,P,am,Z,Q,an,ao,_,ap,R,$,aZ,function(a,b,c,d){var
i=[0,function(i){var
j=g(w[49][2],c,i);function
p(a){return g(w[49][2],a,i)}var
m=Z(j,g(f[17][12],p,d),i),n=$(m,[0,b,0]),l=n[1];if(l)if(!l[2]){var
o=l[1];return m[2]?e(a,e(h[r],[0,j,[0,n[2],o]])):e(a,e(h[r],[0,j,[0,o]]))}throw[0,k,a1]}];return e(L[62][9],i)}];ae(87,z,"Quote_plugin.Quote");e(aq[12],a7);var
K=p[4],aa=e(i[1][5],a8),ab=e(i[1][5],a9);function
ac(a,b){var
c=e(l[2][1],b),d=a4[4][1],f=g(i[1][10][5],ab,c),h=[0,j(i[1][10][4],aa,a,f),d];return g(l[18],h,[29,[0,K,[3,K,[1,[0,K,aa]],[0,[2,[1,[0,K,ab]]],0]]]])}try{var
bg=0,bi=[0,function(a){if(a){var
b=a[2];if(b){var
c=b[2];if(c){var
f=c[2];if(f)if(!f[2]){var
h=a[1],i=e(m[6],d[4]),j=g(l[2][7],i,h),k=b[1],n=e(m[17],d[4]),o=e(m[6],n),p=g(l[2][7],o,k),q=c[1],r=e(m[6],d[8]),s=g(l[2][7],r,q),u=f[1],v=e(m[6],d[14]),w=g(l[2][7],v,u);return function(a){function
b(a){return ac(w,a)}return ad(z[24],b,s,j,p)}}}}}return e(t[2],bh)},bg],bk=[0,function(a){if(a){var
b=a[2];if(b){var
c=b[2];if(c)if(!c[2]){var
f=a[1],h=e(m[6],d[4]),i=g(l[2][7],h,f),j=b[1],k=e(m[6],d[8]),n=g(l[2][7],k,j),o=c[1],p=e(m[6],d[14]),q=g(l[2][7],p,o);return function(a){var
b=0;function
c(a){return ac(q,a)}return ad(z[24],c,n,i,b)}}}}return e(t[2],bj)},bi],bm=[0,function(a){if(a){var
b=a[2];if(b)if(!b[2]){var
c=a[1],f=e(m[6],d[4]),h=g(l[2][7],f,c),i=b[1],j=e(m[17],d[4]),k=e(m[6],j),n=g(l[2][7],k,i);return function(a){return g(z[23],h,n)}}}return e(t[2],bl)},bk],bo=[0,function(a){if(a)if(!a[2]){var
b=a[1],c=e(m[6],d[4]),f=g(l[2][7],c,b);return function(a){return g(z[23],f,0)}}return e(t[2],bn)},bm],bp=e(a6[12],bo);j(a2[9],0,[0,S,bq],bp);var
br=function(a){var
t=e(i[1][6],bs),b=d[14],r=0,s=0;if(0===b[0]){var
u=[0,bt,[0,[1,p[4],[5,[0,b[1]]],t],s]],v=e(i[1][6],bu),c=d[8];if(0===c[0]){var
w=[0,bw,[0,bv,[0,[1,p[4],[5,[0,c[1]]],v],u]]],x=e(i[1][6],bx),f=d[4];if(0===f[0]){var
y=[0,by,[0,[1,p[4],[0,[5,[0,f[1]]]],x],w]],z=e(i[1][6],bz),h=d[4];if(0===h[0]){var
A=[0,[0,bA,[0,[1,p[4],[5,[0,h[1]]],z],y]],r],C=e(i[1][6],bB),j=d[14],B=0;if(0===j[0]){var
D=[0,bC,[0,[1,p[4],[5,[0,j[1]]],C],B]],E=e(i[1][6],bD),l=d[8];if(0===l[0]){var
F=[0,bE,[0,[1,p[4],[5,[0,l[1]]],E],D]],G=e(i[1][6],bF),m=d[4];if(0===m[0]){var
H=[0,[0,bG,[0,[1,p[4],[5,[0,m[1]]],G],F]],A],I=e(i[1][6],bI),n=d[4];if(0===n[0]){var
J=[0,bJ,[0,[1,p[4],[0,[5,[0,n[1]]]],I],bH]],K=e(i[1][6],bK),o=d[4];if(0===o[0]){var
L=[0,[0,bL,[0,[1,p[4],[5,[0,o[1]]],K],J]],H],N=e(i[1][6],bM),q=d[4],M=0;if(0===q[0])return g(a3[4],[0,S,bO],[0,[0,bN,[0,[1,p[4],[5,[0,q[1]]],N],M]],L]);throw[0,k,bP]}throw[0,k,bQ]}throw[0,k,bR]}throw[0,k,bS]}throw[0,k,bT]}throw[0,k,bU]}throw[0,k,bV]}throw[0,k,bW]}throw[0,k,bX]}throw[0,k,bY]};g(aq[19],br,S)}catch(f){f=T(f);if(!e(N[22],f))throw f;var
a_=g(N[18],0,f),bb=g(t[16],ba,a$),bd=g(t[16],bc,bb),be=e(W[1],bd),bf=g(W[13],be,a_);e(a5[13],bf)}var
ar=[0,S,K,aa,ab,ac];ae(98,ar,"Quote_plugin.G_quote");ae(99,[0,z,ar],"Quote_plugin");return}(function(){return this}()));
