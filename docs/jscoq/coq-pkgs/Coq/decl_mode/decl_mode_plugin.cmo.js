(function(a){"use strict";var
dY="*",em='"end claim" expected.',er=104,aD="that",aH=140,eq="decl_mode_plugin",$=",",el="be",eb='"end focus" expected.',aG="such",dX="Insufficient justification.",N=250,bM="consider",bB="(",ea="Not inside a proof per cases or induction.",cy="Exception in vernac extend ",cz="suppose it is",cF=148,_="Init",by="DeclProof",cs="on",E=246,cx="suppose",V="plugins/decl_mode/decl_proof_instr.ml",cE=115,dW="    ",ek="escape",d$="assume",aF="Extension: cannot occur",dV='"end cases" expected.',d_="Datatypes",cw="execute_cases ",bH="DeclReturn",u=113,dU="for",bx=122,d9="with",cv="Classic",cA="then",f="",d8="=~",bG="Previous step is not an equality.",ep="~=",d7="suffices",d6='"end induction" expected.',ej="using",bL="ProofInstr",bA="given",G="and",bK=121,d5="cases",c="IDENT",dT='"thesis for ..." is not applicable here.',aE="Declarative",d4="plugins/decl_mode/decl_interp.ml",cD="by",d3="induction",ei=".",dS="take",ct=": ",cu="proof",aY="thesis",eh="impossible",bw="thus",dR="subcase_",Q=124,eo="of",eg="thesis_for",ef=111,cr=133,dQ="Not enough sub-hypotheses to match statements.",d2="hence",cC="we have",bF=")",cq="_tmp",bE="let",aC=":",ee="proof_instr",en="show",bJ="Logic",bD="we",d1="define",cB="reconsider",bI="claim",bC="to",d0="This case should already be trapped",bz="Specif",ac=114,cp="focus",aX="as",dP="No pop/stop expected here",ec=146,ed="per",ah="have",dZ="end",K=a.jsoo_runtime,U=K.caml_check_bound,b=K.caml_new_string,L=K.caml_obj_tag,aB=K.caml_register_global,D=K.caml_wrap_exception;function
i(a,b){return a.length==1?a(b):K.caml_call_gen(a,[b])}function
j(a,b,c){return a.length==2?a(b,c):K.caml_call_gen(a,[b,c])}function
l(a,b,c,d){return a.length==3?a(b,c,d):K.caml_call_gen(a,[b,c,d])}function
M(a,b,c,d,e){return a.length==4?a(b,c,d,e):K.caml_call_gen(a,[b,c,d,e])}function
P(a,b,c,d,e,f){return a.length==5?a(b,c,d,e,f):K.caml_call_gen(a,[b,c,d,e,f])}function
co(a,b,c,d,e,f,g){return a.length==6?a(b,c,d,e,f,g):K.caml_call_gen(a,[b,c,d,e,f,g])}var
e=K.caml_get_global_data(),k=e.Pp,o=e.Errors,aZ=e.Environ,aI=e.Context,x=e.Proof,H=e.Proof_global,n=e.Util,C=e.Goal,h=e.Evd,F=e.Pervasives,cG=e.Pfedit,ai=e.Option,v=e.Loc,z=e.Tacintern,m=e.Term,A=e.Vars,ar=e.Namegen,ad=e.Global,O=e.Assert_failure,W=e.Pretyping,aq=e.Tacinterp,s=e.Names,T=e.Nameops,I=e.Termops,cL=e.Detyping,J=e.CamlinternalLazy,bP=e.Coqlib,cK=e.Universes,R=e.Closure,aa=e.Not_found,a9=e.Reductionops,q=e.Tacmach,t=e.Tactics,r=e.Proofview,p=e.Tacticals,bV=e.Rtree,ax=e.Feedback,bW=e.Inductiveops,aw=e.Inductive,cX=e.Sigma,ae=e.Printer,cZ=e.Locusops,cY=e.Reduction,cW=e.Typing,cV=e.Unification,c0=e.Evarutil,b5=e.Ppconstr,bn=e.Pptactic,b9=e.Compat,b8=e.Egramml,bp=e.Vernac_classifier,b6=e.Vernacinterp,d=e.Pcoq,ao=e.Genarg,b7=e.Gramext,ab=e.CList,bN=[0,0],eH=b('"end claim"'),eI=b('"end focus"'),eJ=b('"end proof"'),eE=b("lonely suppose"),eG=b('"end induction" or start a new case'),eF=b('"end cases" or start a new case'),eD=b("no previous statement to use"),ew=b("get_info"),eM=b('"(_ | _)" is not allowed here'),eN=b("simple_pattern"),eU=b("Anonymous pattern variable"),e0=[0,b(d4),302,18],e1=[0,b(d4),314,18],fa=b('"thesis for" is not applicable here.'),e8=b(" does not occur in pattern."),e9=b("Variable "),e_=b(cz),e7=[2,[0,0]],e2=b("No proof per cases/induction/inversion in progress."),e3=b("expected."),e4=b("none"),e5=b("Wrong number of extra arguments: "),e6=b(cz),eY=b("undetected disjunctive pattern"),eZ=b("empty pattern list"),eQ=b(bG),eR=b(bG),eP=[0,0],eS=b("__"),gs=b(cq),gw=b(dQ),gt=b("Matching hypothesis not found."),gu=b("Last statements do not match a complete hypothesis."),gv=b(cq),gx=b(dQ),gz=[0,b(V),807,18],gL=b(dP),gM=b("Case pattern belongs to wrong inductive type."),gN=b(dP),gO=b("we should pop here"),gP=b("tree is expected to end here"),gQ=b("Premature end of branch"),gV=b('Only "suppose it is" can be used here.'),g6=[0,b(V),1275,2],g7=b("missing case"),g4=b("Nothing to skip"),g5=[0,b(cw)],g8=[0,b(V),1254,9],g9=b("Nothing to split"),g_=[0,b(cw)],g$=b("End of branch with garbage left"),ha=[0,b(cw)],hb=b("wrong stack size"),hn=[0,b(V),1420,1],ho=[0,b(V),1417,1],hp=[0,b(V),1423,1],hq=[0,b(V),1430,1],hr=b("Not applicable"),ht=b(cv),hu=b(cv),hv=b("hd"),hw=b('"end induction" generated an ill-formed fixpoint'),hm=[0,0],he=b("_fix"),hf=b("_main_arg"),hd=b(d0),hi=b(em),hj=b(eb),hk=b(d0),hh=b(d6),hg=b(dV),hc=[2,0],g3=[0,b(V),1214,30],g2=[0,b(V),1216,2],g1=b(eh),gZ=b(eh),gY=[0,[0,0,0],0],gW=b(dR),gX=b("wrong place for cases"),gR=b("cannot give an induction hypothesis (wrong inductive type)."),gS=b(eg),gT=b("cannot give an induction hypothesis (wrong parameters)."),gU=b(eg),gK=b(dR),gJ=b('Do not mix "suppose" with "suppose it is".'),gI=b("wrong stack state"),gG=[0,b(V),929,5],gH=[0,b(V),920,3],gF=b("anonymous_matched"),gE=b("Case analysis must be done on an inductive object."),gD=b("map_tree: not a splitting node"),gC=b("map_tree_rp: not a splitting node"),gA=b(dT),gy=b(cq),gq=b("_cofact"),gp=b("_hyp"),go=b("_claim"),gm=b("No previous equality."),gn=b("_eq"),gj=b(bG),gk=b(bG),gh=b("_fact"),gg=b('"then" and "hence" require at least one previous fact'),gf=b(dT),ge=b("I could not relate this statement to the thesis."),gc=b("cannot happen"),gd=[0,b("concl_refiner")],gb=[0,2,1],ga=[0,2,2],fQ=b(aE),fO=b(dX),fP=b(dX),fE=b("Weird case occurred ..."),fD=b(ea),fF=b(ea),fz=b(d6),fy=b(dV),fA=b(em),fB=b(eb),fC=b('"end proof" expected.'),fx=b("Lonely suppose on stack."),fv=b('"return" cannot be used outside of Declarative Proof Mode.'),fr=[0,0],fm=b('You are inside a proof per cases/induction.\nPlease "suppose" something or "end" it now.'),fh=b("Cannot clear "),fi=b(f),fk=[0,b("Strict"),[0,b("Proofs"),0]],fl=b("strict mode"),fG=b("___"),fI=b("No automation registered"),fR=b(G),fS=[0,b(_),[0,b(bJ),0]],fT=b("and_rect"),fU=[0,b(_),[0,b(bJ),0]],fV=b("prod"),fW=[0,b(_),[0,b(d_),0]],fX=b("prod_rect"),fY=[0,b(_),[0,b(d_),0]],fZ=b("ex"),f0=[0,b(_),[0,b(bJ),0]],f1=b("ex_ind"),f2=[0,b(_),[0,b(bJ),0]],f3=b("sig"),f4=[0,b(_),[0,b(bz),0]],f5=b("sig_rect"),f6=[0,b(_),[0,b(bz),0]],f7=b("sigT"),f8=[0,b(_),[0,b(bz),0]],f9=b("sigT_rect"),f_=[0,b(_),[0,b(bz),0]],hG=b(G),hH=b("be such that"),hI=b("such that"),il=b(bF),im=b(bB),ic=b(bF),id=b(bB),hU=b(ek),hY=b(d2),hX=b(cA),hW=b(bw),hV=b(ah),hZ=b(bw),h0=b(dW),h1=b(d7),h2=b(cC),h3=b(d$),h4=b(bE),h5=b(bE),h6=b(bA),h7=b(bA),h8=b("from "),h9=b(bM),h_=b(bM),h$=b(bI),ia=b("focus on"),ib=b(aX),ie=b(d1),ig=b(aX),ih=b(cB),ii=b(cC),ij=b(cx),ip=b(cC),iq=b(G),io=b(d9),ik=b(cz),ir=b(dS),is=b(ed),it=b(dZ),iE=b(bF),iF=b(aC),iG=b(bB),iA=b(bF),iB=b(aC),iC=b(bB),iu=b("unknown emphasis"),iv=b(dW),iw=b("*   "),ix=b("**  "),iy=b("*** "),hT=b(ep),hS=b(d8),hQ=b(cs),hR=b(eo),hN=b(cu),hO=b(bI),hP=b(cp),hM=b(d3),hL=b(d5),hJ=b("to show"),hK=b("to have"),hC=b(dU),hD=b(aY),hE=b(aY),hB=b(ej),hy=b($),hz=b(cD),hA=b("by *"),hx=b(aC),kJ=[0,0],jE=b(bH),pa=b(bH),o9=[0,[4,b(aE)],0],o8=b(aF),o6=b(bH),o3=b(aF),jt=b(by),pl=b(by),pi=[0,[4,b(aE)],0],ph=b(aF),pf=b(by),pc=b(aF),je=[0,[0,1]],ja=b(bL),pv=b(bL),ps=b(aF),pq=b(bL),pn=b(aF),iZ=[0,[4,b(cv)],0],iU=b(aE),iS=b("Nothing left to prove here."),iT=b(aE),iP=b(ei),iQ=b("Subproof completed, now type "),iL=b("thesis :="),iM=b("============================"),iN=b("  "),iO=b("     *** Declarative Mode ***"),iJ=b(eq),iK=b(eq),iV=b(ee),iW=b("vernac:proof_command"),iY=b(ee),i1=b(ct),i2=b(bL),i4=b(cy),jf=[0,1],jh=b(aE),jk=b(ct),jl=b(by),jn=b(cy),jr=[0,[0,[0,b(cu)],0],0],jv=b(ct),jw=b(bH),jy=b(cy),jC=[0,[0,[0,b("return")],0],0],jF=b(aY),jG=b("statement"),jH=b("constr_or_thesis"),jI=b("statement_or_thesis"),jJ=b("justification_items"),jK=b("justification_method"),jL=b("simple_cut_or_thesis"),jM=b("simple_cut"),jN=b("elim_type"),jO=b("block_type"),jP=b("elim_obj"),jQ=b("elim_step"),jR=b("rew_step"),jS=b("cut_step"),jT=b("loc_id"),jU=b("hyp"),jV=b("consider_vars"),jW=b("consider_hyps"),jX=b("assume_vars"),jY=b("assume_hyps"),jZ=b("assume_clause"),j0=b("suff_vars"),j1=b("suff_hyps"),j2=b("suff_clause"),j3=b("let_vars"),j4=b("let_hyps"),j5=b("given_vars"),j6=b("given_hyps"),j7=b("suppose_vars"),j8=b("suppose_hyps"),j9=b("suppose_clause"),j_=b("intro_step"),j$=b("emphasis"),ka=b("bare_proof_instr"),kd=[0,[10,[0,b(f),b(aY)]],0],kg=[10,[0,b(f),b(dU)]],kh=[10,[0,b(f),b(aY)]],kl=[10,[0,b(f),b(aC)]],kz=[10,[0,b(f),b(aC)]],kM=[10,[0,b(f),b($)]],kN=[10,[0,b(f),b(cD)]],kP=[0,[10,[0,b(f),b(cD)]],[0,[10,[0,b(f),b(dY)]],0]],kV=[10,[0,b(f),b(ej)]],k4=[0,[10,[0,b(c),b(d3)]],0],k6=[0,[10,[0,b(c),b(d5)]],0],k_=[0,[10,[0,b(c),b(bI)]],0],la=[0,[10,[0,b(c),b(cp)]],0],lc=[0,[10,[0,b(c),b(cu)]],0],li=[10,[0,b(c),b(cs)]],lk=[10,[0,b(c),b(eo)]],lp=[10,[0,b(c),b("from")]],lq=[10,[0,b(c),b(bM)]],ls=[10,[0,b(c),b(ed)]],lu=[10,[0,b(c),b(d7)]],ly=[10,[0,b(f),b(ep)]],lA=[10,[0,b(f),b(d8)]],lE=[10,[0,b(f),b(cA)]],lG=[10,[0,b(f),b(cA)]],lI=[10,[0,b(c),b(bw)]],lK=[10,[0,b(c),b(bw)]],lM=[10,[0,b(c),b(d2)]],lQ=[10,[0,b(c),b(ah)]],lS=[10,[0,b(c),b(bI)]],lU=[10,[0,b(c),b(cs)]],lV=[10,[0,b(c),b(cp)]],lX=[10,[0,b(f),b(dZ)]],lZ=[0,[10,[0,b(c),b(ek)]],0],l8=[10,[0,b(f),b(aC)]],ma=[0,[10,[0,b(f),b($)]],[0,0,0]],mc=[10,[0,b(c),b(aD)]],md=[10,[0,b(c),b(aG)]],mh=[0,[10,[0,b(c),b(G)]],[0,0,0]],mj=[10,[0,b(c),b(bM)]],mk=[10,[0,b(c),b(G)]],mq=[0,[10,[0,b(f),b($)]],[0,0,0]],ms=[10,[0,b(c),b(aD)]],mt=[10,[0,b(c),b(aG)]],mx=[0,[10,[0,b(c),b(G)]],[0,0,0]],mz=[10,[0,b(c),b(ah)]],mA=[10,[0,b(c),b(bD)]],mB=[10,[0,b(c),b(G)]],mG=[10,[0,b(c),b(ah)]],mH=[10,[0,b(c),b(bD)]],mM=[10,[0,b(c),b(en)]],mN=[10,[0,b(c),b(bC)]],mP=[0,[10,[0,b(f),b($)]],[0,0,0]],mR=[10,[0,b(c),b(aD)]],mS=[10,[0,b(c),b(aG)]],mW=[0,[10,[0,b(c),b(G)]],[0,0,0]],mY=[10,[0,b(c),b(ah)]],mZ=[10,[0,b(c),b(bC)]],m0=[10,[0,b(c),b(G)]],m2=[10,[0,b(c),b(en)]],m3=[10,[0,b(c),b(bC)]],m7=[10,[0,b(c),b(ah)]],m8=[10,[0,b(c),b(bC)]],nc=[0,[10,[0,b(f),b($)]],[0,0,0]],ne=[10,[0,b(c),b(aD)]],nf=[10,[0,b(c),b(aG)]],ng=[10,[0,b(c),b(el)]],nk=[0,[10,[0,b(c),b(G)]],[0,0,0]],nm=[10,[0,b(f),b(bE)]],nn=[10,[0,b(c),b(G)]],nt=[0,[10,[0,b(f),b($)]],[0,0,0]],nv=[10,[0,b(c),b(aD)]],nw=[10,[0,b(c),b(aG)]],nA=[0,[10,[0,b(c),b(G)]],[0,0,0]],nC=[10,[0,b(c),b(bA)]],nD=[10,[0,b(c),b(G)]],nJ=[0,[10,[0,b(f),b($)]],[0,0,0]],nM=[10,[0,b(c),b(aD)]],nN=[10,[0,b(c),b(aG)]],nQ=[0,[10,[0,b(c),b(el)]],0],nV=[0,[10,[0,b(c),b(G)]],[0,0,0]],nX=[10,[0,b(c),b(ah)]],nY=[10,[0,b(c),b(bD)]],nZ=[10,[0,b(c),b(G)]],n4=[10,[0,b(c),b(ah)]],n5=[10,[0,b(c),b(bD)]],n_=[10,[0,b(c),b(cx)]],od=[10,[0,b(c),b(G)]],oh=[10,[0,b(f),b($)]],oi=[10,[0,b(f),b(d9)]],ol=b("0"),om=[10,[0,b(c),b("is")]],on=[10,[0,b(c),b("it")]],oo=[10,[0,b(c),b(cx)]],oq=[10,[0,b(f),b(bE)]],ot=[10,[0,b(f),b($)]],ou=[10,[0,b(c),b(dS)]],ow=[10,[0,b(c),b(d$)]],oy=[10,[0,b(c),b(bA)]],oB=[10,[0,b(f),b(aX)]],oC=[10,[0,b(c),b(d1)]],oF=[10,[0,b(f),b(aX)]],oG=[10,[0,b(c),b(cB)]],oJ=[10,[0,b(f),b(aX)]],oK=[10,[0,b(c),b(cB)]],oO=[0,[10,[0,b(f),b(dY)]],0],oQ=[0,[10,[0,b(f),b("**")]],0],oS=[0,[10,[0,b(f),b("***")]],0],o0=[0,[10,[0,b(f),b(ei)]],0],eK=e.Constrintern,fc=e.Failure,fe=e.Type_errors,fg=e.Glob_ops,fd=e.Invalid_argument,ff=e.Goptions,iH=e.Mltop,iI=e.G_vernac;function
es(a){bN[1]=1;return 0}function
et(a){bN[1]=0;return 0}function
eu(a){return bN[1]}var
a0=i(h[3][6],0);function
cH(a){var
b=i(x[33][1],a),d=i(n[17][3],b[1]),e=j(C[4][5],b[2],d),c=j(h[3][3],e,a0),f=c?1:c;return f}function
cI(a){try{var
b=cH(i(cG[9],0));return b}catch(f){f=D(f);if(f===H[11])return 2;throw f}}function
ev(a){return 1===cI(0)?i(o[6],a):0}function
bO(a,b){var
d=j(C[4][5],a,b),c=j(h[3][3],d,a0);return c?c[1]:i(F[1],ew)}function
ex(a,b){var
c=j(C[4][5],a,b);return j(h[3][3],c,a0)}var
a1=i(x[16],0),ey=j(x[18],0,a1);function
ez(a){var
b=i(x[33][1],a),c=i(n[17][3],b[1]),d=bO(b[2],c)[1];function
e(a){return l(x[19],ey,d,1)}return i(H[27],e)}function
eA(a){function
b(a,b){return l(x[23],a1,b,0)}return i(H[27],b)}function
cJ(a){try{var
c=j(x[26],a1,a);return c}catch(f){f=D(f);if(f===x[25]){var
b=i(x[33][3],a);return bO(b[2],b[1])[1]}throw f}}function
eB(a){return j(x[26],a1,a)}function
eC(a){var
b=i(aZ[9],a);return b?i(aI[2][1][1],b[1]):i(o[6],eD)}var
y=[0,es,et,eu,cH,cI,ev,a0,bO,ex,eB,cJ,eC,function(a){var
b=cJ(a);if(b){var
c=b[1];if(typeof
c==="number")switch(c){case
0:var
e=b[2];if(e){var
f=e[1];if(typeof
f==="number")var
d=0;else
var
g=f[1],d=1}else
var
d=0;if(!d){var
h=i(k[1],eE);return l(o[3],0,0,h)}break;case
1:return eH;default:return eI}else
var
g=c[1];return 0===g?eF:eG}return eJ},ez,eA];aB(419,y,"Decl_mode_plugin.Decl_mode");function
cM(a){var
b=a[3],c=a[2];return i(n[17][47],b)?c:[4,a[1],c,b]}function
as(a,b,c){var
d=j(a,b,c[2]);return[0,c[1],d]}function
bQ(a,b,c){return[0,b,j(a,b,c)]}function
bR(a,b){return 0===b[0]?[0,j(z[7],a,b[1])]:[1,b[1]]}function
aJ(a,b){var
c=b[2];return[0,j(s[1][9][4],a,b[1]),c]}function
cN(a,b,c){if(0===c[0]){var
d=c[1],e=d[2],f=e[1],k=e[2],l=i(z[7],b),m=[0,f,j(ai[15],l,k)],n=[0,[0,d[1],m]];return[0,aJ(f,b),n]}var
g=c[1],o=[1,as(a,b,g)],h=g[1],p=h?aJ(h[1],b):b;return[0,p,o]}function
at(d,b,c){function
a(a,b){return cN(d,a,b)}return l(n[17][cE],a,b,c)[2]}function
a2(a,b,c){var
d=j(a,b,c[1]),e=d[1],k=c[3],h=i(z[5],e),l=i(i(ai[15],h),k),m=c[2],f=i(z[7],e),g=i(n[17][12],f),o=i(i(ai[15],g),m);return[0,d[2],o,l]}function
cO(a,b){function
c(a,b){var
c=b[2],d=c[1],e=c[2],f=i(z[7],a),g=[0,d,j(ai[15],f,e)],h=[0,b[1],g];return[0,aJ(d,a),h]}return l(n[17][cE],c,b,a)}function
eL(a,b){var
e=b[1],f=z[7];function
g(a,b){return cN(f,a,b)}var
c=l(n[17][cE],g,a,e),d=c[1],h=bR(d,b[2]);return[0,d,[0,c[2],h]]}function
a3(a,b){var
d=a,c=b;for(;;){switch(c[0]){case
0:var
h=c[2],d=aJ(c[3],d),c=h;continue;case
1:var
m=c[4],p=c[3],q=i(n[17][15],a3),r=l(ai[17],q,d,p);return l(n[17][15],a3,r,m);case
2:var
e=c[2];if(e){var
f=e[1];if(0!==f[0])return aJ(f[1][2],d)}break;case
3:var
s=i(k[1],eM);return j(v[10],c[1],[0,o[5],eN,s]);case
4:var
g=c[3],t=i(n[17][10],[0,g[1],[0,c[4],g[2]]]);return l(n[17][15],a3,d,t);case
7:var
c=c[3];continue}return d}}function
a4(a,b){if(typeof
b==="number")return 0;else
switch(b[0]){case
0:return[0,a4(a,b[1])];case
1:return[1,a4(a,b[1])];case
2:return[2,a4(a,b[1])];case
3:var
o=b[1],p=function(a,b){return as(bR,a,b)};return[3,a2(function(a,b){return bQ(p,a,b)},a,o)];case
4:var
q=b[2],r=z[7],s=function(a,b){return as(r,a,b)},t=a2(function(a,b){return bQ(s,a,b)},a,q);return[4,b[1],t];case
5:return[5,a2(eL,a,b[1])];case
6:return[6,at(z[7],a,b[1])];case
7:return[7,at(z[7],a,b[1])];case
8:return[8,at(z[7],a,b[1])];case
9:var
u=at(z[7],a,b[2]);return[9,j(z[7],a,b[1]),u];case
10:return[10,as(z[7],a,b[1])];case
11:return[11,as(z[7],a,b[1])];case
12:var
v=b[3],d=cO(b[2],a),m=j(z[7],d[1],v);return[12,b[1],d[2],m];case
13:var
w=j(z[7],a,b[2]);return[13,b[1],w];case
14:return[14,at(z[7],a,b[1])];case
15:var
e=b[2],f=cO(b[1],a),x=a3(f[1],e),y=at(bR,x,b[3]);return[15,f[2],e,y];case
16:var
A=b[1],B=i(z[7],a);return[16,j(n[17][12],B,A)];case
17:var
c=b[2];if(0===c[0])var
g=[0,j(z[7],a,c[1])];else
var
h=c[1],k=z[7],l=function(a,b){return as(k,a,b)},g=[1,a2(function(a,b){return bQ(l,a,b)},a,h)];return[17,b[1],g];default:return[18,b[1]]}}function
eO(a,b){var
c=a4(a,b[2]);return[0,b[1],c]}function
au(a,b,c,d){return a?P(W[11],0,eP,b,c,d[1])[1]:P(W[11],0,0,b,c,d[1])[1]}var
a5=[E,function(a){return i(cK[48],bP[30])}];function
aK(a,b,c,d){var
e=l(a,b,c,d[2]);return[0,d[1],e]}function
cP(j,b,c){function
a(a,b){if(0===a[0]){var
d=a[1],e=d[2],f=e[2],c=e[1];return f?[6,v[4],[0,c],0,f[1][1],b]:[6,v[4],[0,c],0,[13,[0,d[1],[1,[0,c]],0,0]],b]}var
g=a[1],h=i(j,g[2]);return[6,v[4],g[1],0,h,b]}return l(n[17][16],a,b,c)}var
cQ=[12,v[4],0];function
bS(a,b,c,d){if(d){var
g=d[1],e=i(m[34],c),f=e[1],h=[0,f,j(A[12],b,e[2])],l=f?[0,i(m[u],f[1]),b]:[0,i(m[ac],0),b],n=0===g[0]?[0,h]:[1,j(a,h,g[1])],k=bS(a,l,e[3],d[2]);return[0,[0,n,k[1]],k[2]]}return[0,0,j(A[12],b,c)]}function
cR(a,b,c,d,e,f){var
g=cP(a,e,f);return bS(b,0,P(W[11],0,0,c,d,g)[1],e)}function
av(a,b,c){function
d(a,b){return a}return cR(function(a){return a[1]},d,a,b,c,cQ)[1]}var
eT=i(s[1][5],eS);function
cS(c,b){if(0===b[0]){var
e=b[2],a=b[1];if(e){var
f=c[1];c[1]=[0,[0,[0,a,e[1]],f[1]],f[2]];return b}var
g=c[1],h=g[2],d=j(ar[25],eT,h);c[1]=[0,[0,[0,a,d],g[1]],[0,d,h]];return[0,a,[0,d]]}var
i=b[4],k=b[3];function
l(a){return cS(c,a)}var
m=j(n[17][12],l,k);return[1,b[1],b[2],m,i]}function
bT(a){if(0===a[0]){var
d=a[2];if(d)return[1,[0,a[1],d[1]]];var
g=i(k[1],eU);return l(o[3],0,0,g)}var
e=a[2],f=e[1],h=i(ad[26],f)[1],p=j(n[17][12],bT,a[3]),b=h[6],c=p;for(;;){if(0<b){var
m=[0,[13,[0,v[4],[4,f,b],0,0]],c],b=b-1|0,c=m;continue}return cM([0,a[1],[0,[0,v[4],[3,e],0]],c])}}function
eV(a){var
c=a[2],d=c[2],b=c[1],e=a[1];if(d){var
f=d[1];return function(a){return[6,v[4],[0,b],0,f[1],a]}}return function(a){return[6,v[4],[0,b],0,[13,[0,e,[1,[0,b]],0,0]],a]}}function
eW(a,b){var
c=a[2];return[6,v[4],[0,c],0,[13,[0,a[1],[1,[0,c]],0,0]],b]}function
eX(a,b){var
c=bT(a[2]);return[7,v[4],[0,a[1]],c,b]}function
cT(a,b){if(0===b[0])return a;var
c=b[4],d=c?[0,[0,c[1],b],a]:a;return l(n[17][15],cT,d,b[3])}function
a6(a,b,c,d){if(d){var
e=i(a,c),f=e[1],h=[0,f,j(A[12],b,e[2])];if(f){var
k=[0,i(m[u],f[1]),b],g=a6(a,k,e[3],d[2]);return[0,[0,h,g[1]],g[2],g[3]]}throw[0,O,e0]}return[0,0,b,j(A[12],b,c)]}function
cU(a,b,c){if(c){var
d=i(m[36],b),e=d[1],g=j(A[12],a,d[3]),h=[0,e,[0,j(A[12],a,d[2]),g]];if(e){var
k=[0,i(m[u],e[1]),a],f=cU(k,d[4],c[2]);return[0,[0,h,f[1]],f[2],f[3]]}throw[0,O,e1]}return[0,0,a,j(A[12],a,b)]}function
a7(a,b,c,d){var
e=l(a,b,c,d[1]),h=d[3],k=i(aq[28],0),m=i(aq[2][6],k),o=j(ai[15],m,h),p=d[2],q=e[1];function
f(a){return P(W[11],0,0,q,c,a[1])[1]}var
g=i(n[17][12],f),r=i(i(ai[15],g),p);return[0,e[2],r,o]}function
bU(a,b,c,d){return[0,b,l(a,b,c,d)]}function
e$(d,b,c){var
a=c[2],f=c[1];if(0===a[0])var
h=a[1][1],k=function(a,b){return a},g=cR(function(a){return a[1]},k,d,b,f,h),e=[0,g[1],[0,g[2]]];else
var
e=a[1]?i(o[6],fa):[0,av(d,b,f),a];function
m(a,b){var
c=a[1],e=c[1];return e?j(aZ[31],[0,e[1],c[2]],b):d}return[0,l(n[17][16],m,e[1],d),e]}function
fb(a){var
c=a[2],d=c[2],b=c[1],e=a[1];if(d){var
f=d[1];return function(a){return[5,v[4],[0,b],0,f[1],a]}}return function(a){return[5,v[4],[0,b],0,[13,[0,e,[1,[0,b]],0,0]],a]}}function
a8(a,b,c,d){if(typeof
d==="number")return 0;else
switch(d[0]){case
0:return[0,a8(a,b,c,d[1])];case
1:return[1,a8(a,b,c,d[1])];case
2:return[2,a8(a,b,c,d[1])];case
3:var
a4=d[1],a9=1,a_=function(a,b,c){return 0===c[0]?[0,au(a9,a,b,c[1])]:[1,c[1]]},a$=function(a,b,c){return aK(a_,a,b,c)};return[3,a7(function(a,b,c){return bU(a$,a,b,c)},b,c,a4)];case
4:var
ba=d[2],aj=i(y[12],b),ah=j(aZ[37],aj,b),ae=l(R[37],0,R[11],b),af=i(R[31],ah),ag=j(R[42],ae,af),p=i(m[aH],ag);if(9===p[0]){var
D=p[2],F=L(a5),ai=N===F?a5[1]:E===F?i(J[2],a5):a5;if(j(m[136],p[1],ai))if(3===D.length-1)var
z=U(D,0)[1],B=1;else
var
B=0;else
var
B=0;if(!B)var
z=i(o[6],eR)}else
var
z=i(o[6],eQ);var
bb=function(a,b,c){return P(W[11],0,[0,[0,z]],a,b,c[1])[1]},bc=function(a,b,c){return aK(bb,a,b,c)},bd=a7(function(a,b,c){return bU(bc,a,b,c)},b,c,ba);return[4,d[1],bd];case
5:return[5,a7(e$,b,c,d[1])];case
6:return[6,av(b,c,d[1])];case
7:return[7,av(b,c,d[1])];case
8:return[8,av(b,c,d[1])];case
9:var
be=av(b,c,d[2]);return[9,au(0,b,c,d[1]),be];case
10:var
bf=d[1],bg=1;return[10,aK(function(a,b,c){return au(bg,a,b,c)},b,c,bf)];case
11:var
bh=d[1],bi=1;return[11,aK(function(a,b,c){return au(bi,a,b,c)},b,c,bh)];case
12:var
$=d[2],a2=l(n[17][16],fb,$,d[3][1]),a3=P(W[11],0,0,b,c,a2)[1],_=a6(m[35],0,a3,$);return[12,d[1],_[1],_[3]];case
13:var
bj=au(1,b,c,d[2]);return[13,d[1],bj];case
14:return[14,av(b,c,d[1])];case
15:var
aa=d[3],ab=d[2],g=d[1],M=a[1];if(M){var
O=M[1];if(typeof
O==="number")var
C=0;else
var
e=O[2],C=1}else
var
C=0;if(!C)var
e=i(o[6],e2)[2];var
Q=i(ad[26],e[3]),r=Q[1][6]-e[7]|0;if(1-(i(n[17][1],g)===r?1:0)){var
ar=i(k[1],e3),as=i(k[17],0),at=0===r?i(k[1],e4):i(k[20],r),aw=i(k[1],e5),ax=j(k[14],aw,at),ay=j(k[14],ax,as),az=j(k[14],ay,ar);j(o[7],e6,az)}var
aA=[0,[0,v[4],[2,e[3]],0]],aB=e[6],aC=function(a){return co(cL[6],0,0,0,b,h[16],a)},aD=j(n[17][12],aC,aB),aE=function(a){return[1,[0,a[1],a[2][1]]]},aF=j(n[17][12],aE,g),aG=function(a){return[13,[0,v[4],e7,0,0]]},aI=j(n[17][48],Q[2][6],aG),aJ=j(n[18],aF,aI),aL=j(n[18],aD,aJ),aM=cM([0,v[4],aA,aL]),G=j(eK[8],b,ab),q=G[2],an=G[1];if(q)if(q[2])var
ao=i(k[1],eY),f=l(o[3],0,0,ao);else
var
H=q[1],K=H[2],ap=H[1],am=cT(0,K),ak=function(a,b,c){return[0,[0,a,j(s[1][12][3],b,c)],c]},al=l(s[1][10][11],ak,ap,am),f=[0,an,i(n[17][6],al),K];else
var
aq=i(k[1],eZ),f=l(o[3],0,0,aq);var
S=f[3],V=f[2],X=f[1],Y=[0,[0,0,X]],aN=cP(function(a){if(0===a[0])return a[1][1];var
b=a[1];if(b){var
c=b[1];if(1-j(s[1][12][2],c,X)){var
d=i(k[1],e8),e=i(T[1],c),f=i(k[1],e9),g=j(k[14],f,e),h=j(k[14],g,d);j(o[7],e_,h)}return[12,v[4],0]}return[12,v[4],0]},aa,cQ),aO=cS(Y,S),Z=i(n[17][6],Y[1][1]),aP=bT(aO),aQ=l(n[17][16],eX,V,[7,v[4],0,[14,v[4],aP,[0,aM]],aN]),aR=l(n[17][16],eW,Z,aQ),aS=l(n[17][16],eV,g,aR),aT=P(W[11],0,0,b,c,aS)[1],t=a6(m[34],0,aT,g),u=a6(m[34],t[2],t[3],Z),w=cU(u[2],u[3],V),x=i(m[36],w[3]),aU=function(a,b){var
c=b[2];return 0===c[0]?[0,a[1],[0,a[2]]]:[0,b[1],[1,c[1]]]},aV=i(I[54],x[4]),aW=bS(aU,w[2],aV,aa)[1];return[15,t[1],[0,u[1],w[1],x[2],x[3],S,ab],aW];case
16:var
bk=d[1],bl=function(a){return P(W[11],0,0,b,c,a[1])[1]};return[16,j(n[17][12],bl,bk)];case
17:var
A=d[2];if(0===A[0])var
ac=[0,P(W[11],0,0,b,c,A[1][1])[1]];else
var
aX=A[1],aY=1,a0=function(a,b,c){return au(aY,a,b,c)},a1=function(a,b,c){return aK(a0,a,b,c)},ac=[1,a7(function(a,b,c){return bU(a1,a,b,c)},b,c,aX)];return[17,d[1],ac];default:return[18,d[1]]}}var
aL=[0,eO,function(a,b,c,d){var
e=a8(a,b,c,d[2]);return[0,d[1],e]}];aB(439,aL,"Decl_mode_plugin.Decl_interp");function
aj(a,b){var
d=b[2],c=b[1],q=s[1][9][1];function
r(a,b){return j(s[1][9][4],b,a)}var
t=l(n[17][15],r,q,a),u=j(C[4][1],d,c),v=j(C[4][2],d,c),w=j(C[4][4],d,c),g=[0,i(h[98],d)];try{var
B=P(c0[52],u,g,v,w,t),e=B}catch(f){f=D(f);if(f[1]!==c0[50])throw f;var
x=i(T[1],f[2]),y=i(k[1],fh),z=j(k[14],y,x),e=j(o[7],fi,z)}var
m=g[1],A=j(C[4][5],m,c),f=M(C[4][6],m,e[1],e[2],A),p=f[1];return[0,[0,p,0],M(C[4][8],f[3],c,p,f[2])]}function
af(a){return j(y[8],a[2],a[1])}var
c1=[0,0];function
fj(a){c1[1]=a;return 0}function
c2(a){return c1[1]}i(ff[4],[0,1,0,fl,fk,c2,fj]);function
c3(a,b){var
c=i(q[1],b),e=i(q[7],b),f=i(q[2],b),g=j(C[4][2],f,c),h=i(q[2],b),k=i(a,j(C[4][5],h,c)),m=i(q[2],b),d=M(C[4][6],m,g,e,k),n=l(C[4][7],d[3],c,d[2]);return[0,[0,d[1],0],n]}function
X(c,b){return c3(function(a){return l(h[3][2],a,y[7],c)},b)}function
aM(a){return c3(function(a){return j(h[3][4],a,y[7])},a)}function
aN(a){var
b=i(q[8],a),c=l(R[37],0,R[11],b);return function(a){var
b=i(R[31],a);return j(R[42],c,b)}}function
c4(a,b){var
c=j(aw[4],a,b),d=c[2],e=0===d[6]?1:0,f=e?1-i(bW[19],[0,b,c[1],d]):e;return f}function
fn(a,b){var
c=i(h[69],b),d=i(h[98],c);function
e(a,b){return M(h[95],a[1],a[2],0,b)}return l(n[17][16],e,a,d)}function
fo(a){return 95===K.caml_string_get(i(s[1][7],a),0)?1:0}function
fp(a){function
c(a){if(a){var
b=c(a[2]),d=a[1],e=function(a){var
b=[0,d,0];function
c(a){return aj(b,a)}return j(p[21],c,a)};return j(p[5],e,b)}return p[1]}var
b=i(q[9],a);if(b)var
e=i(I[80],b[2]),d=j(n[17][29],fo,e);else
var
d=b;return i(c(d),a)}function
ak(a,b){return j(t[138],[0,a],b)}function
fq(a){return X(fr,a)}function
fs(a){var
b=i(r[66][1],fq);i(cG[21],b);var
c=i(H[12],0);return i(y[14],c)}function
bX(a){var
e=i(ad[2],0);function
c(a,b){var
d=i(r[37],r[54]),c=l(x[29],e,d,b);return[0,c[1],c[2][1]]}var
b=1-i(H[26],c);return b?l(ax[8],0,0,3):b}function
al(a){return i(y[15],0)}function
ft(a){return al(0)}function
fu(a){try{bX(0);var
b=al(0);return b}catch(f){f=D(f);if(f===aa)return i(o[6],fv);throw f}}function
fw(a){return fu(0)}function
c5(a){if(i(x[7],a)){var
b=i(y[11],a);if(b){var
d=b[1];if(typeof
d!=="number"){var
j=i(k[1],fE);return l(o[3],0,0,j)}if(0===d){var
e=b[2];if(e)if(typeof
e[1]!=="number")return al(0)}}return i(o[6],fD)}var
c=i(y[10],a);if(c){var
f=c[1],g=typeof
f==="number";if(!g)return g;if(0===f){var
h=c[2];if(h)if(typeof
h[1]!=="number"){bX(0);return al(0)}}}return i(o[6],fF)}var
fH=i(s[1][5],fG);function
bY(a,b){var
c=[0,s[1][9][1]];function
d(a,b){var
e=i(m[aH],a);if(1===e[0]){c[1]=j(s[1][9][4],e[1],c[1]);return i(p[1],b)}var
d=j(q[20],fH,b);c[1]=j(s[1][9][4],d,c[1]);var
f=i(t[75],[0,d,0]),g=i(r[66][8],f),h=P(t[ec],0,[0,d],a,0,cZ[7]),k=i(r[66][8],h);return l(p[5],k,g,b)}var
e=l(p[32],d,a,b),f=0,g=c[1],h=[0,function(a){function
b(a){var
b=i(aI[2][1][1],a);if(j(s[1][9][3],b,g))return p[1];var
c=[0,b,0];function
d(a){return aj(c,a)}return i(p[21],d)}var
c=i(q[9],a);return l(p[32],b,c,a)},f],k=[0,function(a){return e},h];return j(p[7],k,b)}var
fJ=i(k[1],fI),fK=j(o[2],0,fJ),c6=[0,j(r[18],0,fK)];function
fL(a){c6[1]=a;return 0}function
fM(a){return c6[1]}var
fN=i(r[13],0),a_=j(r[14],fN,fM);function
aO(a,b){function
c(a){if(c2(0))return i(o[6],fO);var
b=i(k[1],fP);i(ax[13],b);i(y[1],0);return[0,0,i(h[69],a)]}var
d=i(r[66][8],t[41]),e=[0,j(p[5],a,d),0],f=i(p[20],e);return l(p[4],f,c,b)}function
c7(c,b){var
a=i(r[66][8],a_);function
d(a){return bY(c,a)}return aO(j(p[5],d,a),b)}function
Y(c,b){return[E,function(a){return l(bP[4],fQ,c,b)}]}var
a$=Y(fS,fR),ba=Y(fU,fT),bb=Y(fW,fV),bc=Y(fY,fX),bd=Y(f0,fZ),be=Y(f2,f1),bf=Y(f4,f3),bg=Y(f6,f5),bh=Y(f8,f7),bi=Y(f_,f9);function
bZ(a,b,c){if(c){var
d=c[2],e=c[1];return a===e[1]?j(n[18],b,d):[0,e,bZ(a,b,d)]}throw aa}function
f$(f,e,c,d){var
r=e[2],s=i(aN(d),r),b=i(m[39],s),p=b[2],k=i(m[aH],b[1]);if(11===k[0]){var
a=k[1],g=a[1],t=a[2];if(c4(f,g)){var
o=j(aw[4],f,g),u=j(aw[17],a,[0,o[1],o[2]]),v=function(a,b){var
s=[0,i(m[131],[0,[0,g,a+1|0],t]),p],u=i(m[59],s),v=j(m[76],b,p),w=j(cY[26],f,v);function
r(a,b,c){if(c){var
e=a+1|0,n=c[2],f=r(e,[0,i(m[ac],e),b],n),o=f[3],p=i(aI[1][1][3],c[1]),s=j(A[12],b,p),g=i(q[8],d),h=l(R[37],0,R[12],g),k=i(R[31],s),t=[0,[0,e,j(R[41],h,k)],o];return[0,f[1],f[2],t]}return[0,a,b,0]}var
x=i(n[17][6],w[1]),k=r(e[3],0,x),o=k[3],y=k[1],z=[0,u,i(n[17][6],k[2])],B=i(m[59],z),C=l(h[96],e[1],[0,B,ga],e[5]),D=bZ(e[1],o,e[4]);function
E(a,b){return M(h[95],b[1],b[2],0,a)}var
F=l(n[17][15],E,C,o),G=i(n[17][6],o);function
H(a){return j(n[22][3],[0,a[1],a[2],y,D,F],c)}return j(n[17][11],H,G)};return j(n[19][14],v,u)}}return 0}function
c8(a,b){var
c=b;for(;;){if(c){var
d=c[2],e=c[1],f=e[1];if(j(h[88],a,f)){var
c=d;continue}var
i=c8(a,d),g=[0,[0,f,j(a9[97],a,e[2])],i]}else
var
g=c;return g}}function
c9(a,W,r){var
g=i(q[7],r),v=i(h[69],r),b=i(q[8],r),c=l(cW[4],b,[0,v],g),X=i(m[ef],c);function
w(a,b,c,d){if(d){var
x=d[2],y=d[1],z=y[1],e=j(I[53],c,y[2]),Y=l(ar[9],a,e,0),f=l(t[14],b,Y,r),A=j(aZ[31],[0,f,e],a),Z=l(cW[4],A,[0,v],e),B=i(m[ef],Z),_=[0,[0,z,i(m[u],f)],c];if(i(n[17][47],x)){var
$=j(I[53],_,W);return[0,B,e,l(m[50],f,e,$)]}var
s=w(A,[0,f,b],[0,[0,z,i(m[u],f)],c],x),h=s[2],C=s[1],q=l(m[50],f,e,s[3]),aa=i(m[u],f);if(j(I[52],aa,h)){var
p=l(m[50],f,e,h);if(0===C){if(0===X){var
D=L(bd),ab=[0,e,p],ac=N===D?bd[1]:E===D?i(J[2],bd):bd,F=L(be),ad=i(m[Q],[0,ac,ab]),ae=[0,e,p,g,q],af=N===F?be[1]:E===F?i(J[2],be):be;return[0,0,ad,i(m[Q],[0,af,ae])]}var
G=L(bf),ag=[0,e,p],ah=N===G?bf[1]:E===G?i(J[2],bf):bf,H=i(m[Q],[0,ah,ag]),K=L(bg),ai=[0,e,p,i(m[bx],[0,0,H,g]),q],aj=N===K?bg[1]:E===K?i(J[2],bg):bg;return[0,2,H,i(m[Q],[0,aj,ai])]}var
M=L(bh),ak=[0,e,p],al=N===M?bh[1]:E===M?i(J[2],bh):bh,O=i(m[Q],[0,al,ak]),P=L(bi),am=[0,e,p,i(m[bx],[0,0,O,g]),q],an=N===P?bi[1]:E===P?i(J[2],bi):bi;return[0,2,O,i(m[Q],[0,an,am])]}if(0===B)if(0===C){var
R=L(a$),ao=[0,e,h],ap=N===R?a$[1]:E===R?i(J[2],a$):a$,S=L(ba),aq=i(m[Q],[0,ap,ao]),as=[0,e,h,g,q],at=N===S?ba[1]:E===S?i(J[2],ba):ba;return[0,0,aq,i(m[Q],[0,at,as])]}var
T=L(bb),au=[0,e,h],av=N===T?bb[1]:E===T?i(J[2],bb):bb,U=i(m[Q],[0,av,au]),V=L(bc),aw=[0,e,h,i(m[bx],[0,0,U,g]),q],ax=N===V?bc[1]:E===V?i(J[2],bc):bc;return[0,2,U,i(m[Q],[0,ax,aw])]}var
ay=i(k[1],gc);return l(o[3],0,gd,ay)}var
d=w(b,0,0,a),e=[0,i(m[ac],1)];return i(m[Q],[0,d[3],e])}function
aP(C,b,c,d){try{var
p=i(q[8],d),a=i(q[7],d),v=fn([0,[0,0,a],c],d),g=i(n[22][2],0),H=0,w=0,x=function(a,b){return j(F[5],a,b[1])},y=[0,0,a,l(n[17][15],x,w,c),[0,[0,0,a],0],v];j(n[22][3],y,g);var
f=function(a){for(;;){var
e=i(n[22][9],g);try{var
k=e[2],m=[0,i(cV[5],0)],q=co(cV[8],p,e[5],1,m,b,k);if(0<a)var
j=f(a-1|0);else
var
r=l(h[96],e[1],[0,C,gb],q),s=bZ(e[1],c,e[4]),j=[0,e[1],e[2],e[3],s,r];return j}catch(f){f=D(f);if(i(o[22],f)){f$(p,e,g,d);continue}throw f}}};try{var
e=f(H)}catch(f){f=D(f);if(f===n[22][1])throw aa;throw f}var
z=i(m[ac],0),A=j(a9[97],e[5],z),B=[0,c8(e[5],e[4]),A],k=B}catch(f){f=D(f);if(f!==aa)throw f;var
k=i(o[6],ge)}var
s=k[2],u=k[1];if(i(n[17][47],u)){var
E=i(t[45],s);return j(r[66][8],E,d)}var
G=c9(u,s,d);return j(q[45],G,d)}function
c_(a,b,c){return 0===c[0]?c[1]:c[1]?i(o[6],gf):i(q[7],b)}function
c$(a,b,c,d){if(a)try{var
h=i(q[8],d),k=i(y[12],h),l=[0,i(m[u],k),0],e=l}catch(f){f=D(f);if(f[1]!==o[5])throw f;var
e=i(o[6],gg)}else
var
e=a;function
f(a){var
c=b[2];return c?bY(j(n[18],e,c[1]),a):i(p[1],a)}function
g(a){var
c=b[3];if(c){var
d=c[1],e=i(aq[28],0),f=j(aq[19],e,d);return j(r[66][8],f,a)}return j(r[66][8],a_,a)}return aO(j(p[5],f,g),d)}function
da(a,b,c,d,e){var
g=af(e),h=d[1],k=h[1];if(k)var
f=k[1];else
var
y=i(s[1][5],gh),f=j(q[20],y,e);var
n=l(a,g,e,h[2]),o=[0,function(a){return b?aP(i(m[u],f),n,0,a):i(p[1],a)},0];function
t(a){return c$(c,d,g,a)}var
v=[0,j(p[5],aM,t),o],w=ak(f,n),x=i(r[66][8],w);return l(p[11],x,v,e)}var
S=[E,function(a){return i(cK[48],bP[30])}];function
gi(a,b){var
f=j(q[19],b,a),g=i(aN(b),f),d=i(m[aH],g);if(9===d[0]){var
c=d[2],e=L(S),h=N===e?S[1]:E===e?i(J[2],S):S;if(j(I[62],d[1],h))if(3===c.length-1){var
k=U(c,2)[3],l=U(c,1)[2];return[0,U(c,0)[1],l,k]}return i(o[6],gk)}return i(o[6],gj)}function
gl(C,b,c,d){try{var
am=i(q[8],d),an=i(y[12],am),a=an}catch(f){f=D(f);if(f[1]!==o[5])throw f;var
a=i(o[6],gm)}var
f=gi(a,d),g=f[3],h=f[2],k=f[1];function
F(a){var
b=c[2];return b?bY(b[1],a):i(p[1],a)}function
G(a){var
b=c[3];if(b){var
d=b[1],e=i(aq[28],0),f=j(aq[19],e,d);return j(r[66][8],f,a)}return j(r[66][8],a_,a)}function
n(a){return aO(j(p[5],F,G),a)}var
v=c[1][1];if(v)var
e=v[1];else
var
al=i(s[1][5],gn),e=j(q[20],al,d);function
w(a,b){return C?aP(i(m[u],e),a,0,b):i(p[1],b)}if(0===b){var
x=L(S),H=[0,k,c[1][2],g],I=N===x?S[1]:E===x?i(J[2],S):S,z=i(m[Q],[0,I,H]),K=0,M=[0,function(a){return w(z,a)},K],O=i(m[u],a),P=i(t[45],O),R=[0,n,[0,i(r[66][8],P),0]],T=i(t[cr],h),U=i(r[66][8],T),V=j(p[11],U,R),W=[0,j(p[5],aM,V),M],X=ak(e,z),Y=i(r[66][8],X);return l(p[11],Y,W,d)}var
A=L(S),Z=[0,k,h,c[1][2]],_=N===A?S[1]:E===A?i(J[2],S):S,B=i(m[Q],[0,_,Z]),$=0,aa=[0,function(a){return w(B,a)},$],ab=i(m[u],a),ac=i(t[45],ab),ad=[0,i(r[66][8],ac),[0,n,0]],ae=i(t[cr],g),af=i(r[66][8],ae),ag=j(p[11],af,ad),ah=[0,j(p[5],aM,ag),aa],ai=ak(e,B),aj=i(r[66][8],ai);return l(p[11],aj,ah,d)}function
db(e,b,c){var
f=af(c),a=b[1];if(a)var
d=a[1];else
var
x=i(s[1][5],go),d=j(q[20],x,c);function
g(a){if(e){var
c=b[2];return aP(i(m[u],d),c,0,a)}return i(p[1],a)}var
h=f[1],k=e?2:1,n=[0,[0,k,h]],o=0,t=[0,g,[0,function(a){return X(n,a)},o]],v=ak(d,b[2]),w=i(r[66][8],v);return l(p[11],w,t,c)}function
aQ(a,b,c){if(b)var
d=b[1];else
var
h=i(s[1][5],gp),d=j(q[20],h,c);var
e=[0,i(a,d),0],f=i(t[23],d),g=[0,i(r[66][8],f),e];return j(p[7],g,c)}function
b0(a,b){var
c=p[1];function
d(a){var
b=a[1],c=b[1];function
d(a){var
c=j(t[4],0,[0,a,b[2]]);return i(r[66][8],c)}function
e(a){return aQ(d,c,a)}return i(p[5],e)}return M(n[17][16],d,a,c,b)}function
dc(a,b){var
c=b?[0,a,dc(a+1|0,b[2])]:b;return c}function
b1(a,b){if(a){var
c=a[1][1],g=b1(a[2],b),d=j(A[8],1,g),e=c[1];if(e)var
h=i(m[u],e[1]),f=j(I[57],h,d);else
var
f=d;return i(m[bK],[0,c[1],c[2],f])}return b}function
dd(a,b){if(b){var
c=b[1],e=i(m[34],a),f=b[2],g=[0,i(m[ac],c),0],d=dd(j(m[76],a,g),f);return[0,[0,[0,c,e[2]],d[1]],d[2]]}return[0,0,a]}function
gr(a,b){var
l=j(q[19],b,a),o=i(aN(b),l),d=i(m[39],o),c=i(q[8],b),e=i(m[aH],d[1]);if(11===e[0]){var
f=e[1],g=f[1];if(c4(c,g)){var
h=j(aw[4],c,g),k=j(aw[17],f,[0,h[1],h[2]]);if(1-(1===k.length-1?1:0))throw aa;var
p=d[2],r=U(k,0)[1],s=j(m[76],r,p),t=j(cY[26],c,s)[1];return i(n[17][1],t)}}throw aa}function
de(a,b,c,d){if(0<a){var
f=i(s[1][5],gs),e=j(q[20],f,d),g=[0,e,b],h=a-1|0,k=function(a){return de(h,g,c,a)},m=i(t[23],e),n=i(r[66][8],m);return l(p[5],n,k,d)}return j(c,b,d)}function
ag(g,b,c,d,e){if(c){if(d){var
a=d[1][1],k=d[2],h=c[2],f=c[1],y=function(a){try{var
x=gr(f,a),c=x}catch(f){f=D(f);if(f!==aa)throw f;var
c=i(o[6],gt)}var
e=0;function
k(a){var
c=j(n[17][8],a,h);return function(a){return ag(g,b,c,d,a)}}var
l=0,q=[0,function(a){return de(c,l,k,a)},e],s=i(m[u],f),v=i(t[er],s),w=[0,i(r[66][8],v),q];return j(p[7],w,a)},l=a[1];if(l)var
v=l[1],z=0,A=[0,[0,v,1],b],B=[0,function(a){return ag(g,A,h,k,a)},z],C=i(t[81],[0,[0,f,v],0]),E=[0,i(r[66][8],C),B],w=i(p[7],E);else
var
H=[0,[0,f,0],b],w=function(a){return ag(g,H,h,k,a)};var
F=j(t[4],0,[0,f,a[2]]),G=i(r[66][8],F);return M(p[33],G,w,y,e)}return i(o[6],gu)}if(d){if(g){var
I=i(s[1][5],gv),x=j(q[20],I,e),J=function(a){return i(o[6],gw)},K=[0,x,0],L=0,N=1,O=function(a){return ag(N,L,K,d,a)},P=i(t[23],x),Q=i(r[66][8],P);return M(p[33],Q,O,J,e)}return i(o[6],gx)}return i(p[1],e)}function
df(a,b){if(a){var
c=a[1],d=j(q[15],b,c),e=a[2],f=function(a){return df(e,a)},g=0,h=function(a){return aP(c,d,g,a)};return l(p[5],h,f,b)}return i(p[1],b)}function
dg(a,b){if(a){var
c=a[1],f=dg(a[2],b),g=j(A[8],1,f),d=c[1];if(d){var
e=d[1],h=i(m[u],e),k=j(I[57],h,g);return i(m[bx],[0,[0,e],c[2],k])}throw[0,O,gz]}return b}function
gB(a){var
b=a[1];if(b){var
c=i(bV[6],a[2])[1];if(typeof
c!=="number"&&0===c[0])if(c[1][2]===b[1])return 1;var
d=0}else
var
d=b;return d}function
dh(a,b){var
c=a;for(;;){var
d=i(bV[6],c[2]);if(typeof
d[1]==="number"){var
c=[0,0,i(ad[26],b)[2][12]];continue}var
e=d[2],f=c[1],g=function(a){var
b=i(bV[6],a)[2];function
c(a){return[0,f,a]}return j(n[19][15],c,b)};return j(n[19][15],g,e)}}function
bj(a,b,c,d){var
e=dh(c,b);function
f(a,b){var
c=j(d,a,b);return[0,j(n[19][15],gB,b),c]}return[1,a,b,j(n[19][16],f,e)]}function
di(a,d,c){if(1===c[0]){var
b=function(a,b){var
c=j(d,a,b[2]);return[0,b[1],c]},e=j(n[19][16],b,c[3]),f=c[2];return[1,i(a,c[1]),f,e]}return i(F[2],gD)}function
dj(a,b,c){function
d(a,b){return 0}return bj(s[1][9][1],b,c,d)}function
dk(a,b,c){var
g=i(q[7],c),h=i(q[8],c),d=j(q[15],c,b),w=j(I[44],b,g),x=i(aN(c),d),k=i(m[39],x);try{var
C=i(m[43],k[1]),p=C}catch(f){f=D(f);if(f!==m[28])throw f;var
p=i(o[6],gE)}var
e=p[1],r=i(ad[26],e),s=r[1];if(0===a)var
f=s[6],t=0;else
var
f=s[7],t=[0,e[2]];var
u=j(n[17][98],f,k[2]),v=u[2];function
y(a,b){var
d=j(q[15],c,a),e=[0,d,j(I[57],a,b)];return j(ar[17],h,e)}var
z=[0,d,j(I[57],b,g)],A=j(ar[17],h,z),B=l(n[17][16],y,v,A);return[0,w,[0,b,d,e,B,v,u[1],f,[0,t,r[2][12]]]]}function
b2(a,b,c){return 0<c?[0,b,b2(a,b,c-1|0)]:[2,a]}function
aR(c,b){if(b){var
d=b[2],a=b[1],f=c[1];if(a){var
g=a[2],h=a[1],e=h[1];if(0===e[0]){var
k=aR(c,[0,g,d]);return[0,i(s[1][9][5],f),k]}var
j=e[2],m=e[3],o=j[2],p=function(a,h){var
b=a===(o-1|0)?1:0;if(b)var
j=0,k=function(a,b){return[0,b,U(h,a)[a+1]]},p=aR(c,[0,l(n[17][69],k,j,m),[0,g,d]]),e=[0,[0,i(s[1][9][5],f),p]];else
var
e=b;return e};return bj(s[1][9][1],j[1],h[2],p)}return[2,aR(c,d)]}return[3,c]}function
ay(d,b,c){var
h=d[1];if(b){var
e=b[2],a=b[1];if(a){var
g=a[2],q=a[1],r=q[2],m=q[1];if(0===m[0])switch(c[0]){case
0:var
B=ay(d,[0,g,e],c[2]);return[0,j(s[1][9][4],h,c[1]),B];case
1:var
C=function(a,b){return bk(d,1,[0,g,e],b)};return di(i(s[1][9][4],h),C,c);default:var
D=i(k[1],gL);return l(o[3],0,0,D)}var
t=m[3],u=m[2],v=u[2],w=u[1];switch(c[0]){case
0:var
x=c[2],f=c[1];return bj(f,w,r,function(a,c){if(a===(v-1|0)){var
b=0,i=function(a,b){return[0,b,U(c,a)[a+1]]},k=l(n[17][69],i,b,t),m=ay(d,[0,k,[0,g,e]],b2(x,f,c.length-1));return[0,[0,j(s[1][9][4],h,f),m]]}return[0,[0,f,b2(x,f,c.length-1)]]});case
1:if(1-j(s[37],w,c[2]))i(o[6],gM);if(1===c[0]){var
p=c[2],y=dh(r,p),z=function(a,b){var
c=b[2],h=U(y,a)[a+1];if(a===(v-1|0))var
i=0,j=function(a,b){return[0,b,U(h,a)[a+1]]},f=bk(d,0,[0,l(n[17][69],j,i,t),[0,g,e]],c);else
var
f=c;return[0,b[1],f]},A=j(n[19][16],z,c[3]);return[1,c[1],p,A]}return i(F[2],gC);default:var
E=i(k[1],gN);return l(o[3],0,0,E)}}if(2===c[0])return[2,ay(d,e,c[1])];var
G=i(k[1],gO);return l(o[3],0,0,G)}if(3===c[0])return[3,c[1]];var
H=i(k[1],gP);return l(o[3],0,0,H)}function
bk(a,b,c,d){var
e=a[1];if(d){var
f=d[1],g=bl(a,b,c,f[2]);return[0,[0,j(s[1][9][4],e,f[1]),g]]}var
h=aR(a,c);return[0,[0,i(s[1][9][5],e),h]]}function
bl(e,f,c,d){var
a=e[1];if(0<f)switch(d[0]){case
0:var
b=bl(e,f,c,d[2]);return[0,j(s[1][9][4],a,d[1]),b];case
1:var
g=function(a,b){return bk(e,f+1|0,c,b)};return di(i(s[1][9][4],a),g,d);case
2:return[2,bl(e,f-1|0,c,d[1])];default:var
h=i(k[1],gQ);return l(o[3],0,0,h)}return ay(e,c,d)}function
dl(a,b){var
c=b;for(;;){if(c){var
d=c[1];if(j(s[2][4],d[1],a))return d[2];var
c=c[2];continue}throw aa}}function
b3(a,b,c,d){var
e=i(m[79],b)[1],f=i(m[39],b),p=i(m[43],f[1]);if(1-j(s[37],p[1],c[3])){var
q=i(k[1],gR),r=i(k[17],0),t=l(ae[4],d,h[16],a),u=j(k[14],t,r),v=j(k[14],u,q);j(o[7],gS,v)}var
g=j(n[17][98],c[7],f[2]);if(1-l(n[17][24],I[62],g[1],c[6])){var
w=i(k[1],gT),x=i(k[17],0),y=l(ae[4],d,h[16],a),z=j(k[14],y,x),B=j(k[14],z,w);j(o[7],gU,B)}var
C=j(n[18],g[2],[0,a,0]),D=c[4],E=i(n[17][1],e),F=[0,j(A[8],E,D),C],G=i(m[59],F),H=j(a9[20],h[16],G);return j(m[64],e,H)}function
b4(a,b,c,d,e){if(c){var
h=c[1];if(0===h[0])var
n=h[1],o=n[2],f=n[1];else{var
g=h[1],k=g[2];if(0!==k[0]){var
p=k[1],y=b4(a,b,c[2],d,e),z=j(A[8],1,y),r=g[1],B=r?j(A[20],r[1],z):d;if(p){var
l=p[1],C=i(m[u],l);try{var
E=dl([0,l],a[1]),s=E}catch(f){f=D(f);if(f!==aa)throw f;var
s=dl([0,l],a[2])[2]}var
t=b3(C,s,b,i(q[8],e))}else
var
t=i(q[7],e);return i(m[bK],[0,g[1],t,B])}var
o=k[1],f=g[1]}var
v=b4(a,b,c[2],d,e),w=j(A[8],1,v),x=f?j(A[20],f[1],w):d;return i(m[bK],[0,f,o,x])}return d}function
dm(a,b,c,d,e){var
f=i(q[8],e),g=b4(b,c,d,b3(b[3],b[4],c,f),e);function
h(a,b){var
c=a[1];if(c){var
d=j(A[8],1,b);return l(m[52],c[1],a[2],d)}var
e=j(A[8],1,b);return i(m[bK],[0,0,a[2],e])}function
k(a,b){var
c=a[1];if(c){var
d=j(A[8],1,b);return M(m[51],c[1],a[2][1],a[2][2],d)}var
e=j(A[8],1,b);return i(m[123],[0,0,a[2][1],a[2][2],e])}var
o=l(n[17][16],k,b[2],g),p=j(n[18],a,b[1]);return l(n[17][16],h,p,o)}function
dn(a,b,c,d,e){var
f=e;for(;;){if(typeof
f==="number"){if(0===f)return i(o[6],gV);var
f=[0,dj(b,c[3],c[8])];continue}return[0,ay(a,[0,[0,[0,d,c[8]],0],0],f[1])]}}function
dp(a){function
b(a){return[0,a,gY]}return j(n[17][12],b,a)}function
dq(f,b){function
a(a){var
b=a[2];if(b)var
c=b[1],d=[0,[0,c[1],[0,f,c[2]]],b[2]];else
var
e=i(k[1],gZ),d=l(o[3],0,0,e);return[0,a[1],d]}return j(n[17][12],a,b)}function
dr(f,b,c){function
a(a){var
c=a[1],d=j(s[1][9][3],c,b),e=d?[0,f]:d;return[0,c,[0,[0,e,0],a[2]]]}return j(n[17][12],a,c)}function
g0(a){var
b=a[2];if(b){var
c=b[1],e=c[1];if(b[2])if(e)var
f=b[2],g=f[1],r=i(n[17][6],c[2]),s=i(m[59],[0,e[1],r]),d=[0,[0,g[1],[0,s,g[2]]],f[2]];else
var
p=b[2],q=p[1],t=p[2],u=j(n[18],c[2],q[2]),d=[0,[0,q[1],u],t];else
var
d=b;var
h=d}else
var
v=i(k[1],g1),h=l(o[3],0,0,v);return[0,a[1],h]}function
ds(a){return j(n[17][12],g0,a)}function
dt(a,b,c,d){var
k=i(m[u],d),e=j(q[19],c,d),o=i(m[79],e),f=i(m[39],e),p=i(m[43],f[1]);if(j(s[37],p[1],b[3])){var
g=j(n[17][98],b[7],f[2]);try{var
x=l(n[17][24],I[62],g[1],b[6]),h=x}catch(f){f=D(f);if(f[1]!==fd)throw f;var
h=0}if(h){var
r=j(n[18],g[2],[0,k,0]),t=[0,i(m[u],a),r],v=i(m[59],t),w=j(a9[20],c[2],v);return j(m[66],o[1],w)}throw[0,O,g2]}throw[0,O,g3]}function
bm(B,A,x,d,e,f,g,h){var
v=d,b=e,a=g;for(;;)switch(a[0]){case
0:if(b){var
R=dq(b[1],v),v=R,b=b[2],a=a[2];continue}var
S=i(k[1],g4);return l(o[3],0,g5,S);case
1:var
w=a[2],V=a[1];if(b){var
c=b[1],W=b[2],C=i(ad[26],w),X=C[1][6],Y=i(q[7],h),y=i(q[8],h),D=j(q[15],h,c),Z=i(aN(h),D),E=i(m[39],Z),F=i(m[43],E[1]);if(j(s[37],F[1],w)){var
G=j(n[17][98],X,E[2]),H=G[1],_=function(a,b){var
c=j(q[15],h,a),d=[0,c,j(I[57],a,b)];return j(ar[17],y,d)},$=[0,D,j(I[57],c,Y)],aa=j(ar[17],y,$),ab=l(n[17][16],_,G[2],aa),ae=l(bW[75],y,w,4),af=j(aw[17],[0,w,F[2]],C),ag=function(a){var
b=j(m[76],a,H),c=i(m[88],b);return j(t[15],c,h)},J=j(n[19][15],ag,af),ah=function(a,b){return i(m[ac],a+1|0)},ai=[0,ae,ab,c,j(n[19][16],ah,J)],aj=i(m[cr],ai),ak=function(a,b,c){var
e=b[2],g=b[1],h=U(J,a)[a+1];function
l(a,b){if(b){var
c=b[1],d=l(a+1|0,b[2]),e=d[3],h=d[2],j=d[1];return U(g,a)[a+1]?[0,[0,i(m[u],c),j],[0,c,h],e+1|0]:[0,[0,i(m[u],c),j],h,e]}if(a===g.length-1)return[0,W,0,f];throw[0,O,g6]}var
d=l(0,h),y=d[2],z=0;if(e)var
o=e[1],C=o[1],D=function(a){return j(s[1][9][3],a[1],C)},E=j(n[17][29],D,v),F=[0,i(m[128],[0,w,a+1|0]),H],G=dr(i(m[59],F),V,E),I=o[2],K=d[3],L=d[1],q=function(a){return bm(B,A,x,G,L,K,I,a)};else{var
R=i(k[1],g7);i(ax[13],R);var
q=i(x,i(m[ac],1))}var
M=[0,q,z],N=[0,function(b){function
a(a){return dt(i(T[12],B),A,b,a)}var
c=j(n[17][12],a,y),d=i(t[cF],c);return j(r[66][8],d,b)},M];function
P(a){var
b=i(t[23],a);return i(r[66][8],b)}var
Q=[0,j(p[32],P,h),N];return j(p[7],Q,c)},al=j(n[19][16],ak,a[3]),am=i(q[45],aj);return l(p[12],am,al,h)}throw[0,O,g8]}var
an=i(k[1],g9);return l(o[3],0,g_,an);case
2:var
ao=ds(v),v=ao,a=a[1];continue;default:var
K=a[1],L=K[2],M=L[1],N=K[1];if(b){var
ap=i(k[1],g$);return l(o[3],0,ha,ap)}var
z=j(s[1][12][3],N,v);if(z){var
P=z[1];if(!P[1])if(!z[2]){var
as=function(a){return i(m[ac],a+1|0)},at=j(n[17][48],M+L[2]|0,as),Q=j(n[17][98],M,at),au=j(n[17][8],P[2],Q[2]),av=j(n[17][7],Q[1],au),ay=[0,i(m[u],N),av],az=i(x,i(m[59],ay)),aA=i(r[66][8],t[17]),aB=j(p[26],f,aA);return l(p[5],aB,az,h)}}var
aq=i(k[1],hb);return l(o[3],0,0,aq)}}function
du(k,b){var
a=[0,function(a){var
l=i(cX[6],a),m=i(q[7],b),e=i(q[8],b),f=co(cL[6],0,0,0,e,h[16],k);function
c(a){return 2===a[0]?[13,[0,v[4],hc,0,0]]:j(fg[7],c,a)}var
g=c(f),d=P(W[8],0,e,l,[0,[0,m]],g);return i(cX[21][5],[0,d[2],d[1]])}],c=j(t[160][1],0,a);return j(r[66][8],c,b)}function
hl(a){return X(hm,a)}function
hs(a,b){var
c=b;for(;;)if(typeof
c==="number"){i(y[14],a);return i(H[10],ht)}else
switch(c[0]){case
18:var
d=c[1];if(typeof
d==="number")return 0===d?i(H[10],hu):0;if(0===d[1])return al(0);var
h=i(x[9],a),m=i(n[17][3],h),f=i(x[33][1],a);try{var
r=i(n[17][3],f[1]),s=j(C[4][1],f[2],r),g=s}catch(f){f=D(f);if(f[1]===fc)if(K.caml_string_notequal(f[2],hv))var
e=0;else
var
g=i(ad[2],0),e=1;else
var
e=0;if(!e)throw f}try{j(bW[77],g,m);var
q=ft(0);return q}catch(f){f=D(f);if(f[1]===fe[1])if(14===f[3][0]){var
p=i(k[1],hw);return l(o[3],0,0,p)}throw f}case
0:case
1:case
2:var
c=c[1];continue;case
10:case
11:case
14:case
15:case
17:return i(y[14],a);default:return 0}}function
dv(a,b){var
w=a[2];for(;;){if(typeof
w==="number")var
z=0;else
switch(w[0]){case
18:var
K=w[1];if(typeof
K==="number"){if(i(x[28],b))al(0);else{var
e=i(x[7],b)?i(y[11],b):i(y[10],b);if(e){var
E=e[1];if(typeof
E==="number")if(0===E){var
U=e[2];if(U){var
V=U[1];if(typeof
V==="number")var
R=1;else
var
W=V[1],D=1,R=0}else
var
R=1;if(R){var
as=i(k[1],fx);l(o[3],0,0,as);var
C=1,D=0}}else
var
C=0,D=0;else
var
W=E[1],D=1;if(D)var
C=0===W?(i(o[6],fy),1):(i(o[6],fz),1)}else
var
C=0;if(!C){if(typeof
K==="number"){switch(K){case
0:if(e)var
h=0,f=0;else
var
f=1;break;case
1:if(e){var
Z=e[1];if(typeof
Z==="number")if(2===Z)var
h=0,f=0;else
var
f=1;else
var
f=1}else
var
h=0,f=0;break;default:if(e){var
_=e[1];if(typeof
_==="number"&&2===_)var
f=1;else
var
h=0,f=0}else
var
h=0,f=0}if(f){bX(0);al(0);var
h=1}}else
var
h=0;if(!h)if(e){var
Y=e[1],aU=typeof
Y==="number"?2===Y?(i(o[6],fB),1):0:0;if(!aU)i(o[6],fA)}else
i(o[6],fC)}}var
L=0,z=2}else
var
z=1;break;case
14:case
15:var
z=1;break;case
0:case
1:case
2:var
w=w[1];continue;default:var
z=0}switch(z){case
0:if(1-i(x[7],b)){var
T=i(y[10],b);if(T)if(typeof
T[1]==="number")var
S=1;else{i(o[6],fm);var
ar=1,S=0}else
var
S=1;if(S)var
ar=0}else
var
ar=0;var
L=1;break;case
1:c5(b);var
L=1;break}var
N=i(H[12],0);if(L){var
an=i(x[33][1],N),ao=an[2],ap=[0,i(n[17][3],an[1]),ao],aq=i(q[8],ap),aJ=j(aL[1],[0,s[1][9][1],aq],a),aK=af(ap),v=0,g=0,c=M(aL[2],aK,aq,ao,aJ)[2];for(;;){if(typeof
c==="number")var
d=hl;else
switch(c[0]){case
0:if(g)throw[0,O,hn];var
g=1,c=c[1];continue;case
1:if(v)throw[0,O,ho];var
v=1,c=c[1];continue;case
2:if(!g)if(!v){var
v=1,g=1,c=c[1];continue}throw[0,O,hp];case
3:var
at=c[1],d=function(a){return da(c_,v,g,at,a)};break;case
4:if(g)throw[0,O,hq];var
au=c[2],av=c[1],d=function(a){return gl(v,av,au,a)};break;case
5:var
$=c[1],d=function(a){var
c=af(a),o=i(s[1][5],gq),d=j(q[20],o,a),e=$[1],b=e[1],f=b1(b,c_(c,a,e[2])),h=dc(1,b),k=dd(f,h),t=k[2],v=k[1],w=j(n[17][12],m[ac],h),x=[0,i(m[u],d),w],y=i(m[59],x),z=[0,function(a){return aP(y,t,v,a)},0],A=0,B=[0,aM,[0,function(a){return c$(g,$,c,a)},A]],C=[0,function(a){return b0(b,a)},B],D=[0,i(p[7],C),z],E=ak(d,f),F=i(r[66][8],E);return l(p[11],F,D,a)};break;case
8:var
ax=c[1],d=function(a){return ag(1,0,0,ax,a)};break;case
9:var
aa=c[2],ab=c[1],d=function(a){var
d=i(m[99],ab),b=i(m[aH],d);if(1===b[0])return ag(0,0,[0,b[1],0],aa,a);var
e=i(s[1][5],gy),c=j(q[20],e,a),f=[0,c,0],g=0,h=0;function
k(a){return ag(h,g,f,aa,a)}var
n=j(t[143],[0,c],ab),o=i(r[66][8],n);return l(p[5],o,k,a)};break;case
10:var
ay=c[1],az=0,d=function(a){return db(az,ay,a)};break;case
11:var
aA=c[1],aB=1,d=function(a){return db(aB,aA,a)};break;case
12:var
aC=c[3],aD=c[2],aE=c[1],d=function(a){var
b=dg(aD,aC),c=P(t[ec],0,[0,aE],b,0,cZ[7]);return j(r[66][8],c,a)};break;case
13:var
ae=c[2],F=c[1],d=function(a){if(0===F[0]){var
b=F[1],c=j(q[18],a,b),d=[0,b,i(aI[2][1][2],c),ae],e=i(aI[2][1][18],d),f=j(t[4],0,e);return j(r[66][8],f,a)}if(F[1])return i(o[6],gA);var
g=l(t[3],0,ae,2);return j(r[66][8],g,a)};break;case
14:var
ah=c[1],d=function(a){var
g=af(a),n=i(q[7],a),t=i(s[1][5],gK),h=j(q[20],t,a),u=b1(ah,n),e=g[1],v=[0,[0,0,g[1]]];if(e){var
b=e[1];if(typeof
b==="number")var
d=0;else{var
f=b[4];if(typeof
b[3]==="number")var
c=[0,f,[0,[0,b[1],b[2],0,[0,h,f]],e[2]]],d=1;else
var
c=i(o[6],gJ),d=1}}else
var
d=0;if(!d)var
m=i(k[1],gI),c=l(o[3],0,0,m);var
w=[0,c[2]],x=0,y=[0,function(a){return X(w,a)},x],z=0,A=c[1],B=[0,function(a){return aj(A,a)},z],C=[0,function(a){return b0(ah,a)},B],D=[0,function(a){return X(v,a)},C],E=[0,i(p[7],D),y],F=ak(h,u),G=i(r[66][8],F);return l(p[11],G,E,a)};break;case
15:var
G=c[3],A=c[2],I=c[1],d=function(a){var
m=af(a),x=i(s[1][5],gW),e=j(q[20],x,a),f=m[1];if(f){var
b=f[1];if(typeof
b==="number")var
h=0;else
var
w=b[1],d=b[2],v=b[3],g=b[4],u=f[2],h=1}else
var
h=0;if(!h)var
y=i(k[1],gX),c=l(o[3],0,0,y),w=c[1],d=c[2],v=c[3],g=c[4],u=c[5];var
z=dm(I,A,d,G,a),B=[0,[0,0,m[1]]],C=A[5],D=i(q[8],a),E=i(n[17][1],G),F=[0,[0,[0,w,d,dn([0,e,[0,i(n[17][1],I),E]],D,d,C,v),[0,e,g]],u]],H=0,J=[0,function(a){return X(F,a)},H],K=0,L=[0,function(a){return aj(g,a)},K],N=[0,function(a){var
b=p[1];function
c(a){if(0===a[0])var
b=a[1],d=b[2],c=b[1];else{var
e=a[1],f=e[2],g=e[1];if(0!==f[0]){var
l=function(a){return p[1]},m=function(a){return aQ(l,g,a)};return i(p[5],m)}var
d=f[1],c=g}function
h(a){var
b=j(t[4],0,[0,a,d]);return i(r[66][8],b)}function
k(a){return aQ(h,c,a)}return i(p[5],k)}return M(n[17][16],c,G,b,a)},L],O=A[2],P=[0,function(a){var
b=p[1];function
c(b){var
c=b[1];function
d(a){var
c=j(t[4],0,[1,a,b[2][1],b[2][2]]);return i(r[66][8],c)}function
a(a){return aQ(d,c,a)}return i(p[5],a)}return M(n[17][16],c,O,b,a)},N],Q=j(n[18],I,A[1]),R=[0,function(a){var
b=p[1];function
c(b){var
c=b[1];function
d(a){var
c=j(t[4],0,[0,a,b[2]]);return i(r[66][8],c)}function
a(a){return aQ(d,c,a)}return i(p[5],a)}return M(n[17][16],c,Q,b,a)},P],S=[0,function(a){return X(B,a)},R],T=[0,i(p[7],S),J],U=ak(e,z),V=i(r[66][8],U);return l(p[11],V,T,a)};break;case
16:var
aF=c[1],d=function(a){return df(aF,a)};break;case
17:var
J=c[2],B=c[1],d=function(a){var
g=i(q[8],a),d=af(a);if(0===J[0]){var
e=dk(B,J[1],a),c=e[2],h=e[1]?[0,dj(g,c[3],c[8])]:1;return X([0,[0,[0,B,c,h,0],d[1]]],a)}var
b=J[1];if(0===b[1][1]){var
k=i(s[1][5],gF),f=j(q[20],k,a),n=i(m[u],f),o=[0,[0,[0,f],b[1][2]],b[2],b[3]],r=function(a){var
b=dk(B,n,a);if(b[1])throw[0,O,gG];return X([0,[0,[0,B,b[2],1,0],d[1]]],a)},t=0,v=0,w=function(a,b,c){return c},x=function(a){return da(w,v,t,o,a)};return l(p[5],x,r,a)}throw[0,O,gH]};break;case
18:var
ai=c[1];if(typeof
ai==="number")var
aG=i(k[1],hr),d=l(o[3],0,0,aG);else
var
am=ai[1],d=function(a){var
x=af(a)[1];if(x){var
e=x[1];if(typeof
e==="number"){switch(e){case
0:var
z=i(k[1],hd),f=l(o[3],0,0,z);break;case
1:var
f=i(o[6],hi);break;default:var
f=i(o[6],hj)}var
g=f[1],b=f[2],d=f[3],c=f[4]}else
var
g=e[1],b=e[2],d=e[3],c=e[4]}else
var
$=i(k[1],hk),v=l(o[3],0,0,$),g=v[1],b=v[2],d=v[3],c=v[4];var
A=0===g?0===am?g:i(o[6],hg):0===am?i(o[6],hh):g;if(typeof
d==="number")if(0===d)var
w=0;else
var
Z=i(t[99],b[1]),_=[0,i(r[66][8],Z),0],h=i(p[20],_),w=1;else
var
w=0;if(!w)if(0===A)if(typeof
d==="number")var
B=j(n[17][12],m[u],c),C=function(a){return c7(B,a)},D=i(t[er],b[1]),E=i(r[66][8],D),h=j(p[5],E,C);else
var
F=d[1],H=[0,b[1],0],G=0,I=dp(c),J=function(b){var
a=0,d=i(r[66][8],t[41]),e=[0,function(a){return aO(d,a)},a],f=[0,function(a){return aj(c,a)},e],g=[0,function(a){return du(b,a)},f];return i(p[7],g)},K=0,h=function(a){return bm(K,b,J,I,H,G,F,a)};else
if(typeof
d==="number")var
L=0,M=j(n[17][12],m[u],c),N=[0,function(a){return c7(M,a)},L],O=[0,i(n[17][1],b[5])+1|0],P=i(t[101],O),Q=[0,i(r[66][8],P),N],R=j(n[18],b[5],[0,b[1],0]),S=i(t[cF],R),T=[0,i(r[66][8],S),Q],h=i(p[7],T);else
var
U=d[1],y=i(n[17][1],b[5]),V=function(a){var
f=i(s[1][5],he),d=j(q[20],f,a),g=i(s[1][5],hf),e=j(q[20],g,a),l=[0,i(m[u],e),0],h=0,k=0,n=dp(c);function
o(b){var
a=0,e=i(r[66][8],t[41]),f=[0,function(a){return aO(e,a)},a],g=[0,function(a){return aj(c,a)},f],h=[0,function(a){return du(b,a)},g],j=[0,d,0],k=[0,function(a){return aj(j,a)},h];return i(p[7],k)}var
v=[0,d],w=[0,function(a){return bm(v,b,o,n,l,k,U,a)},h],x=i(t[23],e),z=[0,i(r[66][8],x),w],A=i(r[66][8],t[17]),B=[0,j(p[26],y,A),z],C=j(t[8],[0,d],y+1|0),D=[0,i(r[66][8],C),B];return j(p[7],D,a)},W=j(n[18],b[5],[0,b[1],0]),X=i(t[cF],W),Y=i(r[66][8],X),h=j(p[5],Y,V);return l(p[5],aM,h,a)};break;default:var
aw=c[1],d=function(a){return b0(aw,a)}}var
aN=j(p[5],d,fp),aR=i(r[66][1],aN),aS=i(ad[2],0),Q=l(x[29],aS,aR,N)[1];break}}else
var
Q=N;var
aT=function(a,b){return Q};i(H[27],aT);return hs(Q,a[2])}}var
aS=[0,fs,fw,fL,a_,c9,dv,function(a){return dv(a,i(H[12],0))},X,bm,aR,ay,bk,bl,dm,dn,b3,c5,ds,dr,dq,dt,ag,bj];aB(462,aS,"Decl_mode_plugin.Decl_proof_instr");function
Z(a,b){var
n=i(a,b[2]),c=b[1];if(c)var
e=i(k[17],0),f=i(k[1],hx),g=i(k[17],0),h=i(T[1],c[1]),l=j(k[14],h,g),m=j(k[14],l,f),d=j(k[14],m,e);else
var
d=i(k[9],0);return j(k[14],d,n)}function
am(a,b){if(0===b[0])return i(a,b[1]);var
c=b[1];if(c){var
d=i(T[1],c[1]),e=i(k[17],0),f=i(k[1],hC),g=i(k[17],0),h=i(k[1],hD),l=j(k[14],h,g),m=j(k[14],l,f),n=j(k[14],m,e);return j(k[14],n,d)}return i(k[1],hE)}function
an(a,b,c,d){var
g=d[3];if(g)var
w=i(b,g[1]),x=i(k[17],0),y=i(k[1],hB),z=i(k[17],0),A=j(k[14],z,y),B=j(k[14],A,x),h=j(k[14],B,w);else
var
h=i(k[9],0);var
m=d[2];if(m){var
f=m[1];if(f)var
n=function(a){return i(k[1],hy)},o=l(k[54],n,a,f),p=i(k[17],0),q=i(k[1],hz),r=i(k[17],0),s=j(k[14],r,q),t=j(k[14],s,p),e=j(k[14],t,o);else
var
e=i(k[9],0)}else
var
u=i(k[1],hA),v=i(k[17],0),e=j(k[14],v,u);var
C=i(c,d[1]),D=j(k[30],1,C),E=j(k[14],D,e);return j(k[14],E,h)}function
hF(a){return 0===a[0]?a[1]:m[117]}function
az(a){return a}function
aA(a,b,c,d,e,f,g){if(d)var
m=i(k[17],0),n=i(k[1],hG),h=j(k[14],n,m);else
var
h=i(k[9],0);if(g){var
l=g[1];if(0===l[0]){var
o=aT(a,b,c,0,e,f,g),p=i(k[1],f),q=i(k[17],0),r=j(k[14],q,h),s=j(k[14],r,p);return j(k[14],s,o)}var
t=aA(a,b,c,1,e,f,g[2]),u=Z(b,l[1]),v=i(k[17],0),w=j(k[14],v,h),x=j(k[14],w,u);return j(k[14],x,t)}return i(k[9],0)}function
aT(a,b,c,d,e,f,g){if(g){var
h=g[1];if(0===h[0]){var
l=d?i(k[44],0):i(k[9],0),m=aT(a,b,c,1,e,f,g[2]),n=i(a,h[1]),o=i(k[17],0),p=j(k[14],o,l),q=j(k[14],p,n);return j(k[14],q,m)}var
r=e?i(k[1],hH):i(k[1],hI),s=aA(a,b,c,0,e,f,g),t=i(k[17],0),u=j(k[14],t,r);return j(k[14],u,s)}return i(k[9],0)}function
dw(a){return 0===a?i(k[1],hL):i(k[1],hM)}function
bo(g,b,c,d,e){var
q=0,h=0,a=e[2];for(;;){if(typeof
a==="number")var
f=i(k[1],hU);else
switch(a[0]){case
0:var
q=1,a=a[1];continue;case
1:var
h=1,a=a[1];continue;case
2:var
q=1,h=1,a=a[1];continue;case
3:var
n=a[1];if(0===q)if(0===h)var
I=function(a){return am(b,a)},J=an(b,d,function(a){return Z(I,a)},n),K=i(k[17],0),L=i(k[1],hV),M=j(k[14],L,K),f=j(k[14],M,J);else
var
N=function(a){return am(b,a)},O=an(b,d,function(a){return Z(N,a)},n),P=i(k[17],0),Q=i(k[1],hW),R=j(k[14],Q,P),f=j(k[14],R,O);else
if(0===h)var
S=function(a){return am(b,a)},U=an(b,d,function(a){return Z(S,a)},n),V=i(k[17],0),W=i(k[1],hX),X=j(k[14],W,V),f=j(k[14],X,U);else
var
Y=function(a){return am(b,a)},_=an(b,d,function(a){return Z(Y,a)},n),$=i(k[17],0),aa=i(k[1],hY),ab=j(k[14],aa,$),f=j(k[14],ab,_);break;case
4:var
ac=a[2],ad=an(b,d,function(a){return Z(b,a)},ac),ae=i(k[17],0),af=0===a[1]?i(k[1],hS):i(k[1],hT),ag=i(k[17],0),ah=h?i(k[1],hZ):i(k[1],h0),ai=j(k[14],ah,ag),aj=j(k[14],ai,af),ak=j(k[14],aj,ae),f=j(k[14],ak,ad);break;case
5:var
al=a[1],ao=an(b,d,function(a){var
c=am(b,a[2]),d=i(k[17],0),e=i(k[1],hJ),f=i(k[17],0),h=aA(g,b,az,0,0,hK,a[1]),l=j(k[14],h,f),m=j(k[14],l,e),n=j(k[14],m,d);return j(k[14],n,c)},al),ap=i(k[1],h1),f=j(k[14],ap,ao);break;case
6:var
aq=aA(g,b,az,0,0,h2,a[1]),ar=i(k[1],h3),f=j(k[14],ar,aq);break;case
7:var
as=aT(g,b,az,0,1,h4,a[1]),at=i(k[1],h5),f=j(k[14],at,as);break;case
8:var
au=aT(g,b,az,0,0,h6,a[1]),av=i(k[1],h7),f=j(k[14],av,au);break;case
9:var
aw=i(b,a[1]),ax=i(k[1],h8),ay=i(k[17],0),aB=aT(g,b,az,0,0,h9,a[2]),aC=i(k[1],h_),aD=j(k[14],aC,aB),aE=j(k[14],aD,ay),aF=j(k[14],aE,ax),f=j(k[14],aF,aw);break;case
10:var
aG=Z(b,a[1]),aH=i(k[17],0),aI=i(k[1],h$),aJ=j(k[14],aI,aH),f=j(k[14],aJ,aG);break;case
11:var
aK=Z(b,a[1]),aL=i(k[17],0),aM=i(k[1],ia),aN=j(k[14],aM,aL),f=j(k[14],aN,aK);break;case
12:var
aO=i(b,a[3]),aP=i(k[1],ib),aQ=i(k[17],0),aR=a[2],aS=function(a){var
b=i(k[1],ic),c=i(g,a),d=i(k[1],id),e=j(k[14],d,c);return j(k[14],e,b)},aU=l(k[54],k[17],aS,aR),aV=i(k[17],0),aW=i(T[1],a[1]),aX=i(k[17],0),aY=i(k[1],ie),aZ=j(k[14],aY,aX),a0=j(k[14],aZ,aW),a1=j(k[14],a0,aV),a2=j(k[14],a1,aU),a3=j(k[14],a2,aQ),a4=j(k[14],a3,aP),f=j(k[14],a4,aO);break;case
13:var
a5=i(b,a[2]),a6=i(k[17],0),a7=i(k[1],ig),a8=i(k[17],0),a9=am(T[1],a[1]),a_=i(k[17],0),a$=i(k[1],ih),ba=j(k[14],a$,a_),bb=j(k[14],ba,a9),bc=j(k[14],bb,a8),bd=j(k[14],bc,a7),be=j(k[14],bd,a6),f=j(k[14],be,a5);break;case
14:var
bf=aA(g,b,az,0,0,ii,a[1]),bg=i(k[1],ij),f=j(k[14],bg,bf);break;case
15:var
t=a[3],u=a[1];if(0===t)var
v=i(k[9],0);else
var
bw=0,bx=0,by=aA(g,function(a){return am(b,a)},hF,bx,bw,ip,t),bz=i(k[1],iq),bA=i(k[17],0),bB=j(k[14],bA,bz),v=j(k[14],bB,by);if(0===u)var
w=i(k[9],0);else
var
bn=i(k[17],0),bo=function(a){var
b=i(k[1],il),c=i(g,a),d=i(k[1],im),e=j(k[14],d,c);return j(k[14],e,b)},bp=l(k[54],k[17],bo,u),bq=i(k[17],0),br=i(k[1],io),bs=i(k[17],0),bt=j(k[14],bs,br),bu=j(k[14],bt,bq),bv=j(k[14],bu,bp),w=j(k[14],bv,bn);var
bh=i(c,a[2]),bi=i(k[17],0),bj=i(k[1],ik),bk=j(k[14],bj,bi),bl=j(k[14],bk,bh),bm=j(k[14],bl,w),f=j(k[14],bm,v);break;case
16:var
bC=l(k[54],k[44],b,a[1]),bD=i(k[17],0),bE=i(k[1],ir),bF=j(k[14],bE,bD),f=j(k[14],bF,bC);break;case
17:var
r=a[2];if(0===r[0])var
z=i(b,r[1]),A=i(k[17],0),B=i(k[1],hQ),C=j(k[14],B,A),x=j(k[14],C,z);else
var
D=r[1],E=an(b,d,function(a){return Z(b,a)},D),F=i(k[17],0),G=i(k[1],hR),H=j(k[14],G,F),x=j(k[14],H,E);var
bG=i(k[17],0),bH=dw(a[1]),bI=i(k[17],0),bJ=i(k[1],is),bK=j(k[14],bJ,bI),bL=j(k[14],bK,bH),bM=j(k[14],bL,bG),f=j(k[14],bM,x);break;default:var
s=a[1];if(typeof
s==="number")switch(s){case
0:var
p=i(k[1],hN);break;case
1:var
p=i(k[1],hO);break;default:var
p=i(k[1],hP)}else
var
p=dw(s[1]);var
bN=i(k[17],0),bO=i(k[1],it),bP=j(k[14],bO,bN),f=j(k[14],bP,p)}var
bR=i(k[17],0),y=e[1];if(3<y>>>0)var
bQ=i(k[1],iu),m=l(o[3],0,0,bQ);else
switch(y){case
0:var
m=i(k[1],iv);break;case
1:var
m=i(k[1],iw);break;case
2:var
m=i(k[1],ix);break;default:var
m=i(k[1],iy)}var
bS=j(k[14],m,bR);return j(k[14],bS,f)}}function
iz(e,b,c,d){var
a=i(c,bn[29]),f=b5[24];return bo(function(a){var
b=a[2],c=b[2],d=b[1];if(c){var
f=i(k[1],iA),g=i(e,c[1]),h=i(k[1],iB),l=i(T[1],d),m=i(k[1],iC),n=j(k[14],m,l),o=j(k[14],n,h),p=j(k[14],o,g);return j(k[14],p,f)}return i(T[1],d)},b,f,a,d)}function
iD(e,b,c,d){var
a=i(c,bn[29]),f=b5[24];return bo(function(a){var
b=a[2],c=b[2],d=b[1];if(c){var
f=i(k[1],iE),g=i(e,c[1]),h=i(k[1],iF),l=i(T[1],d),m=i(k[1],iG),n=j(k[14],m,l),o=j(k[14],n,h),p=j(k[14],o,g);return j(k[14],p,f)}return i(T[1],d)},b,f,a,d)}var
aU=[0,bo,iz,iD,function(e,b,c,d){var
a=i(c,bn[29]);function
f(a){return i(b5[24],a[6])}return bo(function(a){return Z(e,a)},b,f,a,d)}];aB(465,aU,"Decl_mode_plugin.Ppdecl_proof");i(iH[12],iK);function
b_(a){var
f=i(h[68],a),g=i(q[2],a),c=j(C[4][13],g,f),b=c[2],d=c[1],e=j(C[4][1],b,d),m=j(C[4][4],b,d),n=l(ae[19],e,b,m),o=i(k[18],0),p=i(k[1],iL),r=i(k[18],0),s=i(k[1],iM),t=i(k[18],0),u=j(ae[61],e,b),v=j(k[14],u,t),w=j(k[14],v,s),x=j(k[14],w,r),y=j(k[14],x,p),z=j(k[14],y,o),A=j(k[14],z,n),B=j(k[28],0,A),D=i(k[1],iN),E=i(k[6],0),F=i(k[6],0),G=i(k[1],iO),H=j(k[14],G,F),I=j(k[14],H,E),J=j(k[14],I,D);return j(k[14],J,B)}function
dx(a,b,c,d,e,f,g){var
h=a?a[1]:1;if(g)if(!g[2])if(h)return b_([0,g[1],c]);var
l=i(H[12],0),m=i(y[13],l),n=i(k[1],iP),o=i(k[1],m),p=i(k[1],iQ),q=j(k[14],p,o);return j(k[14],q,n)}function
iR(a,b){var
c=b[2],d=b[1],e=j(C[4][1],c,d),f=j(y[8],c,d);return l(aL[2],f,e,c)}function
dy(a){var
b=i(H[12],0);return i(x[7],b)?i(o[6],iS):(i(aS[1],0),i(H[10],iT))}function
dz(a){i(aS[2],0);return i(H[10],iU)}function
dA(a){return i(aS[7],a)}var
ap=i(ao[2],iV),bq=i(d[1][10],iW),iX=i(ao[4],ap),dB=l(d[13],d[9],iY,iX);M(bn[1],ap,aU[2],aU[3],aU[4]);function
dC(a){var
b=a[2];if(typeof
b==="number")var
c=1;else
if(18===b[0])var
d=b[1],c=typeof
d==="number"?0===d?1:0:0;else
var
c=0;return c?iZ:bp[7]}try{var
pm=0,po=[0,[0,0,function(a){if(a)if(!a[2]){var
b=a[1],c=i(ao[4],ap),d=j(ao[8],c,b);return function(a){return dA(d)}}return i(F[2],pn)}],pm],pp=function(a,b){return l(b6[1],b[1],[0,pq,a],b[2])};j(ab[80],pp,po);var
pr=0,pt=[0,function(a){if(a)if(!a[2]){var
b=a[1],c=i(ao[4],ap),d=j(ao[8],c,b);return function(a){return dC(d)}}return i(F[2],ps)},pr],pu=function(a,b){return j(bp[3],[0,pv,a],b)};j(ab[80],pu,pt)}catch(f){f=D(f);if(!i(o[22],f))throw f;var
i0=j(o[18],0,f),i3=j(F[16],i2,i1),i5=j(F[16],i4,i3),i6=i(k[1],i5),i7=j(k[13],i6,i0);i(ax[13],i7)}var
i8=[6,i(d[12],ap)],i9=i(ao[4],ap),i_=[0,[0,[1,v[4],i9,i8],0],0];function
i$(a,b){return l(b8[1],[0,ja,a],[0,bq],b)}j(ab[80],i$,i_);var
jb=0,jc=0;function
jd(a,b){return i(a,je)}l(d[1][7],bq,jf,[0,[0,0,0,[0,[0,[0,[2,iI[10]],0],jd],jc]],jb]);function
jg(a){i(d[21],d[18][8]);return i(ae[84],ae[85])}var
ji=[0,jh,function(a){i(d[21],bq);return i(ae[84],[0,dx,ae[85][2],b_])},jg];i(H[1],ji);try{var
pb=0,pd=[0,[0,0,function(a){return a?i(F[2],pc):function(a){return dy(0)}}],pb],pe=function(a,b){return l(b6[1],b[1],[0,pf,a],b[2])};j(ab[80],pe,pd);var
pg=0,pj=[0,function(a){return a?i(F[2],ph):function(a){return pi}},pg],pk=function(a,b){return j(bp[3],[0,pl,a],b)};j(ab[80],pk,pj)}catch(f){f=D(f);if(!i(o[22],f))throw f;var
jj=j(o[18],0,f),jm=j(F[16],jl,jk),jo=j(F[16],jn,jm),jp=i(k[1],jo),jq=j(k[13],jp,jj);i(ax[13],jq)}function
js(a,b){return l(b8[1],[0,jt,a],0,b)}j(ab[80],js,jr);try{var
o2=0,o4=[0,[0,0,function(a){return a?i(F[2],o3):function(a){return dz(0)}}],o2],o5=function(a,b){return l(b6[1],b[1],[0,o6,a],b[2])};j(ab[80],o5,o4);var
o7=0,o_=[0,function(a){return a?i(F[2],o8):function(a){return o9}},o7],o$=function(a,b){return j(bp[3],[0,pa,a],b)};j(ab[80],o$,o_)}catch(f){f=D(f);if(!i(o[22],f))throw f;var
ju=j(o[18],0,f),jx=j(F[16],jw,jv),jz=j(F[16],jy,jx),jA=i(k[1],jz),jB=j(k[13],jA,ju);i(ax[13],jB)}function
jD(a,b){return l(b8[1],[0,jE,a],0,b)}j(ab[80],jD,jC);function
b$(a){var
b=a?a[1]:a;return b}var
g=d[1][5][1],br=i(g,jF),B=i(g,jG),bs=i(g,jH),aV=i(g,jI),bt=i(g,jJ),bu=i(g,jK),aW=i(g,jL),bv=i(g,jM),ca=i(g,jN),dD=i(g,jO),dE=i(g,jP),cb=i(g,jQ),cc=i(g,jR),dF=i(g,jS),cd=i(g,jT),w=i(g,jU),ce=i(g,jV),dG=i(g,jW),cf=i(g,jX),cg=i(g,jY),ch=i(g,jZ),ci=i(g,j0),cj=i(g,j1),dH=i(g,j2),ck=i(g,j3),dI=i(g,j4),cl=i(g,j5),dJ=i(g,j6),cm=i(g,j7),cn=i(g,j8),dK=i(g,j9),dL=i(g,j_),dM=i(g,j$),dN=i(g,ka),kb=0,kc=0,ke=[0,[0,kd,function(a,b){return 0}],kc];function
kf(a,b,c,d){return[0,a]}l(d[1][7],br,0,[0,[0,0,0,[0,[0,[0,kh,[0,kg,[0,[2,d[15][6]],0]]],kf],ke]],kb]);var
ki=0,kj=0;function
kk(a,b,c,d){return[0,[0,c],a]}var
km=[0,[0,[0,[2,d[15][6]],[0,kl,[0,[2,d[15][1]],0]]],kk],kj];function
kn(a,b){return[0,0,[0,[1,[0,i(b9[6],b),a]],0]]}var
ko=[0,[0,[0,[2,d[15][6]],0],kn],km];function
kp(a,b){return[0,0,a]}l(d[1][7],B,0,[0,[0,0,0,[0,[0,[0,[2,d[15][1]],0],kp],ko]],ki]);var
kq=0,kr=0;function
ks(a,b){return[0,a]}var
kt=[0,[0,0,0,[0,[0,[0,[2,d[15][1]],0],ks],kr]],kq],ku=0,kv=[0,[0,0,0,[0,[0,[0,[2,br],0],function(a,b){return[1,a]}],ku]],kt];l(d[1][7],bs,0,kv);var
kw=0,kx=0;function
ky(a,b,c,d){return[0,[0,c],a]}var
kA=[0,[0,[0,[2,d[15][6]],[0,kz,[0,[2,bs],0]]],ky],kx];function
kB(a,b){return[0,0,[0,[0,[1,[0,i(b9[6],b),a]],0]]]}var
kC=[0,[0,[0,[2,d[15][6]],0],kB],kA];function
kD(a,b){return[0,0,[0,a]]}var
kE=[0,[0,0,0,[0,[0,[0,[2,d[15][1]],0],kD],kC]],kw],kF=0,kG=[0,[0,0,0,[0,[0,[0,[2,br],0],function(a,b){return[0,0,[1,a]]}],kF]],kE];l(d[1][7],aV,0,kG);var
kH=0,kI=0,kK=[0,[0,0,function(a){return kJ}],kI];function
kL(a,b,c){return[0,a]}var
kO=[0,[0,[0,kN,[0,[7,[2,d[15][1]],kM,0],0]],kL],kK],kQ=[0,[0,0,0,[0,[0,kP,function(a,b,c){return 0}],kO]],kH];l(d[1][7],bt,0,kQ);var
kR=0,kS=0,kT=[0,[0,0,function(a){return 0}],kS];function
kU(a,b,c){return[0,a]}l(d[1][7],bu,0,[0,[0,0,0,[0,[0,[0,kV,[0,[2,d[17][18]],0]],kU],kT]],kR]);var
kW=0,kX=0,kY=[0,[0,0,0,[0,[0,[0,[2,aV],[0,[2,bt],[0,[2,bu],0]]],function(a,b,c,d){return[0,c,b,a]}],kX]],kW];l(d[1][7],aW,0,kY);var
kZ=0,k0=0,k1=[0,[0,0,0,[0,[0,[0,[2,B],[0,[2,bt],[0,[2,bu],0]]],function(a,b,c,d){return[0,c,b,a]}],k0]],kZ];l(d[1][7],bv,0,k1);var
k2=0,k3=0,k5=[0,[0,k4,function(a,b){return 1}],k3],k7=[0,[0,0,0,[0,[0,k6,function(a,b){return 0}],k5]],k2];l(d[1][7],ca,0,k7);var
k8=0,k9=0,k$=[0,[0,k_,function(a,b){return 1}],k9],lb=[0,[0,la,function(a,b){return 2}],k$],ld=[0,[0,lc,function(a,b){return 0}],lb],le=[0,[0,0,0,[0,[0,[0,[2,ca],0],function(a,b){return[0,a]}],ld]],k8];l(d[1][7],dD,0,le);var
lf=0,lg=0;function
lh(a,b,c){return[0,a]}var
lj=[0,[0,[0,li,[0,[2,d[15][1]],0]],lh],lg],ll=[0,[0,0,0,[0,[0,[0,lk,[0,[2,bv],0]],function(a,b,c){return[1,a]}],lj]],lf];l(d[1][7],dE,0,ll);var
lm=0,ln=0;function
lo(a,b,c,d,e){return[9,a,c]}var
lr=[0,[0,[0,lq,[0,[2,ce],[0,lp,[0,[2,d[15][1]],0]]]],lo],ln],lt=[0,[0,[0,ls,[0,[2,ca],[0,[2,dE],0]]],function(a,b,c,d){return[17,b,a]}],lr],lv=[0,[0,0,0,[0,[0,[0,lu,[0,[2,dH],[0,[2,bt],[0,[2,bu],0]]]],function(a,b,c,d,e){return[5,[0,c,b,a]]}],lt]],lm];l(d[1][7],cb,0,lv);var
lw=0,lx=0,lz=[0,[0,[0,ly,[0,[2,bv],0]],function(a,b,c){return[0,1,a]}],lx],lB=[0,[0,0,0,[0,[0,[0,lA,[0,[2,bv],0]],function(a,b,c){return[0,0,a]}],lz]],lw];l(d[1][7],cc,0,lB);var
lC=0,lD=0,lF=[0,[0,[0,lE,[0,[2,cb],0]],function(a,b,c){return[0,a]}],lD],lH=[0,[0,[0,lG,[0,[2,aW],0]],function(a,b,c){return[0,[3,a]]}],lF],lJ=[0,[0,[0,lI,[0,[2,cc],0]],function(a,b,c){return[1,[4,a[1],a[2]]]}],lH],lL=[0,[0,[0,lK,[0,[2,aW],0]],function(a,b,c){return[1,[3,a]]}],lJ],lN=[0,[0,[0,lM,[0,[2,aW],0]],function(a,b,c){return[2,[3,a]]}],lL],lO=[0,[0,[0,[2,cb],0],function(a,b){return a}],lN],lP=[0,[0,[0,[2,cc],0],function(a,b){return[4,a[1],a[2]]}],lO],lR=[0,[0,[0,lQ,[0,[2,aW],0]],function(a,b,c){return[3,a]}],lP],lT=[0,[0,[0,lS,[0,[2,B],0]],function(a,b,c){return[10,a]}],lR],lW=[0,[0,[0,lV,[0,lU,[0,[2,B],0]]],function(a,b,c,d){return[11,a]}],lT],lY=[0,[0,[0,lX,[0,[2,dD],0]],function(a,b,c){return[18,a]}],lW],l0=[0,[0,0,0,[0,[0,lZ,function(a,b){return 0}],lY]],lC];l(d[1][7],dF,0,l0);var
l1=0,l2=0;function
l3(a,b,c){return[0,i(b9[6],b),[0,a,c]]}l(d[1][7],cd,0,[0,[0,0,0,[0,[0,[0,[2,d[15][6]],0],l3],l2]],l1]);var
l4=0,l5=0,l6=[0,[0,[0,[2,cd],0],function(a,b){return i(a,0)}],l5];function
l7(a,b,c,d){return i(c,[0,a])}l(d[1][7],w,0,[0,[0,0,0,[0,[0,[0,[2,cd],[0,l8,[0,[2,d[15][1]],0]]],l7],l6]],l4]);var
l9=0,l_=0,l$=[0,[0,[0,[2,w],0],function(a,b){return[0,[0,a],0]}],l_],mb=[0,[0,[0,[2,w],ma],function(a,b,c,d){return[0,[0,c],a]}],l$],me=[0,[0,0,0,[0,[0,[0,[2,w],[0,md,[0,mc,[0,[2,dG],0]]]],function(a,b,c,d,e){return[0,[0,d],a]}],mb]],l9];l(d[1][7],ce,0,me);var
mf=0,mg=0,mi=[0,[0,[0,[2,B],mh],function(a,b,c,d){return[0,[1,c],a]}],mg],ml=[0,[0,[0,[2,B],[0,mk,[0,mj,[0,[2,ce],0]]]],function(a,b,c,d,e){return[0,[1,d],a]}],mi],mm=[0,[0,0,0,[0,[0,[0,[2,B],0],function(a,b){return[0,[1,a],0]}],ml]],mf];l(d[1][7],dG,0,mm);var
mn=0,mo=0,mp=[0,[0,[0,[2,w],0],function(a,b){return[0,[0,a],0]}],mo],mr=[0,[0,[0,[2,w],mq],function(a,b,c,d){return[0,[0,c],a]}],mp],mu=[0,[0,0,0,[0,[0,[0,[2,w],[0,mt,[0,ms,[0,[2,cg],0]]]],function(a,b,c,d,e){return[0,[0,d],a]}],mr]],mn];l(d[1][7],cf,0,mu);var
mv=0,mw=0,my=[0,[0,[0,[2,B],mx],function(a,b,c,d){return[0,[1,c],a]}],mw],mC=[0,[0,[0,[2,B],[0,mB,[0,mA,[0,mz,[0,[2,cf],0]]]]],function(a,b,c,d,e,f){return[0,[1,e],a]}],my],mD=[0,[0,0,0,[0,[0,[0,[2,B],0],function(a,b){return[0,[1,a],0]}],mC]],mv];l(d[1][7],cg,0,mD);var
mE=0,mF=0,mI=[0,[0,[0,mH,[0,mG,[0,[2,cf],0]]],function(a,b,c,d){return a}],mF],mJ=[0,[0,0,0,[0,[0,[0,[2,cg],0],function(a,b){return a}],mI]],mE];l(d[1][7],ch,0,mJ);var
mK=0,mL=0,mO=[0,[0,[0,[2,w],[0,mN,[0,mM,[0,[2,bs],0]]]],function(a,b,c,d,e){return[0,[0,[0,d],0],a]}],mL],mQ=[0,[0,[0,[2,w],mP],function(a,b,c,d){return[0,[0,[0,c],a[1]],a[2]]}],mO],mT=[0,[0,0,0,[0,[0,[0,[2,w],[0,mS,[0,mR,[0,[2,cj],0]]]],function(a,b,c,d,e){return[0,[0,[0,d],a[1]],a[2]]}],mQ]],mK];l(d[1][7],ci,0,mT);var
mU=0,mV=0,mX=[0,[0,[0,[2,B],mW],function(a,b,c,d){return[0,[0,[1,c],a[1]],a[2]]}],mV],m1=[0,[0,[0,[2,B],[0,m0,[0,mZ,[0,mY,[0,[2,ci],0]]]]],function(a,b,c,d,e,f){return[0,[0,[1,e],a[1]],a[2]]}],mX],m4=[0,[0,0,0,[0,[0,[0,[2,B],[0,m3,[0,m2,[0,[2,bs],0]]]],function(a,b,c,d,e){return[0,[0,[1,d],0],a]}],m1]],mU];l(d[1][7],cj,0,m4);var
m5=0,m6=0,m9=[0,[0,[0,m8,[0,m7,[0,[2,ci],0]]],function(a,b,c,d){return a}],m6],m_=[0,[0,0,0,[0,[0,[0,[2,cj],0],function(a,b){return a}],m9]],m5];l(d[1][7],dH,0,m_);var
m$=0,na=0,nb=[0,[0,[0,[2,w],0],function(a,b){return[0,[0,a],0]}],na],nd=[0,[0,[0,[2,w],nc],function(a,b,c,d){return[0,[0,c],a]}],nb],nh=[0,[0,0,0,[0,[0,[0,[2,w],[0,ng,[0,nf,[0,ne,[0,[2,dI],0]]]]],function(a,b,c,d,e,f){return[0,[0,e],a]}],nd]],m$];l(d[1][7],ck,0,nh);var
ni=0,nj=0,nl=[0,[0,[0,[2,B],nk],function(a,b,c,d){return[0,[1,c],a]}],nj],no=[0,[0,[0,[2,B],[0,nn,[0,nm,[0,[2,ck],0]]]],function(a,b,c,d,e){return[0,[1,d],a]}],nl],np=[0,[0,0,0,[0,[0,[0,[2,B],0],function(a,b){return[0,[1,a],0]}],no]],ni];l(d[1][7],dI,0,np);var
nq=0,nr=0,ns=[0,[0,[0,[2,w],0],function(a,b){return[0,[0,a],0]}],nr],nu=[0,[0,[0,[2,w],nt],function(a,b,c,d){return[0,[0,c],a]}],ns],nx=[0,[0,0,0,[0,[0,[0,[2,w],[0,nw,[0,nv,[0,[2,dJ],0]]]],function(a,b,c,d,e){return[0,[0,d],a]}],nu]],nq];l(d[1][7],cl,0,nx);var
ny=0,nz=0,nB=[0,[0,[0,[2,B],nA],function(a,b,c,d){return[0,[1,c],a]}],nz],nE=[0,[0,[0,[2,B],[0,nD,[0,nC,[0,[2,cl],0]]]],function(a,b,c,d,e){return[0,[1,d],a]}],nB],nF=[0,[0,0,0,[0,[0,[0,[2,B],0],function(a,b){return[0,[1,a],0]}],nE]],ny];l(d[1][7],dJ,0,nF);var
nG=0,nH=0,nI=[0,[0,[0,[2,w],0],function(a,b){return[0,[0,a],0]}],nH],nK=[0,[0,[0,[2,w],nJ],function(a,b,c,d){return[0,[0,c],a]}],nI];function
nL(a,b,c,d,e,f){return[0,[0,e],a]}var
nO=[0,nN,[0,nM,[0,[2,cn],0]]],nP=0,nR=[0,[0,nQ,function(a,b){return a}],nP],nS=[0,[0,0,0,[0,[0,[0,[2,w],[0,[8,i(b7[2],nR)],nO]],nL],nK]],nG];l(d[1][7],cm,0,nS);var
nT=0,nU=0,nW=[0,[0,[0,[2,aV],nV],function(a,b,c,d){return[0,[1,c],a]}],nU],n0=[0,[0,[0,[2,aV],[0,nZ,[0,nY,[0,nX,[0,[2,cm],0]]]]],function(a,b,c,d,e,f){return[0,[1,e],a]}],nW],n1=[0,[0,0,0,[0,[0,[0,[2,aV],0],function(a,b){return[0,[1,a],0]}],n0]],nT];l(d[1][7],cn,0,n1);var
n2=0,n3=0,n6=[0,[0,[0,n5,[0,n4,[0,[2,cm],0]]],function(a,b,c,d){return a}],n3],n7=[0,[0,0,0,[0,[0,[0,[2,cn],0],function(a,b){return a}],n6]],n2];l(d[1][7],dK,0,n7);var
n8=0,n9=0,n$=[0,[0,[0,n_,[0,[2,ch],0]],function(a,b,c){return[14,a]}],n9];function
oa(a,b,c,d,e,f,g){var
h=b$(a);return[15,b$(b),c,h]}var
ob=0,oc=0,oe=[0,[0,[0,od,[0,[2,dK],0]],function(a,b,c){return a}],oc],of=[0,[8,i(b7[2],oe)],ob],og=0,oj=[0,[0,[0,oi,[0,[7,[2,w],oh,0],0]],function(a,b,c){return a}],og],ok=[0,[8,i(b7[2],oj)],of],op=[0,[0,[0,oo,[0,on,[0,om,[0,[3,d[15][9],ol],ok]]]],oa],n$],or=[0,[0,[0,oq,[0,[2,ck],0]],function(a,b,c){return[7,a]}],op];function
os(a,b,c){return[16,a]}var
ov=[0,[0,[0,ou,[0,[7,[2,d[15][1]],ot,0],0]],os],or],ox=[0,[0,[0,ow,[0,[2,ch],0]],function(a,b,c){return[6,a]}],ov],oz=[0,[0,[0,oy,[0,[2,cl],0]],function(a,b,c){return[8,a]}],ox];function
oA(a,b,c,d,e,f){return[12,d,c,a]}var
oD=[0,[0,[0,oC,[0,[2,d[15][6]],[0,[4,[2,w]],[0,oB,[0,[2,d[15][1]],0]]]]],oA],oz];function
oE(a,b,c,d,e){return[13,[0,c],a]}var
oH=[0,[0,[0,oG,[0,[2,d[15][6]],[0,oF,[0,[2,d[15][1]],0]]]],oE],oD];function
oI(a,b,c,d,e){return[13,[1,c],a]}l(d[1][7],dL,0,[0,[0,0,0,[0,[0,[0,oK,[0,[2,br],[0,oJ,[0,[2,d[15][1]],0]]]],oI],oH]],n8]);var
oL=0,oM=0,oN=[0,[0,0,function(a){return 0}],oM],oP=[0,[0,oO,function(a,b){return 1}],oN],oR=[0,[0,oQ,function(a,b){return 2}],oP],oT=[0,[0,0,0,[0,[0,oS,function(a,b){return 3}],oR]],oL];l(d[1][7],dM,0,oT);var
oU=0,oV=0,oW=[0,[0,[0,[2,dF],0],function(a,b){return a}],oV],oX=[0,[0,0,0,[0,[0,[0,[2,dL],0],function(a,b){return a}],oW]],oU];l(d[1][7],dN,0,oX);var
oY=0,oZ=0,o1=[0,[0,0,0,[0,[0,[0,[2,dM],[0,[2,dN],o0]],function(a,b,c,d){return[0,c,b]}],oZ]],oY];l(d[1][7],dB,0,o1);var
dO=[0,iJ,b_,dx,iR,dy,dz,dA,ap,bq,dB,dC,b$];aB(476,dO,"Decl_mode_plugin.G_decl_mode");aB(477,[0,y,aL,aS,aU,dO],"Decl_mode_plugin");return}(function(){return this}()));
