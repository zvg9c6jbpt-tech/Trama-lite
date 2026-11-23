# Aggiunge alla lista i comuni ISTAT che mancano o non hanno identificativo. Serve LAVORO (vedi LEGGIMI.md): istat.csv e scarico-wikidata/comuni*.json.
import csv,json,re,unicodedata,math,collections,io,os
def k(s): return re.sub(r'[^a-z0-9]','',''.join(c for c in unicodedata.normalize('NFD',s.lower()) if unicodedata.category(c)!='Mn'))
P=os.path.join(os.path.dirname(os.path.abspath(__file__)),'..','..','dati')+'/'
B=os.environ['LAVORO'].rstrip('/')+'/'
rows=list(csv.reader(open(P+'luoghi.csv',encoding='utf-8'),delimiter=';')); head=rows[0]; L={r[0]:r for r in rows[1:]}
ist=list(csv.DictReader(open(B+'istat.csv',encoding='utf-8')))
# Per ogni codice ISTAT, fra gli elementi trovati si sceglie quello con più collegamenti ai progetti Wikimedia (`nsl`) e le coordinate: il comune,
# non una sua frazione omonima (con «l'ultimo che capita» nove comuni avevano preso l'elemento di una frazione).
cand=collections.defaultdict(dict)
for f in ['comuni.json','comuni2.json']:
    for b in json.load(open(B+'scarico-wikidata/'+f))['results']['bindings']:
        m=re.match(r'Point\(([-0-9.]+) ([-0-9.]+)\)',b.get('coord',{}).get('value',''))
        if m:
            q=b['item']['value'].split('/')[-1]; n=int(b['nsl']['value']) if 'nsl' in b else 0
            c=b['istat']['value']; vecchio=cand[c].get(q)
            cand[c][q]=(max(n,vecchio[0]) if vecchio else n,'%.6f, %.6f'%(float(m.group(2)),float(m.group(1))))
w={c:(max(d.items(),key=lambda a:a[1][0])[0],max(d.items(),key=lambda a:a[1][0])[1][1]) for c,d in cand.items()}
Q={r[3] for r in L.values() if r[3]}
cnt=collections.Counter(r['nome'] for r in ist)
def km(a,b):
    la1,lo1=map(float,a.split(', '));la2,lo2=map(float,b.split(', '));r=math.pi/180
    x=math.sin((la2-la1)*r/2)**2+math.cos(la1*r)*math.cos(la2*r)*math.sin((lo2-lo1)*r/2)**2
    return 12742*math.asin(min(1,math.sqrt(x)))
corr=[];log=[]
for r in ist:
    c=r['codice']
    if c not in w or w[c][0] in Q: continue
    if r['sigla'] in ('SS','NU','OR','OG','CA','CI','OT','VS','SU'): 
        if r['nome']!='San Teodoro': continue
    q,co=w[c]; N=r['nome']; kn=k(N); prov='Sassari' if r['provincia'].startswith('Gallura') else r['provincia']
    chiavi_prov=kn+'prov'+k(prov)
    if cnt[N]>1:
        if kn in L and not L[kn][3]: corr.append((kn,N)); del L[kn]
        L[chiavi_prov]=[chiavi_prov,N,'Italia',q,co]; log.append(('nuova (omonimo)',N,chiavi_prov)); continue
    e=L.get(kn)
    if e and e[1]==N and (not e[3] or (e[4] and km(e[4],co)<30)):
        log.append(('aggiornata',N,kn,e[3],q)); e[3]=q; e[4]=co; continue
    t=kn if kn not in L else chiavi_prov
    L[t]=[t,N,'Italia',q,co]; log.append(('nuova',N,t))
out=[head]+[L[x] for x in sorted(L)]
b=io.StringIO(); csv.writer(b,delimiter=';',quoting=csv.QUOTE_ALL,lineterminator='\n').writerows(out)
assert len({r[0] for r in out[1:]})==len(out)-1, 'chiavi duplicate'
open(P+'luoghi.csv','w',encoding='utf-8').write(b.getvalue())
t=open(P+'luoghi_correzioni.csv',encoding='utf-8').read()
for kk,N in corr:
    riga='"%s";"%s";"";"";"";""\n'%(kk,N)
    if riga not in t: t+=riga     # rieseguito, non riscrive la stessa correzione
open(P+'luoghi_correzioni.csv','w',encoding='utf-8').write(t)
for x in log: print(x)
print(len(L),'voci')
