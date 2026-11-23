import json,re,csv,unicodedata,collections,math
import os
B=os.environ['LAVORO'].rstrip('/')+'/'
def k(s): return re.sub(r'[^a-z0-9]','',''.join(c for c in unicodedata.normalize('NFD',s.lower()) if unicodedata.category(c)!='Mn'))
ist={i['codice']:i for i in csv.DictReader(open(B+'istat.csv',encoding='utf-8'))}
allw=json.load(open(B+'scarico-wikidata/comuni.json'))['results']['bindings']+json.load(open(B+'scarico-wikidata/comuni2.json'))['results']['bindings']
def pt(x):
    m=re.match(r'Point\(([-\d.]+) ([-\d.]+)\)',x.get('coord',{}).get('value','')); return (float(m.group(2)),float(m.group(1))) if m else None
cand=collections.defaultdict(dict)   # codice -> {Q: (coord,nsl)}
orph=collections.defaultdict(dict)
for x in allw:
    c=x['istat']['value']; q=x['item']['value'].rsplit('/',1)[1]; n=int(x['nsl']['value']) if 'nsl' in x else 0
    t=cand if c in ist else orph
    key=c if c in ist else k(x['itemLabel']['value'])
    t[key][q]=(pt(x),max(n,t[key].get(q,(0,0))[1]))
def scegli(d):
    if not d: return None
    qs=sorted(d.items(),key=lambda a:(a[1][0] is None,-a[1][1])); return qs[0][0],qs[0][1][0]
com={}   # codice -> (Q,coord)
for c in ist:
    s=scegli(cand.get(c)) or (scegli(orph.get(k(ist[c]['nome']))) if len(orph.get(k(ist[c]['nome']),{}))==1 else None)
    com[c]=s
gn={}; gnP=collections.defaultdict(list)
for r in csv.reader(open(B+'scarico-geonames/IT.txt',encoding='utf-8'),delimiter='\t',quoting=csv.QUOTE_NONE):
    if r[6]=='A' and r[7]=='ADM3' and r[12]: gn[r[12]]=(float(r[4]),float(r[5]))
    if r[6]=='P' and r[12]: gnP[r[12]].append((r[1],float(r[4]),float(r[5]),int(r[14] or 0)))
byname=collections.defaultdict(list)
for c,i in ist.items(): byname[k(i['nome'])].append(c)
for c,i in ist.items():
    if i['altra']: byname[k(i['altra'])].append(c)   # nomi in altra lingua
base=list(csv.DictReader(open(os.path.join(os.path.dirname(os.path.abspath(__file__)),'..','..','dati','luoghi.csv'),encoding='utf-8'),delimiter=';'))
