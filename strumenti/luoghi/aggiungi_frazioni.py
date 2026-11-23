import json,csv,re,unicodedata,collections,sys
import os
B=os.environ['LAVORO'].rstrip('/')+'/'
L=os.path.join(os.path.dirname(os.path.abspath(__file__)),'..','..','dati','luoghi.csv')
REG=sys.argv[1:]
def k(s): return re.sub(r'[^a-z0-9]','',''.join(c for c in unicodedata.normalize('NFD',s.lower()) if unicodedata.category(c)!='Mn'))
def pt(v):
    m=re.match(r'Point\(([-\d.]+) ([-\d.]+)\)',v or ''); return f'{float(m.group(2)):.6f}, {float(m.group(1)):.6f}' if m else ''
cpr=json.load(open(B+'comuni_per_regione.json'))
nomeq={q:n for lst in cpr.values() for (q,n,c) in lst}
righe=list(csv.DictReader(open(L,encoding='utf-8'),delimiter=';'))
chiavi={r['chiave'] for r in righe}; nomik={k(r['nome']) for r in righe}
cand={}
for reg in REG:
    for x in json.load(open(B+f'scarico-wikidata/frazioni_{reg}.json')):
        n=x['l']['value'].strip(); q=x['item']['value'].rsplit('/',1)[1]; c=pt(x['coord']['value']); cq=x['c']['value'].rsplit('/',1)[1]
        if not c or len(k(n))<4 or re.match(r'^(Q\d+|.*\d.*)$',n): continue
        cand.setdefault(q,(n,c,nomeq.get(cq,''),int(x['nsl']['value'])))
byk=collections.defaultdict(list)
for q,(n,c,cn,s) in cand.items(): byk[k(n)].append(q)
nuovi=[]; saltati=collections.Counter()
for kk,qs in byk.items():
    # se il nome c'è già (comune, Stato, voce di prima), la frazione entra solo col comune accanto, per non confondere (vedi `singolo` sotto)
    for q in qs:
        n,c,cn,s=cand[q]
        singolo = len(qs)==1 and kk not in chiavi and kk not in nomik
        chiave = kk if singolo else kk+k(cn)
        if chiave in chiavi or not k(cn) and not singolo: saltati['chiave già presa o senza comune']+=1; continue
        chiavi.add(chiave); nuovi.append(dict(chiave=chiave,nome=n,paese='Italia',wikidata=q,coordinate=c,comune=cn,singolo=singolo))
print('frazioni candidate',len(cand),'nuove voci',len(nuovi),'con chiave semplice',sum(x['singolo'] for x in nuovi),'con comune nella chiave',sum(not x['singolo'] for x in nuovi),dict(saltati))
tot=[{k_:r[k_] for k_ in ('chiave','nome','paese','wikidata','coordinate')} for r in righe]+[{k_:x[k_] for k_ in ('chiave','nome','paese','wikidata','coordinate')} for x in nuovi]
tot.sort(key=lambda r:r['chiave']); assert len({r['chiave'] for r in tot})==len(tot)
with open(L,'w',encoding='utf-8',newline='') as o:
    w=csv.writer(o,delimiter=';',quoting=csv.QUOTE_ALL,lineterminator='\n'); w.writerow(['chiave','nome','paese','wikidata','coordinate'])
    for r in tot: w.writerow([r['chiave'],r['nome'],r['paese'],r['wikidata'],r['coordinate']])
print('totale',len(tot)); print([ (x['nome'],x['comune']) for x in nuovi if x['nome'] in('Bernaudo','Posillipo','Chiaia','Pozzuoli','Secondigliano')][:8])
