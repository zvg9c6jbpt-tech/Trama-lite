import os
exec(open(os.path.join(os.path.dirname(os.path.abspath(__file__)),'base.py')).read())
import json
reg=collections.defaultdict(list)
for c,i in ist.items():
    if com.get(c): reg[i['regione']].append((com[c][0],i['nome'],c))
print({r:len(v) for r,v in reg.items()})
json.dump(reg,open(B+'comuni_per_regione.json','w'),ensure_ascii=False)
