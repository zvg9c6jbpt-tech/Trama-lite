const {chromium}=require('../../verifiche/browser/node_modules/playwright');
const fs=require('fs');
const R=JSON.parse(fs.readFileSync('comuni_per_regione.json','utf8'));
const scelte=process.argv.slice(2);
(async()=>{const b=await chromium.launch({headless:true});const c=await b.newContext({userAgent:'TramaLite-luoghi/1.0 (indirizzo-di-contatto; ricerca storica, una tantum)'});
for(const nome of scelte){
 const lista=R[nome]; const out=[]; const B=40;
 for(let i=0;i<lista.length;i+=B){
  const v=lista.slice(i,i+B).map(x=>'wd:'+x[0]).join(' ');
  const s=`SELECT ?item ?l ?coord ?nsl ?c WHERE { VALUES ?c { ${v} } ?item wdt:P131 ?c; wdt:P31 ?cls; rdfs:label ?l; wdt:P625 ?coord; wikibase:sitelinks ?nsl . ?cls rdfs:label "frazione"@it . FILTER(LANG(?l)="it") }`;
  for(let t=0;t<4;t++){
   const r=await c.request.get('https://query.wikidata.org/sparql?format=json&query='+encodeURIComponent(s),{timeout:120000,headers:{Accept:'application/sparql-results+json'}}).catch(e=>null);
   if(r&&r.status()==200){out.push(...(await r.json()).results.bindings);break;}
   console.log(nome,i,'stato',r&&r.status(),'ritento'); await new Promise(z=>setTimeout(z,4000*(t+1)));
  }
 }
 fs.writeFileSync('scarico-wikidata/frazioni_'+nome+'.json',JSON.stringify(out));
 console.log(nome,lista.length,'comuni →',out.length,'righe',new Set(out.map(x=>x.item.value)).size,'frazioni');
}
await b.close();})();
