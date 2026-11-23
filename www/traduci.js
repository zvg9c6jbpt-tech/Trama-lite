// ─────────────────────────────────────────────────────────────
// traduci.js - l'interfaccia in inglese (TODO 14, strato 2; 1/10)
// Con «English» acceso sostituisce i testi dello schermo con quelli di
// trama-en.json (generato da traduzione/costruisci_dizionario.R dal foglio
// tradotto da Stefano); spento, rimette l'italiano. Lavora sul DOM: il
// codice R resta com'è, il database e gli export restano italiani.
//
// Tre modi di riconoscere un testo, dal più sicuro:
//  1. esatto: tutto il testo di un nodo è una voce del dizionario;
//  2. modello: voce con numeri o nomi dentro («3 fascicoli spuntati»);
//  3. composto: frasi messe insieme a pezzi dal server. Si traducono i
//     pezzi noti in testa, in coda e (se lunghi) in mezzo.
// Dove ci sono DATI (celle delle tabelle, voci dei menu, caselle) vale
// solo l'1 e il 2: un titolo o un nome non si toccano mai.
// ─────────────────────────────────────────────────────────────
(function () {
  var diz = null, modelli = [], perTrim = {}, frammenti = [], attivo = false, rg = null;
  var originali = new WeakMap();             // nodo di testo -> {it, en}
  // Ciò che è stato tradotto a pezzi (non per intero): è lì che un dato scritto
  // fuori da una zona di dati può uscire mezzo tradotto. tramaTraduci.aPezzi() lo elenca.
  var aPezzi = {};
  var ATTRIBUTI = ['placeholder', 'title', 'aria-label'];
  // .chip-campo: le parole chiave della ricerca («reato:», «fascicolo:»)
  // sono sintassi, restano italiane.
  var SALTA = 'script, style, textarea, code, pre, .no-trad, [contenteditable], .chip-campo';
  // .dato: un valore del database scritto fuori da una tabella (un titolo, un nome):
  // solo corrispondenze intere. .no-trad: mai, nemmeno intere (nomi di archivi e fondi).
  // option: il testo dei <option> sotto un menu selectize è un dato (un archivio, un luogo)
  // come la voce che si vede: «Archivio Storico…» diventava «Archive Storico…» (5/10).
  var DATI = 'td, option, .selectize-input .item, .selectize-dropdown .option, .selectize-dropdown .create, .cella-riga, .dato';

  function prepara(d) {
    diz = d.esatti;
    Object.keys(diz).forEach(function (k) {
      var t = k.trim(); if (!t) return;
      if (!(t in perTrim)) perTrim[t] = diz[k].trim();
      if (t.length >= 3) frammenti.push([k, diz[k]]);
    });
    frammenti.sort(function (a, b) { return b[0].length - a[0].length; });
    modelli = d.modelli.map(function (m) { return { re: new RegExp(m.re), en: m.en, en1: m.en1, e: m.e }; });
    rg = d.regesto || null;
  }

  // Il regesto composto narrativo: «Tipo per reato; reato contro|circa Nomi».
  // Si traduce solo se OGNI pezzo fisso è noto (tipo, ogni reato, connettore):
  // un regesto scritto a mano resta com'è. I nomi non si toccano.
  function regesto(testo) {
    if (!rg) return null;
    var t = testo.trim(), punto = /\.$/.test(t) ? '.' : ''; if (punto) t = t.slice(0, -1);
    var reati = function (x) {
      var v = x.split('; '), en = [];
      for (var i = 0; i < v.length; i++) { if (!(v[i] in rg.reati)) return null; en.push(rg.reati[v[i]]); }
      return en.join('; ');
    };
    var m = t.match(/^Atti per diversi per (.+)$/), r;
    if (m) { r = reati(m[1]); return r === null ? null : rg.per_diversi + ' ' + rg.per + ' ' + r + punto; }
    m = t.match(/^(.+?) per (.+?) (contro|circa) (.+)$/);
    if (!m || !(m[1] in rg.tipi) || !(m[3] in rg.connettori)) return null;
    r = reati(m[2]); if (r === null) return null;
    var chi = m[4] === 'ignoti' ? rg.ignoti : m[4] === 'diversi soggetti' ? rg.diversi : m[4].replace(/ e /g, ' ' + rg.e + ' ');
    return rg.tipi[m[1]] + ' ' + rg.per + ' ' + r + ' ' + rg.connettori[m[3]] + ' ' + chi + punto;
  }

  // Un pezzo inserito in un modello può essere a sua volta un testo
  // dell'interfaccia («ultima copia» + «8 minuti fa»): si traduce anche quello.
  // «2 di forma (…) e 1 di contenuto (…) e 2 da decidere»: un elenco con «e» si
  // traduce solo se OGNI pezzo è un testo noto (i nomi propri non si toccano).
  function congiunti(v, livello) {
    var coda = / e $/.test(v) ? ' and ' : '', p = v.replace(/ e $/, '').split(' e '), out = [], r, i;
    if (p.length < 2 && !coda) return null;
    for (i = 0; i < p.length; i++) { r = intero(p[i], livello); if (r === null) return null; out.push(r.trim()); }
    return out.join(' and ') + coda;
  }

  function conModello(t, livello) {
    for (var i = 0; i < modelli.length; i++) {
      var m = t.match(modelli[i].re); if (!m) continue;
      var en = (modelli[i].en1 && m[1] === '1') ? modelli[i].en1 : modelli[i].en;
      return en.replace(/\$(\d)/g, function (_, n) {
        var v = m[+n]; if (v === undefined) return '';
        if (modelli[i].e) return v.replace(/ e /g, modelli[i].e);     // titoli: «X e Y» -> «X and Y», i nomi restano
        if ((livello || 0) < 2 && /[A-Za-zÀ-ÿ]{2}/.test(v)) { var dentro = intero(v, (livello || 0) + 1); if (dentro === null) dentro = congiunti(v, (livello || 0) + 1); if (dentro !== null) return dentro; }
        return v;
      });
    }
    return null;
  }

  function lettera(c) { return !!c && /[A-Za-zÀ-ÿ]/.test(c); }

  // Il testo intero: esatto, o un modello. null se non si riconosce.
  // Nel DOM un testo arriva con spazi e a capo attorno: si prova com'è,
  // rifilato, e con uno spazio solo davanti o dietro (« su %d. Scaffale…»).
  function intero(testo, livello) {
    if (testo in diz) return diz[testo];
    var t = testo.trim(); if (!t) return null;
    var p = testo.indexOf(t), prima = testo.slice(0, p), dopo = testo.slice(p + t.length);
    if (t in perTrim) return prima + perTrim[t] + dopo;
    var forme = [testo, t, ' ' + t, t + ' ', ' ' + t + ' '], i, r;
    for (i = 0; i < forme.length; i++) {
      r = (i > 0 && forme[i] in diz) ? diz[forme[i]] : conModello(forme[i], livello);
      if (r !== null) return i === 0 ? r : prima + r.trim() + dopo;
    }
    return null;
  }

  // Frase composta a pezzi: i pezzi noti in testa e in coda, poi quelli
  // lunghi in mezzo (mai dentro una parola).
  function composto(testo) {
    // Nel DOM i pezzi arrivano separati da a capo e rientri (che il browser
    // mostra come uno spazio): si lavora sul testo con gli spazi ridotti a uno.
    var s = testo.replace(/\s+/g, ' ').trim(), cambiato = false, giro, i, k, en, p;
    var bordoPrima = /^\s/.test(testo) ? ' ' : '', bordoDopo = /\s$/.test(testo) ? ' ' : '';
    for (giro = 0; giro < 4; giro++) {
      var trovato = false;
      for (i = 0; i < frammenti.length && !trovato; i++) {
        k = frammenti[i][0]; en = frammenti[i][1];
        if (k.trim().length < 4 && !/^\s|\s$/.test(k)) continue;
        // (se il pezzo finisce con una lettera, dopo non deve continuare la parola)
        if (s.indexOf(k) === 0 && s.length > k.length && !(lettera(k.slice(-1)) && lettera(s.charAt(k.length)))) {
          var resto = intero(s.slice(k.length));
          // Una parola sola in testa («Archivio», «Importa») non basta a dire che
          // la frase è nostra: «Archivio Storico Diocesano di Capua» diventava
          // «Archive Storico…» (5/10). Si accetta solo se anche il resto si traduce.
          if (resto === null && k.trim().indexOf(' ') < 0 && lettera(k.slice(-1))) continue;
          s = en + (resto === null ? s.slice(k.length) : resto); trovato = cambiato = true;
        }
      }
      if (!trovato) break;
    }
    for (i = 0; i < frammenti.length; i++) {
      k = frammenti[i][0]; en = frammenti[i][1];
      if (k.trim().length < 8) break;
      p = s.indexOf(k);
      while (p >= 0) {
        if (!lettera(s.charAt(p - 1)) && !lettera(s.charAt(p + k.length))) {
          s = s.slice(0, p) + en + s.slice(p + k.length); cambiato = true; p = s.indexOf(k, p + en.length);
        } else p = s.indexOf(k, p + 1);
      }
    }
    for (i = 0; i < frammenti.length; i++) {           // un pezzo corto in coda (« buste», « righe»)
      k = frammenti[i][0]; en = frammenti[i][1];
      if (/^\s/.test(k) && s.length > k.length && s.slice(-k.length) === k) { s = s.slice(0, -k.length) + en; cambiato = true; break; }
    }
    return cambiato ? bordoPrima + s + bordoDopo : null;
  }

  // Un elenco di voci del vocabolario in una cella («Censura, Eresia»): si
  // traduce solo se OGNI voce è nel dizionario, così «Rossi, Mario» e
  // «Napoli, Roma» restano come sono.
  function elenco(testo) {
    var t = testo.trim(), sep = t.indexOf('; ') >= 0 ? '; ' : ', ';
    var voci = t.split(sep); if (voci.length < 2) return null;
    var en = [];
    for (var i = 0; i < voci.length; i++) { if (!(voci[i] in perTrim)) return null; en.push(perTrim[voci[i]]); }
    var p = testo.indexOf(t);
    return testo.slice(0, p) + en.join(sep) + testo.slice(p + t.length);
  }

  function traduci(testo, dati) {
    if (!diz || !testo || !/[A-Za-zÀ-ÿ]/.test(testo)) return null;
    var r = intero(testo);
    if (r === null) r = regesto(testo);
    if (r === null) r = elenco(testo);
    if (r === null && !dati) { r = composto(testo); if (r !== null && r !== testo) aPezzi[testo] = r; }
    // In una cella, una nota «etichetta: esempi» (Qualità: «coda di lavoro: Civitella…»):
    // si traduce l'etichetta fino ai due punti, gli esempi sono dati e restano.
    if (r === null && dati) {
      var d2 = testo.indexOf(': '), testa = d2 > 0 ? testo.slice(0, d2 + 1).trim() : '';
      if (testa && testa in perTrim) r = testo.replace(testa, perTrim[testa]);
    }
    return (r === null || r === testo) ? null : r;
  }

  // Titolo e regesto composti nella Scheda: il campo tiene l'italiano (è ciò che
  // si salva), e in inglese gli si mette sopra la stessa frase tradotta. Un clic
  // sulla vista riapre il campo italiano per scriverci. Un testo scritto a mano,
  // o che non si riconosce, resta com'è.
  function visteComposte() {
    var campi = document.querySelectorAll('.campo-composto');
    for (var i = 0; i < campi.length; i++) {
      var c = campi[i], campo = c.querySelector('input[type=text], textarea'); if (!campo) continue;
      var v = c.querySelector('.vista-en');
      var en = (attivo && diz && c.classList.contains('e-composto') && !c.classList.contains('scrivo') && campo.value)
        ? (intero(campo.value) || regesto(campo.value)) : null;
      if (en === null || en === campo.value) { if (v) v.remove(); c.classList.remove('ha-vista-en'); continue; }
      if (!v) {
        v = document.createElement('div'); v.className = 'vista-en no-trad form-control'; v.tabIndex = 0;
        v.title = 'Shown in English. The text is stored in Italian: click to edit it.';
        campo.insertAdjacentElement('afterend', v);
      }
      if (v.textContent !== en) v.textContent = en;
      c.classList.add('ha-vista-en');
    }
  }
  document.addEventListener('click', function (e) {
    var v = e.target.closest && e.target.closest('.vista-en'); if (!v) return;
    var c = v.closest('.campo-composto'); c.classList.add('scrivo'); visteComposte();
    var campo = c.querySelector('input[type=text], textarea'); if (campo) campo.focus();
  });
  document.addEventListener('focusout', function (e) {
    var c = e.target.closest && e.target.closest('.campo-composto.scrivo'); if (c) { c.classList.remove('scrivo'); setTimeout(visteComposte, 0); }
  });
  setInterval(visteComposte, 500);

  function nodoTesto(n) {
    var p = n.parentElement; if (!p || p.closest(SALTA)) return;
    var o = originali.get(n), ora = n.nodeValue;
    if (o && ora === o.en) return;                       // è già la nostra traduzione
    var en = traduci(ora, !!p.closest(DATI));
    if (en === null) { originali.delete(n); return; }
    originali.set(n, { it: ora, en: en }); n.nodeValue = en;
  }

  function attributi(el) {
    if (el.closest && el.closest(SALTA)) return;
    ATTRIBUTI.forEach(function (a) {
      if (!el.hasAttribute(a)) return;
      var v = el.getAttribute(a), chiave = 'data-it-' + a;
      if (el.getAttribute('data-en-' + a) === v) return;
      var en = traduci(v, false); if (en === null) return;
      el.setAttribute(chiave, v); el.setAttribute('data-en-' + a, en); el.setAttribute(a, en);
    });
  }

  function percorri(radice) {
    if (radice.nodeType === 3) return nodoTesto(radice);
    if (radice.nodeType !== 1) return;
    attributi(radice);
    var w = document.createTreeWalker(radice, NodeFilter.SHOW_TEXT | NodeFilter.SHOW_ELEMENT, null), n;
    while ((n = w.nextNode())) { if (n.nodeType === 3) nodoTesto(n); else attributi(n); }
  }

  function ripristina() {
    var w = document.createTreeWalker(document.body, NodeFilter.SHOW_TEXT | NodeFilter.SHOW_ELEMENT, null), n, o;
    while ((n = w.nextNode())) {
      if (n.nodeType === 3) { o = originali.get(n); if (o && n.nodeValue === o.en) n.nodeValue = o.it; originali.delete(n); }
      else ATTRIBUTI.forEach(function (a) {
        if (n.hasAttribute('data-it-' + a)) {
          if (n.getAttribute(a) === n.getAttribute('data-en-' + a)) n.setAttribute(a, n.getAttribute('data-it-' + a));
          n.removeAttribute('data-it-' + a); n.removeAttribute('data-en-' + a);
        }
      });
    }
  }

  var osservatore = new MutationObserver(function (lista) {
    if (!attivo) return;
    lista.forEach(function (m) {
      if (m.type === 'characterData') nodoTesto(m.target);
      else if (m.type === 'attributes') attributi(m.target);
      else m.addedNodes.forEach(percorri);
    });
  });

  // La lingua della pagina si dichiara subito: `attivo` parte falso e imposta(false) uscirebbe senza scrivere `lang` (WCAG 3.1.1).
  if (!document.documentElement.lang) document.documentElement.lang = 'it';

  function imposta(inglese) {
    if (inglese === attivo) return;
    attivo = inglese;
    document.documentElement.lang = inglese ? 'en' : 'it';
    if (!inglese) return ripristina();
    var parti = function () { percorri(document.body); };
    if (diz) parti();
    else fetch('trama-en.json?v=' + (window.TRAMA_VERSIONE || '')).then(function (r) { return r.json(); }).then(function (d) { prepara(d); if (attivo) parti(); })
      .catch(function (e) { console.error('Trama: dizionario inglese non caricato', e); });
  }

  // Per il collaudo: i testi visibili che con English acceso sono rimasti
  // com'erano (fuori dalle zone di dati). Non serve all'app.
  function nonTradotti(radice) {
    var out = [], w = document.createTreeWalker(radice || document.body, NodeFilter.SHOW_TEXT, null), n, p, t;
    while ((n = w.nextNode())) {
      p = n.parentElement; t = n.nodeValue.trim();
      if (!p || !t || !/[A-Za-zÀ-ÿ]{3}/.test(t) || p.closest(SALTA) || p.closest(DATI) || originali.has(n)) continue;
      if (p.offsetParent === null) continue;
      out.push(t);
    }
    return out;
  }
  window.tramaTraduci = { imposta: imposta, traduci: function (t) { return traduci(t, false); }, nonTradotti: nonTradotti,
                         aPezzi: function () { return Object.keys(aPezzi).map(function (k) { return k.trim() + '  =>  ' + aPezzi[k].trim(); }); } };

  document.addEventListener('DOMContentLoaded', function () {
    osservatore.observe(document.body, { childList: true, subtree: true, characterData: true,
                                        attributes: true, attributeFilter: ATTRIBUTI });
  });
  // L'interruttore «English» della barra del contesto (input$inglese).
  $(document).on('shiny:inputchanged', function (e) { if (e.name === 'inglese') imposta(!!e.value); });
})();

// La barra di caricamento di Shiny scrive «Upload complete» in inglese: in italiano si porta in italiano, con English acceso resta com'è.
(function () {
  var it = function () { return document.documentElement.lang !== 'en'; };
  function italiano(radice) {
    var w = document.createTreeWalker(radice, NodeFilter.SHOW_TEXT);
    for (var n = w.nextNode(); n; n = w.nextNode())
      if (n.nodeValue === 'Upload complete') n.nodeValue = 'Caricamento completato';
  }
  new MutationObserver(function (l) {
    if (!it()) return;
    l.forEach(function (m) {
      if (m.type === 'characterData') { if (m.target.nodeValue === 'Upload complete') m.target.nodeValue = 'Caricamento completato'; }
      else m.addedNodes.forEach(function (x) { if (x.nodeType === 1) italiano(x); });
    });
  }).observe(document.documentElement, { childList: true, subtree: true, characterData: true });
})();
