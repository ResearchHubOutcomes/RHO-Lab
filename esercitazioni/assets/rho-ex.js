// RHO - esercitazioni interattive: R nel browser con webR.
// Ogni blocco .cell contiene una textarea con codice R; opzioni:
//   data-check="<espressione R>"  -> dopo l'esecuzione la valuta: TRUE = risposta corretta
//   data-ok / data-ko              -> messaggi per risposta corretta / da rivedere
// Un pulsante con [data-use-solution="<id textarea>"] dentro una soluzione copia il codice nel blocco.
import { WebR } from 'https://cdn.jsdelivr.net/npm/webr@0.4.2/dist/webr.mjs';

const statusEl = document.getElementById('status');
const statusTxt = statusEl ? statusEl.querySelector('[data-txt]') : null;
const cells = [...document.querySelectorAll('.cell')];

for (const cell of cells) {
  const ta = cell.querySelector('textarea');
  ta.dataset.orig = ta.value;
  const bar = document.createElement('div');
  bar.className = 'bar';
  bar.innerHTML = '<button type="button" data-run disabled>Esegui</button>' +
    '<button type="button" class="ghost" data-reset>Ripristina il codice</button>' +
    '<span class="kbd">Ctrl + Invio per eseguire</span>';
  cell.append(bar);
  const check = document.createElement('p'); check.className = 'check'; check.setAttribute('aria-live', 'polite');
  const out = document.createElement('pre'); out.className = 'out';
  const plots = document.createElement('div'); plots.className = 'plots';
  cell.append(out, plots, check);
}

const setStatus = (cls, txt) => { if (!statusEl) return; statusEl.className = 'status ' + cls; statusTxt.textContent = txt; };

const webR = new WebR();
try {
  await webR.init();
  setStatus('ready', 'R è pronto: premi Esegui sotto ogni esercizio.');
  cells.forEach(c => c.querySelector('[data-run]').disabled = false);
} catch (e) {
  setStatus('fail', 'Non sono riuscito a caricare R nel browser. Prova a ricaricare la pagina con Chrome, Edge o Firefox aggiornati.');
}

async function run(cell) {
  const btn = cell.querySelector('[data-run]');
  const out = cell.querySelector('.out');
  const plots = cell.querySelector('.plots');
  const check = cell.querySelector('.check');
  const code = cell.querySelector('textarea').value;
  btn.disabled = true; btn.textContent = 'Eseguo…';
  out.textContent = ''; plots.innerHTML = ''; check.textContent = ''; check.className = 'check';
  const shelter = await new webR.Shelter();
  let failed = false;
  try {
    const res = await shelter.captureR(code, {
      withAutoprint: true, captureStreams: true, captureConditions: false,
      captureGraphics: { width: 640, height: 420 }
    });
    for (const line of res.output) {
      const span = document.createElement('span');
      if (line.type === 'stderr') span.className = 'err';
      span.textContent = line.data + '\n';
      out.append(span);
    }
    for (const img of res.images) {
      const cv = document.createElement('canvas');
      cv.width = img.width; cv.height = img.height;
      cv.getContext('2d').drawImage(img, 0, 0);
      plots.append(cv);
    }
  } catch (e) {
    failed = true;
    const span = document.createElement('span'); span.className = 'err';
    span.textContent = 'Errore: ' + String(e.message || e).replace(/^Error:\s*/, '');
    out.append(span);
  } finally {
    shelter.purge();
  }
  if (cell.dataset.check) {
    let ok = false;
    if (!failed) { try { ok = await webR.evalRBoolean('isTRUE(tryCatch({' + cell.dataset.check + '}, error = function(e) FALSE))'); } catch (e) { ok = false; } }
    check.className = 'check ' + (ok ? 'ok' : 'ko');
    check.textContent = ok ? '✓ ' + (cell.dataset.ok || 'Corretto.') : '✗ ' + (cell.dataset.ko || 'Non ancora: rileggi la consegna e riprova.');
  }
  btn.disabled = false; btn.textContent = 'Esegui';
}

for (const cell of cells) {
  cell.querySelector('[data-run]').addEventListener('click', () => run(cell));
  cell.querySelector('[data-reset]').addEventListener('click', () => {
    const ta = cell.querySelector('textarea'); ta.value = ta.dataset.orig;
    cell.querySelector('.out').textContent = ''; cell.querySelector('.plots').innerHTML = '';
    const ch = cell.querySelector('.check'); ch.textContent = ''; ch.className = 'check';
  });
  cell.querySelector('textarea').addEventListener('keydown', e => {
    if ((e.ctrlKey || e.metaKey) && e.key === 'Enter') { e.preventDefault(); if (!cell.querySelector('[data-run]').disabled) run(cell); }
    if (e.key === 'Tab' && !e.shiftKey) { e.preventDefault(); const t = e.target, s = t.selectionStart; t.setRangeText('  ', s, t.selectionEnd, 'end'); }
  });
}

for (const b of document.querySelectorAll('[data-use-solution]')) {
  b.addEventListener('click', () => {
    const ta = document.getElementById(b.dataset.useSolution);
    const sol = b.closest('details').querySelector('pre');
    if (ta && sol) { ta.value = sol.textContent; ta.focus(); ta.closest('.cell').scrollIntoView({ behavior: 'smooth', block: 'center' }); }
  });
}
