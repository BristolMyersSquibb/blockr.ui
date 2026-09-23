// Answer boxes for a design-system topic page. For every question heading
// (h3 with a .qtag) it finds the option titles (.opt .t, "A. ...") and the
// proposal (.rec <b>), puts an answer box under the proposal, and adds a bar
// at the bottom that copies all answers as text. Answers persist per page.
(function () {
  const page = document.body.dataset.topic || location.pathname.split('/').pop();
  const KEY = 'blockr-ds-answers:' + page;
  const saved = JSON.parse(localStorage.getItem(KEY) || '{}');
  const css = `
.answer{max-width:86ch;margin:10px 0 8px;padding:10px 14px;border:1px solid var(--blockr-color-border-strong);border-radius:8px;background:var(--blockr-color-bg-surface)}
.answer .ah{font-size:12px;font-weight:600;color:var(--blockr-color-text-muted);margin-bottom:6px}
.answer .aq{color:var(--blockr-color-text-warning);margin-right:4px}
.answer .ar{display:flex;flex-wrap:wrap;gap:4px 18px}
.answer label{display:inline-flex;gap:6px;align-items:baseline;font-size:13px;cursor:pointer}
.answer .pr{font-size:11px;color:var(--blockr-color-text-accent)}
.answer textarea{display:block;width:100%;box-sizing:border-box;margin-top:8px;font:12px/1.4 "Open Sans",sans-serif;border:1px solid var(--blockr-color-border-default);border-radius:6px;background:var(--blockr-color-bg-field);color:var(--blockr-color-text-default);padding:5px 8px;resize:vertical}
.answer.changed{border-color:var(--blockr-color-border-warning);background:var(--blockr-color-bg-warning)}
.answer.discuss{border-color:var(--blockr-color-border-accent);background:var(--blockr-color-bg-accent-subtle)}
.ans-bar{position:sticky;bottom:0;z-index:20;background:var(--blockr-color-bg-page);border-top:1px solid var(--blockr-color-border-strong);padding:10px 0;display:flex;gap:8px;align-items:center;flex-wrap:wrap;margin-top:30px}
.ans-bar .st{font-size:12px;color:var(--blockr-color-text-muted)}
.ans-bar textarea{flex:1 1 100%;font:12px/1.45 var(--blockr-font-mono);border:1px solid var(--blockr-color-border-default);border-radius:6px;background:var(--blockr-color-bg-surface);color:var(--blockr-color-text-default);padding:8px}`;
  const st = document.createElement('style'); st.textContent = css; document.head.appendChild(st);
  const esc = s => s.replace(/[&<>"]/g, c => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;' }[c]));
  const boxes = [], proposal = {};
  document.querySelectorAll('h3').forEach(h => {
    const tag = h.querySelector('.qtag'); if (!tag) return;
    const q = tag.textContent.trim();
    const opts = [], it = [];
    let el = h.nextElementSibling, rec = null;
    while (el && !el.matches('h3')) {
      if (el.matches('.rec')) { rec = el; break; }
      el.querySelectorAll('.opt > .t, p.t').forEach(t => { const x = t.textContent.trim(); if (/^[A-F]\./.test(x)) opts.push(x); });
      el = el.nextElementSibling;
    }
    if (!rec || !opts.length) return;
    const b = rec.querySelector('b'); const m = b && /^(?:keep\s+)?([A-F])\b/.exec(b.textContent.trim());
    const p = m ? m[1] : '';
    proposal[q] = p;
    const box = document.createElement('div'); box.className = 'answer'; box.dataset.q = q;
    box.innerHTML = `<div class="ah"><span class="aq">${q}</span> Your answer</div><div class="ar">` +
      opts.map(o => { const v = o[0]; return `<label><input type="radio" name="${q}" value="${v}"${v === p ? ' checked' : ''}> ${esc(o)}${v === p ? ' <span class="pr">proposed</span>' : ''}</label>`; }).join('') +
      `<label><input type="radio" name="${q}" value="discuss"> Discuss separately</label></div><textarea rows="1" placeholder="note (optional)"></textarea>`;
    rec.after(box); boxes.push(box);
    if (saved[q]) { const i = box.querySelector(`input[value="${saved[q].v}"]`); if (i) i.checked = true; box.querySelector('textarea').value = saved[q].n || ''; }
  });
  if (!boxes.length) return;
  const bar = document.createElement('div'); bar.className = 'ans-bar';
  bar.innerHTML = '<button class="b s main">Copy answers</button><button class="b s quiet">Show text</button><button class="b s quiet">Reset to proposals</button><span class="st"></span><textarea rows="5" readonly hidden></textarea>';
  (document.querySelector('.wrap') || document.body).appendChild(bar);
  const [bCopy, bShow, bReset] = bar.querySelectorAll('button'), stat = bar.querySelector('.st'), ta = bar.querySelector('textarea');
  const title = (document.querySelector('h1') || {}).textContent || page;
  function update() {
    const store = {}, acc = [], chg = [], dis = [], notes = [];
    boxes.forEach(a => {
      const q = a.dataset.q, c = a.querySelector('input:checked'), v = c ? c.value : '', n = a.querySelector('textarea').value.trim();
      store[q] = { v, n };
      a.classList.toggle('changed', v !== proposal[q] && v !== 'discuss'); a.classList.toggle('discuss', v === 'discuss');
      const label = c ? c.parentElement.textContent.replace('proposed', '').trim() : v;
      if (v === 'discuss') dis.push(q + (n ? ' (' + n + ')' : ''));
      else if (v !== proposal[q]) chg.push(q + ': ' + label + (n ? ' (' + n + ')' : ''));
      else { acc.push(q); if (n) notes.push(q + ': ' + n); }
    });
    localStorage.setItem(KEY, JSON.stringify(store));
    const lines = [title.trim(), 'Proposal accepted: ' + (acc.join(', ') || 'none')];
    if (chg.length) lines.push('Changed: ' + chg.join('; '));
    if (dis.length) lines.push('Discuss separately: ' + dis.join('; '));
    if (notes.length) lines.push('Notes: ' + notes.join('; '));
    ta.value = lines.join(String.fromCharCode(10));
    stat.textContent = acc.length + ' as proposed, ' + chg.length + ' changed, ' + dis.length + ' to discuss';
  }
  boxes.forEach(a => a.querySelectorAll('input, textarea').forEach(e => e.addEventListener('input', update)));
  bCopy.onclick = () => { update(); (navigator.clipboard ? navigator.clipboard.writeText(ta.value) : Promise.reject()).then(() => stat.textContent += ' · copied', () => { ta.hidden = false; ta.select(); document.execCommand('copy'); }); };
  bShow.onclick = () => { ta.hidden = !ta.hidden; };
  bReset.onclick = () => { localStorage.removeItem(KEY); boxes.forEach(a => { const i = a.querySelector(`input[value="${proposal[a.dataset.q]}"]`); if (i) i.checked = true; a.querySelector('textarea').value = ''; }); update(); };
  update();
})();
