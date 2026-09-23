#!/usr/bin/env python3
"""Merge the parts into questions.html: one page, each part in dependency
order, and under every open question an answer box (options, "discuss
separately", a note). A bar at the bottom copies all answers as a summary.
Run from dev/design-system/:  python3 parts/build.py"""
import re, html, pathlib

ORDER = [("layout", "Inside a block"), ("gear", "Gear"), ("choices", "Choice controls"),
         ("menus", "Menus and popovers"), ("badges", "Badges and pills"), ("special", "Special blocks")]
here = pathlib.Path(__file__).parent
icons = (here / "_icons.svg.html").read_text()

def text(s):
    return html.unescape(re.sub(r"<[^>]+>", "", s)).strip()

# Questions already settled; shown in place instead of an answer box.
SETTLED = {
    "L1": "A. Small caps, muted",
    "L2": "A. 12px regular, all packages",
    "L3": "B. 16px, 600",
    "L4": "replaced by T6: B, one grid with two field sizes",
    "G2": "A. Title shares the row (changed from the proposal)",
    "C4": "B. Tags for both (changed from the proposal)",
    "B3": "C. Tint of the host (changed from the proposal)",
    "S5": "B. Everything to the canon (changed from the proposal)",
    "G1": "proposal accepted",
    "G4": "proposal accepted",
    "G6": "proposal accepted",
    "C2": "proposal accepted",
    "C3": "proposal accepted",
    "C5": "proposal accepted",
    "C6": "proposal accepted",
    "M1": "proposal accepted",
    "M2": "proposal accepted",
    "M3": "proposal accepted",
    "M4": "proposal accepted",
    "B4": "proposal accepted",
    "B5": "proposal accepted",
    "B6": "proposal accepted",
    "S1": "proposal accepted",
    "S2": "proposal accepted",
    "S3": "proposal accepted",
    "S4": "proposal accepted",
    "S6": "proposal accepted",
    "G3": "B. In flow, sliding open (topics/02)",
    "G5": "A. The gear and Escape only (topics/02)",
    "C1": "A. Checkbox everywhere, bare (topics/03)",
    "B1": "four kinds, told apart by shape (topics/04, B)",
    "B2": "B. Capsule, bordered (topics/04)",
    "M6": "C. Light card, one style with chart tooltips (topics/05)",
    "M5": "No popovers; details head the ... menu (topics/05, C)",
}
# Questions asked on topic pages; their answer box goes at the end of the part.
EXTRA = {}
DONE = [
    ("Tokens", "names, three text levels, surfaces, status sets, dark file (naming.md)"),
    ("Buttons", "main = tinted accent, secondary, quiet, destructive; 42 / 30 / 26 (buttons.html)"),
    ("Small icons, rows", "thin, muted, shown on hover; handle in the left padding"),
    ("Font", "Open Sans"),
    ("L1 Section titles", SETTLED["L1"]),
    ("L2 Field labels", SETTLED["L2"]),
    ("L3 Output title", SETTLED["L3"]),
    ("Checkbox in the grid", "C. Bare, no label row (topics/06b)"),
    ("Rows", "42px like inputs"),
    ("T1 Gear band", "B. Grey tray (topics/01)"),
    ("T6 Grid", "B. One grid, two field sizes (topics/06)"),
    ("From this page", "25 answers, recorded in naming.md"),
]


def answer_box(qid, opts, proposed, title=None, href=None):
    radios = "".join(
        f'<label class="o"><input type="radio" name="{qid}" value="{html.escape(o.split(".")[0])}"'
        f'{" checked" if o.startswith(proposed + ".") else ""}> {html.escape(o)}'
        f'{" <span class=pr>proposed</span>" if o.startswith(proposed + ".") else ""}</label>'
        for o in opts)
    radios += f'<label class="o"><input type="radio" name="{qid}" value="discuss"> Discuss separately</label>'
    head = f'<span class="aq">{qid}</span> Your answer'
    if title:
        head = (f'<span class="aq">{qid}</span> {html.escape(title)} '
                f'<a href="{href}">options on the topic page</a>')
    return (f'<div class="answer" data-q="{qid}"><div class="ah">{head}</div>'
            f'<div class="ar">{radios}</div>'
            f'<textarea class="note" rows="1" placeholder="note (optional)"></textarea></div>')


parts, toc, n_open = [], [], 0
for key, label in ORDER:
    f = here / f"{key}.html"
    if not f.exists():
        continue
    s = f.read_text()
    toc.append(f'<a href="#{key}">{label}</a>')
    out, pos = [], 0
    for m in re.finditer(r'<h3[^>]*>\s*<span class="qtag">([A-Z]\d+)</span>(.*?)</h3>', s, re.S):
        qid = m.group(1)
        rec = re.search(r'<div class="rec">(.*?)</div>', s[m.end():], re.S)
        if not rec:
            continue
        end = m.end() + rec.end()
        if qid in SETTLED:
            box = f'<div class="answer settled"><span class="aq">{qid}</span> Settled: {html.escape(SETTLED[qid])}</div>'
        else:
            between = s[m.end():m.end() + rec.start()]
            opts = [text(t) for t in re.findall(r'<p class="t">(.*?)</p>', between, re.S)]
            opts = [o for o in opts if re.match(r"[A-F]\.", o)]
            b = re.search(r"<b>(.*?)</b>", rec.group(1), re.S)
            letter = re.match(r"(?:keep\s+)?([A-F])\b", text(b.group(1)) if b else "")
            box = answer_box(qid, opts, letter.group(1) if letter else "")
            n_open += 1
        out.append(s[pos:end] + box)
        pos = end
    rest = s[pos:]
    extra = "".join(answer_box(q, o, p, t, h) for q, h, t, o, p in EXTRA.get(key, []))
    n_open += len(EXTRA.get(key, []))
    if extra:
        i = rest.rfind("</section>")
        rest = rest[:i] + extra + rest[i:]
    out.append(rest)
    parts.append("".join(out))

done = "".join(f"<li><b>{html.escape(a)}:</b> {html.escape(b)}</li>" for a, b in DONE)

page = f'''<!doctype html>
<html lang="en">
<head>
<meta charset="utf-8">
<title>blockr design system: the open questions</title>
<link rel="stylesheet" href="fonts/open-sans.css">
<link rel="stylesheet" href="../../inst/assets/css/blockr-tokens.css">
<link rel="stylesheet" href="../../inst/assets/css/blockr-tokens-dark.css">
<link rel="stylesheet" href="ds-page.css">
<style>
.top {{ position: sticky; top: 0; z-index: 20; background: var(--blockr-color-bg-page); border-bottom: 1px solid var(--blockr-color-border-default); padding: 12px 0 10px; margin-bottom: 18px; }}
.top .r {{ display: flex; justify-content: space-between; align-items: baseline; }}
.top nav {{ display: flex; gap: 16px; font-size: 13px; margin-top: 6px; flex-wrap: wrap; }}
.top nav a {{ color: var(--blockr-color-text-muted); text-decoration: none; }}
.top nav a:hover {{ color: var(--blockr-color-text-default); }}
.part {{ scroll-margin-top: 90px; }}
details.done {{ margin: 0 0 24px; font-size: 13px; }}
details.done summary {{ cursor: pointer; color: var(--blockr-color-text-muted); }}

.answer {{ max-width: 86ch; margin: 10px 0 8px; padding: 10px 14px; border: 1px solid var(--blockr-color-border-strong); border-radius: 8px; background: var(--blockr-color-bg-surface); }}
.answer .ah {{ font-size: 12px; font-weight: 600; color: var(--blockr-color-text-muted); margin-bottom: 6px; }}
.answer .ah a {{ font-weight: 400; margin-left: 6px; }}
.answer .aq {{ color: var(--blockr-color-text-warning); margin-right: 4px; }}
.answer .ar {{ display: flex; flex-wrap: wrap; gap: 4px 18px; }}
.answer label.o {{ display: inline-flex; gap: 6px; align-items: baseline; font-size: 13px; cursor: pointer; }}
.answer .pr {{ font-size: 11px; color: var(--blockr-color-text-accent); }}
.answer textarea.note {{ display: block; width: 100%; box-sizing: border-box; margin-top: 8px; font: 12px/1.4 "Open Sans", sans-serif; border: 1px solid var(--blockr-color-border-default); border-radius: 6px; background: var(--blockr-color-bg-field); color: var(--blockr-color-text-default); padding: 5px 8px; resize: vertical; }}
.answer.changed {{ border-color: var(--blockr-color-border-warning); background: var(--blockr-color-bg-warning); }}
.answer.discuss {{ border-color: var(--blockr-color-border-accent); background: var(--blockr-color-bg-accent-subtle); }}
.answer.settled {{ border-color: color-mix(in srgb, var(--blockr-color-border-success) 45%, transparent); background: var(--blockr-color-bg-success); color: var(--blockr-color-text-success); font-size: 13px; }}

.out {{ position: sticky; bottom: 0; z-index: 20; background: var(--blockr-color-bg-page); border-top: 1px solid var(--blockr-color-border-strong); padding: 10px 0; display: flex; gap: 8px; align-items: center; flex-wrap: wrap; }}
.out .st {{ font-size: 12px; color: var(--blockr-color-text-muted); }}
.out textarea {{ flex: 1 1 100%; font: 12px/1.45 var(--blockr-font-mono); border: 1px solid var(--blockr-color-border-default); border-radius: 6px; background: var(--blockr-color-bg-surface); color: var(--blockr-color-text-default); padding: 8px; box-sizing: border-box; }}
.out textarea[hidden] {{ display: none; }}
</style>
</head>
<body>
{icons}
<div class="wrap">
<div class="top"><div class="r"><h1>blockr design system: the open questions</h1>
<div class="switch"><button data-scheme="light" aria-pressed="true">Light</button> <button data-scheme="dark" aria-pressed="false">Dark</button></div></div>
<nav>{" ".join(toc)}</nav></div>
<p class="lede">{n_open} open questions, in dependency order. Under each proposal there is an answer box with the proposal
selected: change it, pick "Discuss separately", or add a note. Answers are kept in this browser. When you are done, press
<b>Copy answers</b> in the bar at the bottom and paste the text to me.</p>
<details class="done"><summary>Settled so far ({len(DONE)})</summary><ul>{done}</ul></details>
{"".join(parts)}
<div class="out"><button class="b s main" id="copy">Copy answers</button>
<button class="b s quiet" id="show">Show text</button>
<button class="b s quiet" id="reset">Reset to proposals</button>
<span class="st" id="status"></span>
<textarea id="summary" rows="6" readonly hidden></textarea></div>
</div>
<script>
const KEY = 'blockr-ds-answers-v1';
const saved = JSON.parse(localStorage.getItem(KEY) || '{{}}');
const boxes = [...document.querySelectorAll('.answer[data-q]')];
const proposal = {{}};
boxes.forEach(a => {{
  const q = a.dataset.q, c = a.querySelector('input:checked');
  proposal[q] = c ? c.value : '';
  if (saved[q]) {{
    const i = a.querySelector(`input[value="${{saved[q].v}}"]`); if (i) i.checked = true;
    a.querySelector('textarea').value = saved[q].n || '';
  }}
}});
const optText = (a, v) => {{ const i = a.querySelector(`input[value="${{v}}"]`); return i ? i.parentElement.textContent.replace('proposed', '').trim() : v; }};
function update() {{
  const store = {{}}, acc = [], chg = [], dis = [], notes = [];
  boxes.forEach(a => {{
    const q = a.dataset.q, c = a.querySelector('input:checked'), v = c ? c.value : '';
    const n = a.querySelector('textarea').value.trim();
    store[q] = {{ v, n }};
    a.classList.toggle('changed', v !== proposal[q] && v !== 'discuss');
    a.classList.toggle('discuss', v === 'discuss');
    if (v === 'discuss') dis.push(q + (n ? ' (' + n + ')' : ''));
    else if (v !== proposal[q]) chg.push(q + ': ' + optText(a, v) + (n ? ' (' + n + ')' : ''));
    else {{ acc.push(q); if (n) notes.push(q + ': ' + n); }}
  }});
  localStorage.setItem(KEY, JSON.stringify(store));
  const lines = ['Design-system answers (questions.html)', 'Proposal accepted: ' + (acc.join(', ') || 'none')];
  if (chg.length) lines.push('Changed: ' + chg.join('; '));
  if (dis.length) lines.push('Discuss separately: ' + dis.join('; '));
  if (notes.length) lines.push('Notes: ' + notes.join('; '));
  document.getElementById('summary').value = lines.join(String.fromCharCode(10));
  document.getElementById('status').textContent = acc.length + ' as proposed, ' + chg.length + ' changed, ' + dis.length + ' to discuss';
}}
document.querySelectorAll('.answer input, .answer textarea').forEach(e => e.addEventListener('input', update));
document.getElementById('copy').addEventListener('click', () => {{
  update(); const t = document.getElementById('summary').value;
  (navigator.clipboard ? navigator.clipboard.writeText(t) : Promise.reject()).then(
    () => document.getElementById('status').textContent += ' · copied',
    () => {{ const s = document.getElementById('summary'); s.hidden = false; s.select(); document.execCommand('copy'); }});
}});
document.getElementById('show').addEventListener('click', () => {{ const s = document.getElementById('summary'); s.hidden = !s.hidden; }});
document.getElementById('reset').addEventListener('click', () => {{
  localStorage.removeItem(KEY);
  boxes.forEach(a => {{ const i = a.querySelector(`input[value="${{proposal[a.dataset.q]}}"]`); if (i) i.checked = true; a.querySelector('textarea').value = ''; }});
  update();
}});
update();
document.querySelectorAll('.switch button').forEach(b => b.addEventListener('click', () => {{
  document.documentElement.setAttribute('data-bs-theme', b.dataset.scheme);
  document.querySelectorAll('.switch button').forEach(x => x.setAttribute('aria-pressed', x === b));
}}));
</script>
</body>
</html>
'''
(here.parent / "questions.html").write_text(page)
print(f"questions.html: {len(parts)} parts, {n_open} open questions")
