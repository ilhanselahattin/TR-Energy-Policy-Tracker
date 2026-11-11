# karbon-atlasi-turkiye

**A facility-level atlas of Türkiye's carbon-intensive infrastructure.**

An open-source R Shiny atlas that maps 300 individual installations — 88 industrial plants
in the EU CBAM sectors and 212 energy assets — and assembles the evidence linking them:
the grid carbon intensity that industrial indirect emissions depend on, and the fleet that
produces it.

> ### What works today, and what does not
>
> **Built and verified:** the acquisition pipeline with a full provenance chain, the
> coverage audit that fixes `t₀` empirically, the 300-facility register with province and
> İBBS-2 assignment, commissioning years from GEM, three cross-validated estimates of grid
> carbon intensity, the **1,800-row facility × year emissions panel** (2021–2026), and an
> interactive map of both populations **driven by a time slider**, with projected years
> drawn dashed and partial years labelled by their month count, plus a second
> **2000–2026 fleet development view** carrying its own slider, a coverage band and
> Ember's national installed capacity as a reference line.
>
> **Not built:** the CBAM liability calculation. The panel's `co2_direct_t` and
> `co2_indirect_t` columns are deliberately empty pending the direct/indirect
> decomposition, which is a modelling decision and not a parsing one. The 2000–2026 fleet
> animation is also still absent — the time slider covers 2021–2026, which is the emissions
> panel, not the fleet. The regulatory parameters are in place and cited. See
> [ROADMAP.md](ROADMAP.md).
>
> **This tool does not yet produce a CBAM exposure figure.** When it does, this paragraph
> will say so.

---

## Türkçe özet

Bu proje, Türkiye'nin karbon yoğun altyapısını **tesis düzeyinde** haritalayan açık
kaynaklı bir R Shiny atlasıdır. Birbirinden ayrı incelenen iki popülasyonu ve aralarındaki
bağı bir araya getirir:

- **88 sanayi tesisi** — demir-çelik, çimento, alüminyum (SKDM sektörleri)
- **212 enerji varlığı** — 157 elektrik santrali, 38 kömür ocağı, 17 petrol-gaz tesisi

Bağ şudur: sanayi tesislerinin **dolaylı** emisyonu, tükettikleri elektriğin karbon
yoğunluğuna bağlıdır; o yoğunluğu da haritalanabilir bir filo üretir.

**Bugün ne var, ne yok.** Veri boru hattı, tesis kütüğü, devreye giriş yılları, şebeke
yoğunluğunun üç bağımsız tahmini, **1.800 satırlık tesis × yıl emisyon paneli (2021–2026)**
ve **zaman slider'lı harita** çalışıyor. **SKDM maruziyet hesabı henüz yok** — gerekli
mevzuat parametreleri hazır ve kaynaklı, ama dayandıkları modelleme kararları verilmedi.
Filo gelişimi ayrı bir sekmede, kendi slider'ıyla 2000–2026 arasını gösterir.

**Siteye yerleştirme.** Uygulama bir web sayfasına `<iframe>` ile gömülmek üzere
hazırlandı. Önce `Rscript scripts/06_bundle_app.R` ile veri paketlenir, sonra `app/`
klasörü yayımlanır. Ev sahibi sayfa, `?tab=…&year=…&facility=…` bağlantıları ve
`postMessage` komutlarıyla görünümü yönetebilir. Uygulama da yüksekliğini ve o anki
durumunu sayfaya bildirir. Ayrıntılar aşağıdaki **Deployment** ve **Embedding**
bölümlerinde.

**Kestirim yılları gözlem gibi çizilmez.** 2025 ve 2026 Climate TRACE'in kestirimidir;
haritada kesik çizgili ve soluk, grafikte içi boş noktayla gösterilir. 2026 üstelik
**kısmi** bir yıldır ve iki popülasyon için farklı uzunluktadır — sanayi 5 ay, enerji 6 ay.
Arayüz bunu her seferinde söyler; kısmi yıl tam yıla ölçeklenerek tamamlanmaz.

**Önemli — bu araç bir vergi hesaplayıcısı değildir.** SKDM sertifikalarını AB'deki
ithalatçı satın alır, Türk üretici değil. Hesap eklendiğinde üretilecek rakam, tesisin
AB'ye giden üretimine gömülü emisyonun yarattığı **maliyet baskısı** olacaktır — tesise
kesilecek bir fatura değil. Bu maliyetin ne kadarının üreticiye yansıyacağı bir pazarlık
gücü sorusudur ve bu modelin dışındadır.

Tüm emisyon rakamları **modellenmiş tahmindir.** Türkiye'de tesis düzeyinde doğrulanmış
sera gazı raporları kamuya açık değildir.

---

## Why this exists

Existing analyses of Turkish CBAM exposure work at national or sectoral resolution. That
is enough to say "Turkish steel is exposed"; it is not enough to say *which plants, in
which provinces, by how much*. Regional development agencies, journalists and policymakers
need the second answer and cannot get it from an aggregate.

The contribution is the combination — facility-level, spatially explicit, open source,
with an audit trail from every displayed figure back to its inputs. The arithmetic itself
is commodity.

---

## Scope

### What is covered

| Dimension | Current |
|---|---|
| Industrial sectors | Iron & steel 27, cement 58, aluminium 3 — **88 facilities**, CO₂ basis |
| Energy assets | Electricity generation 157, coal mining 38, oil & gas 17 — **212 facilities**, CO₂e basis |
| Total | **300 records at 290 distinct locations** |
| Geography | Türkiye, facility → province → İBBS-2 region → national |
| Emissions years | 2021–2026 (`t₀ = 2021`, established empirically; 2026 partial at 5 months) |
| Commissioning years | **66% populated**, back to 1911, from GEM. Aluminium, refining and oil & gas are 0% because GEM publishes no tracker for them |
| Regime | EU CBAM |

**The two populations use different gas bases and are never summed.** CBAM is a CO₂
instrument, so industrial figures are CO₂. Only 18% of Turkish coal mining's footprint is
CO₂ — the rest is fugitive methane — so energy figures are CO₂e over 100 years. A total
spanning both would be meaningless, and the interface says so before the idea forms.

### What is not covered, and why

These are decisions with reasons, not gaps left by accident. Each is argued in
[ROADMAP.md](ROADMAP.md).

- **Three of the six CBAM goods categories.** CBAM also covers fertilisers, electricity
  and hydrogen. Electricity and hydrogen are excluded by choice — Turkish electricity
  exports to the EU are marginal and hydrogen volumes negligible. **Fertilisers are
  excluded on evidence:** no open source publishes Turkish fertiliser-*production*
  facilities. Climate TRACE's similarly named `synthetic-fertilizer-application` covers
  N₂O from fertiliser applied to soils — a different emission source, and not what CBAM
  regulates.
- **Aluminium PFCs.** CBAM covers CO₂ *and* perfluorocarbons for aluminium. No PFC country
  package is published, so aluminium exposure computed here is an underestimate. The gap
  is flagged in the interface rather than filled.
- **No equilibrium modelling.** This is an accounting framework. No price elasticity, no
  substitution, no trade diversion. The claim is narrow and deliberate: *given this output
  and this emission intensity, liability under scenario X is Y.*

---

## Installation

Requires **R 4.5.1 or later**. Development and testing were done on Windows 11.

```r
# 1. Clone, then from the project root:
install.packages("renv")
renv::restore()      # installs the exact package versions in renv.lock
```

`sf` needs GDAL, GEOS and PROJ. On Windows these ship with the CRAN binary. On Linux
install `libgdal-dev libgeos-dev libproj-dev` first.

### Build the data

`data/raw/` is not committed — reproducibility comes from the fetch scripts, not from a
45 MB blob in git history. Run the pipeline in order:

```bash
Rscript scripts/00_coverage_audit.R      # availability evidence, fixes t₀
Rscript scripts/01_fetch_climate_trace.R # downloads sources, writes SOURCES.md
Rscript scripts/01b_fetch_eu_trade.R     # EU trade quantities from Eurostat Comext
Rscript scripts/01c_ingest_gem.R         # commissioning years — NEEDS A MANUAL STEP
Rscript scripts/01d_fetch_ember.R        # national generation, grid intensity
Rscript scripts/02_build_facilities.R    # builds facilities.rds
Rscript scripts/03_build_panel.R         # builds facility_panel.rds
Rscript scripts/04_build_fleet_timeline.R # builds the 2000–2026 fleet series
Rscript scripts/05_build_ownership.R     # builds the parent-company layer
```

On Windows use `Rscript`, not `R` — in PowerShell `R` collides with the `Invoke-History`
alias.

#### The one manual step

Everything else fetches itself. **`01c_ingest_gem.R` does not**, because Global Energy
Monitor distributes its trackers through a form rather than a download URL:

1. Download the **Global Integrated Power Tracker** workbook from
   [globalenergymonitor.org](https://globalenergymonitor.org/projects/global-integrated-power-tracker/download-data/)
2. Save it, unrenamed, into `data/raw/gem/`
3. Optionally add the **Global Coal Mine Tracker** workbook to the same folder

The script stops with these instructions if the file is absent; it never proceeds with an
empty table.

This step is here because the open alternative was measured and found unusable. WRI's
Global Power Plant Database downloads freely but populates `commissioning_year` for only
25 of 163 Turkish plants — 15% — and stops at 2017, reporting 695 MW of solar against a
real figure more than an order of magnitude larger. A fleet timeline built on that would
show 18 plants appearing while 145 stayed invisible. One manual download was the better
trade; the reasoning is recorded in [ROADMAP.md](ROADMAP.md) under E2.

### Run the app

```r
shiny::runApp("app")
```

---

## Deployment

Every hosting option publishes the `app/` directory on its own, but inside the repository
the app reads `data/processed/` and `policies/` from the project root. Bundle first:

```bash
Rscript scripts/06_bundle_app.R      # writes app/bundle/ (untracked)
```

This copies all of `data/processed/` and `policies/`, plus `LICENSE` and the
`SOURCES*.md` attribution files, into `app/bundle/`. It also writes `MANIFEST.json`, which
records the commit, whether the working tree was clean, and the SHA-256 of every file.
Published without a project root above it, the app reads the bundle. Inside the
repository it always reads the live data, so a stale bundle cannot shadow a fresh build.
Then publish `app/` with whichever route is chosen:

| Route | Command | Notes |
|---|---|---|
| shinyapps.io / Posit Connect Cloud | `rsconnect::deployApp("app")` | Fast first load; usage quota on free plans |
| Shinylive (static, no server) | `shinylive::export("app", "<out>")` | Runs in the browser via webR; 10–30 s first load |
| Shiny Server (own machine) | copy `app/` to the server's app directory | Full control |

Re-run the bundle after every pipeline run. `tests/testthat/test-bundle.R` fails while
`app/bundle/` differs from the data it was cut from. It also proves the deployment path:
it builds a throwaway copy of `app/` with a fresh bundle outside the repository and
starts the server there.

Redistributing the bundle redistributes CC BY 4.0 Adapted Material. The attribution
files travel inside it for that reason; keep them there.

---

## Embedding

The atlas is built to sit in an `<iframe>` on a host page (recepresearch.org, "araçlar").
The host supplies its own interface around it. The two sides talk through
`window.postMessage` (`app/www/embed.js`, `server.R` section 8). This works across
origins and needs no cookies.

```html
<iframe id="atlas" src="https://<app-host>/?tab=map&year=2024"
        style="width:100%; height:900px; border:0" loading="lazy"
        title="Türkiye Karbon Atlası"></iframe>
```

### Opening a specific view — query string

| Parameter | Values | Effect |
|---|---|---|
| `tab` | `map`, `fleet`, `grid`, `sources` | opens that tab |
| `year` | a panel year (2021–2026) | sets the map tab's slider |
| `fleet_year` | a timeline year (2000–2026) | sets the fleet tab's slider |
| `facility` | a `facility_id` from `facilities.rds` | selects it, opens the map, zooms to it |

Every value is checked against what exists. An unknown value is ignored, never an error,
so a stale link still opens a working atlas. Selecting a facility hidden by the current
filters resets the filters first.

### App → host messages

Every message has `source: "karbon-atlasi"` and `version: 1`.

| `type` | Fields | Use |
|---|---|---|
| `ready` | — | the session is connected; commands are accepted from now on |
| `resize` | `height` (CSS px) | sizes the iframe to its content, sent on every change |
| `state` | `tab`, `year`, `fleet_year`, `facility`, `facility_name` | reflects the view in the host's own URL for shareable links |

`height` is measured from the page content, not from the document. AdminLTE stretches the
document to the window, and reporting that height looped: the host grew the iframe, the
page grew with it, and the next report was taller still. If the iframe is auto-sized, the
host page scrolls instead of the iframe, and the sticky map no longer sticks. With a fixed
iframe height the app scrolls inside the frame and the map stays in view. Pick one per
layout.

### Host → app commands

```js
document.getElementById("atlas").contentWindow.postMessage(
  { target: "karbon-atlasi", tab: "fleet", fleet_year: 2010 }, "*");
document.getElementById("atlas").contentWindow.postMessage(
  { target: "karbon-atlasi", facility: "CT16560" }, "*");
```

Fields and validation are the same as the query string. A message is accepted only when
it comes from the embedding parent and is addressed to `"karbon-atlasi"`.

### A minimal host

```js
const atlas = document.getElementById("atlas");
window.addEventListener("message", (e) => {
  const m = e.data;
  if (!m || m.source !== "karbon-atlasi") return;
  if (m.type === "resize") atlas.style.height = m.height + "px";
  if (m.type === "state") {
    const q = new URLSearchParams({ tab: m.tab, year: m.year });
    if (m.facility) q.set("facility", m.facility);
    history.replaceState(null, "", "?" + q);   // shareable link on the host
  }
});
```

### What the host may restyle, and what it must keep

The look is meant to be replaced: `app/www/atlas.css`, the fonts, and the colour
constants in `global.R`. The sector colours were measured against a dark basemap; on a
light basemap they need re-measuring. Three things are functional rather than visual, and
must survive a restyle:

- `app/www/embed.js`, the contract above.
- The `tags$style` rules in `ui.R` marked *FUNCTIONAL, NOT THEME*. One makes the
  place-name layer click-through; without it no facility can be selected, and nothing
  errors. Another shrinks the maps on phones.
- The epistemic markings: dashed, desaturated projection years; "modelled estimate"
  notices; the CC BY notices on the sources tab and inside the CSV download.

The class `atlas-embedded` is set on `<html>` only when framed, as a hook for
embed-specific styling.

### Download

The map tab's "Seçili veriyi indir (CSV)" button writes the current selection and year.
The file is UTF-8 with a BOM, so Excel shows Turkish characters correctly. It uses comma
separators and decimal points. The licence notice sits in `#` lines above the header:
read the file with `read.csv(f, comment.char = "#")`. Text cells beginning with `= + @`
are prefixed with `'`, so a spreadsheet cannot execute them as formulas.

---

## Repository layout

```
app/          global.R, ui.R, server.R — the dashboard
  www/        atlas.css (theme, replaceable), embed.js (host contract, keep)
  bundle/     deployment snapshot from scripts/06_bundle_app.R (not tracked)
data/raw/     downloaded sources (not tracked)
data/processed/  facilities.rds, coverage matrices, SOURCES.md
policies/     regulatory parameters as JSON, read at runtime
scripts/      numbered ETL pipeline plus _sources.R helpers
```

Regulatory parameters live in `policies/*.json` and are **never hardcoded in R**. This is
not a style preference: TR-ETS secondary legislation was still being finalised through
mid-2026 and CBAM certificate prices update quarterly. If maintenance required editing R
source, this project would be dead within two years.

---

## Data sources

| Source | Use | Licence |
|---|---|---|
| [Climate TRACE](https://climatetrace.org) | Facility emissions, production, capacity, technology | CC BY 4.0 |
| [Global Energy Monitor](https://globalenergymonitor.org) | Commissioning years, captive-plant status, cross-validation | CC BY 4.0 |
| [Ember](https://ember-energy.org) | National generation by fuel, grid carbon intensity | CC BY 4.0 |
| [Natural Earth](https://www.naturalearthdata.com) | Province and İBBS-2 assignment | Public domain |
| European Commission | CBAM phase-in factors, certificate prices | — |

**No ShareAlike anywhere in the chain**, which is why geoBoundaries was rejected for
province boundaries: its Turkey layer is CC BY-SA 2.0 and ShareAlike would have propagated
into this project's CC BY 4.0 derived data.

Full provenance — release tags, SHA-256 digests, retrieval dates and the complete
attribution chain — is in [SOURCES.md](data/processed/SOURCES.md),
[SOURCES_GEM.md](data/processed/SOURCES_GEM.md) and
[SOURCES_EMBER.md](data/processed/SOURCES_EMBER.md), each regenerated by the script that
fetches it.

**Attribution is a licence condition, not a courtesy**, and naming the source is not
enough: CC BY 4.0 §3(1)(a)(ii) requires stating *that* the material was modified, and §4
attaches the condition to derived databases. Those statements are generated on every
ingest and a test fails the build if data is present without them. If you redistribute
data derived from this project, carry the chain forward.

---

## Known data-quality issues

Published rather than hidden, because a tool that shows 300 confident dots while knowing
some of them are uncertain is overstating what it knows.

One of these is not specific to Türkiye and is worth stating plainly for anyone doing
similar work: **a national grid carbon intensity derived from Climate TRACE's
`electricity-generation` register alone will run high wherever the fleet is substantially
renewable**, because that register is combustion-only. The size of the error tracks the
renewable share, so it is largest in the countries decarbonising fastest. The full
argument, with its measurements, will appear in `METHODOLOGY.md`.

- **80 of 300 facilities** sit within 2 km of a province boundary, were snapped to the
  nearest polygon, or are offshore. Every facility carries a `geocode_quality` flag and
  per-facility distances are in `data/processed/facilities_geocode_report.csv`.
- **Twelve same-subsector near-duplicates**, including four coal-mine pairs at identical
  coordinates and the Kars cement pair 71 m apart whose operators are consistent with one
  plant recorded twice across an ownership change. Unresolved; nothing has been dropped on
  suspicion. See ROADMAP E7.
- **300 records are 290 places.** Climate TRACE lists each oil and gas field twice, under
  production and under transport, at one location. Correct accounting, misleading
  cartography — both counts are reported.
- **One probable province misassignment.** Koç Metalurji Toprakkale was assigned to Hatay
  while two neighbouring plants 600 m away went to Osmaniye. Both are İBBS-2 TR63, so
  regional figures are unaffected.
- **Renewables emit nothing and are therefore not modelled.** Climate TRACE's Turkish power
  register lists combustion plants only. It covers 52% of national generation, and the
  missing half is almost exactly the renewable half — which is why a grid factor computed
  from it alone runs 57% high. The correct denominator comes from Ember instead. The
  missing plants themselves are drawn as an optional **fleet context layer** from GEM —
  3,136 operating plants, 58.8 GW — kept in a separate register (`fleet_renewables.rds`)
  rather than merged into `facilities.rds`, so the 300 modelled facilities stay the 300
  modelled facilities.
- **A Climate TRACE figure is not reproducible without its release tag, and the download
  URL does not carry one.** The bulk packages are served from a `/latest` alias. The two
  packages this project uses were downloaded on the same day and arrived at *different*
  releases — `co2` at v5_9_0, `co2e_100yr` at v5_10_0 — and for the identical 151 Turkish
  power stations, v5_10_0 reports **12.4% more generation in 2024** than v5_9_0. Activity
  is a physical quantity in MWh and does not depend on which gas you asked for, so that is
  a version revision. Every panel row therefore carries a `vintage` column naming its own
  release, and any figure quoted from this project should carry it too.
- **The two populations' 2026 is not the same 2026.** `co2` stops after 5 months,
  `co2e_100yr` after 6. The panel counts months from the data rather than assuming twelve
  and stores the count, because a bar labelled "2026" beside another labelled "2026"
  asserts a comparability that does not hold here. 2026 is never annualised by scaling.
- **Ownership is reported as counts of facilities, never as a share of emissions.**
  Climate TRACE's `overall_share_percent` is a product along an ownership path, not a
  partition of the asset: summed per facility it reaches 342%, because several paths can
  reach the same ultimate parent. So "X% of Turkish industrial carbon is EU-owned" is not
  computable from this data and is not attempted. Each ownership flag also comes in two
  strengths — any chain, and chains whose final link to the plant is quantified. The gap is
  real: facilities with a foreign parent fall from 53 to 32 under that filter, while EU27
  parents fall only from 28 to 26.
- **The policy timeline covers EU CBAM and the Turkish ETS, and nothing else.** All 13
  events in `policies/policy_timeline.json` are dated 2023 or later, so they sit at the
  right-hand edge of the 2000–2026 fleet chart and **do not explain the curve behind
  them** — the Turkish renewable build-out of 2015–2024 happened before any of that
  regulation existed. The Turkish energy policy that would explain it (YEKDEM, the
  capacity mechanism, coal support, the licensing regime) is absent: no open
  machine-readable register of it exists, so each entry has to be read out of a primary
  source and cited. The interface states this above the calendar rather than leaving the
  reader to infer a causal link the file cannot support.
- **The fleet timeline shows cumulative commissioning, not the operating fleet.** Global
  Energy Monitor records a retirement year for 2 of 4,174 Turkish rows, so a plant that
  closed in 2015 stays on the curve for ever. Ember's actual installed capacity is drawn
  beside it: in 2025 this atlas accounts for 92.8 GW of Ember's 123.1 GW. Read that gap
  carefully — it is incomplete coverage *minus* unmodelled retirement, and the two work in
  opposite directions, so it cannot be attributed to either alone.
- **Commissioning years come from a spatial join, and 34% of facilities have none.** GEM
  and Climate TRACE are matched by proximity, which is not identity, so every dated
  facility carries `commissioning_source` with the match distance. Three rules keep the
  join honest: zero-carbon plants are excluded from the pool (Climate TRACE's power
  register is combustion-only, so they can never be the right answer), a facility matches
  only a register that covers its own sector, and a GEM site may be claimed by only one
  facility. Aluminium, refining and oil & gas have no GEM tracker at all and carry `NA`
  rather than a borrowed date. An earlier version of this join had none of those rules and
  dated 25 of 235 facilities from small neighbouring solar farms.
- **The İBBS-2 mapping is transcribed, not fetched** from an authoritative file, and
  carries a verification marker pending a citation to the official TÜİK classification.
- **Two carbon price scenarios lack a citation.** `policies/carbon_price_scenarios.json`
  flags them `citation_required: true`. Until that is resolved the project is in breach of
  its own rule that a regulatory number without a source is unusable.

---

## Licence

- Code: [MIT](LICENSE)
- Documentation and derived data: CC BY 4.0
- Upstream data retains its original licence

## Citation

See [CITATION.cff](CITATION.cff).

## Author

**Selahattin İlhan** · [ORCID 0009-0007-4824-752X](https://orcid.org/0009-0007-4824-752X)

### Disclosure

Code in this repository was written with AI coding assistance (Claude). All scope
decisions, source selections and methodological choices were made by the author, and the
analytical core — the CBAM liability calculation and emissions decomposition — is authored
by the author. `METHODOLOGY.md` is written by the author.
