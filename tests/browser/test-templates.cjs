const assert = require("node:assert/strict");
const { chromium } = require("playwright");
(async () => {
  const browser = await chromium.launch({ executablePath: process.env.CHROMIUM_PATH,
    args: process.env.CI ? ["--no-sandbox"] : [] });
  let checks = 0;
  const check = (value, message) => { assert.ok(value, message); checks++; };
  try {
    for (const [kind, url] of Object.entries({ dashboard: process.env.DASHBOARD_URL || "http://127.0.0.1:3922",
      single: process.env.SINGLE_URL || "http://127.0.0.1:3923" })) {
      const page = await browser.newPage({ viewport: { width: 1440, height: 1100 } });
      const errors = [], external = [];
      page.on("pageerror", error => errors.push(error.message));
      await page.route("**/*", route => {
        if (new URL(route.request().url()).origin !== new URL(url).origin) {
          external.push(route.request().url()); route.abort();
        } else route.continue();
      });
      await page.addInitScript(() => {
        window.loaderStates = [];
        new MutationObserver(() => {
          const host = document.querySelector(".rsh-loader-host:not([hidden])");
          if (host) window.loaderStates.push(host.innerText);
        }).observe(document, { childList: true, subtree: true, attributes: true, attributeFilter: ["hidden"] });
      });
      await page.goto(url);
      const initial = page.locator(".waiter-overlay .rsh-loader-card");
      await initial.waitFor({ state: "visible" });
      check(await initial.count() === 1, `${kind}: preloader único`);
      await initial.locator("img").evaluate(image => image.decode());
      check(await initial.locator("img").evaluate(image => image.naturalWidth > 0), `${kind}: logo inicial local`);
      await page.locator("#demo-importe").waitFor();
      await page.locator(".waiter-overlay").waitFor({ state: "detached" });
      check((await page.locator("#user").innerText()) === "HCYATE", `${kind}: usuario por defecto`);
      check((await page.locator("#group").innerText()) === "ANALÍTICA", `${kind}: grupo por defecto`);
      check(await page.locator(".brand-link img, .navbar-brand img").first().evaluate(image => image.complete && image.naturalWidth > 0),
        `${kind}: logo de navbar`);
      check((await page.locator(".main-footer").innerText()).includes("DEMO"), `${kind}: badge de entorno`);
      check((await page.locator("#demo-importe").inputValue()) === "1,234.56", `${kind}: formato de input`);
      for (const number of ["1,234", "1,234.56", "1,234,567.89", "-1,234,567.89", "0.125"]) {
        check((await page.locator("#demo-numeros").innerText()).includes(number), `${kind}: formato ${number}`);
      }
      if (kind === "single") {
        check(await page.locator(".main-sidebar:visible").count() === 0, "Single: sin sidebar visible");
        check(await page.locator('[data-widget="pushmenu"]').count() === 0, "Single: sin toggle lateral");
        const bounds = await page.locator(".content-wrapper").boundingBox();
        check(bounds.x < 5 && bounds.width > 1400, "Single: contenido sin espacio lateral reservado");
      } else {
        check(await page.locator(".main-sidebar:visible").count() === 1, "Dashboard: sidebar visible");
        await page.locator(".nav-sidebar > .nav-item").nth(1).locator("a").first().click();
        await page.locator('a[data-value="SerieTiempo"]').click();
        await page.getByRole("heading", { name: "Serie de tiempo", exact: true }).waitFor();
        check(true, "Dashboard: navegación a Histórico");
        await page.locator(".nav-sidebar > .nav-item").nth(0).locator("a").first().click();
        await page.locator('a[data-value="TablaGeneral"]').click();
        await page.locator("#demo-importe").waitFor({ state: "visible" });
      }
      await page.locator("#BTN_Actualizar").click();
      await page.locator("#ultima_actualizacion").filter({ hasText: "1" }).waitFor();
      await page.waitForFunction(() => !document.querySelector(".rsh-loader-host:not([hidden])"));
      check(await page.evaluate(() => window.loaderStates.some(text => text.includes("Actualizando"))),
        `${kind}: actualización con loader inmediato`);
      await page.locator(".rf-notification:visible").filter({ hasText: "Información actualizada" }).waitFor();
      check(true, `${kind}: aviso corporativo visible al terminar`);
      await page.locator("#demo-consultar").focus(); await page.locator("#demo-consultar").press("Enter");
      await page.locator("#demo-resultado").filter({ hasText: "2,469.12" }).waitFor();
      check(true, `${kind}: cálculo numérico y teclado`);
      await page.locator("#demo-guardar").click();
      await page.locator("#demo-panel .waiter-overlay").waitFor({ state: "visible" });
      check(await page.locator(".waiter-overlay:visible").count() === 1, `${kind}: waiter programático sin duplicados`);
      await page.locator("#demo-panel .waiter-overlay").waitFor({ state: "detached" });
      await page.locator("#demo-resultado").filter({ hasText: "Guardado en memoria" }).waitFor();
      check(true, `${kind}: guardado local con cierre seguro`);
      await page.locator("#demo-fallar").click();
      await page.locator("#demo-resultado").filter({ hasText: "Error controlado" }).waitFor();
      await page.locator(".waiter-overlay").waitFor({ state: "detached" });
      check(true, `${kind}: error cierra todos los loaders`);
      const number = page.locator("#demo-importe");
      await number.click(); await number.press("ControlOrMeta+a"); await number.pressSequentially("-1234.56");
      await number.press("Tab"); await page.locator("#demo-consultar").click();
      await page.locator("#demo-resultado").filter({ hasText: "-2,469.12" }).waitFor();
      check(true, `${kind}: valores negativos siguen siendo numéricos`);
      await page.waitForFunction(() => !document.querySelector(".rsh-loader-host:not([hidden])"));
      await page.evaluate(() => document.querySelectorAll(".rf-notification .shiny-notification-close").forEach(el => el.click()));
      await page.waitForFunction(() => !document.querySelector(".rf-notification"));
      await page.waitForTimeout(400);
      if (kind === "dashboard") {
        check((await page.locator(".content-wrapper").boundingBox()).x >= 249, "Dashboard: contenido junto al sidebar");
      }
      await page.screenshot({ path: `docs/${kind}-desktop.png`, animations: "disabled" });
      await page.setViewportSize({ width: 390, height: 844 });
      await page.waitForTimeout(200);
      check(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth + 1), `${kind}: sin overflow móvil`);
      if (kind === "single") {
        check(await page.locator('[data-widget="pushmenu"]').count() === 0, "Single móvil: sin toggle lateral");
        const box = await page.locator(".content-wrapper").boundingBox();
        check(box.x < 5, "Single móvil: sin espacio lateral");
      }
      check(await page.locator(".template-date:visible").count() === 0, `${kind}: navbar compacto en móvil`);
      await page.evaluate(() => scrollTo(0, 0));
      await page.waitForTimeout(200);
      await page.screenshot({ path: `docs/${kind}-mobile.png`, animations: "disabled" });
      check(await page.locator('script[src*="loaders.js"]').count() === 1, `${kind}: loaders JS una vez`);
      check(await page.locator('link[href*="loaders.css"]').count() === 1, `${kind}: loaders CSS una vez`);
      check(await page.locator('script[src*="waiter.js"]').count() === 1, `${kind}: waiter una vez`);
      check(external.length === 0, `${kind}: sin recursos externos: ${external.join(", ")}`);
      check(errors.length === 0, errors.join("\n"));
      await page.close();
    }
    console.log(`${checks} comprobaciones de ambas plantillas superadas`);
  } finally { await browser.close(); }
})().catch(error => { console.error(error); process.exitCode = 1; });
