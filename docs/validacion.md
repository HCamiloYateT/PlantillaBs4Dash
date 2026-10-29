# Validación de las dos plantillas

Validado el 29 de octubre de 2026 en R 4.5.0, Shiny 1.10.0, bs4Dash 2.3.5,
racafeCore 1.0.0, racafeShiny 1.2.0 y racafeForms 0.5.0, con Chromium 154 y
Playwright 1.62.1. Las aplicaciones se ejecutaron completas mediante shiny::runApp,
con paquetes instalados realmente, sin reemplazos ni arneses de funciones.

## Resultados

- 56 comprobaciones de R: parse de todos los scripts de ambas aplicaciones y misc,
  grupos NULL/vacíos/múltiples/NA, números positivos/negativos y descarga selectiva.
- 60 comprobaciones de navegador entre las dos apps: inicio con logo local,
  preloader único y cerrado, navbar, identidad, badge, navegación del dashboard,
  single sin sidebar/toggle/espacio lateral, formularios y cálculo numérico, actualización
  funcional, objetos waiter, guardado/error con cierre seguro, notificaciones,
  interacción por teclado, escritorio/móvil, recursos deduplicados y sin peticiones externas.
- Capturas de escritorio y móvil generadas por el test de navegador en docs/.
- Arranque consecutivo de APP y APP_SINGLE en una misma sesión de R, sin conflictos de recursos.
- No se registraron errores de carga, módulos fallidos, avisos de separadores ni errores
  JavaScript en las ejecuciones finales. La búsqueda de locales puede informar un fallback
  de fechas cuando el sistema carece de locales españoles; no cambia LC_NUMERIC.

El navegador verificó las presentaciones 1,234; 1,234.56; 1,234,567.89;
-1,234,567.89 y 0.125. Al editar el input a -1,234.56, el resultado fue -2,469.12:
el valor reactivo siguió siendo numérico y participó en un cálculo real.

Las pruebas R de descarga usan un repositorio Git local controlado: ambas carpetas,
archivos ocultos, destino con espacios, ausencia de .git/otros archivos, rechazo de
sobrescritura y limpieza después de error. También se descargaron ambas carpetas desde la rama publicada en GitHub y se analizaron
sus scripts y recursos locales. Los comandos de sparse checkout son los
mismos usados por el procedimiento manual documentado. El filtro de blobs depende del
soporte del servidor; la copia final siempre contiene solo la carpeta elegida.

## Procedencia de recursos

Cada app incluye una copia de los recursos públicos necesarios de Compartido 1.1.0,
revisión `483c6909689d532df47f706dcbd8cc9cc7d3bd98`: ASSETS_VERSION, los dos CSS,
Scripts/scripts, clipboard.js, forms.js y logo.png/logo2.png. Se cargan con la API pública
racafeSharedAssets; no se reimplementan sus componentes. Las dos copias son idénticas. Cada app usa un prefijo de recursos propio para poder
ejecutarlas consecutivamente en una misma sesión de R sin conflictos de rutas.
El preloader usa exclusivamente el logo empaquetado por racafeShiny.

Solo se modificó PlantillaBs4Dash. No se modificaron Libreria-Racafe ni Compartido.
Los paquetes se instalaron desde sus fuentes existentes para la validación del entorno;
se resolvieron las dependencias ausentes antes de ejecutar las aplicaciones.

## Reproducción

Desde la raíz del repositorio:

```sh
Rscript tests/test-templates.R
```

Inicie cada app en una terminal:

```r
shiny::runApp("APP", port = 3922)
shiny::runApp("APP_SINGLE", port = 3923)
```

En una tercera terminal, con Playwright y Chromium disponibles:

```sh
DASHBOARD_URL=http://127.0.0.1:3922 SINGLE_URL=http://127.0.0.1:3923 \
  node tests/browser/test-templates.cjs
```

En contenedores use NODE_PATH, CHROMIUM_PATH y CI=1 según su instalación.
Las pruebas no envían correos ni modifican servicios externos o bases de datos.
Los nombres, grupos y resultados de las capturas son datos de demostración.
