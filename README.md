# PlantillaBs4Dash

Plantilla base para construir aplicaciones **Shiny** con **bs4Dash** y una estructura modular orientada a proyectos de analítica.

## Descripción

Este repositorio contiene una app organizada en capas:

- **Inicialización global** (`APP/global.R`): configuración de entorno, locales, opciones globales, carga de librerías y carga dinámica de módulos.
- **Interfaz de usuario** (`APP/ui.R` + `APP/misc/ui/`): definición del layout principal (header, sidebar, body, footer, controlbar y preloader).
- **Lógica de servidor** (`APP/server.R` + `APP/misc/modules/`): reactividad y módulos funcionales de ejemplo.

La plantilla está pensada como punto de partida para dashboards corporativos con navegación por pestañas, tablas y componentes reutilizables.

## Estructura principal

```text
APP/
├── global.R
├── ui.R
├── server.R
└── misc/
    ├── functions.R
    ├── parametros.R
    ├── values.R
    ├── modules/
    │   ├── ModuloPrueba.R
    │   └── ModuloTablaPais.R
    └── ui/
        ├── header.R
        ├── sidebar.R
        ├── body.R
        ├── footer.R
        ├── controlbar.R
        └── preloader.R
```

## Requisitos

- R (versión reciente recomendada).
- Paquetes usados por la app (se cargan en `global.R`), incluyendo `shiny`, `bs4Dash`, `tidyverse`, `gt`, `plotly`, entre otros.
- Dependencias internas de la organización: familia de paquetes `racafe*`.

## Clonar para desarrollo local

1. Clona el repositorio:

```bash
git clone https://github.com/<organizacion>/PlantillaBs4Dash.git
```

2. Entra a la carpeta del proyecto:

```bash
cd PlantillaBs4Dash
```

3. Abre una sesión de R en la raíz del repositorio y ejecuta la app:

```r
shiny::runApp("APP")
```

> Si trabajas desde RStudio, abre el proyecto/carpeta en la raíz y luego ejecuta `shiny::runApp("APP")`.

## Variables que debes definir en `APP/global.R`

Antes de extender la plantilla, valida que estas variables estén declaradas en `global.R`:

- `verbose`: activa/desactiva mensajes informativos de arranque y carga de módulos.
- `debug`: activa opciones de depuración (por ejemplo, `shiny.fullstacktrace`).
- `tit_app`: título principal mostrado por la aplicación.
- `badge_estado`: estado visible de la app (`prototipo`, `pruebas`, `staging`, `produccion`, `demo`, `mantenimiento`, `ninguno`).

Adicionalmente, se configuran valores globales del entorno al inicio de `global.R` que debes revisar según tu ambiente:

- `TZ` (zona horaria), definido por `Sys.setenv(TZ = "America/Bogota")`.
- `LANG` (idioma de mensajes del sistema), definido por `Sys.setenv(LANG = "es_CO.UTF-8")`.
- `LC_TIME`, `LC_MONETARY` y `LC_MESSAGES` (localización regional) mediante `Sys.setlocale(...)`.

## Ejecución local

Desde la raíz del repositorio, en una sesión de R:

```r
shiny::runApp("APP")
```

## Estado

Repositorio en estado de **plantilla/prototipo**, con componentes base para extender módulos, fuentes de datos y visualizaciones.
