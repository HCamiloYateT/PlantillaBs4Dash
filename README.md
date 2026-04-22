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

## Ejecución local

Desde la raíz del repositorio, en una sesión de R:

```r
shiny::runApp("APP")
```

## Estado

Repositorio en estado de **plantilla/prototipo**, con componentes base para extender módulos, fuentes de datos y visualizaciones.
