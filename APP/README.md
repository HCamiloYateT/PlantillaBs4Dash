# APP - Plantilla Shiny con bs4Dash

Directorio principal de la aplicación Shiny.

## Componentes

- `global.R`: inicializa entorno, paquetes, datos y scripts auxiliares.
- `ui.R`: arma la interfaz principal con `bs4DashPage`.
- `server.R`: define la lógica reactiva de servidor.
- `misc/ui/`: fragmentos de UI (header, sidebar, body, footer, controlbar, preloader).
- `misc/modules/`: módulos Shiny reutilizables.
- `misc/functions.R`, `misc/parametros.R`, `misc/values.R`: soporte funcional y configuración.

## Flujo de carga

1. Se ejecuta `global.R`.
2. `global.R` carga módulos y scripts de `misc/`.
3. `ui.R` construye el dashboard.
4. `server.R` conecta reactividad y outputs.

## Ejecutar solo esta carpeta

```r
shiny::runApp(".")
```

> Ejecutar desde `APP/` como directorio de trabajo.
