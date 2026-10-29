# Contribuir a PlantillaBs4Dash

Trabaje en una rama desde main. Mantenga `APP` y `APP_SINGLE` ejecutables e independientes:
cada una debe incluir los recursos y módulos que necesita, sin rutas hacia su hermana.

## Preparar el entorno

Instale las dependencias documentadas en README y ejecute cada app con `shiny::runApp()`.
Si su proyecto usa renv, active o inicialice su entorno y versione su propio lockfile;
no confirme bibliotecas locales, credenciales, sesiones ni datos privados.

## Cambios

- Respete R/Shiny modular, namespaces explícitos y líneas de hasta 120 caracteres.
- Use componentes públicos de racafeShiny/racafeForms; no copie loaders, bindings,
  notificaciones ni formateadores numéricos del paquete dentro de las apps.
- Registre únicamente triggers con acción de servidor. Cierre loaders explícitos ante
  éxito y error; la animación no convierte procesos síncronos en tareas paralelas.
- Mantenga `output$user`, `output$group`, `BTN_Actualizar` y la carga `load_modules()`.
- `session$groups` sirve para presentación. No lo utilice como autorización implícita.
- Formatee solo los outputs: coma de miles y punto decimal; preserve tipos numéricos.
- Si actualiza Compartido, copie la misma revisión de sus recursos públicos en ambas
  aplicaciones, conservando ASSETS_VERSION. No cambie sus componentes internos aquí.
- Conserve `.gitkeep` de los módulos. Añada nuevas funciones de módulo sin ejecutarlas
  globalmente ni registrar observers repetidos dentro de otros observers.

## Comprobaciones

```sh
Rscript tests/test-templates.R
```

Inicie APP y APP_SINGLE en procesos separados y ejecute el test de navegador con las
URLs correspondientes; README documenta las variables y la instalación de Playwright.
Si tiene lintr instalado, puede ejecutar `lintr::lint_dir("APP")` y
`lintr::lint_dir("APP_SINGLE")`. Las comprobaciones obligatorias son parse, arranque,
interacción real, cierre de loaders, números y descarga sin sobrescribir proyectos.

## Pull requests

Explique el problema y el comportamiento final. Incluya resultados de pruebas,
limitaciones verificadas y capturas de escritorio/móvil cuando cambie la UI.
No publique correos, consultas ni operaciones externas durante la validación.
Los archivos de `docs/` contienen capturas de datos simulados, no datos de usuarios.

Las contribuciones se distribuyen bajo la licencia MIT del proyecto; los recursos
corporativos conservan su procedencia documentada en el informe de validación.
