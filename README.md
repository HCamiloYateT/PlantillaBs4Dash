# PlantillaBs4Dash

Plantilla base para construir aplicaciones **Shiny** con **bs4Dash** y una estructura modular orientada a proyectos de analítica.

## Descripción

Este repositorio contiene una app organizada en capas:

- **Inicialización global** (`APP/global.R`): configuración de entorno, locales, opciones globales, carga de librerías y preparación para la carga dinámica de módulos.
- **Interfaz de usuario** (`APP/ui.R` + `APP/misc/ui/`): definición del layout principal (header, sidebar, body, footer, controlbar y preloader).
- **Lógica de servidor** (`APP/server.R`): reactividad básica de la aplicación.

La versión actual de la plantilla **todavía no incluye módulos de ejemplo** ni el directorio `APP/misc/modules/`. Cuando el proyecto incorpore módulos, ese directorio puede crearse como punto de extensión para que `load_modules()` los cargue desde `APP/global.R`.

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
- Paquetes usados por la app (se cargan en `global.R`), incluyendo `shiny`, `bs4Dash`, `tidyverse`, `shinyWidgets`, `waiter`, entre otros.
- Dependencias internas de la organización: familia de paquetes `racafe*`.

## Descarga selectiva de la subcarpeta `APP`

Si solo necesitas la app y no todo el repositorio, puedes usar **sparse checkout** desde R para descargar únicamente `APP`:

```r
# Descarga selectiva de subcarpeta desde GitHub via sparse checkout
repo_url  <- "https://github.com/HCamiloYateT/PlantillaBs4Dash"
subcarpeta <- "APP"
destino    <- file.path(getwd(), subcarpeta)

# Clonar solo la subcarpeta usando sparse checkout (sin historial completo)
tmp <- tempfile()
system2("git", c(
  "clone",
  "--depth=1",
  "--filter=blob:none",
  "--sparse",
  repo_url,
  tmp
))
system2("git", c("-C", tmp, "sparse-checkout", "set", subcarpeta))

# Copiar la carpeta APP al directorio del proyecto
file.copy(
  from      = file.path(tmp, subcarpeta),
  to        = getwd(),
  recursive = TRUE,
  overwrite = TRUE
)

# Limpiar temporal
unlink(tmp, recursive = TRUE)
message("Listo: carpeta APP disponible en ", destino)
```

## Variables que debes definir en `APP/global.R`

Antes de extender la plantilla, valida que estas variables estén declaradas en `global.R`:

- `verbose`: activa/desactiva mensajes informativos de arranque y carga de módulos.
- `debug`: activa opciones de depuración (por ejemplo, `shiny.fullstacktrace`).
- `tit_app`: título principal mostrado por la aplicación.
- `badge_estado`: estado visible de la app (`prototipo`, `pruebas`, `staging`, `produccion`, `demo`, `mantenimiento`, `ninguno`).

Adicionalmente, se configuran valores globales del entorno al inicio de `global.R` que debes revisar según tu ambiente:

- `TZ` (zona horaria), definido por `Sys.setenv(TZ = "America/Bogota")`.
- `LANG` (idioma de mensajes del sistema), definido por `Sys.setenv(LANG = "es_CO.UTF-8")`.
- `LC_TIME`, `LC_MONETARY` y `LC_MESSAGES` (localización regional) mediante `Sys.setlocale(...)`; en producción el servidor debe tener instalado/generado un locale en español (`es_CO.UTF-8` o `es_ES.UTF-8`). Si el sistema no lo permite, usa `format_fecha_es()` para fechas visibles en la UI, porque no depende del locale del servidor.

## Ejecución local

Desde la raíz del repositorio, en una sesión de R:

```r
renv::restore()
shiny::runApp("APP")
```

El archivo `.Rprofile` activa `renv` automáticamente cuando el proyecto ya
contiene `renv/activate.R`. En un clon que todavía no lo contenga, inicializa el
entorno una sola vez con `renv::init()` y confirma `renv.lock` y los archivos de
infraestructura de `renv` (pero no `renv/library/`).

## Calidad de código

La configuración de `lintr` admite líneas de hasta 120 caracteres y excluye el
archivo de activación generado por `renv`. Para revisar el código de la app:

```r
install.packages("lintr") # solo si no forma parte aún del entorno
lintr::lint_dir("APP")
```

Consulta [CONTRIBUTING.md](CONTRIBUTING.md) antes de proponer cambios.

## Solución de problemas

### No se puede activar un locale en español

La app intenta usar `es_CO.UTF-8` y luego variantes de Colombia y España. En
Linux comprueba los locales disponibles con `locale -a`. Si faltan, instala el
paquete de locales de la distribución y genera `es_CO.UTF-8` o `es_ES.UTF-8`;
por ejemplo, en Debian/Ubuntu un administrador puede ejecutar
`sudo locale-gen es_CO.UTF-8`. Reinicia R después de cambiar la configuración.

En Windows usa un locale de la familia `Spanish_Colombia.1252` o
`Spanish_Spain.1252`. Si el servidor no permite instalar locales, conserva el
locale disponible y usa `format_fecha_es()` para el texto visible: esta función
no depende de la configuración regional del sistema. No cambies `LC_NUMERIC`,
porque R y varios paquetes esperan el punto como separador decimal.

### No se pueden instalar los paquetes privados `racafe*`

Los paquetes `racafeCore`, `racafeBD`, `racafeDrive`, `racafeGraph`,
`racafeShiny` y `racafeForecast` requieren acceso al registro o repositorio
privado de la organización. Comprueba primero que tu cuenta tenga permisos y
que Git pueda acceder al origen privado desde la misma máquina.

- Guarda tokens en el gestor de credenciales del sistema o en `~/.Renviron`,
  nunca en el repositorio. `.Renviron` está ignorado deliberadamente.
- Si se usa GitHub, configura `GITHUB_PAT` con un token de alcance mínimo y
  reinicia R. Para un repositorio Git privado también puedes usar una llave SSH
  registrada y probarla antes de restaurar dependencias.
- Configura el repositorio corporativo mediante `options(repos = ...)` en tu
  perfil de usuario si los paquetes se distribuyen desde un repositorio R
  interno. No reemplaces el repositorio corporativo por CRAN si las dependencias
  privadas solo están disponibles allí.
- Tras autenticarte, ejecuta `renv::restore()` de nuevo. Si el lockfile apunta a
  una revisión que ya no existe, solicita al mantenedor que restaure esa versión
  o actualice y confirme `renv.lock`; no sustituyas silenciosamente la versión.

Para diagnosticar sin divulgar secretos, incluye en el reporte la salida de
`R.version.string`, `renv::diagnostics()` y el nombre del paquete que falla,
eliminando tokens, URLs con credenciales y nombres de usuario.

## Estado

Repositorio en estado de **plantilla/prototipo**, con componentes base para extender fuentes de datos y visualizaciones. La infraestructura de carga está preparada para incorporar módulos, pero esta versión aún no trae módulos funcionales de ejemplo.

## Instrucciones para subir un proyecto a GitHub

El siguiente script de R inicializa Git en la raíz del proyecto, configura el remoto `origin`, descarga un `.gitignore` compartido, crea el primer commit y publica la rama `main` en GitHub.

> Antes de ejecutarlo, ajusta `repo_github` con la URL SSH del repositorio destino y confirma que tu llave SSH (`~/.ssh/id_ed25519`) tenga acceso al repositorio.

```r
ruta_proyecto <- here::here()
repo_github   <- "git@github.com:HCamiloYateT/OCR.git"
gitignore_url <- "https://raw.githubusercontent.com/HCamiloYateT/Compartido/refs/heads/main/git/.gitignore"

setwd(ruta_proyecto)

top_git <- tryCatch(system2("git", c("rev-parse","--show-toplevel"), stdout = TRUE, stderr = FALSE), error = function(e) NULL)

if(!is.null(top_git) && normalizePath(top_git) != normalizePath(getwd())){
  unlink(file.path(top_git, ".git"), recursive = TRUE, force = TRUE)
  message("Git incorrecto eliminado de: ", top_git)
}

system2("git", "init")

Sys.chmod("~/.ssh", "700")
if(file.exists("~/.ssh/id_ed25519")) Sys.chmod("~/.ssh/id_ed25519", "600")

remotos <- tryCatch(system2("git", "remote", stdout = TRUE), error = function(e) character(0))

if("origin" %in% remotos){
  system2("git", c("remote","set-url","origin", repo_github))
} else {
  system2("git", c("remote","add","origin", repo_github))
}

download.file(gitignore_url, ".gitignore", quiet = TRUE)

system2("git", "status")
system2("git", c("add","."))
system2("git", c("commit","-m","Primer commit"))
system2("git", c("branch","-M","main"))
system2("git", c("push","-u","origin","main"))

message("Proyecto sincronizado con GitHub")
```
