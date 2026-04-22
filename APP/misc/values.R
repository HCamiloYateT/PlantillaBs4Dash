## Estilos Datatables ----
js <- "
function(cell) {
  var $cell = $(cell);
  var contentText = $cell.text().trim();  // Obtener el texto de la celda
  var maxLength = 100;  // Definir la longitud máxima antes de truncar el texto

  // Verifica si el contenido supera el límite de longitud
  if (contentText.length > maxLength) {
    var shortText = contentText.substring(0, maxLength) + '...';  // Texto truncado

    $cell.contents().wrapAll('<div class=\\\"content\\\"></div>');
    var $content = $cell.find('.content');
    $content.css({
      height: 'auto',  // Mostrar el contenido completo si es necesario
      overflow: 'hidden'
    });

    // Mostrar el texto truncado inicialmente
    $cell.html('<span class=\\\"short-text\\\">' + shortText + '</span><span class=\\\"full-text\\\" style=\\\"display:none;\\\">' + contentText + '</span>');

    $cell.hover(
      function() {
        // Al pasar el mouse sobre la celda, se muestra el texto completo
        $cell.find('.short-text').hide();
        $cell.find('.full-text').show();
      },
      function() {
        // Al quitar el mouse, se vuelve a mostrar el texto truncado
        $cell.find('.short-text').show();
        $cell.find('.full-text').hide();
      }
    );
  }
}
"
lang <- list(url = '//cdn.datatables.net/plug-ins/1.13.1/i18n/es-ES.json')

## Márgenes Plotly ----
m <- list(l = 50,r = 50,b = 5,t = 20)