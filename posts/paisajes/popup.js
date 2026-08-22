// `onRender()` inyecta JavaScript que se ejecuta en el navegador cuando el
// mapa ya está creado (esto funciona aunque el post sea estático, sin Shiny).
// mapgl guarda los marcadores en `window.maplibreglMarkers` y expone el mapa
// como `el.map`. El problema a resolver: por defecto el popup se abre al instante
// (anclado hacia donde haya espacio, p. ej. hacia abajo), y luego al centrar el
// mapa MapLibre recalcula el anclaje y lo voltea hacia arriba, generando un salto
// feo. Para evitarlo, tomamos control del momento de apertura:
//  1) Desvinculamos el popup del marcador (`setPopup()`), así el clic ya no lo
//     abre automáticamente.
//  2) En nuestro propio clic, primero reposicionamos el mapa con
//     `easeTo(offset: [0, 180])`, dejando el marcador bajo el centro.
//  3) Recién cuando el mapa termina de moverse (`moveend`) abrimos el popup, por
//     lo que el anclaje se calcula una sola vez en la posición final (sin salto).
//  4) La imagen del popup se hace clickeable a pantalla completa con GLightbox
//     (el lightbox de Quarto solo procesa imágenes presentes al cargar la página,
//     pero estos popups se crean dinámicamente).
(
  function(el, x) {
    function setup() {
      var map = el.map;
      var markers = window.maplibreglMarkers || [];
      // Lista compartida de todos los popups, para poder cerrar los demás.
      var popups = [];
      markers.forEach(function(marker) {
        if (marker._mapglBound) return;
        var popup = marker.getPopup();
        if (!popup) return;
        marker._mapglBound = true;
        popups.push(popup);

        // Desvincular el popup para controlar cuándo se abre.
        marker.setPopup();
        popup.setLngLat(marker.getLngLat());

        // Lightbox cada vez que el popup se abre.
        popup.on('open', function() {
          var pel = popup.getElement();
          if (!pel) return;
          var img = pel.querySelector('img.lightbox');
          if (img && !img._lightboxBound) {
            img._lightboxBound = true;
            img.style.cursor = 'zoom-in';
            img.addEventListener('click', function() {
              if (window.GLightbox) {
                GLightbox({ elements: [{ href: img.src, type: 'image' }] }).open();
              }
            });
          }
        });

        var mel = marker.getElement();
        mel.style.cursor = 'pointer';
        mel.addEventListener('click', function(e) {
          e.stopPropagation();
          // Segundo clic sobre el mismo marcador: cerrar.
          if (popup.isOpen()) { popup.remove(); return; }
          // Cerrar cualquier otro popup que esté abierto.
          popups.forEach(function(p) { if (p !== popup && p.isOpen()) p.remove(); });
          // Primero reposicionar el mapa, luego abrir el popup al terminar.
          map.easeTo({ center: marker.getLngLat(), offset: [0, 180], duration: 500 });
          var opened = false;
          function openPopup() {
            if (opened || popup.isOpen()) return;
            opened = true;
            popup.addTo(map);
          }
          map.once('moveend', openPopup);
          // Respaldo por si el mapa ya estaba centrado y no emite 'moveend'.
          setTimeout(openPopup, 700);
        });
      });
    }
    if (el.map && el.map.loaded()) { setup(); }
    else if (el.map) { el.map.on('load', setup); }
  }
)
    
    