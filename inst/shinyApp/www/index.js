

let checkExist = setInterval(() => {
   if ($("#Vizumap-transparency").length) {
     var map = $('#Vizumap-map').data('leaflet-map');
	
	// Disable dragging when user's cursor enters the element
    $(".info.legend.leaflet-control").on('mouseover', function () {
      map.dragging.disable();
    });

    // Re-enable dragging when user's cursor leaves the element
    $(".info.legend.leaflet-control").on('mouseout', function () {
        map.dragging.enable();
	});
	
	clearInterval(checkExist);
   }
}, 100); // check every 100ms


let features = L.featureGroup()

//Scale a numeric array between 0 and 1
function scale(array) {
    return array.map((val) => {
        return (val - Math.min(...array)) / ( Math.max(...array) - Math.min(...array));
    })
}

//Return an array of numberOfItems length of a colour gradient between colour1 and colour2
function colourScale(colour1, colour2, numberOfItems) {
    var rainbow = new Rainbow(); 
    rainbow.setNumberRange(1, numberOfItems);
    rainbow.setSpectrum(colour1, colour2);
    var s = '';
    for (var i = 0; i <= numberOfItems; i++) {
        var hexColour = rainbow.colourAt(i);
        s += '#' + hexColour + ', ';
    }

    return s.split(",").reverse();
}

function clearMap(m) {
    for(i in m._layers) {
        if(m._layers[i]._path != undefined) {
            try {
                m.removeLayer(m._layers[i]);
            }
            catch(e) {
                console.log("problem with " + e + m._layers[i]);
            }
        }
    }
}

Shiny.addCustomMessageHandler("init", (nothing) =>	{
	
	
});

Shiny.addCustomMessageHandler("bivariate", (data) => {
 
    var map = $('#Vizumap-map').data('leaflet-map');
        
    map.removeLayer(features);
    features = L.featureGroup();

    map.flyToBounds(features.getBounds());
})


Shiny.addCustomMessageHandler("glyph", (data) => {

    var map = $('#Vizumap-map').data('leaflet-map');
        
    map.removeLayer(features)
    features = L.featureGroup()

    let rotation = scale(data.dataset[data.error])
    rotation = rotation.map(err => err * 180)
  
    let gradient = colourScale(data.palette[0], data.palette[1], data.dataset[data.estimate].length);
    let estimate = scale(data.dataset[data.estimate])  

    data.centroids.coords.forEach((coord, index) => {
       let marker = L.marker(
            [coord[1], coord[0]], 
            { 
                icon: L.IconMaterial.icon({
                    icon: "",
                    scale: 0.5,
                    markerColor: gradient[Math.floor(estimate[index] * estimate.length) + 1],
                    fillOpacity: $("#Vizumap-transparency").val(),
                    outlineColor: 'black',
                    outlineWidth: 1,
                    }), 
                rotationAngle: rotation[index]
            }
        )
        
        marker.bindPopup(`
            <strong>Estimate: </strong> ${data.dataset[data.estimate][index]} 
            </br>
            <strong>Margin of Error: </strong> ${data.dataset[data.error][index]}
        `);

        features.addLayer(marker)        
    });

    features.addTo(map);
    map.flyToBounds(features.getBounds());

});

Shiny.addCustomMessageHandler("pixel", (data) => {

    var map = $('#Vizumap-map').data('leaflet-map');

    map.removeLayer(features)
    features = L.featureGroup()
   
    let error = scale(data.dataset[data.error]);

    let gradient = colourScale(data.palette[0], data.palette[1], data.dataset[data.estimate].length);
    let estimate = scale(data.dataset[data.estimate]);
    
    data.polygons.features.forEach((x, index) => {
        let pixel = L.geoJSON(x, {fill:`url(${(Math.round(error[index] * 100) / 100)}.gif)`, color: "black", weight: 0.5, fillOpacity:$("#Vizumap-transparency").val()}).addTo(map);
        let poly = L.geoJSON(x, {fillColor: gradient[Math.floor(estimate[index] * estimate.length) + 1], color: "black", weight: 0.5, fillOpacity:$("#Vizumap-transparency").val()}).addTo(map);
        poly.bindPopup(`
            <strong>Estimate: </strong> ${data.dataset[data.estimate][index]} 
            </br>
            <strong>Margin of Error: </strong> ${data.dataset[data.error][index]}
        `);

        features.addLayer(pixel)
        features.addLayer(poly) 
    });

    features.addTo(map)
    map.flyToBounds(features.getBounds());
})

Shiny.addCustomMessageHandler("excedance", (data) => {

    var map = $('#Vizumap-map').data('leaflet-map');

    map.removeLayer(features)
    features = L.featureGroup()

    let gradient = colourScale(data.palette[0], data.palette[1], data.exc[data.exc_name].length);

    console.log(data.exc)
    let exc = scale(data.exc[data.exc_name]);
    
     data.polygons.features.forEach((x, index) => {
        let poly = L.geoJSON(x, {fillColor: gradient[Math.floor(exc[index] * exc.length) + 1], color: "black", weight: 0.5, fillOpacity:$("#Vizumap-transparency").val()})
        
        poly.bindPopup(`
            <strong>Excedance Pr</strong> ${data.exc[data.exc_name][index]} 
        `);

        features.addLayer(poly)
    });

    features.addTo(map)

    map.flyToBounds(features.getBounds());
})

