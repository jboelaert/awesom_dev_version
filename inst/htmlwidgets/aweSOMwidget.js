HTMLWidgets.widget({

  name: 'aweSOMwidget',

  type: 'output',

  factory: function(el, width, height) {

    return {
           renderValue: function(data) {

      
    document.getElementById("cell-info").style.textAlign = "center";
    document.getElementById("plot-message").style.textAlign = "center";

    //remove the old graph
    document.getElementById("theWidget").innerHTML = "";

    // remove old plot messages
    document.getElementById("cell-info").innerHTML = "Hover over the plot for information.";
    document.getElementById("plot-message").innerHTML = "-";
    document.getElementById("plot-names").innerHTML = "-";

    // Import common data
    if(data == null ) {return;}

    //console.log(data); //to evaluate data structure
    var plotType= data.plotType;
    var nbRows= data.gridInfo.nbLines;
    var nbColumns= data.gridInfo.nbColumns;
    var topology= data.gridInfo.topology;
    var saveToPng=data.saveToPng;
    var cellSize=data.sizeInfo;
    var w = cellSize*nbColumns;
    var h = cellSize*nbRows;
    var superclass = data.superclass;
    var superclassColor = data.superclassColor;
  	var cellNames = data.cellNames;
  	var cellPop = data.cellPop;

    // Plot-specific data
    if(plotType.localeCompare("Radar")==0) {
      // Radar
      var parts = data.parts;
      var label = data.label;
      var labelColor = data.labelColor;
      if(!Array.isArray(label)) {label= [label];}
      if(!Array.isArray(labelColor)) {labelColor= [labelColor];}
      var radarNormalizedSize = data.radarNormalizedSize;
      var radarRealSize = data.radarRealSize;
      var radarNormalizedValues = data.radarNormalizedValues;
      var radarRealValues = data.radarRealValues;
    } else if(plotType.localeCompare("Camembert")==0) {
      // Pie
      var parts = data.parts;
      var label = data.label;
      var labelColor = data.labelColor;
      if(!Array.isArray(label)) {label= [label];}
      if(!Array.isArray(labelColor)) {labelColor= [labelColor];}
      var pieNormalizedSize = data.pieNormalizedSize;
      var pieRealSize = data.pieRealSize;
      var pieNormalizedValues = data.pieNormalizedValues;
      var pieRealValues = data.pieRealValues;
    } else if(plotType.localeCompare("Barplot")==0) {
      // Barplot
      var nbBatons = data.nbBatons;
      var isHist = data.isHist;
      var isCatBarplot = data.isCatBarplot;
	    var label = data.label;
	    var labelColor = data.labelColor;
      if(!Array.isArray(label)) {label= [label];}
      if(!Array.isArray(labelColor)) {labelColor= [labelColor];}
	    var batonNormalizedValues = data.batonNormalizedValues;
	    var batonRealValues = data.batonRealValues;
    } else if(plotType.localeCompare("Boxplot")==0) {
      // Boxplot
      var nbBox = data.nbBox;
      var label = data.label;
    	var labelColor = data.labelColor;
      if(!Array.isArray(label)) {label= [label];}
      if(!Array.isArray(labelColor)) {labelColor= [labelColor];}
    	var boxPlotNormalizedValues = data.boxPlotNormalizedValues;
    	var boxPlotRealValues = data.boxPlotRealValues;
    	var boxNormalizedExtremesValues= data.boxNormalizedExtremesValues;
    	var boxRealExtremesValues= data.boxRealExtremesValues;
    } else if(plotType.localeCompare("Color")==0) {
      // Color
      var activate = data.activate;
      var colorNormalizedValues = data.colorNormalizedValues;
    	var colorRealValues = data.colorRealValues;
    	var label = data.label;
    } else if(plotType.localeCompare("Star")==0) {
      // Star
      var nbSommet = data.nbSommet;
      var label = data.label;
      if(!Array.isArray(label)) {label= [label];}
    	var starPlotNormalizedValues = data.starPlotNormalizedValues;
    	var starPlotRealValues = data.starPlotRealValues;
    } else if(plotType.localeCompare("Hitmap")==0) {
      // Hitmap
      var hitmapNormalizedValues = data.hitmapNormalizedValues;
      var hitmapRealValues = data.hitmapRealValues;
    } else if(plotType.localeCompare("Line")==0) {
      // Lines
      var nbPoints = data.nbPoints;
      var label = data.label;
      var lineNormalizedValues = data.lineNormalizedValues;
    	var lineRealValues = data.lineRealValues;
    } else if(plotType.localeCompare("Names")==0) {
      // Wordcloud of names
      var nbWord = data.nbWord;
      var wordClouds = data.wordClouds;
    }

    // Plot download handler
    function downloadCanvas(link, filename) {
      var division = document.select(el);
      var svg = division.children[0];
      var img = document.getElementById("fromcanvasPlot");

      if(saveToPng){
        svg.toDataURL("image/png", {
          callback: function(data) {
            link.href = data;
            link.download = filename;
          }
        })
      }
      else {
        svg.toDataURL("image/svg+xml", {
          callback: function(data) {
            link.href = data;
            link.download = filename;
          }
        })
      }
    }
    document.getElementById('downloadLink').addEventListener('click', function() {
      if(saveToPng){
        downloadCanvas(this, 'somplot.png');
      }else{
        downloadCanvas(this, 'somplot.svg');
      }
    }, false);

    // Call grid following topology and type
    if(topology.localeCompare('rectangular')==0){
      commonSquareGrid();
    } else if(topology.localeCompare('hexagonal')==0){
      commonHexGrid();
    }

    // Star function
//    if(plotType.localeCompare("Star")==0) {


      function StarChart(id, d, options) {
        var cfg = {
      	 radius: 5,
      	 w: 600,
      	 h: 600,
      	 factor: 1,
      	 factorLegend: .85,
      	 levels: 3,
      	 maxValue: 0,
      	 radians: 2 * Math.PI,
      	 opacityArea: 0.5,
      	 ToRight: 5,
      	 TranslateX: 80,
      	 TranslateY: 30,
      	 ExtraWidthX: 100,
      	 ExtraWidthY: 100,
      	 color: d3.scale.category10(),
      	 x : 0,
      	 y : 0
      	};

      	if('undefined' !== typeof options){
      	  for(var i in options){
        		if('undefined' !== typeof options[i]){
        		  cfg[i] = options[i];
        		}
      	  }
      	}
      	cfg.maxValue = Math.max(cfg.maxValue, d3.max(d, function(i){return d3.max(i.map(function(o){return o.value;}))}));
      	var allAxis = (d[0].map(function(i, j){return i.axis}));
      	var total = allAxis.length;
      	var radius = cfg.factor*Math.min(cfg.w/2, cfg.h/2);
      	var Format = d3.format('%');
      	var g = d3.select(id).select("svg").append("g").attr("transform", "translate("+cfg.x+","+cfg.y+")");
      	var tooltip;

      	//Circular segments
      	for(var j=0; j<cfg.levels; j++){
      	  var levelFactor = cfg.factor*radius*((j+1)/cfg.levels);
      	  g.selectAll(".levels")
      	   .data(allAxis)
      	   .enter()
      	   .append("line")
      	   .attr("x1", function(d, i){return levelFactor*(1-cfg.factor*Math.sin(i*cfg.radians/total));})
      	   .attr("y1", function(d, i){return levelFactor*(1-cfg.factor*Math.cos(i*cfg.radians/total));})
      	   .attr("x2", function(d, i){return levelFactor*(1-cfg.factor*Math.sin((i+1)*cfg.radians/total));})
      	   .attr("y2", function(d, i){return levelFactor*(1-cfg.factor*Math.cos((i+1)*cfg.radians/total));})
      	   .attr("class", "line")
      	   .style("stroke", "#414141")
      	   .style("stroke-opacity", "0.75")
      	   .style("stroke-width", "0.3px")
      	   .attr("transform", "translate(" + ((cfg.w/2)-levelFactor) + ", " + (cfg.h/2-levelFactor) + ")");
      	}

      	series = 0;
      	var axis = g.selectAll(".axis")
      			.data(allAxis)
      			.enter()
      			.append("g")
      			.attr("class", "axis");
      	axis.append("line")
      		.attr("x1", cfg.w/2)
      		.attr("y1", cfg.h/2)
      		.attr("x2", function(d, i){return cfg.w/2*(1-cfg.factor*Math.sin(i*cfg.radians/total));})
      		.attr("y2", function(d, i){return cfg.h/2*(1-cfg.factor*Math.cos(i*cfg.radians/total));})
      		.attr("class", "line")
      		.style("stroke", "grey")
      		.style("stroke-width", "1px");

      	d.forEach(function(y, x){
      	  dataValues = [];
      	  g.selectAll(".nodes")
        		.data(y, function(j, i){
        		  dataValues.push([
        			cfg.w/2*(1-(parseFloat(Math.max(j.value, 0))/cfg.maxValue)*cfg.factor*Math.sin(i*cfg.radians/total)),
        			cfg.h/2*(1-(parseFloat(Math.max(j.value, 0))/cfg.maxValue)*cfg.factor*Math.cos(i*cfg.radians/total))
        		  ]);
        		});
      	  dataValues.push(dataValues[0]);

      	  g.selectAll(".area")
      					 .data([dataValues])
      					 .enter()
      					 .append("polygon")
      					 .attr("class", "radar-chart-serie"+series)
      					 .style("stroke-width", "1px")
      					 .style("stroke", "#112E45")
      					 .attr("points",function(d) {
      						 var str="";
      						 for(var pti=0;pti<d.length;pti++){
      							 str=str+d[pti][0]+","+d[pti][1]+" ";
      						 }

      						 return str;
      					  })
      					 .style("fill", function(j, i){return "#77ADD9"})
      					 .style("fill-opacity", cfg.opacityArea)
      					 /*.on('mouseover', function (d){
      										z = "polygon."+d3.select(this).attr("class");
      										g.selectAll("polygon")
      										 .transition(200)
      										 .style("fill-opacity", 0.3);
      										g.selectAll(z)
      										 .transition(200)
      										 .style("fill-opacity", 0.2);
      									  })
      					 .on('mouseout', function(){
      										g.selectAll("polygon")
      										 .transition(200)
      										 .style("fill-opacity", cfg.opacityArea);
      					 })*/;
      	  series++;
      	});
      	series=0;

      	d.forEach(function(y, x){
      	  var circles= g.selectAll(".nodes")
      		.data(y).enter()
      		.append("circle")
      		.attr("class", "radar-chart-serie"+series)
      		.attr('r', cfg.radius)
      		.attr("alt", function(j){return Math.max(j.value, 0)})
      		.attr("cx", function(j, i){
      		  dataValues.push([
      			cfg.w/2*(1-(parseFloat(Math.max(j.value, 0))/cfg.maxValue)*cfg.factor*Math.sin(i*cfg.radians/total)),
      			cfg.h/2*(1-(parseFloat(Math.max(j.value, 0))/cfg.maxValue)*cfg.factor*Math.cos(i*cfg.radians/total))
      		]);
      		return cfg.w/2*(1-(Math.max(j.value, 0)/cfg.maxValue)*cfg.factor*Math.sin(i*cfg.radians/total));
      		})
      		.attr("cy", function(j, i){
      		  return cfg.h/2*(1-(Math.max(j.value, 0)/cfg.maxValue)*cfg.factor*Math.cos(i*cfg.radians/total));
      		})
      		.attr("data-id", function(j){return j.axis})
      		.style("fill", "#112E45").style("fill-opacity", .9)
      		//.on('mouseover', function (d){
      		;
      		circles.append("path").attr("class", function(j, i) {return "tavu"+i;});
      		circles.on('mouseover', function (j, i){
      					newX =  parseFloat(d3.select(this).attr('cx')) - 10;
      					newY =  parseFloat(d3.select(this).attr('cy')) - 5;

      					// Highlight
      					//d3.select(this).style("fill", "#C9E5FC");
      					g.selectAll("path.tavu"+i).style("fill", "#C9E5FC");
      					//g.selectAll("circle.radar-chart-serie"+series).style("fill", "#C9E5FC");

      					d3.select('#plot-message').text(function () {
      						var ch = j.axis+': ' + j.realValue;
      						return ch;
      					});

      					z = "polygon."+d3.select(this).attr("class");
      					g.selectAll("polygon")
      						.transition(200)
      						.style("fill-opacity", 0.3);
      					g.selectAll(z)
      						.transition(200)
      						.style("fill-opacity", 0.2);
      				  })
      		.on('mouseout', function(j, i){
      					//d3.select(this).style("fill", "#112E45");
      					g.selectAll("path.tavu"+i).style("fill", "#112E45");

      					g.selectAll("polygon")
      						.transition(200)
      						.style("fill-opacity", cfg.opacityArea);
      				  })
      		.append("title");

      	  series++;
      	});
      	//Tooltip
      	tooltip = g.append('text')
      			   .style('opacity', 0)
      			   .style('font-family', 'sans-serif')
      			   .style('font-size', '13px');
      }
    //}



    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    // Square grid function
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    function commonSquareGrid(){
      var svg = d3.select(el).append('svg')
      .attr("style"," display:block; margin:auto; margin-top:30px")
      .attr({width: w, height: h});

      if(plotType.localeCompare("Color")==0) {
        ////////////////////////////////////////////////////////////////////////
        // Square color plot
        ////////////////////////////////////////////////////////////////////////
      	_.times(nbRows, function(n) {
    			var rows = svg.selectAll('rect' + ' .row-' + (n + 1))
    				.data(d3.range(nbColumns))
    				.enter().append("g");

          rows.append('rect')
    			.attr({
    				class: function(d, i) {
    					return 'Square row-' + (n + 1) + ' ' + 'col-' + (i + 1);
    				},
    				id: function(d, i) {
    					return 's-' + (n + 1) + (i + 1);
    				},
    				width: cellSize,
    				height: cellSize,
    				x: function(d, i) {
    					return i * cellSize;
    				},
    				y: n * cellSize,
    				fill: function(d, i) {
              return colorNormalizedValues[(n*nbColumns)+i];
    				},
    				stroke: '#FDBB30'
    			});
      		if(activate){ // Superclass numbers on cells for Color plot
      			rows.append("text")
      				.attr("x", function(d, i) { return i * cellSize + cellSize*45/100; })
      				.attr("y", n * cellSize + cellSize*50/100)
      				.text(function(d, i) { return superclass[n*nbColumns+i]; })
      				.attr("font-family", "sans-serif")
      				.attr("font-size", cellSize*20/100)
      				.attr("fill", "#112E45");
      		}
      		rows.on('mouseover', function (d, i) {
      			var el = d3.select(this)
      				.transition()
      				.duration(10)
      				.style("fill-opacity", 0.8);
      			d3.select('#cell-info').text(function () {
      				return 'Cell ' + (1 + (n*nbColumns)+i) + ', Superclass ' +
      				  superclass[(n*nbColumns)+i] + ', N= ' + cellPop[(n*nbColumns)+i];
      				});
      			d3.select('#plot-message').text(function () {
      				return label + ': ' + colorRealValues[(n*nbColumns)+i];
      				});
      			d3.select('#plot-names').text(function () { //for the box below
      				return cellNames[(n*nbColumns)+i];
      				});
        		});
        	rows.on('mouseout', function (d, i) {
        			var el = d3.select(this)
        				.transition()
        				.duration(1000)
        				.style("fill-opacity", 1);
        		});
      			d3.select('#plot-message').text(function () {
      				return '-';
      			});
        	});
      } else if(plotType.localeCompare("Hitmap")==0) {
        //////////////////////////////////////////////////////////////////////
        // Square Hitmap
        //////////////////////////////////////////////////////////////////////
      	_.times(nbRows, function(n) {
    			var rows = svg.selectAll('rect' + ' .row-' + (n + 1))
    				.data(d3.range(nbColumns))
    				.enter().append('rect')
    				.attr({
    					class: function(d, i) {
    						return 'square row-' + (n + 1) + ' ' + 'col-' + (i + 1);
    					},
    					id: function(d, i) {
    						return 's-' + (n + 1) + (i + 1);
    					},
    					width: cellSize,
    					height: cellSize,
    					x: function(d, i) {
    						return i * cellSize;
    					},
    					y: n * cellSize,
    					fill: function(d, i) {
    						var indice = superclass[(n*nbColumns)+i];
    						return superclassColor[indice-1];
    					},
    					stroke: '#FDBB30'
    				});
    			var inside = svg.selectAll('rect' + ' .row-' + (n + 1))
    				.data(d3.range(nbColumns))
    				.enter().append('rect')
    				.attr("class", "squareIn")
    				.attr({
    				  class: function(d, i) {
    					return 'row-' + (n + 1) + ' ' + 'col-' + (i + 1);
    				  },
    				  id: function(d, i) {
    					return 's-' + (n + 1) + (i + 1);
    				  },
    				  width: function(d, i) {
    					return hitmapNormalizedValues[(n*nbColumns)+i]*cellSize*0.97;
    				  },
    				  height: function(d, i) {
    					return hitmapNormalizedValues[(n*nbColumns)+i]*cellSize;
    				  },
    				  x: function(d, i) {
    					return (i * cellSize + cellSize/2 - (hitmapNormalizedValues[(n*nbColumns)+i]*cellSize)*0.97/2);
    				  },
    				  y: function(d, i) {
    					return (n * cellSize + cellSize/2 - (hitmapNormalizedValues[(n*nbColumns)+i]*cellSize)*0.97/2);
    				  },
    				  fill: '#112E45',
    				  });

      		rows.on('mouseover', function (d, i) {
      			var el = d3.select(this)
      				.transition()
      				.duration(10)
      				.style("fill-opacity", 0.8);
      			d3.select('#cell-info').text(function () {
      				return 'Cell ' + (1 + (n*nbColumns)+i) + ', Superclass ' +
      				  superclass[(n*nbColumns)+i] + ', N= ' + cellPop[(n*nbColumns)+i];
      				});
      			d3.select('#plot-names').text(function () {
      				return cellNames[(n*nbColumns)+i];
      				});
        		});

    			rows.on('mouseout', function (d, i) {
    				var el = d3.select(this)
    					.transition()
    					.duration(1000)
    					.style("fill-opacity", 1);
    			});
    		});

      } else if (plotType.localeCompare("Line")==0) {
        //////////////////////////////////////////////////////////////////////
        // Square Lines
        //////////////////////////////////////////////////////////////////////
        _.times(nbRows, function(n) {
    			var rows = svg.selectAll('rect' + ' .row-' + (n + 1))
    				.data(d3.range(nbColumns))
    				.enter().append('rect')
    				.attr({
    					class: function(d, i) {
    						return 'square row-' + (n + 1) + ' ' + 'col-' + (i + 1);
    					},
    					id: function(d, i) {
    						return 's-' + (n + 1) + (i + 1);
    					},
    					width: cellSize,
    					height: cellSize,
    					x: function(d, i) {
    						return i * cellSize;
    					},
    					y: n * cellSize,
    					fill: function(d, i) {
    						var indice = superclass[(n*nbColumns)+i];
    						return superclassColor[indice-1];
    					},
    					stroke: '#FDBB30'
    				});

    			var points = svg.selectAll('rect' + ' .row-' + (n + 1))
    				.data(d3.range(nbColumns))
    				.enter()
    				.append("path")
    				.attr("class", "ligne")
    				.attr("d", function(d, i) {
    				  if (cellPop[(n*nbColumns)+i] == 0) return null;
    					innerArrayNormalizedValues= lineNormalizedValues[(n*nbColumns)+i];
    					innerArrayRealValues= lineRealValues[(n*nbColumns)+i];

    					var arrayValues = [];
    					for(var j=0; j<nbPoints; j++){
    						arrayValues[j] = [];
    						arrayValues[j].px = cellSize*10/100+j*((cellSize-(cellSize*20/100))/(nbPoints-1));
    						arrayValues[j].py = innerArrayNormalizedValues[j]*cellSize;
    						arrayValues[j].realValue = innerArrayRealValues[j];
    					}

    					var lineFunction = d3.svg.line()
                            .x(function(d) { return d.px+i*cellSize; })
                            .y(function(d) { return -d.py+(n+1)*cellSize; })
                            .interpolate("linear");

    					return lineFunction(arrayValues);
    				})
    				.attr("stroke", function(d) {
    					return "#112E45";
    				})
    				.attr("stroke-width", 1.2)
    				.attr("fill", "none");

    			var focus = points.select("path.ligne")
    				.data(d3.range(nbColumns))
    				.enter()
    				.append("circle")
    				.attr("class", function(d, i) {
    					return "y"+(n*nbColumns+i);
    				})
    				.attr("cx", function(d, i) {
    					var px = cellSize*10/100+i*cellSize;
    					return px;
    				})
    				.attr("cy", function(d, i) {
    					innerArrayNormalizedValues= lineNormalizedValues[(n*nbColumns)+i];
    					var py = (-innerArrayNormalizedValues[0]*cellSize)+(n+1)*cellSize;
    					return py;
    				})
    				//.attr("r", 4) //.attr("r", 4)
            .attr("r", function(d,i){
              if(lineNormalizedValues[(n*nbColumns)+i][i] != null &&  lineNormalizedValues[(n*nbColumns)+i][i] != 0){ //what I edited

                //console.log(lineNormalizedValues[(n*nbColumns)+i][1])
                return 4;
              }
              else{
                return 0;
              }

            })
    				.style("fill", "none")
    				.style("stroke", "#112E45");



    			rows.on('mouseover', function(d, i) {
      			var el = d3.select(this)
      				.transition()
      				.duration(10)
      				.style("fill-opacity", 0.8);
      			d3.select('#plot-names').text(function () {
      				return cellNames[(n*nbColumns)+i];
      				});
      			d3.select('#cell-info').text(function () {
      				return 'Cell ' + (1 + (n*nbColumns)+i) + ', Superclass ' +
      				  superclass[(n*nbColumns)+i] + ', N= ' + cellPop[(n*nbColumns)+i];
      				});
    			});
    			rows.on('mouseout', function(d, i) {
      			var el = d3.select(this)
      				.transition()
      				.duration(10)
      				.style("fill-opacity", 1);
    			});
    			rows.on('mousemove', function (d, i) { //what do these mousemovde methods provide
    				for(var k=0; k<nbRows; k++){
    					for(var l=0; l<nbColumns; l++){
    						innerArrayNormalizedValues= lineNormalizedValues[(k*nbColumns)+l];
    						innerArrayRealValues= lineRealValues[(k*nbColumns)+l];

    						var arrayValues = [];
    						for(var j=0; j<nbPoints; j++){
    							arrayValues[j] = [];
    							arrayValues[j].px = cellSize*10/100+j*((cellSize-(cellSize*20/100))/(nbPoints-1))+l*cellSize;
    							arrayValues[j].py = -(innerArrayNormalizedValues[j]*cellSize)+(k+1)*cellSize;
    							arrayValues[j].realValue = innerArrayRealValues[j];
    						}

    						var bisectPoints = d3.bisector(function(d) { return d.px; }).left;
    						var x = d3.time.scale().range([arrayValues[0].px, arrayValues[arrayValues.length - 1].px]);
    						x.domain([arrayValues[0].px, arrayValues[arrayValues.length - 1].px]);

    						var additionalX = (l-i)*cellSize;

    						var x0 = x.invert(d3.mouse(this)[0]+additionalX),
    							p = bisectPoints(arrayValues, x0, 1),
    							d0 = arrayValues[p - 1],
    							d1 = arrayValues[p];
    							if(p<nbColumns){
    								d = x0 - d0.px > d1.px - x0 ? d1 : d0;
    							}
    							d = d0;

    						svg.select("circle.y"+(k*nbColumns+l))
    							.attr("transform", "translate(" + (d.px-cellSize*10/100-l*cellSize)+ "," + (d.py-arrayValues[0].py) + ")");

    						if(l==i && k==n){
    							d3.select('#plot-message').text(function () {
    								var pointValue = d == d0 ? arrayValues[p-1].realValue : arrayValues[p].realValue;
    								var pointLabel = d == d0 ? label[p-1] : label[p];
    								return pointLabel + ": " + pointValue;
    							});
    						}
    					}
    				}
    			});
      	});
      } else {
        //////////////////////////////////////////////////////////////////////
        // Other square plots
        //////////////////////////////////////////////////////////////////////
//merge this together with the previous ones or look for similar elements
        // Loop on rows
        _.times(nbRows, function(n) {
          var rows = svg.selectAll('rect' + ' .row-' + (n + 1))
          .data(d3.range(nbColumns))
    			.enter().append('rect')
    			.attr({
    				class: function(d, i) {
    					return 'square row-' + (n + 1) + ' ' + 'col-' + (i + 1);
    				},
    				id: function(d, i) {
    					return 's-' + (n + 1) + (i + 1);
    				},
    				width: cellSize,
    				height: cellSize,
    				x: function(d, i) {
    					return i * cellSize;
    				},
    				y: n * cellSize,
    				fill: function(d, i) {
    					var indice = superclass[(n*nbColumns)+i];
    					return superclassColor[indice-1];
    				},
    				stroke: '#FDBB30'
    			});

          // Global mouse events
      		rows.on('mouseover', function (d, i) {
      			var el = d3.select(this)
      				.transition()
      				.duration(10)
      				.style("fill-opacity", 0.8);
      			d3.select('#cell-info').text(function () {
      				return 'Cell ' + (1 + (n*nbColumns)+i) + ', Superclass ' +
      				  superclass[(n*nbColumns)+i] + ', N= ' + cellPop[(n*nbColumns)+i];
      				});
      			d3.select('#plot-names').text(function () {
      				return cellNames[(n*nbColumns)+i];
      				});
        		});
      		rows.on('mouseout', function (d, i) {
      			var el = d3.select(this)
      				.transition()
      				.duration(10)
      				.style("fill-opacity", 1);
        		});

      		var array = d3.range(nbColumns);
          svg.selectAll('rect' + ' .row-' + (n + 1))
          .data(d3.range(nbColumns))
    			.append('g')

          if (plotType.localeCompare("Radar")==0) {
            //////////////////////////////////////////////////////////////////////
            // Square Radar
            ///////////////////////////////////////////////////////////////////////
            var array = d3.range(nbColumns);
            var width = cellSize*.5;
            var height = cellSize*.5;
            var radius = Math.min(width, height) / 2;
            for (p = 0; p < array.length; p++) {
              var arc = d3.svg.arc().outerRadius(function function_name(d,i) {
                return d.data.normalizedValue*0.4;});
              var innerArrayNormalizedValues = [];
              innerArrayNormalizedValues= radarNormalizedValues[(n*nbColumns)+p];
              var innerArrayRealValues = [];
              innerArrayRealValues= radarRealValues[(n*nbColumns)+p];

              var arrayValues = [];
              for(var j=0; j<parts; j++){
                arrayValues[j] = [];
                arrayValues[j].normalizedValue = innerArrayNormalizedValues[j]*cellSize;
                arrayValues[j].realValue = innerArrayRealValues[j];
              }

              var pie = d3.layout.pie()
              .value(function(d) {  return 100/parts; })
              .sort(null);

              var pieParts = svg.selectAll('rect' + ' .row-' + (n + 1))
              .data(pie(arrayValues))
      				.enter()
      				.append("path")
    					.attr("class", function(d, i) {
      					return "r"+i;
            	})
    					.attr('d', arc)
    					.attr('transform', 'translate(' + (array[p] * cellSize + cellSize/2) +
    						',' + (n * cellSize + cellSize/2) + ')')
    					.attr('fill', function(d, i) {
    						return labelColor[i];
    					});

              // Mouse actions
              pieParts.on('mouseenter', function (d, i) {
                d3.select('#plot-message').text(function () {
                  var innerArrayRealValues = [];
                  var ch=label[i]+": " + d.data.realValue;
                  return ch;
                });
              svg.selectAll("path.r"+i)
    						.attr("stroke","white")
    						.transition()
    						.duration(50)
    						.attr("stroke-width",2);
              });
              pieParts.on('mouseleave', function (d, i) {
                svg.selectAll("path.r"+i)
                  .transition()
      						.duration(50)
    	  					.attr("stroke","none");
                d3.select('#plot-message').text(function () {
                  return "-";
                });
              });
            }
          } else if (plotType.localeCompare("Camembert")==0) {
            //////////////////////////////////////////////////////////////////////
            // Square Pie
            ///////////////////////////////////////////////////////////////////////
            var array = d3.range(nbColumns);
            var width = cellSize*.5;
            var height = cellSize*.5;
            var radius = Math.min(width, height) / 2;
            for (p = 0; p < array.length; p++) {
              var arc = d3.svg.arc().outerRadius(pieNormalizedSize[(n*nbColumns)+p]*(cellSize*50/100));
              var innerArrayNormalizedValues = [];
              innerArrayNormalizedValues= pieNormalizedValues[(n*nbColumns)+p];
              var innerArrayRealValues = [];
              innerArrayRealValues= pieRealValues[(n*nbColumns)+p];

    					var arrayValues = [];
    					for(var j=0; j<parts; j++){
    						arrayValues[j] = [];
    						arrayValues[j].normalizedValue = innerArrayNormalizedValues[j]*cellSize;
    						arrayValues[j].realValue = innerArrayRealValues[j];
    						arrayValues[j].innerCellPop = cellPop[(n*nbColumns)+p];
    					}

    					var pie = d3.layout.pie()
    						.value(function(d) { return d.normalizedValue; })
    						.sort(null);

    					var pieParts = svg.selectAll('rect' + ' .row-' + (n + 1))
    						.data(pie(arrayValues))
    						.enter()
    						.append('path')
                .attr("class", function(d, i) {
              					return "r"+i;
              	})
    						.attr('d', arc)
    						.attr('transform', 'translate(' + (array[p] * cellSize + cellSize/2) + ',' + (n * cellSize + cellSize/2) + ')')
    						.attr('fill', function(d, i) {
    							return labelColor[i];
    						});

              // Mouse actions
              pieParts.on('mouseenter', function (d, i) {
                d3.select('#plot-message').text(function () {
                  var innerArrayRealValues = [];
                  var ch=label[i]+": n= " + d.data.realValue + " (" +
    						    (100 * d.data.realValue / d.data.innerCellPop).toFixed(1) + "%)";
                  return ch;
                });
              svg.selectAll("path.r"+i)
    						.attr("stroke","white")
    						.transition()
    						.duration(50)
    						.attr("stroke-width",2);
              });
              pieParts.on('mouseleave', function (d, i) {
                svg.selectAll("path.r"+i)
                  .transition()
      						.duration(50)
    	  					.attr("stroke","none");
                d3.select('#plot-message').text(function () {
                  return "-";
                });
              });
            }
          } else if (plotType.localeCompare("Barplot")==0) {
            //////////////////////////////////////////////////////////////////////
            // Square Barplot
            ///////////////////////////////////////////////////////////////////////
            var k=0;
        		var array = d3.range(nbColumns);
      			var width = cellSize;
      			var height = cellSize*.6;
            for (var cpt = 0; cpt < array.length; cpt++) {

      				var innerArrayNormalizedValues= batonNormalizedValues[(n*nbColumns)+cpt];
      				var innerArrayRealValues= batonRealValues[(n*nbColumns)+cpt];
      				// Check if single variable
      				if (!Array.isArray(innerArrayNormalizedValues)) {
      				  innerArrayNormalizedValues= [innerArrayNormalizedValues];
      				}
      				if (!Array.isArray(innerArrayRealValues)) {
      				  innerArrayRealValues= [innerArrayRealValues];
      				}

      				var arrayValues = [];
      				for(var j=0; j<nbBatons; j++){
      					arrayValues[j] = [];
      					arrayValues[j].normalizedValue = innerArrayNormalizedValues[j]*cellSize;
      					arrayValues[j].realValue = innerArrayRealValues[j];
      					arrayValues[j].innerCellPop = cellPop[(n*nbColumns)+cpt]
      				}

      				var y = d3.scale.linear()
      					.domain([0,nbBatons])
      					.range([0,height])

      				var x = d3.scale.linear()
      					.domain([0,nbBatons])
      					.range([0,width])


      				var xAxis = d3.svg.axis()
      					.scale(x)
      					.orient("bottom");

      				var layout = d3.layout.pie()
      						.value(function(d) { return d.normalizedValue; })
      						.sort(null);

      				var bars = svg.selectAll('rect' + ' .row-' + (n + 1))
      					.data(layout(arrayValues))
      					.enter()
      					.append("rect")
      					.attr("class", function(d, i) {
      						return "r"+i;
      					})
      					.attr("x", function (d, i) {
      						return i*((width-(cellSize*40/100))/nbBatons)+k+(cellSize*20/100);
      					})
      					.attr("y", function (d, i) {
                  return (cellSize - (cellSize*5/100) - (innerArrayNormalizedValues[i]*0.9)*cellSize +n*cellSize); // same here see below
                })
      					.attr("width", function (d) { return ((width-(cellSize*40/100))/nbBatons)-(cellSize*2/100)})
      					.attr("height", function (d, i) {
                          //console.log(innerArrayNormalizedValues[i]*cellSize);
                  return (innerArrayNormalizedValues[i]*0.9)*cellSize; //*0.9 applied to reduce full bar level to not reach the box limit
                })
      					.attr("fill", function(d, i) {
      							return labelColor[i];
      					});
      					k+=cellSize;

      				bars.on('mouseenter', function (d, i) {
      					d3.select('#plot-message').text(function () {
      						var ch=label[i]+": " + d.data.realValue;
      						if (isCatBarplot)
      						  ch= ch + " (" +
      						    (100 * d.data.realValue / d.data.innerCellPop).toFixed(1) +  "%)";
      						return ch;
      					});

      					svg.selectAll("rect.r"+i)
      						.attr("stroke","white")
      						.transition()
      						.duration(50)
      						.attr("stroke-width",2);
      				});

      				bars.on('mouseleave', function (d, i) {
      					d3.select('#plot-message').text(function () {
      						return "-";
      					});
      					svg.selectAll("rect.r"+i).transition()
      						.duration(50)
      						.attr("stroke","none");
      				});
      			}
          } else  if (plotType.localeCompare("Boxplot")==0) {
            //////////////////////////////////////////////////////////////////////
            // Square Boxplot
            ///////////////////////////////////////////////////////////////////////
            var width = (cellSize*80/100)/(nbBox)-(cellSize*10/100),
      				height = (cellSize*70/100);
      			if(nbBox==1){width = (cellSize*20/100);
      						 height = (cellSize*40/100);}
      			var min = Infinity,
      				max = -Infinity;
      			var chart = d3.box()
      				.width(width)
      				.height(height);
      			var array = d3.range(nbColumns);

      			for (p = 0; p < array.length; p++) {
      			  if (cellPop[(n*nbColumns)+p] == 0) {continue;}
      				var innerArrayNormalizedValues = boxPlotNormalizedValues[(n*nbColumns)+p];
      				var innerArrayRealValues = boxPlotRealValues[(n*nbColumns)+p];
      				var innerArrayExtremesNormalizedValues = boxNormalizedExtremesValues[(n*nbColumns)+p];
      				var innerArrayExtremesRealValues = boxRealExtremesValues[(n*nbColumns)+p];

      				var arrayValues = [];
      				var data = [];

      				for(var j=0; j<nbBox; j++){
      					arrayValues[j] = [];
      					arrayValues[j].normalizedValues = innerArrayNormalizedValues[j];
      					arrayValues[j].realValues = innerArrayRealValues[j];

      					var speed = arrayValues[j].normalizedValues;
      					for(l=0; l<5; l++){
      						var e = Math.floor(j),
      						r = Math.floor(l),
      						s = Math.floor(speed[l]*cellSize),
      						d = data[e];

      						if (!d) { d = data[e] = [s];}
      						else { d.push(s);}
      						if (s > max) max = s;
      						if (s < min) min = s;
      					}
      					data[j].realValues = arrayValues[j].realValues;
                data[j].labels_plot = label[j]; // I added this

      				}

      				chart.domain([min, max]);

      				var boxs = svg.selectAll('rect' + ' .row-' + (n + 1))
      					.data(data)
      					.enter()
      					.append("g")
      					.attr("class", "box")
      					.attr("width",  width)
      					.attr("height", height)
      					.attr('transform', function(d, i) {
      						if(nbBox==1){
      							return 'translate(' + (cellSize*40/100+i*((cellSize*40/100)/(nbBox)) + p*cellSize) + ',' + (n * cellSize + cellSize*30/100) + ')';
      						}else{
      							return 'translate(' + ((cellSize*15/100+i*((cellSize*80/100)/(nbBox))) + p*cellSize) + ',' + (n * cellSize + cellSize*15/100) + ')';
      						}
      					})
      					.attr('fill', function(d, i) {
      							return labelColor[i];
      					})
      					.call(chart);

//              // Path for highlight
//    					boxs
//    					  .append("path")
//    					  .attr("class", function(d, i) {
//    					   return "r"+i;
//    					  });


      				for(var j=0; j<nbBox; j++){

      					var arrayExtremesValues = [];
      					var arrNormalizedValues = innerArrayExtremesNormalizedValues[j];
      					var arrRealValues = innerArrayExtremesRealValues[j];

      					for(var l=0; l<arrNormalizedValues.length; l++){
      						arrayExtremesValues[l] = [];
      						arrayExtremesValues[l].normalizedValues = arrNormalizedValues[l];
      						arrayExtremesValues[l].realValues = arrRealValues[l];
      						arrayExtremesValues[l].label = label[j];
      					}

      					var focus = boxs.select("g.box")
      						.data(arrayExtremesValues)
      						.enter()
      						.append("circle")
      						.attr("class", "y")
      						.attr('transform', function(d, i) {
      							if(nbBox==1){
      								return 'translate(' + (cellSize*40/100+j*(cellSize*40/100) + width/2 + p*cellSize) + ',' + (cellSize*0.9 - d.normalizedValues*cellSize*0.9 + n * cellSize) + ')';
      							}else{
      								return 'translate(' + ((cellSize*15/100+j*((cellSize*80/100)/(nbBox)) + width/2) + p*cellSize) + ',' + (cellSize*0.90 - d.normalizedValues*cellSize*0.85 + n * cellSize) + ')';
      							}
      						})
      						.attr("r", cellSize*4/100)
      						.attr('fill', function(d, i) {
      								return labelColor[j];
      						})
      						.style("stroke", "#112E45")
      						.attr("stroke-width",1);

      					// Circles mouse action
      					focus.on('mouseenter', function (d, i) {
      						d3.select('#plot-message').text(function () {
      							var ch=d.label+": " + d.realValues;
      							return ch;
      						});




      						d3.select(this)
      							.transition()
      							.duration(50)
      							.attr("stroke-width",2)
      					});

      					focus.on('mouseleave', function (d, i) {
      						d3.select(this).transition()
      							.duration(50)
      							.attr("stroke-width",1);
      					});
      				}


      				boxs.on('mouseenter', function (d, i) {
      					d3.select('#plot-message').text(function () {
      						var ch=label[i]+ ": Q1= " + d.realValues[1] + " ; Med= " +
      							  d.realValues[2] + " ; Q3= " + d.realValues[3];
      						return ch;
      					});


                var selected = d3.select(this).data()[0].labels_plot;
                console.log(selected);

      					//svg.selectAll("path.r"+i)
    						d3.selectAll("g.box")
      						.transition()
      						.duration(50)
                  .attr("stroke-width", function(d, i){ //this conditional is probably the right way of doing it
                    if(d.labels_plot == selected) {
                    //console.log(this.labels_plot);
                      return 2}
                    else {return 1}

                  });
      				});
      				boxs.on('mouseleave', function (d, i) {
      					d3.select('#plot-message').text(function () {
      						return "-";
      					});
      					//svg.selectAll("path.r"+i)
    						d3.select(this)
      					  .transition()
      						.duration(50)
      						.attr("stroke-width",1);
      				});
      			}
          } else if(plotType.localeCompare("Star")==0) {
            ////////////////////////////////////////////////////////////////////
            // Square Star
            ////////////////////////////////////////////////////////////////////
  		      var array = d3.range(nbColumns);
        		for (p = 0; p < array.length; p++) {
      				var innerArrayNormalizedValues= starPlotNormalizedValues[(n*nbColumns)+p];
      				var innerArrayRealValues= starPlotRealValues[(n*nbColumns)+p];
      				// Check if single variable
      				if (!Array.isArray(innerArrayNormalizedValues)) {
      				  innerArrayNormalizedValues= [innerArrayNormalizedValues];
      				}
      				if (!Array.isArray(innerArrayRealValues)) {
      				  innerArrayRealValues= [innerArrayRealValues];
      				}

      				var arrayValues = new Array(1);
      				arrayValues[0] = new Array(nbSommet);
      				for(var j=0; j<nbSommet; j++){
      					arrayValues[0][j] = {axis: label[j], value: innerArrayNormalizedValues[j], realValue: innerArrayRealValues[j]};
      				}
      				// Dimension of the graph
      				var w = cellSize*80/100,
      					h = cellSize*80/100;
      				//Options for the Radar chart, other than default
      				var mycfg = {
      					w: w,
      					h: h,
      					levels: 6,
      					ExtraWidthX: cellSize,
      					radius: cellSize*5/100,
      					x: (p * cellSize)+cellSize*10/100,
      					y: (n * cellSize)+cellSize*10/100.
      				}
      				//Call function to draw the Radar chart
      				//Will expect that data is in %'s
      				// RadarChart.draw("#thePlot", arrayValues, mycfg);
      				StarChart("#theWidget", arrayValues, mycfg);

      			}
          } else if(plotType.localeCompare("Names")==0) {
            ////////////////////////////////////////////////////////////////////
            // Square names wordcloud
            ////////////////////////////////////////////////////////////////////
      			function draw(words) {
    					svg.selectAll('rect' + ' .row-' + (n + 1))
    						.data(words)
    						.enter()
    						.append("g")
    						.attr('transform', 'translate(' + (array[p] * cellSize + cellSize/2) + ',' + (n * cellSize + cellSize/2 ) + ')')
    						.append("text")
    						.style("font-size", function(d) { return d.size + "px"; })
    						.style("fill", function(d) { return "#112E45"; })
    						.attr("text-anchor", "middle")
    						.attr("transform", function(d) {
    							return "translate(" + [d.x, d.y] + ")rotate(" + d.rotate + ")";
    						})
    						.text(function(d) { return d.text; });
    				}

        		for (p = 0; p < array.length; p++) {
      				var innerArrayWordClouds= wordClouds[(n*nbColumns)+p];
      				var nbWordInnerArray = 	nbWord[(n*nbColumns)+p];
      				var arrayValues = [];
      				var wordWithMaxLetters=0;
      				for(var j=0; j<nbWordInnerArray; j++){
      					arrayValues[j] = [];
      					arrayValues[j] = innerArrayWordClouds[j];
      					if(arrayValues[j].length>wordWithMaxLetters){
      						wordWithMaxLetters=arrayValues[j].length;
      					}
      				}

      				d3.layout.cloud().size([cellSize, cellSize]) // => ici l'intervale est tres important car il joue sur l'apparence des mots !
      					.words(arrayValues.map(function(d) {
      						return {text: d, size: (cellSize/wordWithMaxLetters) };
      					}))
      					.rotate(function() { return ~~(Math.random() * 1) * 90; })
      					.fontSize(function(d) { return d.size; })
      					.on("end", draw)
      					.start();
      			}
          }
        });
      }
    }


    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    // Hexagonal grid function
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    function commonHexGrid(){
      var margin = {top: 30, right: 20, bottom: 20, left: 50};
      var width = 850;
      var height = 350;
      var widtht = 850;
      var heightt = 350;
      var hexRadius=cellSize/2;
      var hexInRadius;

      //Set the new height and width of the SVG based on the max possible
      width = nbColumns*hexRadius*Math.sqrt(3)+hexRadius;
      height = nbRows*1.5*hexRadius+2.5*hexRadius;
      //Set the hexagon radius
      var hexbin = d3.hexbin().radius(hexRadius);
      //Calculate the center positions of each hexagon
      var points = [];
       //for (var i = 0; i < nbRows; i++) {
      for (var i = nbRows; i > 0; i--) {  //for (var i = 0; i < nbRows; i++) { <-- this is the edit to change the order of class numbering
        for (var j = 0; j < nbColumns; j++) {
          points.push([(hexRadius * j * 1.75)+hexRadius, (hexRadius * i * 1.5) ]); //+hexRadius added to get different shape
        }
      }

      var svg = d3.select(el).append("svg")
			.attr("width", width+hexRadius+5)
			.attr("height", height)
			.attr("style"," display:block; margin:auto; margin-top:30px;")
			.append("g")
			.attr("transform", "translate(" + hexRadius + "," + hexRadius+ ")");


      if (plotType.localeCompare("Color")==0) {
        ////////////////////////////////////////////////////////////////////////
        // Hexagonal Color plot
        ////////////////////////////////////////////////////////////////////////
        var hexa = svg.selectAll(".hexagon")
    			.data(hexbin(points))
    			.enter().append("g");

    		var coordinatesArray = hexbin(points); //what type of function is hexbin()

    		hexa.append("path")
    			.attr("class", "hexagon")
    			.attr("d", function (d) {
    				return "M" + d.x + "," + d.y + hexbin.hexagon();
    			})
    			.attr("stroke", function (d,i) {
    				return "#fff";
    			})
    			.style("fill", function (d,i) {
            return colorNormalizedValues[i];
    			});

    		if(activate){
    			hexa.append("text")
    					.attr("x", function(d, i) { return coordinatesArray[i].x - cellSize*5/100 ; })
    					.attr("y", function(d, i) { return coordinatesArray[i].y + cellSize*5/100 ; })
    					.text(function(d, i) { return superclass[i]; })
    					.attr("font-family", "sans-serif")
    					.attr("font-size", cellSize*20/100)
    					.attr("fill", "#112E45");
    		}

    		//hover effects for the hexagons to change opacity and get the text for the cell
    		hexa.on('mouseover', function (d, i) {
    			var el = d3.select(this)
    				.transition()
    				.duration(10)
    				.style("fill-opacity", 0.8);
    			d3.select('#cell-info').text(function () {
    				return 'Cell ' + parseInt(i+1,10) + ', superclass ' +
              superclass[i] + ', N= ' + cellPop[i];
    			});
    			d3.select('#plot-message').text(function () {
    				return label + ': ' + colorRealValues[i];
    			});
    			d3.select('#plot-names').text(function () {
    				return cellNames[i];
  				});
    		});
    		hexa.on('mouseout', function (d, i) {
    			var el = d3.select(this)
    				.transition()
    				.duration(1000)
    				.style("fill-opacity", 1);
    		});
      } if (plotType.localeCompare("Hitmap")==0) {
        ////////////////////////////////////////////////////////////////////////
        // Hexagonal Hitmap
        ////////////////////////////////////////////////////////////////////////
        var hexa = svg.selectAll(".hexagon")
    			.data(hexbin(points))
    			.enter().append("path")
    			.attr("class", "hexagon")
    			.attr("d", function (d) {
    				return "M" + d.x + "," + d.y + hexbin.hexagon();
    			})
    			.attr("stroke", function (d,i) {
    				return "#fff";
    			})
    			.attr("stroke-width", "1px")
    			.style("fill", function (d,i) {
    				var indice = superclass[i];
    				return superclassColor[indice-1];
    			});

    		var i=0;
    		var inner_hex  = svg.append("g")
    			.selectAll(".hexagon")
    			.data(hexbin(points))
    			.enter()
    			.append("path")
    			.attr("class", "InnerHexagon")
    			.attr("d", function (d) {
    				hexInRadius = (hitmapNormalizedValues[i]*cellSize)/2;
    				i++;
    				hexbint = d3.hexbin().radius(hexInRadius);

    				return "M" + d.x + "," + d.y + hexbint.hexagon();
    			})
    			.style("fill", function (d,i) {
    				return "#112E45";
    			})


        var el_enter;


        inner_hex.on('mouseover', function (d, i) {
          var el_enter_ins = el_enter
           .transition()
           .duration(10)
           .style("fill-opacity", 0.5);

        })


        inner_hex.on('mouseout', function (d, i) {
          var el_enter_ins = el_enter
           .transition()
           .duration(10)
           .style("fill-opacity", 1);

        })


    		hexa.on('mouseover', function (d, i) {



           el_enter = d3.select(this);

           el_enter
    				.transition()
    				.duration(10)
    				.style("fill-opacity", 0.5);



    			d3.select('#cell-info').text(function () {
    				var ch = 'Cell ' + parseInt(i+1,10) + ', Superclass ' +
              superclass[i] + ', N= ' + cellPop[i];
    				return ch;
    			});
    			d3.select('#plot-names').text(function () {
    				return cellNames[i];
  				});
    		});
    		hexa.on('mouseout', function (d, i) {
          console.log(el_enter);
    			var el = d3.select(this)
    				.transition()
    				.duration(1000)
    				.style("fill-opacity", 1);
    		});






      } else {
        ////////////////////////////////////////////////////////////////////////
        // Other hexagonal plots
        ////////////////////////////////////////////////////////////////////////
        var hexa = svg.selectAll(".hexagon")
    		.data(hexbin(points))
  			.enter().append("path")
  			.attr("class", "hexagon")
  			.attr("d", function (d) {
  				return "M" + d.x + "," + d.y + hexbin.hexagon();
  			})
  			.attr("stroke", function (d,i) {
  				return "#fff";
  			})
  			.attr("stroke-width", "1px")
  			.style("fill", function (d,i) {
  				var indice = superclass[i];
  				return superclassColor[indice-1];
  			});

        // Hexagon cell mouse actions for all plots
        hexa.on('mouseover', function (d, i) {
  				var el = d3.select(this)
  					.transition()
  					.duration(10)
  					.style("fill-opacity", 0.8);

          d3.select('#cell-info').text(function () {
            var ch = 'Cell ' + parseInt(i+1,10) + ', Superclass ' +
              superclass[i] + ', N= ' + cellPop[i];
            return ch;
          });
    			d3.select('#plot-names').text(function () {
    				return cellNames[i];
  				});
        });
        hexa.on('mouseout', function (d, i) {
          var el = d3.select(this)
					.transition()
					.duration(1000)
					.style("fill-opacity", 1);
        });

        var array = d3.range(nbColumns);
        var width = hexRadius;
        var height = hexRadius;
        var radius = Math.min(width, height)/ 2;

        svg.selectAll(".hexagon").data(d3.range(nbColumns)).append('g')
        var coordinatesArray = hexbin(points);

        if (plotType.localeCompare("Radar")==0) {
          //////////////////////////////////////////////////////////////////////////
          // Hexagonal Radar
          //////////////////////////////////////////////////////////////////////////
          var array = d3.range(nbColumns);
          var width = cellSize*.5;
          var height = cellSize*.5;
          var radius = Math.min(width, height) / 2;
          for (var n = 0; n < nbRows*nbColumns; n++) {
            var arc = d3.svg.arc().outerRadius(function function_name(d,i) { return d.data.normalizedValue*0.4;});
            var innerArrayNormalizedValues = [];
            innerArrayNormalizedValues= radarNormalizedValues[n];
            var innerArrayRealValues = [];
            innerArrayRealValues= radarRealValues[n];
            var arrayValues = [];

            for(var j=0; j<parts; j++){
              arrayValues[j] = [];
              arrayValues[j].normalizedValue = innerArrayNormalizedValues[j]*cellSize;
              arrayValues[j].realValue = innerArrayRealValues[j];
            }

            var pie = d3.layout.pie()
            .value(function(d) { return 100/parts; })
            .sort(null);

            var pieParts = svg.append("g")
    				.selectAll(".hexagon")
    				.data(pie(arrayValues))
    				.enter()
    				.append('path')
    				.attr("class", function(d, i) {
      					return "r"+i;
          	})
    				.attr('d', arc)
    				.attr('transform', 'translate(' + (coordinatesArray[n].x) +',' + coordinatesArray[n].y + ')')
    				.attr('fill', function(d, i) {
    					return labelColor[i];
    				})

            pieParts.on('mouseenter', function (d, i) {
              d3.select('#plot-message').text(function () {
                var ch = label[i] + ": " + d.data.realValue;
                return ch;
              });
              svg.selectAll("path.r"+i)
                .attr("stroke","white")
                .transition()
                .duration(50)
                .attr("stroke-width",2);
            });
            pieParts.on('mouseleave', function (d, i) {
              d3.select('#plot-message').text(function () {
                return "-";
              });
              svg.selectAll("path.r"+i)
                .transition()
                .duration(50)
                .attr("stroke","none");
            });
          }
        } else if(plotType.localeCompare("Camembert")==0) {
          //////////////////////////////////////////////////////////////////////////
          // Hexagonal Pie
          //////////////////////////////////////////////////////////////////////////
          var array = d3.range(nbColumns);
          var width = cellSize*.5;
          var height = cellSize*.5;
          var radius = Math.min(width, height) / 2;
          for (var n = 0; n < nbRows*nbColumns; n++) {
    				var arc = d3.svg.arc().outerRadius(pieNormalizedSize[n]*(cellSize*40/100));

    				var innerArrayNormalizedValues = [];
    				innerArrayNormalizedValues= pieNormalizedValues[n];

    				var innerArrayRealValues = [];
    				innerArrayRealValues= pieRealValues[n];

    				var arrayValues = [];
    				for(var j=0; j<parts; j++){
    					arrayValues[j] = [];
    					arrayValues[j].normalizedValue = innerArrayNormalizedValues[j]*cellSize;
    					arrayValues[j].realValue = innerArrayRealValues[j];
    					arrayValues[j].innerCellPop = cellPop[n];
    				}

    				var pie = d3.layout.pie()
    					.value(function(d) { return d.normalizedValue; })
    					.sort(null);

    				var pieParts = svg.append("g")
    					.selectAll(".hexagon")
    					.data(pie(arrayValues))
    					.enter()
    					.append('path')
              .attr("class", function(d, i) {
            					return "r"+i;
            	})
    					.attr('d', arc)
    					.attr('transform', 'translate(' + (coordinatesArray[n].x) +',' + coordinatesArray[n].y + ')')
    					.attr('fill', function(d, i) {
    						return labelColor[i];
    					})

            pieParts.on('mouseenter', function (d, i) {
    					d3.select('#plot-message').text(function () {
    						var ch=label[i]+ ": n= " + d.data.realValue + " (" +
    						  (100 * d.data.realValue / d.data.innerCellPop).toFixed(1) + "%)";
    						return ch;
    					});
  						svg.selectAll("path.r"+i)
    						.attr("stroke","white")
    						.transition()
    						.duration(50)
    						.attr("stroke-width",2);
    				});
            pieParts.on('mouseleave', function (d, i) {
              d3.select('#plot-message').text(function () {
                return "-";
              });
  						svg.selectAll("path.r"+i)
    					  .transition()
    						.duration(50)
    						.attr("stroke","none");
    				});
    			}
        } else if(plotType.localeCompare("Barplot")==0) {
          //////////////////////////////////////////////////////////////////////////
          // Hexagonal Barplot
          //////////////////////////////////////////////////////////////////////////
        	var array = d3.range(nbColumns);
    			var width = cellSize;
    			var height = cellSize*.6;
        	for (var indice = 0; indice < nbRows*nbColumns; indice++) {
            var innerArrayNormalizedValues= batonNormalizedValues[indice];
            var innerArrayRealValues= batonRealValues[indice];
    				// Check if single variable
    				if (!Array.isArray(innerArrayNormalizedValues)) {
    				  innerArrayNormalizedValues= [innerArrayNormalizedValues];
    				}
    				if (!Array.isArray(innerArrayRealValues)) {
    				  innerArrayRealValues= [innerArrayRealValues];
    				}
            var arrayValues = [];
            for(var j=0; j<nbBatons; j++){
      				arrayValues[j] = [];
      				arrayValues[j].normalizedValue = innerArrayNormalizedValues[j]*cellSize;
      				arrayValues[j].realValue = innerArrayRealValues[j];
      				arrayValues[j].innerCellPop = cellPop[indice]
      			}
            var y = d3.scale.linear()
      				.domain([0,nbBatons])
      				.range([0,height])

      			var x = d3.scale.linear()
      				.domain([0,nbBatons])
      				.range([0,width])

      			var layout = d3.layout.pie()
      					.value(function(d) { return d.normalizedValue; })
      					.sort(null);

      			var bars = svg.append("g").selectAll(".hexagon")
      				.data(layout(arrayValues))
      				.enter()
      				.append("rect")
      				.attr("class", function(d, i) {
      					return "r"+i;
      				})
      				.attr("x", function (d, i) {
      					return i*((width-(cellSize*40/100))/nbBatons)+(cellSize*20/100);
      				})
      				.attr("y", function (d, i) { return -innerArrayNormalizedValues[i]*(cellSize*55/100)+cellSize*25/100; })
      				.attr("width", function (d) { return ((width-(cellSize*40/100))/nbBatons)-(cellSize*2/100)})
      				.attr("height", function (d, i) {return innerArrayNormalizedValues[i]*cellSize*55/100; })
      				.attr('transform', 'translate(' + (coordinatesArray[indice].x-cellSize/2) + ',' + (coordinatesArray[indice].y) + ')')
      				.attr("fill", function(d, i) {
      						return labelColor[i];
      				});

      			bars.on('mouseenter', function (d, i) {
      				d3.select('#plot-message').text(function () {
      					var ch=label[i]+": " + d.data.realValue
      					if (isCatBarplot)
      					  ch= ch + " (" +
    						  (100 * d.data.realValue / d.data.innerCellPop).toFixed(1) + "%)";
      					return ch;
      				});
      				svg.selectAll("rect.r"+i)
      					.attr("stroke","white")
      					.transition()
      					.duration(50)
      					.attr("stroke-width",2);
      			});
      			bars.on('mouseleave', function (d, i) {
              d3.select('#plot-message').text(function () {
                return "-";
              });
      				svg.selectAll("rect.r"+i).transition()
      						.duration(50)
      						.attr("stroke","none");
      			});

      		}
        } else if(plotType.localeCompare("Boxplot")==0) {
          //////////////////////////////////////////////////////////////////////////
          // Hexagonal Boxplot
          //////////////////////////////////////////////////////////////////////////

          var width = (cellSize*80/100)/(nbBox)-(cellSize*10/100),
      			height = (cellSize*50/100);
      		if(nbBox==1){width = (cellSize*20/100);
      					 height = (cellSize*30/100);}
      		var min = Infinity,
      			max = -Infinity;
      		var chart = d3.box()
      			.width(width)
      			.height(height);
      		var array = d3.range(nbColumns);
      		var coordinatesArray = hexbin(points);

      		for (var p = 0; p < nbRows*nbColumns; p++) {
      		  if (cellPop[p] == 0) {continue;}
      			var innerArrayNormalizedValues = boxPlotNormalizedValues[p];
      			var innerArrayRealValues = boxPlotRealValues[p];
      			var innerArrayExtremesNormalizedValues = boxNormalizedExtremesValues[p];
      			var innerArrayExtremesRealValues = boxRealExtremesValues[p];

      			var arrayValues = [];
      			var data = [];
      			for(var j=0; j<nbBox; j++){
      				arrayValues[j] = [];
      				arrayValues[j].normalizedValues = innerArrayNormalizedValues[j];
      				arrayValues[j].realValues = innerArrayRealValues[j];

      				var speed = arrayValues[j].normalizedValues;

      				for(l=0; l<5; l++){
      					var e = Math.floor(j),
      					r = Math.floor(l),
      					s = Math.floor(speed[l]*cellSize),
      					d = data[e];

      					if (!d) { d = data[e] = [s];}
      					else { d.push(s);}
      					if (s > max) max = s;
      					if (s < min) min = s;
      				}
      				data[j].realValues = arrayValues[j].realValues;
              data[j].labels_plot = label[j]; // I added this
      			}

      			chart.domain([min, max]);

      			var boxs = svg.selectAll("hexagon")
      				.data(data)
      				.enter().append("g")
      				.attr("class", "box")
      				.attr("width",  width)
      				.attr("height", height)
      				.attr('transform', function(d, i) {
      					if(nbBox==1){
      						return 'translate(' + (cellSize*40/100+i*(cellSize*30/100) + coordinatesArray[p].x -cellSize/2) + ',' + (coordinatesArray[p].y - cellSize*15/100)+ ')';
      					}else{
      						return 'translate(' + ((cellSize*15/100+i*((cellSize*80/100)/(nbBox))) + coordinatesArray[p].x -cellSize/2) + ',' + (coordinatesArray[p].y - cellSize*25/100)+ ')';
      					}
      				})
      				.attr('fill', function(d, i) {
      						return labelColor[i];
      				})
      				.call(chart);

      			for(var j=0; j<nbBox; j++){
      				var arrayExtremesValues = [];
      				var arrNormalizedValues = innerArrayExtremesNormalizedValues[j];
      				var arrRealValues = innerArrayExtremesRealValues[j];

      				for(var l=0; l<arrNormalizedValues.length; l++){
      					arrayExtremesValues[l] = [];
      					arrayExtremesValues[l].normalizedValues = arrNormalizedValues[l];
      					arrayExtremesValues[l].realValues = arrRealValues[l]
      					arrayExtremesValues[l].label = label[j];

      				}



      				var focus = boxs.select("g.box")
      					.data(arrayExtremesValues)
      					.enter()
      					.append("circle")
      					.attr("class", "y")
      					.attr('transform', function(d, i) {
      						if(nbBox==1){
      							return 'translate(' + (cellSize*40/100+j*(cellSize*40/100) + width/2 + coordinatesArray[p].x -cellSize/2) + ',' + (coordinatesArray[p].y + cellSize*25/100 - d.normalizedValues*cellSize*60/100) + ')';
      						}else{
      							return 'translate(' + ((cellSize*15/100+j*((cellSize*80/100)/(nbBox)) + width/2) + coordinatesArray[p].x -cellSize/2) + ',' + (coordinatesArray[p].y + cellSize*25/100 - d.normalizedValues*cellSize*52/100) + ')';
      						}
      					})
      					.attr("r", cellSize*3/100)
      					.attr('fill', function(d, i) {
      							return labelColor[j];
      					})
      					.style("stroke", "#112E45")
      					.attr("stroke-width",1);

      				focus.on('mouseenter', function (d, i) {
      					d3.select('#plot-message').text(function () {
      						var ch=d.label+": " + d.realValues;
      						return ch;
      					});
      					d3.select(this) //this
      						.transition()
      						.duration(50)
      						.attr("stroke-width",2)
      				});
      				focus.on('mouseleave', function (d, i) {
                d3.select('#plot-message').text(function () {
                  return "-";
                });
      					d3.select(this).transition()
      						.duration(50)
      						.attr("stroke-width",1);
      				});
      			}

      			boxs.on('mouseenter', function (d, i) {
      				d3.select('#plot-message').text(function () {
    						var ch=label[i]+ ": Q1= " + d.realValues[1] + " ; Med= " +
    							  d.realValues[2] + " ; Q3= " + d.realValues[3];
      					return ch;
      				});
              var selected = d3.select(this).data()[0].labels_plot; // <- this gets me the variable of the hovered boxplot
              //var selected = d3.select("g.box").data();
              //console.log(selected);
              //console.log(selected[0].labels_plot);

      				d3.selectAll("g.box") // this is where it needs to be edited for the hex version!!!!
      					.transition()
      					.duration(50)
      					.attr("stroke-width", function(d, i){ //this conditional is probably the right way of doing it
                  if(d.labels_plot == selected) {
                  //console.log(this.labels_plot);
                    return 4}
                  else {return 1}

                });
      			});
      			boxs.on('mouseleave', function (d, i) {
              d3.select('#plot-message').text(function () {
                return "-";
              });
      				d3.selectAll("g.box").transition()
      					.duration(50)
      					.attr("stroke-width",1);
      			});
      		}
        } else if(plotType.localeCompare("Star")==0) {
          //////////////////////////////////////////////////////////////////////////
          // Hexagonal Star
          //////////////////////////////////////////////////////////////////////////

        	for (p = 0; p < nbRows*nbColumns; p++) {
      			var innerArrayNormalizedValues= starPlotNormalizedValues[p];
      			var innerArrayRealValues= starPlotRealValues[p];
    				// Check if single variable
    				if (!Array.isArray(innerArrayNormalizedValues)) {
    				  innerArrayNormalizedValues= [innerArrayNormalizedValues];
    				}
    				if (!Array.isArray(innerArrayRealValues)) {
    				  innerArrayRealValues= [innerArrayRealValues];
    				}

      			var arrayValues = new Array(1);
      			arrayValues[0] = new Array(nbSommet);
      			for(var j=0; j<nbSommet; j++){
      				arrayValues[0][j] = {axis: label[j], value: innerArrayNormalizedValues[j], realValue: innerArrayRealValues[j]};
      			}

      			// Dimension of the graph
      			var w = cellSize*70/100,
      				h = cellSize*70/100;

      			//Options for the Radar chart, other than default
      			var mycfg = {
      				w: w,
      				h: h,
      				levels: 6,
      				ExtraWidthX: cellSize,
      				radius: cellSize*5/100,
      				x: coordinatesArray[p].x+cellSize*15/100,
      				y: coordinatesArray[p].y+cellSize*15/100
      			}

      			//Call function to draw the Radar chart
      			//Will expect that data is in %'s
      			//RadarChart.draw("#thePlot", arrayValues, mycfg);
      			StarChart("#theWidget", arrayValues, mycfg);

      		}
        } else if(plotType.localeCompare("Line")==0) {
          //////////////////////////////////////////////////////////////////////////
          // Hexagonal Line
          //////////////////////////////////////////////////////////////////////////
        	for (var indice = 0; indice < nbRows*nbColumns; indice++) {
        	  if (cellPop[indice] == 0) { continue; }
      			innerArrayNormalizedValues= lineNormalizedValues[indice];
      			innerArrayRealValues= lineRealValues[indice];
      			var arrayValues = [];
      			for(var j=0; j<nbPoints; j++){
      				arrayValues[j] = [];
      				arrayValues[j].px = cellSize*20/100+j*((cellSize-(cellSize*40/100))/(nbPoints-1));
      				arrayValues[j].py = innerArrayNormalizedValues[j]*cellSize*0.5;
      				arrayValues[j].realValue = innerArrayRealValues[j];
      			}
      			var points = svg.append("g").selectAll(".hexagon")
      				.data(d3.range(1))
      				.enter()
      				.append("path")
      				.attr("class", "ligne")
      				.attr("d", function(d, i) {
      					var lineFunction = d3.svg.line()
      						.x(function(d) { return d.px; })
      						.y(function(d) { return -d.py;})
      						.interpolate("linear");
      					return lineFunction(arrayValues);
      				})
      				.attr('transform', 'translate(' + (coordinatesArray[indice].x-cellSize/2) + ',' + (coordinatesArray[indice].y+cellSize*25/100) + ')')
      				.attr("stroke", function(d) {
      					return "#112E45";
      				})
      				.attr("stroke-width", 1.2)
      				.attr("fill", "none");

      			var focus = points.select("path.ligne")
      				.data(d3.range(1))
      				.enter()
      				.append("circle")
      				.attr("class", function(d, i) {
      					return "y"+(indice);
      				})
      				.attr("cx", function(d, i) {
      					var px = cellSize*20/100+i*cellSize;
      					return px;
      				})
      				.attr("cy", function(d, i) {
      					innerArrayNormalizedValues= lineNormalizedValues[indice];
      					var py = (-innerArrayNormalizedValues[0]*cellSize)*0.5;
      					return py;
      				})
      				.attr('transform', 'translate(' + (coordinatesArray[indice].x-cellSize/2) + ',' + (coordinatesArray[indice].y+cellSize*25/100) + ')')
      				.attr("r", 4)
      				.style("fill", "none")
      				.style("stroke", "#112E45");

      			hexa.on('mousemove', function (d, i) { //responsible for the information
      				for(var k=0; k<nbRows; k++){
      					for(var l=0; l<nbColumns; l++){
      						var indice = (parseInt((i/nbRows),10)*nbColumns)+ parseInt((i%nbRows),10);
      						innerArrayNormalizedValues= lineNormalizedValues[(k*nbColumns)+l];
      						innerArrayRealValues= lineRealValues[(k*nbColumns)+l];

      						var arrayValues = [];
      						for(var j=0; j<nbPoints; j++){
      							arrayValues[j] = [];
      							arrayValues[j].px = cellSize*20/100+j*((cellSize-(cellSize*40/100))/(nbPoints-1))+coordinatesArray[(k*nbColumns)+l].x-cellSize/2;
      							arrayValues[j].py = -(innerArrayNormalizedValues[j]*cellSize*0.5)+coordinatesArray[(k*nbColumns)+l].y+cellSize*25/100;
      							arrayValues[j].realValue = innerArrayRealValues[j];
      						}

      						var bisectPoints = d3.bisector(function(d) { return d.px; }).left;
      						var x = d3.time.scale().range([arrayValues[0].px, arrayValues[arrayValues.length - 1].px]);
      						x.domain([arrayValues[0].px, arrayValues[arrayValues.length - 1].px]);

      						var additionalX = coordinatesArray[(k*nbColumns)+l].x - coordinatesArray[indice].x;

      						var x0 = x.invert(d3.mouse(this)[0] + additionalX),
      							p = bisectPoints(arrayValues, x0, 1),
      							d0 = arrayValues[p - 1],
      							d1 = arrayValues[p];
      							if(p<nbColumns){
      								d = x0 - d0.px > d1.px - x0 ? d1 : d0;
      							}
      							d = d0;

      						svg.select("circle.y"+((k*nbColumns)+l))
      							.attr("transform", "translate(" + (d.px-cellSize*20/100) + "," + (d.py-arrayValues[0].py+(cellSize-cellSize*75/100)+k*cellSize*75/100) + ")");

      						if(l==parseInt((i%nbRows),10) && k==parseInt((i/nbRows),10)){
      							d3.select('#plot-message').text(function () {
      								var pointValue = d == d0 ? arrayValues[p-1].realValue : arrayValues[p].realValue;
      								var pointLabel = d == d0 ? label[p-1] : label[p];
      								return pointLabel + ': ' + pointValue;
      							});
      						}
      					}
      				}
      			});
      		}
        } else if (plotType.localeCompare("Names")==0) {
          //////////////////////////////////////////////////////////////////////////
          // Hexagonal Wordcloud of Names
          //////////////////////////////////////////////////////////////////////////
      		function draw(words) {
    				svg.append("g").selectAll(".hexagon")
    					.data(words)
    					.enter()
    					.append("g")
    					.attr('transform', 'translate(' + (coordinatesArray[p].x) + ',' + (coordinatesArray[p].y) + ')')
    					.append("text")
    					.style("font-size", function(d) { return d.size + "px"; })
    					.style("fill", function(d) { return "#112E45"; })
    					.attr("text-anchor", "middle")
    					.attr("transform", function(d) {
    						return "translate(" + [d.x, d.y] + ")rotate(" + d.rotate + ")";
    					})
    					.text(function(d) { return d.text; });
    			}
        	for (p = 0; p < nbRows*nbColumns; p++) {

      			var innerArrayWordClouds= wordClouds[p];
      			var nbWordInnerArray = 	nbWord[p];

      			var arrayValues = [];
      			var wordWithMaxLetters=0;
      			for(var j=0; j<nbWordInnerArray; j++){
      				arrayValues[j] = [];
      				arrayValues[j] = innerArrayWordClouds[j];
      				if(arrayValues[j].length>wordWithMaxLetters){
      					wordWithMaxLetters=arrayValues[j].length;
      				}
      			}

            // => ici l'intervalle est tres important car il joue sur l'apparence des mots !
      			d3.layout.cloud().size([cellSize*0.6, cellSize*0.6])
      				.words(arrayValues.map(function(d) {
      					return {text: d, size: (cellSize/wordWithMaxLetters)*0.7 };
      				}))
      				.rotate(function() { return ~~(Math.random() * 1) * 90; })
      				.fontSize(function(d) { return d.size; })
      				.on("end", draw)
      				.start();

      		}
        }
      }
    }
           }
    };
  }
});