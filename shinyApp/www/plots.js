Shiny.addCustomMessageHandler("jsondata",function(message){

  var dataset = message;

  var width = 500,
      height = 500,
      radius = Math.min(width, height) / 2;

  var color = d3.scale.ordinal()
      .range(["#332288", "#6699CC", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#661100", "#CC6677", "#AA4466", "#882255", "#AA4499"]);

  var arc = d3.svg.arc()
      .outerRadius(radius - 10)
      .innerRadius(radius - 70);

  var pie = d3.layout.pie()
      .padAngle(.02)
      .sort(null)
  	  .startAngle(1.1*Math.PI)
      .endAngle(3.1*Math.PI)
      .value(function(d) { return d.total; });

  var svg = d3.select("#plot").append("svg")
      .attr("width", width)
      .attr("height", height)
    .append("g")
      .attr("transform", "translate(" + width / 2 + "," + height / 2 + ")");

  var div = d3.select("body").append("div").attr("class", "toolTip");

  var g = svg.selectAll(".arc")
        .data(pie(dataset))
      .enter().append("g")
        .attr("class", "arc");

    g.append("path")
  	.style("fill", function(d) { return color(d.data.name); })
      .transition().delay(function(d,i) {
  	return i * 500; }).duration(500)
  	.attrTween('d', function(d) {
  		var i = d3.interpolate(d.startAngle+0.1, d.endAngle);
  		return function(t) {
  			d.endAngle = i(t);
  			return arc(d)
  			}
  		});

   // g.append("text")
  	// .attr("text-anchor", "middle")
  	// .attr('font-size', '3em')
    // .attr('font-family', 'Lato')
    // .style('fill', '#2c3e50')
  	// .attr('y', 20)
    // .text("Cohorts");

    // g.append("text")
    //     .attr("transform", function(d) { return "translate(" + arc.centroid(d) + ")"; })
    //     .attr("dy", ".35em")
  	//   .transition()
  	//   .delay(1000)
    //     .text(function(d) { return d.data.name; });

  	d3.selectAll("path").on("mousemove", function(d) {
  	    div.style("left", d3.event.pageX+10+"px");
  		  div.style("top", d3.event.pageY-25+"px");
  		  div.style("display", "inline-block");
      div.html("Sample #: " + (d.data.total) + "<br>" + "Percent: " + (d.data.percent) + "%");
  });

  d3.selectAll("path").on("mouseout", function(d){
      div.style("display", "none");
  });

  var legendRectSize = 22;
  var legendSpacing = 6;

  var legend = svg.selectAll('.legend')                     // NEW
    .data(color.domain())                                   // NEW
    .enter()                                                // NEW
    .append('g')                                            // NEW
    .attr('class', 'legend')                                // NEW
    .attr('transform', function(d, i) {                     // NEW
      var height = legendRectSize + legendSpacing;          // NEW
      var offset =  height * color.domain().length / 2;     // NEW
      var horz = -2.75 * legendRectSize;                       // NEW
      var vert = i * height - offset;                       // NEW
      return 'translate(' + horz + ',' + vert + ')';        // NEW
    });                                                     // NEW

  legend.append('rect')                                     // NEW
    .attr('width', legendRectSize)                          // NEW
    .attr('height', legendRectSize)                         // NEW
    .style('fill', color)                                   // NEW
    .style('stroke', color);                                // NEW

  legend.append('text')                                     // NEW
    .attr('x', legendRectSize + legendSpacing)              // NEW
    .attr('y', legendRectSize - legendSpacing)              // NEW
    .text(function(d) { return d; });

  //d3.select("body").transition().style("background-color", "#d3d3d3");
  function type(d) {
    d.total = +d.total;
    return d;
  }
})
