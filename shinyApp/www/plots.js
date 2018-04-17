Shiny.addCustomMessageHandler("jsondata",function(message){
  
  var dataset = message;
  
  var data = {};
  var sites = [];
  dataset.forEach(function(e) {
    sites.push(e.name);
    data[e.name] = e.total;
  })
  
  var chart = c3.generate({
    bindto: '#plot',
    data: {
        json: [ data ],
        keys: {
            value: sites,
        },
        type : 'donut',
        onclick: function (d, i) { console.log("onclick", d, i); },
        onmouseover: function (d, i) { console.log("onmouseover", d, i); },
        onmouseout: function (d, i) { console.log("onmouseout", d, i); }
    },
    donut: {
        title: 'Cohorts'
    },
    legend: {
        show: false
    },
    color: {
        pattern: ['#18BC9C', '#2C3E50', '#F39C12', '#E74C3C', '#3498DB', '#18BC9C', '#2C3E50', '#F39C12']
    },
    tooltip: {
      format: {
        title: function (d) { return 'Data ' + d; },
  }
}
});
  
})
