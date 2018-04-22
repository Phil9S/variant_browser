Shiny.addCustomMessageHandler("cohortjson",function(cohort){
  
  var Dataset = cohort;
  
  var data = {};
  var sites = [];
  Dataset.forEach(function(e) {
    sites.push(e.name);
    data[e.name] = e.total;
  })
  
  var chart = c3.generate({
    bindto: '#plotCohort',
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
        pattern: ['#3182bd', '#9ecae1', '#e6550d', '#fdae6b', '#31a354', '#a1d99b', '#756bb1', '#bcbddc', '#636363', '#bdbdbd']
    },
    tooltip: {
      format: {
        title: function (d) { return 'Data ' + d; },
        }
    }
  });
})

Shiny.addCustomMessageHandler("agejson",function(age){
    
    var Dataset = age;
    
    var data = {};
    var sites = [];
    Dataset.forEach(function(e) {
      sites.push(e.name);
      data[e.name] = e.total;
    })
      
    var chart5 = c3.generate({
    bindto: '#plotAge',
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
        title: 'Age'
    },
    legend: {
        show: false
    },
    color: {
        pattern: ['#3182bd', '#9ecae1', '#e6550d', '#fdae6b', '#31a354', '#a1d99b', '#756bb1', '#bcbddc', '#636363', '#bdbdbd']
    },
    tooltip: {
      format: {
        title: function (d) { return 'Data ' + d; },
        }
    }
  });
})

Shiny.addCustomMessageHandler("sexjson",function(sex){
    
    var Dataset = sex;
    
    var data = {};
    var sites = [];
    Dataset.forEach(function(e) {
      sites.push(e.name);
      data[e.name] = e.total;
    })
      
    var chart2 = c3.generate({
    bindto: '#plotSex',
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
        title: 'Sex'
    },
    legend: {
        show: false
    },
    color: {
        pattern: ['#d99fb8', '#3182bd']
    },
    tooltip: {
      format: {
        title: function (d) { return 'Data ' + d; },
        }
    }
  });
})

Shiny.addCustomMessageHandler("ethnojson",function(ethno){
    
    var Dataset = ethno;
    
    var data = {};
    var sites = [];
    Dataset.forEach(function(e) {
      sites.push(e.name);
      data[e.name] = e.total;
    })
    
    var chart3 = c3.generate({
    bindto: '#plotEthno',
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
        title: 'Population'
    },
    legend: {
        show: false
    },
    color: {
        pattern: ['#3182bd', '#9ecae1', '#e6550d', '#fdae6b', '#31a354', '#a1d99b', '#756bb1', '#bcbddc', '#636363', '#bdbdbd']
    },
    tooltip: {
      format: {
        title: function (d) { return 'Data ' + d; },
        }
    }
  });
}) 

Shiny.addCustomMessageHandler("pheno_barjson",function(phenoPlot){    
  
  var mainDataset = phenoPlot
  
  var chart4 = c3.generate({
    bindto: '#plotPheno',
    data: {
        type: 'bar',
        json: mainDataset,
        keys: {
            x: 'pheno',
            value: ['total']
        }
    },
    color: {
        pattern: ['#3182bd']
    },
    bar: {
      width: {
            ratio: 0.7 
        }
    },
    axis: {
        rotated: true,
            x: {
                show: true,
                type: 'category'
            },
            y: {
              label: 'Samples'
            }
    },
    legend: {
      show: false,
      position: 'right'
    },
    grid: {
      y: {
        show: true
  }
}
});
  
})

Shiny.addCustomMessageHandler("SampleVarjson",function(SampleVarPlot){
  
  var mainDataset = SampleVarPlot
  
  var chart6 = c3.generate({
    bindto: '#plotSampleVars',
    data: {
        type: 'bar',
        json: mainDataset,
        keys: {
            x: 'conseq',
            value: ['total']
        }
    },
    color: {
        pattern: ['#3182bd']
    },
    bar: {
      width: {
            ratio: 0.7 
        }
    },
    axis: {
      rotated: true,
            x: {
                show: true,
                type: 'category'
            },
            y: {
              label: 'Frequency'
            }
    },
    legend: {
      show: false,
      position: 'right'
    },
    grid: {
      y: {
        show: true
      }
    }
  });
  setTimeout(function () {
    chart6.load({
      data: {
        type: 'bar',
        json: mainDataset,
        keys: {
            x: 'conseq',
            value: ['total']
        }
    },
    color: {
        pattern: ['#3182bd']
    },
    bar: {
      width: {
            ratio: 0.7 
        }
    },
    axis: {
      rotated: true,
            x: {
                show: true,
                type: 'category'
            },
            y: {
              label: 'Frequency'
            }
    },
    legend: {
      show: false,
      position: 'right'
    },
    grid: {
      y: {
        show: true
      }
    }
    })
  }, 0);
})

Shiny.addCustomMessageHandler("SampleMutjson",function(SampleMutPlot){
  
  var mainDataset = SampleMutPlot
  
  var chart7 = c3.generate({
    bindto: '#plotSampleMut',
    data: {
        type: 'bar',
        json: mainDataset,
        keys: {
            x: 'mutation',
            value: ['total']
        }
    },
    color: {
        pattern: ['#9ecae1']
    },
    bar: {
      width: {
            ratio: 0.7 
        }
    },
    axis: {
            x: {
                show: true,
                type: 'category'
            },
            y: {
              label: 'Frequency'
            }
    },
    legend: {
      show: false,
      position: 'right'
    },
    grid: {
      y: {
        show: true
      }
    }
  });
  setTimeout(function () {
    chart7.load({
        data: {
        type: 'bar',
        json: mainDataset,
        keys: {
            x: 'mutation',
            value: ['total']
        }
    },
    color: {
        pattern: ['#9ecae1']
    },
    bar: {
      width: {
            ratio: 0.7 
        }
    },
    axis: {
            x: {
                show: true,
                type: 'category'
            },
            y: {
              label: 'Frequency'
            }
    },
    legend: {
      show: false,
      position: 'right'
    },
    grid: {
      y: {
        show: true
      }
    }
    })
  }, 0);
})

Shiny.addCustomMessageHandler("SampleRarejson",function(SampleRarePlot){
  
  var labels = ["< 0.01","0.01 - 0.02","0.02 - 0.03","0.03 - 0.04","0.04 - 0.05"]
  
  var mainDataset = SampleRarePlot
  
  var chart8 = c3.generate({
      bindto: '#plotSampleRare',
      padding: {
        right: 20,
      },
      data: {
          columns: mainDataset,
          type: 'area-spline'
      },
      color: {
        pattern: ['#1f77b4', '#aec7e8', '#ff7f0e']
      },
      axis: {
          x: {
          type: 'indexed',
              tick: {
                  fit: true,
                  format: function (x) { return labels[x];
                  }
              }
          }
      },
      legend: {
        position: 'inset',
        inset: {
          anchor: 'top-right'
        }
      }
});

setTimeout(function () {
  chart8.load({
      bindto: '#plotSampleRare',
      padding: {
        right: 20,
      },
      data: {
          columns: mainDataset,
          type: 'area-spline'
      },
      color: {
        pattern: ['#1f77b4', '#aec7e8', '#ff7f0e']
      },
      axis: {
          x: {
          type: 'indexed',
              tick: {
                  fit: true,
                  format: function (x) { return labels[x];
                  }
              }
          }
      },
      legend: {
        position: 'inset',
        inset: {
          anchor: 'top-right'
        }
      }
    })
  }, 0);
})