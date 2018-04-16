Shiny.addCustomMessageHandler("jsondata",function(message){
  
  var dataset = message;
  
  var chart = c3.generate({
        bindto: '#plot',
        data: {
            json: dataset,
            type : 'donut'
        },
        color: {
            pattern: ['#FF6B84', '#5AAEFF']
        },
        legend: {
          show: false
        }

    });
  
})
