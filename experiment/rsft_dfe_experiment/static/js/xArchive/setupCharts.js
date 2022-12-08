Highcharts.setOptions({
  credits:    { enabled: false },
  exporting:  { enabled: false },
  tooltip:    { enabled: false },
  title:      { text: null },
  legend:     { enabled: false },
  hover:      { mode: null },
  chart: {
    spacingTop: 0,
    spacingBottom: 0,
    spacingLeft: 0,
    spacingRight: 0,
    style: {
      fontSize: '.8rem',
    }
  },
  plotOptions: {
    series: {
      allowPointSelect: false,
      animation: {
        duration: 1500
      },
      states: { hover: { enabled: false } }
    }
  }
});