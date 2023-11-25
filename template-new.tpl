<!doctype html>
<html>
  <head>
    <meta charset="utf-8"/>
    <title>criterion report</title>
    <script>
     {{{js-chart}}}
     {{{js-criterion}}}
    </script>
    <style>
     {{{criterion-css}}}
    </style>
    <script type="application/json" id="report-data">
     {{{json}}}
    </script>
    <meta name="viewport" content="width=device-width, initial-scale=1">
  </head>
  <body>
    <div class="content">
      <div class="no-print" style="visibility:hidden;">
        <select id="sort-overview" class="select">
          <option value="report-index">index</option>
          <option value="lex">lexical</option>
          <option value="colex">colexical</option>
          <option value="duration">time ascending</option>
          <option value="rev-duration">time descending</option>
        </select>
        <span class="overview-info">
          <a href="#controls-explanation" class="info" title="click bar/label to zoom; x-axis to toggle logarithmic scale; background to reset">&#9432;</a>
          <a id="legend-toggle" class="chevron button"></a>
        </span>
      </div>
      <aside id="overview-chart"></aside>
    </div>
  </body>
</html>
