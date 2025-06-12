# BA_table works without CI

    Code
      print(BA_table(comp_co), "html")
    Output
      <!-- preamble start -->
      <!DOCTYPE html> 
      <html lang="en">
        <head>
          <meta charset="UTF-8">
          <meta name="viewport" content="width=device-width, initial-scale=1.0">
          <title>tinytable_h1ihnn9bfzm0goah538z</title>
          <link href="https://cdn.jsdelivr.net/npm/bootstrap@5.3.2/dist/css/bootstrap.min.css" rel="stylesheet">
        </head>
        <body>
      <!-- preamble end -->
      
          <script>
      
            function styleCell_h1ihnn9bfzm0goah538z(i, j, css_id) {
                var table = document.getElementById("tinytable_h1ihnn9bfzm0goah538z");
                var cell = table.rows[i]?.cells[j];  // Safe navigation to avoid errors
                if (cell) {
                    console.log(`Styling cell at (${i}, ${j}) with class ${css_id}`);
                    cell.classList.add(css_id);
                } else {
                    console.warn(`Cell at (${i}, ${j}) not found.`);
                }
            }
            function insertSpanRow_pf18bvcqjmvaney22qpf(i, colspan, content) {
              var table = document.getElementById('tinytable_h1ihnn9bfzm0goah538z');
              var newRow = table.insertRow(i);
              var newCell = newRow.insertCell(0);
              newCell.setAttribute("colspan", colspan);
              // newCell.innerText = content;
              // this may be unsafe, but innerText does not interpret <br>
              newCell.innerHTML = content;
            }
            function spanCell_h1ihnn9bfzm0goah538z(i, j, rowspan, colspan) {
              var table = document.getElementById("tinytable_h1ihnn9bfzm0goah538z");
              const targetRow = table.rows[i];
              const targetCell = targetRow.cells[j];
              for (let r = 0; r < rowspan; r++) {
                // Only start deleting cells to the right for the first row (r == 0)
                if (r === 0) {
                  // Delete cells to the right of the target cell in the first row
                  for (let c = colspan - 1; c > 0; c--) {
                    if (table.rows[i + r].cells[j + c]) {
                      table.rows[i + r].deleteCell(j + c);
                    }
                  }
                }
                // For rows below the first, delete starting from the target column
                if (r > 0) {
                  for (let c = colspan - 1; c >= 0; c--) {
                    if (table.rows[i + r] && table.rows[i + r].cells[j]) {
                      table.rows[i + r].deleteCell(j);
                    }
                  }
                }
              }
              // Set rowspan and colspan of the target cell
              targetCell.rowSpan = rowspan;
              targetCell.colSpan = colspan;
            }
            // tinytable span after
      window.addEventListener('load', function () { insertSpanRow_pf18bvcqjmvaney22qpf(9, 2, 'Limits of agreement (95%)') });
      window.addEventListener('load', function () { insertSpanRow_pf18bvcqjmvaney22qpf(5, 2, '<strong>Method comparison (alternative - reference)</strong>') });
      window.addEventListener('load', function () { insertSpanRow_pf18bvcqjmvaney22qpf(1, 2, '<strong>Distribution</strong><sup>1</sup>') });
            window.addEventListener('load', function () {
                var cellsToStyle = [
                  // tinytable style arrays after
                { positions: [ { i: 16, j: 0 }, { i: 16, j: 1 },  ], css_id: 'tinytable_css_1oskonae6weyln2d75i1',}, 
                { positions: [ { i: 0, j: 0 }, { i: 0, j: 1 },  ], css_id: 'tinytable_css_1mzfusajr7hw3l9l46li',}, 
                ];
      
                // Loop over the arrays to style the cells
                cellsToStyle.forEach(function (group) {
                    group.positions.forEach(function (cell) {
                        styleCell_h1ihnn9bfzm0goah538z(cell.i, cell.j, group.css_id);
                    });
                });
            });
          </script>
      
          <style>
            /* tinytable css entries after */
            .table td.tinytable_css_1oskonae6weyln2d75i1, .table th.tinytable_css_1oskonae6weyln2d75i1 { border-bottom: solid #d3d8dc 0.1em; }
            .table td.tinytable_css_1mzfusajr7hw3l9l46li, .table th.tinytable_css_1mzfusajr7hw3l9l46li { border-top: solid #d3d8dc 0.1em; border-bottom: solid #d3d8dc 0.05em; }
          </style>
          <div class="container">
            <table class="table table-borderless" id="tinytable_h1ihnn9bfzm0goah538z" style="width: auto; margin-left: auto; margin-right: auto;" data-quarto-disable-processing='true'>
              <thead>
              
                    <tr>
                      <th scope="col"></th>
                      <th scope="col">Estimate</th>
                    </tr>
              </thead>
              <tfoot><tr><td colspan='2'><sup>1</sup> Distribution of the means of simultaneous measurements.</td></tr>
      <tr><td colspan='2'><sup>2</sup> Percentage error = 1.96 · Total (or Within-subject) variation (SD) / mean.</td></tr>
      <tr><td colspan='2'><sup>3</sup> Change limits of agreement (95%) = 1.96 · √2 · Within-subject variation (SD).</td></tr></tfoot>
              <tbody>
                      <tr>
                        <td>Mean</td>
                        <td>5.03</td>
                      </tr>
                      <tr>
                        <td>Between-subject variation (SD)</td>
                        <td>1.21</td>
                      </tr>
                      <tr>
                        <td>Within-subject variation (SD)</td>
                        <td>0.28</td>
                      </tr>
                      <tr>
                        <td>Total variation (SD)</td>
                        <td>1.24</td>
                      </tr>
                      <tr>
                        <td>Bias</td>
                        <td>0.70</td>
                      </tr>
                      <tr>
                        <td>Between-subject variation (SD)</td>
                        <td>0.93</td>
                      </tr>
                      <tr>
                        <td>Within-subject variation (SD)</td>
                        <td>0.41</td>
                      </tr>
                      <tr>
                        <td>Total variation (SD)</td>
                        <td>1.02</td>
                      </tr>
                      <tr>
                        <td>  Upper limit</td>
                        <td>2.71</td>
                      </tr>
                      <tr>
                        <td>  Lower limit</td>
                        <td>-1.30</td>
                      </tr>
                      <tr>
                        <td>Percentage error<sup>2</sup></td>
                        <td>39.9 %</td>
                      </tr>
                      <tr>
                        <td>Within-subject percentage error<sup>2</sup></td>
                        <td>16.1 %</td>
                      </tr>
                      <tr>
                        <td>Change limits of agreement (95%)<sup>3</sup></td>
                        <td>±1.15</td>
                      </tr>
              </tbody>
            </table>
          </div>
      <!-- postamble start -->
        </body>
      
      </html>
      <!-- postamble end -->
      <!-- hack to avoid NA insertion in last line --> 

# BA_table works with CI

    Code
      print(BA_table(comp_co_w_ci), "html")
    Output
      <!-- preamble start -->
      <!DOCTYPE html> 
      <html lang="en">
        <head>
          <meta charset="UTF-8">
          <meta name="viewport" content="width=device-width, initial-scale=1.0">
          <title>tinytable_u7yv1p6jk26zglpvarlu</title>
          <link href="https://cdn.jsdelivr.net/npm/bootstrap@5.3.2/dist/css/bootstrap.min.css" rel="stylesheet">
        </head>
        <body>
      <!-- preamble end -->
      
          <script>
      
            function styleCell_u7yv1p6jk26zglpvarlu(i, j, css_id) {
                var table = document.getElementById("tinytable_u7yv1p6jk26zglpvarlu");
                var cell = table.rows[i]?.cells[j];  // Safe navigation to avoid errors
                if (cell) {
                    console.log(`Styling cell at (${i}, ${j}) with class ${css_id}`);
                    cell.classList.add(css_id);
                } else {
                    console.warn(`Cell at (${i}, ${j}) not found.`);
                }
            }
            function insertSpanRow_uxfmnl3g69nr4y7diu3w(i, colspan, content) {
              var table = document.getElementById('tinytable_u7yv1p6jk26zglpvarlu');
              var newRow = table.insertRow(i);
              var newCell = newRow.insertCell(0);
              newCell.setAttribute("colspan", colspan);
              // newCell.innerText = content;
              // this may be unsafe, but innerText does not interpret <br>
              newCell.innerHTML = content;
            }
            function spanCell_u7yv1p6jk26zglpvarlu(i, j, rowspan, colspan) {
              var table = document.getElementById("tinytable_u7yv1p6jk26zglpvarlu");
              const targetRow = table.rows[i];
              const targetCell = targetRow.cells[j];
              for (let r = 0; r < rowspan; r++) {
                // Only start deleting cells to the right for the first row (r == 0)
                if (r === 0) {
                  // Delete cells to the right of the target cell in the first row
                  for (let c = colspan - 1; c > 0; c--) {
                    if (table.rows[i + r].cells[j + c]) {
                      table.rows[i + r].deleteCell(j + c);
                    }
                  }
                }
                // For rows below the first, delete starting from the target column
                if (r > 0) {
                  for (let c = colspan - 1; c >= 0; c--) {
                    if (table.rows[i + r] && table.rows[i + r].cells[j]) {
                      table.rows[i + r].deleteCell(j);
                    }
                  }
                }
              }
              // Set rowspan and colspan of the target cell
              targetCell.rowSpan = rowspan;
              targetCell.colSpan = colspan;
            }
            // tinytable span after
      window.addEventListener('load', function () { insertSpanRow_uxfmnl3g69nr4y7diu3w(9, 2, 'Limits of agreement (95%)') });
      window.addEventListener('load', function () { insertSpanRow_uxfmnl3g69nr4y7diu3w(5, 2, '<strong>Method comparison (alternative - reference)</strong>') });
      window.addEventListener('load', function () { insertSpanRow_uxfmnl3g69nr4y7diu3w(1, 2, '<strong>Distribution</strong><sup>1</sup>') });
            window.addEventListener('load', function () {
                var cellsToStyle = [
                  // tinytable style arrays after
                { positions: [ { i: 16, j: 0 }, { i: 16, j: 1 },  ], css_id: 'tinytable_css_b9rbv5rjyl8mlmedutty',}, 
                { positions: [ { i: 0, j: 0 }, { i: 0, j: 1 },  ], css_id: 'tinytable_css_knntugb9cccplzzg6r21',}, 
                ];
      
                // Loop over the arrays to style the cells
                cellsToStyle.forEach(function (group) {
                    group.positions.forEach(function (cell) {
                        styleCell_u7yv1p6jk26zglpvarlu(cell.i, cell.j, group.css_id);
                    });
                });
            });
          </script>
      
          <style>
            /* tinytable css entries after */
            .table td.tinytable_css_b9rbv5rjyl8mlmedutty, .table th.tinytable_css_b9rbv5rjyl8mlmedutty { border-bottom: solid #d3d8dc 0.1em; }
            .table td.tinytable_css_knntugb9cccplzzg6r21, .table th.tinytable_css_knntugb9cccplzzg6r21 { border-top: solid #d3d8dc 0.1em; border-bottom: solid #d3d8dc 0.05em; }
          </style>
          <div class="container">
            <table class="table table-borderless" id="tinytable_u7yv1p6jk26zglpvarlu" style="width: auto; margin-left: auto; margin-right: auto;" data-quarto-disable-processing='true'>
              <thead>
              
                    <tr>
                      <th scope="col"></th>
                      <th scope="col">Estimate [95% CI]</th>
                    </tr>
              </thead>
              <tfoot><tr><td colspan='2'><sup>1</sup> Distribution of the means of simultaneous measurements.</td></tr>
      <tr><td colspan='2'><sup>2</sup> Percentage error = 1.96 · Total (or Within-subject) variation (SD) / mean.</td></tr>
      <tr><td colspan='2'><sup>3</sup> Change limits of agreement (95%) = 1.96 · √2 · Within-subject variation (SD).</td></tr></tfoot>
              <tbody>
                      <tr>
                        <td>Mean</td>
                        <td>5.03 [4.28; 5.67]</td>
                      </tr>
                      <tr>
                        <td>Between-subject variation (SD)</td>
                        <td>1.21 [0.64; 1.78]</td>
                      </tr>
                      <tr>
                        <td>Within-subject variation (SD)</td>
                        <td>0.28 [0.22; 0.32]</td>
                      </tr>
                      <tr>
                        <td>Total variation (SD)</td>
                        <td>1.24 [0.71; 1.79]</td>
                      </tr>
                      <tr>
                        <td>Bias</td>
                        <td>0.70 [0.10; 1.22]</td>
                      </tr>
                      <tr>
                        <td>Between-subject variation (SD)</td>
                        <td>0.93 [0.56; 1.34]</td>
                      </tr>
                      <tr>
                        <td>Within-subject variation (SD)</td>
                        <td>0.41 [0.33; 0.50]</td>
                      </tr>
                      <tr>
                        <td>Total variation (SD)</td>
                        <td>1.02 [0.69; 1.42]</td>
                      </tr>
                      <tr>
                        <td>  Upper limit</td>
                        <td>2.71 [1.96; 3.63]</td>
                      </tr>
                      <tr>
                        <td>  Lower limit</td>
                        <td>-1.30 [-2.35; -0.44]</td>
                      </tr>
                      <tr>
                        <td>Percentage error<sup>2</sup></td>
                        <td>39.9 [27.0; 55.4] %</td>
                      </tr>
                      <tr>
                        <td>Within-subject percentage error<sup>2</sup></td>
                        <td>16.1 [12.8; 19.6] %</td>
                      </tr>
                      <tr>
                        <td>Change limits of agreement (95%)<sup>3</sup></td>
                        <td>±1.15 [0.91; 1.39]</td>
                      </tr>
              </tbody>
            </table>
          </div>
      <!-- postamble start -->
        </body>
      
      </html>
      <!-- postamble end -->
      <!-- hack to avoid NA insertion in last line --> 

# BA_table works with log and CI

    Code
      print(BA_table(comp_co_log_w_ci), "html")
    Output
      <!-- preamble start -->
      <!DOCTYPE html> 
      <html lang="en">
        <head>
          <meta charset="UTF-8">
          <meta name="viewport" content="width=device-width, initial-scale=1.0">
          <title>tinytable_t9tto22z2r1hpz8kszmt</title>
          <link href="https://cdn.jsdelivr.net/npm/bootstrap@5.3.2/dist/css/bootstrap.min.css" rel="stylesheet">
        </head>
        <body>
      <!-- preamble end -->
      
          <script>
      
            function styleCell_t9tto22z2r1hpz8kszmt(i, j, css_id) {
                var table = document.getElementById("tinytable_t9tto22z2r1hpz8kszmt");
                var cell = table.rows[i]?.cells[j];  // Safe navigation to avoid errors
                if (cell) {
                    console.log(`Styling cell at (${i}, ${j}) with class ${css_id}`);
                    cell.classList.add(css_id);
                } else {
                    console.warn(`Cell at (${i}, ${j}) not found.`);
                }
            }
            function insertSpanRow_ffvljxc7ww1hy0f6sfgg(i, colspan, content) {
              var table = document.getElementById('tinytable_t9tto22z2r1hpz8kszmt');
              var newRow = table.insertRow(i);
              var newCell = newRow.insertCell(0);
              newCell.setAttribute("colspan", colspan);
              // newCell.innerText = content;
              // this may be unsafe, but innerText does not interpret <br>
              newCell.innerHTML = content;
            }
            function spanCell_t9tto22z2r1hpz8kszmt(i, j, rowspan, colspan) {
              var table = document.getElementById("tinytable_t9tto22z2r1hpz8kszmt");
              const targetRow = table.rows[i];
              const targetCell = targetRow.cells[j];
              for (let r = 0; r < rowspan; r++) {
                // Only start deleting cells to the right for the first row (r == 0)
                if (r === 0) {
                  // Delete cells to the right of the target cell in the first row
                  for (let c = colspan - 1; c > 0; c--) {
                    if (table.rows[i + r].cells[j + c]) {
                      table.rows[i + r].deleteCell(j + c);
                    }
                  }
                }
                // For rows below the first, delete starting from the target column
                if (r > 0) {
                  for (let c = colspan - 1; c >= 0; c--) {
                    if (table.rows[i + r] && table.rows[i + r].cells[j]) {
                      table.rows[i + r].deleteCell(j);
                    }
                  }
                }
              }
              // Set rowspan and colspan of the target cell
              targetCell.rowSpan = rowspan;
              targetCell.colSpan = colspan;
            }
            // tinytable span after
      window.addEventListener('load', function () { insertSpanRow_ffvljxc7ww1hy0f6sfgg(9, 2, 'Limits of agreement (95%)') });
      window.addEventListener('load', function () { insertSpanRow_ffvljxc7ww1hy0f6sfgg(5, 2, '<strong>Method comparison, exp(log(alternative) - log(reference))</strong>') });
      window.addEventListener('load', function () { insertSpanRow_ffvljxc7ww1hy0f6sfgg(1, 2, '<strong>Distribution</strong><sup>1</sup>') });
            window.addEventListener('load', function () {
                var cellsToStyle = [
                  // tinytable style arrays after
                { positions: [ { i: 16, j: 0 }, { i: 16, j: 1 },  ], css_id: 'tinytable_css_1jppgvpttqwbkscpsm6t',}, 
                { positions: [ { i: 0, j: 0 }, { i: 0, j: 1 },  ], css_id: 'tinytable_css_sbxk781ibh8aoye10lph',}, 
                ];
      
                // Loop over the arrays to style the cells
                cellsToStyle.forEach(function (group) {
                    group.positions.forEach(function (cell) {
                        styleCell_t9tto22z2r1hpz8kszmt(cell.i, cell.j, group.css_id);
                    });
                });
            });
          </script>
      
          <style>
            /* tinytable css entries after */
            .table td.tinytable_css_1jppgvpttqwbkscpsm6t, .table th.tinytable_css_1jppgvpttqwbkscpsm6t { border-bottom: solid #d3d8dc 0.1em; }
            .table td.tinytable_css_sbxk781ibh8aoye10lph, .table th.tinytable_css_sbxk781ibh8aoye10lph { border-top: solid #d3d8dc 0.1em; border-bottom: solid #d3d8dc 0.05em; }
          </style>
          <div class="container">
            <table class="table table-borderless" id="tinytable_t9tto22z2r1hpz8kszmt" style="width: auto; margin-left: auto; margin-right: auto;" data-quarto-disable-processing='true'>
              <thead>
              
                    <tr>
                      <th scope="col"></th>
                      <th scope="col">Estimate [95% CI]</th>
                    </tr>
              </thead>
              <tfoot><tr><td colspan='2'><sup>1</sup> Distribution of the means of simultaneous measurements.</td></tr>
      <tr><td colspan='2'><sup>2</sup> Percentage error = 1.96 · Total (or Within-subject) variation (SD) / mean.</td></tr>
      <tr><td colspan='2'><sup>3</sup> Change limits of agreement (95%) = 1.96 · √2 · Within-subject variation (SD).</td></tr></tfoot>
              <tbody>
                      <tr>
                        <td>Mean</td>
                        <td>5.03 [4.26; 5.73]</td>
                      </tr>
                      <tr>
                        <td>Between-subject variation (SD)</td>
                        <td>1.21 [0.65; 1.72]</td>
                      </tr>
                      <tr>
                        <td>Within-subject variation (SD)</td>
                        <td>0.28 [0.23; 0.35]</td>
                      </tr>
                      <tr>
                        <td>Total variation (SD)</td>
                        <td>1.24 [0.71; 1.74]</td>
                      </tr>
                      <tr>
                        <td>Bias</td>
                        <td>1.16 [1.04; 1.29]</td>
                      </tr>
                      <tr>
                        <td>Between-subject variation (SD)</td>
                        <td>1.20 [1.11; 1.30]</td>
                      </tr>
                      <tr>
                        <td>Within-subject variation (SD)</td>
                        <td>1.10 [1.07; 1.12]</td>
                      </tr>
                      <tr>
                        <td>Total variation (SD)</td>
                        <td>1.22 [1.15; 1.32]</td>
                      </tr>
                      <tr>
                        <td>  Upper limit</td>
                        <td>1.72 [1.43; 2.10]</td>
                      </tr>
                      <tr>
                        <td>  Lower limit</td>
                        <td>0.78 [0.66; 0.93]</td>
                      </tr>
                      <tr>
                        <td>Percentage error<sup>2</sup></td>
                        <td>–</td>
                      </tr>
                      <tr>
                        <td>Within-subject percentage error<sup>2</sup></td>
                        <td>–</td>
                      </tr>
                      <tr>
                        <td>Change limits of agreement (95%)<sup>3</sup></td>
                        <td>⋇1.30 [1.22; 1.38]</td>
                      </tr>
              </tbody>
            </table>
          </div>
      <!-- postamble start -->
        </body>
      
      </html>
      <!-- postamble end -->
      <!-- hack to avoid NA insertion in last line --> 

