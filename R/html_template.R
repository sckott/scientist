html_template <- '
<!DOCTYPE html>
<head>
<meta charset="utf-8">
<title>scientist: {{name}}</title>
<meta name="viewport" content="width=device-width, initial-scale=1.0">
<meta name="description" content="scientist">
<link href="http://netdna.bootstrapcdn.com/bootstrap/3.1.1/css/bootstrap.min.css" rel="stylesheet">
<link href="http://netdna.bootstrapcdn.com/font-awesome/4.0.3/css/font-awesome.css" rel="stylesheet">
</head>

<body>

<div class="container">

<center><h2>scientist</h2></center>
<center><h3>experiment name: <strong>{{name}}</strong></h3></center>

<table class="table table-striped table-hover" align="center">
<thead>
<tr>
<th>Which</th>
<th>Execution Time</th>
<th>Result</th>
</tr>
</thead>
<tbody>

{{#control}}
<tr>
<td><i>Control</i></td>
<td>{{time}}</td>
<td>{{result}}</td>
</tr>
{{/control}}

{{#candidates}}
<tr>
<td><i>Candidate ({{name}})</i></td>
<td>{{time}}</td>
<td>{{result}}</td>
</tr>
{{/candidates}}

</tbody>
</table>

</div>

</body>
</html>
'
