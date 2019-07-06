// Template Literal (ES6) Polyfill Start

function stripIndent(str) {
  var match = str.match(/^[ \t]*(?=\S)/gm);
  if (!match) return str;

  var indent = Math.min.apply(
    Math,
    match.map(function (x) {
      return x.length;
    })
  );

  var re = new RegExp("^[ \\t]{" + indent.toString() + "}", "gm");
  return indent > 0 ? str.replace(re, "") : str;
}

function literal(fn) {
  var reCommentContents = /\/\*!?(?:@preserve)?[ \t]*(?:\r\n|\n)([\s\S]*?)(?:\r\n|\n)[ \t]*\*\//;
  var matchedString = reCommentContents.exec(fn.toString())[1];
  var stringWithEvalResult = matchedString.replace(/\${([^}]*)}/g, function (
    occurency,
    jsExprMatch
  ) {
    return eval(jsExprMatch.trim());
  });

  return stripIndent(stringWithEvalResult).trim();
}

// Template Literal (ES6) Polyfill End
// Usage:
literal(function () {
  /*
  <div>
    <h1>${Math.cos(Math.PI / 2).toFixed(2)}</h1>
  </div>
  */
});
