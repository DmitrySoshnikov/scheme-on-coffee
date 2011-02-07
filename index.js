var commandsHistory = [];
var idx = 0;
function onSourceKeyPressed(event) {
  if (!event.ctrlKey) {
    return true;
  }
  var keyCode = event.keyCode;
  if (keyCode == 13) {
    document.getElementById('evalButton').click();
  } else if (keyCode == 38 || keyCode == 40 && commandsHistory.length > 0) { // up, down arrows
    idx < 0 && (idx = commandsHistory.length - 1);
    idx >= commandsHistory.length && (idx = 0);
    document.getElementById('source').value = commandsHistory[idx] || '';
    idx += (keyCode == 38 ? -1 : 1);
  }
}
function onEvalButtonClick(event) {
  var sourceEl = document.getElementById('source');
  var exp = sourceEl.value;
  if (!exp.replace(/^\s+|\s+$/g, '')) {
    return;
  }
  if (commandsHistory[commandsHistory.length - 1] != exp) {
    commandsHistory.push(exp);
    idx = commandsHistory.length;
  }
  var evalRes = execute(sourceEl.value);
  var result = '<em>' + inputPrompt + '</em><br /><pre onclick="showExample.call(this, event)">' + exp + '</pre><em>' + outputPrompt.replace(/  /g, ' &nbsp;') + '</em><br /><br />' + evalRes + '<br /><br />';
  var resultEl = document.getElementById('result');
  resultEl.innerHTML += result;
  resultEl.scrollTop += 1000;
  document.getElementById('clear-checkbox').checked && (sourceEl.value = '');
}
function onParseButtonClick(event) {
  var source = document.getElementById('source').value;
  if (!source.replace(/^\s+|\s+$/g, '')) return;
  var parsed = parse(document.getElementById('source').value);
  var expressions = parsed.map(function (exp, index) {
    return "    " + index + ": " + JSON.stringify(exp).replace(/,(?=[^ ])/g, ', ')
  });
  var parsedValue = '<b>Scheme expressions (' + expressions.length + ') in Coffee</b>:<br /><pre>[\n' + expressions.join("\n") + '\n]</pre></br />';
  document.getElementById('result').innerHTML += parsedValue;
}
function onGlobalEnvButtonClick(event) {
  var keys = LispList2JSArray(car(car(G)));
  var values = LispList2JSArray(cdr(car(G)));
  var value = '';
  var res = '';
  for (var k = 0; k < keys.length; k++) {
    res += '    <b>' + strPad(8, keys[k]) + '</b> :   ';
    value = values[k];
    res += (value && value.constructor == Array && (value[0] == 'primitive' || value[0] == 'procedure') ? 'function' : value) + (k == keys.length - 1 ? '' : ',') + '\n';
  }
  var globalEnvRes = '<b>Global environment:</b><br /><pre>{\n' + res + '}</pre><br/>';
  document.getElementById('result').innerHTML += globalEnvRes;
}

function showExample(event) {
  document.getElementById('source').value = this.innerHTML;
  onEvalButtonClick();
}

function strPad(n, str, from, ch) {
  ch || (ch = " ");
  var len = str.length;
  var res = "";
  while (len++ < n) {
    res += ch;
  }
  return (from == "left" ? res + str : str + res);
}

window.onload = function () {
  // window.setTimeout(onGlobalEnvButtonClick, 3000);
  var examples = document.getElementsByTagName('pre');
  for (var k = examples.length; k--;) {
    examples[k].onclick = showExample;
  }
};