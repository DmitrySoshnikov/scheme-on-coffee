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
  var parsedValue = 'Scheme expression in Coffee: ' + JSON.stringify(parsed).replace(/,(?=[^ ])/g, ', ') + '<br /></br />';
  document.getElementById('result').innerHTML += parsedValue;
}
function onGlobalEnvButtonClick(event) {
  var keys = LispList2JSArray(car(car(G)));
  var values = LispList2JSArray(cdr(car(G)));
  var keysData = '';
  var valuesData = '';
  var value;
  for (var k = 0; k < keys.length; k++) {
    keysData += ' &nbsp; &nbsp;<b>' + keys[k] + '</b><br />';
    value = values[k];
    valuesData += ': ' + (value && value.constructor == Array && (value[0] == 'primitive' || value[0] == 'procedure') ? 'function' : value) + (k == keys.length - 1 ? '' : ',') + '<br />';
  }
  var globalEnvRes = 'Global environment:<br /><br />{<table class="env-table" valign=\'top\'><tr><td>' + keysData + '</td><td>' + valuesData + '</td></tr><table><span>}</span><br/><br/>';
  document.getElementById('result').innerHTML += globalEnvRes;
}

function showExample(event) {
  document.getElementById('source').value = this.innerHTML;
  onEvalButtonClick();
}

window.onload = function () {
  //window.setTimeout(onGlobalEnvButtonClick, 1);
  var examples = document.getElementsByTagName('pre');
  for (var k = examples.length; k--;) {
    examples[k].onclick = showExample;
  }
};