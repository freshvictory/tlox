/**
 * Editor functionality like auto indentation.
 * Because some things are tough to do in Elm.
 */

export default function setupEditor() {
  const editor = document.getElementById('code-input');
  editor.addEventListener('keydown', (e) => {
    switch (e.code) {
      case 'BracketRight': {
        e.preventDefault();
        deindent(editor);
        const currentIndentLevel = getCurrentIndent(editor);
        const tabs = new Array(currentIndentLevel + 1).join('\t');
        insertText('}\n' + tabs, editor);
        issueEvent(editor);
        return;
      }
      case 'BracketLeft': {
        e.preventDefault();
        const currentIndentLevel = getCurrentIndent(editor);
        const tabs = new Array(currentIndentLevel + 2).join('\t');
        insertText('{\n' + tabs, editor);
        issueEvent(editor);
        return;
      }
      case 'Enter': {
        e.preventDefault();
        const currentIndentLevel = getCurrentIndent(editor);
        const tabs = new Array(currentIndentLevel + 1).join('\t');
        insertText('\n' + tabs, editor);
        issueEvent(editor);
        return;
      }
      default:
        return;
    }
  });
}

function deindent(input) {
  const { selectionStart, selectionEnd } = input;
  input.value =
    input.value.slice(0, selectionStart - 1)
    + input.value.slice(selectionEnd);

  input.setSelectionRange(selectionStart - 1, selectionEnd - 1);
}

function issueEvent(input) {
  const event = new Event('input', {
      bubbles: true,
      cancelable: true,
  });

  input.dispatchEvent(event);
}

function insertText(text, input) {
  const { selectionStart, selectionEnd } = input;
  input.value =
    input.value.slice(0, selectionStart)
    + text
    + input.value.slice(selectionEnd);

  input.setSelectionRange(selectionStart + text.length, selectionStart + text.length);
  return;
}

function getCurrentIndent(input) {
  let indentLevel = 0;
  let i = input.selectionStart;
  do {
    if (input.value[i] === '\t') {
      indentLevel++;
    } else {
      indentLevel = 0;
    }
    i--;
  } while (i >= 0 && input.value[i] !== '\n');

  return indentLevel;
}
