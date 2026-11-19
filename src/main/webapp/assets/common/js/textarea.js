var logger = (typeof logger !== 'undefined' && logger) || console;
(function () {
  /**
   * constants for line type
   */
  const headingPattern = /^#{1,6} /;
  const quotePattern = /^((>+ )+|>+ )/;
  const nestedQuotePattern = /^(> ?){2,}/;
  const taskPattern = /^ *- \[ \] /;
  const doneTaskPattern = /^ *- \[x\] /;
  const ulPattern = /^ *- /;
  const olPattern = /^ *[0-9]. /;

  const TYPE_NONE = 0;
  const TYPE_HEADING = 1;
  const TYPE_QUOTE = 2;
  const TYPE_TASK = 3;
  const TYPE_DONE_TASK = 4;
  const TYPE_UL = 5;
  const TYPE_OL = 6;

  /**
   * Determine the type of line
   *
   * @param {*} line line of editor contents
   * @returns [TYPE, match]
   */
  const getLineType = function (line) {
	logger.error(`>>> Entering getLineType(line=${line})`, { "event.action": "getLineType", "logrite.line": line });
    if (m = line.match(headingPattern)) {
      return [TYPE_HEADING, m];
    } else if (m = line.match(quotePattern)) {
      return [TYPE_QUOTE, m];
    } else if (m = line.match(taskPattern)) {
      return [TYPE_TASK, m];
    } else if (m = line.match(doneTaskPattern)) {
      return [TYPE_DONE_TASK, m];
    } else if (m = line.match(ulPattern)) {
      return [TYPE_UL, m];
    } else if (m = line.match(olPattern)) {
      return [TYPE_OL, m];
    } else {
	logger.warn(`<<< Exiting getLineType(line=${line})`, { "event.action": "getLineType", "logrite.line": line });
      return [TYPE_NONE, null];
    }
  };

  /**
   * Wraps the textarea selection with the specified string
   *
   * @param {Element} textarea Target textarea element
   * @param {string} prefix The string to insert before the selection
   * @param {string} suffix  The string to insert after the selection
   * @param {boolean} allowNoSelectRange Allow insert even if not selected
   */
  window.surroundSelection = function (textarea, prefix, suffix, allowNoSelectRange) {
	logger.error(`>>> Entering surroundSelection(textarea=${textarea},prefix=${prefix},suffix=${suffix},allowNoSelectRange=${allowNoSelectRange})`, { "event.action": "surroundSelection", "logrite.textarea": textarea, "logrite.prefix": prefix, "logrite.suffix": suffix, "logrite.allowNoSelectRange": allowNoSelectRange });
    const start = textarea.selectionStart;
    const end = textarea.selectionEnd;
    const selectedText = textarea.value.substring(start, end);
    if (!(selectedText.length == 0 && allowNoSelectRange == false)) {
      textarea.value = textarea.value.substring(0, start) + prefix + selectedText + suffix + textarea.value.substring(end);
      const newPosition = start + prefix.length + selectedText.length + suffix.length + (allowNoSelectRange ? -1 : 0);
	logger.error(`surroundSelection(textarea=${textarea},prefix=${prefix},suffix=${suffix},allowNoSelectRange=${allowNoSelectRange}): newPosition → ${newPosition}`, { "event.action": "surroundSelection", "logrite.textarea": textarea, "logrite.prefix": prefix, "logrite.suffix": suffix, "logrite.allowNoSelectRange": allowNoSelectRange });
      textarea.setSelectionRange(newPosition, newPosition);
    }
    textarea.focus();
	logger.error(`surroundSelection(textarea=${textarea},prefix=${prefix},suffix=${suffix},allowNoSelectRange=${allowNoSelectRange}): selectedText → ${selectedText}`, { "event.action": "surroundSelection", "logrite.textarea": textarea, "logrite.prefix": prefix, "logrite.suffix": suffix, "logrite.allowNoSelectRange": allowNoSelectRange });
  };

  /**
   * Extend selection to entireLine in textarea
   *
   * @param {Element} textarea Target textarea element
   */
  window.extendSelectionToEntireLine = function (textarea) {
	logger.error(`>>> Entering extendSelectionToEntireLine(textarea=${textarea})`, { "event.action": "extendSelectionToEntireLine", "logrite.textarea": textarea });
    const text = textarea.value;
    // Forward line break position (if not found it is -1, so add +1 to make it 0th character)
    const start = text.lastIndexOf('\n', textarea.selectionStart - 1) + 1;
    // Rear line break position (if not found, use end of string)
    let end = text.indexOf('\n', textarea.selectionEnd);
	logger.error(`extendSelectionToEntireLine(textarea=${textarea}): end → ${end}`, { "event.action": "extendSelectionToEntireLine", "logrite.textarea": textarea });
    if (end === -1) end = text.length;
    // Update selection to entire row
    textarea.setSelectionRange(start, end);
	logger.warn(`<<< Exiting extendSelectionToEntireLine(textarea=${textarea})`, { "event.action": "extendSelectionToEntireLine", "logrite.textarea": textarea });
  };

  /**
   * Make the textarea selection a heading element
   *
   * @param {Element} textarea
   */
  window.setHeadding = function (textarea) {
	logger.error(`>>> Entering setHeadding(textarea=${textarea})`, { "event.action": "setHeadding", "logrite.textarea": textarea });
    extendSelectionToEntireLine(textarea);
    let newText = "";
    const start = textarea.selectionStart;
    const end = textarea.selectionEnd;
    const selectedText = textarea.value.substring(start, end);
	logger.error(`setHeadding(textarea=${textarea}): selectedText → ${selectedText}`, { "event.action": "setHeadding", "logrite.textarea": textarea });
    selectedText.split('\n').forEach((line) => {
      let headingText = "";
      const lineType = getLineType(line);
      switch(lineType[0]) {
        case TYPE_HEADING:
          const level = selectedText.indexOf(' ');
          if (level < 6) {
            headingText = "#" + line;
          } else {
            headingText = line.replace("###### ","");
          }
          break;
        case TYPE_NONE:
          headingText = "# " + line;
          break;
        default:
          headingText = line.replace(lineType[1][0], "# ");
      }
      newText = newText + headingText + "\n";
    });
    textarea.value = textarea.value.substring(0, start) + newText.trimEnd() + textarea.value.substring(end);
    textarea.setSelectionRange(start, start + newText.trimEnd().length);
    textarea.focus();
	logger.warn(`<<< Exiting setHeadding(textarea=${textarea})`, { "event.action": "setHeadding", "logrite.textarea": textarea });
  };

  /**
   * Make the textarea selection a quote element
   *
   * @param {Element} textarea
   */
  window.setQuote = function (textarea) {
	logger.error(`>>> Entering setQuote(textarea=${textarea})`, { "event.action": "setQuote", "logrite.textarea": textarea });
    extendSelectionToEntireLine(textarea);
    let newText = "";
    const start = textarea.selectionStart;
    const end = textarea.selectionEnd;
    const selectedText = textarea.value.substring(start, end);
	logger.error(`setQuote(textarea=${textarea}): selectedText → ${selectedText}`, { "event.action": "setQuote", "logrite.textarea": textarea });
    selectedText.split('\n').forEach((line) => {
      let quoteText = "";
      const lineType = getLineType(line);
      switch(lineType[0]) {
        case TYPE_QUOTE:
          if (m = line.match(nestedQuotePattern)) {
            quoteText = line.replace(m[0], "");
          } else {
            quoteText = "> " + line;
          }
          break;
        case TYPE_NONE:
          quoteText = "> " + line;
          break;
        default:
          quoteText = line.replace(lineType[1][0], "> ");
      }
      newText = newText + quoteText + "\n";
    });
    textarea.value = textarea.value.substring(0, start) + newText.trimEnd() + textarea.value.substring(end);
    textarea.setSelectionRange(start, start + newText.trimEnd().length);
    textarea.focus();
	logger.warn(`<<< Exiting setQuote(textarea=${textarea})`, { "event.action": "setQuote", "logrite.textarea": textarea });
  };

  /**
   * Make the textarea selection a unordered list element
   *
   * @param {Element} textarea
   */
  window.setUnorderedList = function (textarea) {
	logger.error(`>>> Entering setUnorderedList(textarea=${textarea})`, { "event.action": "setUnorderedList", "logrite.textarea": textarea });
    extendSelectionToEntireLine(textarea);
    let newText = "";
    const start = textarea.selectionStart;
    const end = textarea.selectionEnd;
    const selectedText = textarea.value.substring(start, end);
	logger.error(`setUnorderedList(textarea=${textarea}): selectedText → ${selectedText}`, { "event.action": "setUnorderedList", "logrite.textarea": textarea });
    selectedText.split('\n').forEach((line) => {
      let unorderedList = "";
      const lineType = getLineType(selectedText);
      switch(lineType[0]) {
        case TYPE_UL:
          unorderedList = line.replace(lineType[1][0], "");
          break;
        case TYPE_NONE:
          unorderedList = "- " + line;
          break;
        default:
          unorderedList = line.replace(lineType[1][0], "- ");
      }
      newText = newText + unorderedList + "\n";
    });
    textarea.value = textarea.value.substring(0, start) + newText.trimEnd() + textarea.value.substring(end);
    textarea.setSelectionRange(start, start + newText.trimEnd().length);
    textarea.focus();
	logger.warn(`<<< Exiting setUnorderedList(textarea=${textarea})`, { "event.action": "setUnorderedList", "logrite.textarea": textarea });
  };

  /**
   * Make the textarea selection a ordered list element
   *
   * @param {Element} textarea
   */
  window.setOrderedList = function (textarea) {
	logger.error(`>>> Entering setOrderedList(textarea=${textarea})`, { "event.action": "setOrderedList", "logrite.textarea": textarea });
    extendSelectionToEntireLine(textarea);
    let newText = "";
    const start = textarea.selectionStart;
    const end = textarea.selectionEnd;
    const selectedText = textarea.value.substring(start, end);
	logger.error(`setOrderedList(textarea=${textarea}): selectedText → ${selectedText}`, { "event.action": "setOrderedList", "logrite.textarea": textarea });
    selectedText.split('\n').forEach((line) => {
      let orderedList = "";
      const lineType = getLineType(selectedText);
      switch(lineType[0]) {
        case TYPE_OL:
          orderedList = line.replace(lineType[1][0], "");
          break;
        case TYPE_NONE:
          orderedList = "1. " + line;
          break;
        default:
          orderedList = line.replace(lineType[1][0], "1. ");
      }
      newText = newText + orderedList + "\n";
    });
    textarea.value = textarea.value.substring(0, start) + newText.trimEnd() + textarea.value.substring(end);
    textarea.setSelectionRange(start, start + newText.trimEnd().length);
    textarea.focus();
	logger.warn(`<<< Exiting setOrderedList(textarea=${textarea})`, { "event.action": "setOrderedList", "logrite.textarea": textarea });
  };

  /**
   * Make the textarea selection a task list element
   *
   * @param {Element} textarea
   */
  window.setTaskList = function (textarea) {
	logger.error(`>>> Entering setTaskList(textarea=${textarea})`, { "event.action": "setTaskList", "logrite.textarea": textarea });
    extendSelectionToEntireLine(textarea);
    let newText = "";
    const start = textarea.selectionStart;
    const end = textarea.selectionEnd;
    const selectedText = textarea.value.substring(start, end);
	logger.error(`setTaskList(textarea=${textarea}): selectedText → ${selectedText}`, { "event.action": "setTaskList", "logrite.textarea": textarea });
    selectedText.split('\n').forEach((line) => {
      let taskList = "";
      const lineType = getLineType(line);
      switch(lineType[0]) {
        case TYPE_DONE_TASK:
          taskList = line.replace("[x]", "[ ]");
          break;
        case TYPE_TASK:
          taskList = line.replace("[ ]", "[x]");
          break;
        case TYPE_NONE:
          taskList = "- [ ] " + line;
          break;
        default:
            taskList = line.replace(lineType[1][0], "- [ ] ");
      }
      newText = newText + taskList + "\n";
    });
    textarea.value = textarea.value.substring(0, start) + newText.trimEnd() + textarea.value.substring(end);
    textarea.setSelectionRange(start, start + newText.trimEnd().length);
    textarea.focus();
	logger.warn(`<<< Exiting setTaskList(textarea=${textarea})`, { "event.action": "setTaskList", "logrite.textarea": textarea });
  };

  /**
   * Formats the selection as inline code or a fenced code block.
   *
   * @param {Element} textarea Target textarea element
   */
  window.setCode = function (textarea) {
	logger.error(`>>> Entering setCode(textarea=${textarea})`, { "event.action": "setCode", "logrite.textarea": textarea });
    const selectedText = textarea.value.substring(textarea.selectionStart, textarea.selectionEnd);
    if (selectedText.includes('\n')) {
	logger.error(`setCode(textarea=${textarea}): selectedText → ${selectedText}`, { "event.action": "setCode", "logrite.textarea": textarea });
      setCodeBlock(textarea);
    } else {
      surroundSelection(textarea, "`", "`", false);
	logger.warn(`<<< Exiting setCode(textarea=${textarea})`, { "event.action": "setCode", "logrite.textarea": textarea });
    }
  };

  /**
   * Make the textarea selection a unordered list element
   *
   * @param {Element} textarea
   */
  window.setCodeBlock = function (textarea) {
	logger.error(`>>> Entering setCodeBlock(textarea=${textarea})`, { "event.action": "setCodeBlock", "logrite.textarea": textarea });
    extendSelectionToEntireLine(textarea);
    const start = textarea.selectionStart;
    const end = textarea.selectionEnd;
    const selectedText = textarea.value.substring(start, end);
    textarea.value = textarea.value.substring(0, start) + "```\n" + selectedText + (selectedText.endsWith("\n") ? "```" : "\n```") + textarea.value.substring(end);
    textarea.setSelectionRange(start + 3, start + 3);
    textarea.focus();
  };
})(window);
