// Button click handler
$(document).on('click', 'button[id^="ghqc_status_app-modal_btn_"]', function() {
  Shiny.setInputValue(this.id, Math.random());
});

// Checkbox change handler for `show_qcer`
$(document).on('shiny:connected', function() {
  var checkbox = document.getElementById('show_qcer');
  if (checkbox) {
    Shiny.setInputValue('show_qcer', checkbox.checked);
    checkbox.addEventListener('change', function() {
      Shiny.setInputValue('show_qcer', checkbox.checked);
    });
  }
});

function triggerDefaultAction(id, action) {
  console.log("triggerDefaultAction called with:", id, action);
  let labelMap = {
    'Notify file changes': 'btn-info',
    'Approve': 'btn-success',
    'Repost last QC Notification': 'btn-plum',
    'Notify latest commit': 'btn-plum',
    'Unapprove': 'btn-danger'
  };

  let btnMain = document.getElementById('main-btn-' + id);
  let btnCaret = document.getElementById('caret-btn-' + id);
  let btnClass = labelMap[action] || 'btn-default';

  if (btnMain) {
    btnMain.innerText = action;
    btnMain.className = 'btn btn-sm ' + btnClass;
  }

  if (btnCaret) {
    btnCaret.className = 'btn btn-sm dropdown-toggle ' + btnClass;
  }

  if (action === 'Approve') {
    console.log("Setting input value:", ns_prefix + 'show_approve_modal_row');
    Shiny.setInputValue(ns_prefix + 'show_approve_modal_row', { row: parseInt(id), nonce: Math.random() });
  } else if (action === 'Notify file changes' || action === 'Notify latest commit') {
    Shiny.setInputValue(ns_prefix + 'show_notify_modal_row', { row: parseInt(id), nonce: Math.random() });
  } else {
    Shiny.setInputValue(ns_prefix + 'action_' + id, action, {priority: 'event'});
  }

}
