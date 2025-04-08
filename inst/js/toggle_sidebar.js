/*import { adjust_grid } from './adjust_grid.js';

$('button[id$="-toggle_sidebar"]').on('click', function() {
  var sidebar = $('div[id$="-sidebar"]');
  sidebar.toggleClass('collapsed');
  // Change the icon based on the sidebar state
  var icon = $(this).find('i'); // find icon
  if (sidebar.hasClass('collapsed')) {
    icon.removeClass('fas fa-angles-left').addClass('fas fa-angles-right');
  } else {
    icon.removeClass('fas fa-angles-right').addClass('fas fa-angles-left');
  }
  // call adjust_grid after toggle
  setTimeout(adjust_grid, 100)
});*/


import { adjust_grid } from './adjust_grid.js';

$('button[id$="-toggle_sidebar"]').on('click', function() {
  const container = $('div[id$="-main_container"]'); // the wrapping div
  const icon = $(this).find('i'); // find icon

  container.toggleClass('sidebar-collapsed');

  // Toggle icon direction
  if (container.hasClass('sidebar-collapsed')) {
    icon.removeClass('fa-angles-left').addClass('fa-angles-right');
  } else {
    icon.removeClass('fa-angles-right').addClass('fa-angles-left');
  }

  // call adjust_grid after toggle
  setTimeout(adjust_grid, 100);
});
