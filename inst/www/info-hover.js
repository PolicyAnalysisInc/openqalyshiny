(function() {
  var floating = null;
  function show(trigger) {
    var content = trigger.querySelector('.info-popover');
    if (!content) return;
    floating = document.createElement('div');
    floating.className = 'info-popover-floating';
    var text = document.createElement('span');
    text.textContent = content.textContent;
    floating.appendChild(text);
    var arrow = document.createElement('div');
    arrow.className = 'arrow';
    floating.appendChild(arrow);
    document.body.appendChild(floating);
    position(trigger);
  }
  function position(trigger) {
    if (!floating) return;
    var rect = trigger.getBoundingClientRect();
    var fw = floating.offsetWidth;
    var fh = floating.offsetHeight;
    var arrow = floating.querySelector('.arrow');
    var pad = 8;
    var above = rect.top - fh - 8;
    var below = rect.bottom + 8;
    var placeBelow = above < pad;
    var top = placeBelow ? below : above;
    var left = rect.left + rect.width / 2 - fw / 2;
    left = Math.max(pad, Math.min(left, window.innerWidth - fw - pad));
    floating.style.top = top + 'px';
    floating.style.left = left + 'px';
    var arrowLeft = rect.left + rect.width / 2 - left - 5;
    arrowLeft = Math.max(12, Math.min(arrowLeft, fw - 12));
    if (placeBelow) {
      arrow.style.top = '-4px';
      arrow.style.bottom = '';
      arrow.style.left = arrowLeft + 'px';
    } else {
      arrow.style.bottom = '-4px';
      arrow.style.top = '';
      arrow.style.left = arrowLeft + 'px';
    }
  }
  function hide() {
    if (floating) { floating.remove(); floating = null; }
  }
  $(document).on('mouseenter', '.info-trigger', function() { show(this); });
  $(document).on('mouseleave', '.info-trigger', hide);
})();
