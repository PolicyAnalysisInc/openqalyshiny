// Override Input - Tabbed override cards for Shiny
// Provides TimeframeInputBinding, reset button handling, and numeric validation

(function() {
  "use strict";

  function waitForShiny() {
    return new Promise(function(resolve) {
      var check = function() {
        if (typeof Shiny !== "undefined" && Shiny.inputBindings) {
          resolve();
        } else {
          setTimeout(check, 50);
        }
      };
      check();
    });
  }

  waitForShiny().then(function() {
    function formatNumber(num, thousandSep, decimalSep) {
      var parts = num.toString().split(".");
      parts[0] = parts[0].replace(
        /(\d{1,3}(?=(?:\d\d\d)+(?!\d)))/g,
        "$1" + thousandSep
      );

      if (parts.length === 1) return parts[0];
      if (parts.length === 2) return parts[0] + decimalSep + parts[1];

      return "";
    }

    function getSliderPrettifyer(dataType, timeFormat, timezone) {
      var timeFormatter;

      if (dataType === "date") {
        timeFormatter = window.strftime.utc();
        return function(num) {
          return timeFormatter(timeFormat, new Date(num));
        };
      }

      if (dataType === "datetime") {
        timeFormatter = timezone ? window.strftime.timezone(timezone) : window.strftime;
        return function(num) {
          return timeFormatter(timeFormat, new Date(num));
        };
      }

      return function(num) {
        return formatNumber(num, this.prettify_separator, ".");
      };
    }

    function getOverrideSliderNumValues(el) {
      var slider = $(el).data("ionRangeSlider");
      if (!slider) return 1;
      return slider.options.type === "double" ? 2 : 1;
    }

    function setOverrideSliderValue(el, value) {
      var $el = $(el);
      var slider = $el.data("ionRangeSlider");

      if (!slider) return;

      $el.data("overrideSliderSilent", true);
      try {
        if (getOverrideSliderNumValues(el) === 2 && Array.isArray(value)) {
          slider.update({ from: value[0], to: value[1] });
        } else {
          slider.update({ from: value });
        }
      } finally {
        $el.data("overrideSliderSilent", false);
      }
    }

    function commitOverrideSliderValue(el) {
      if (!el) return;
      $(el).trigger("override-slider:commit");
    }

    function setTimeframeValue(el, value) {
      if (!value) return;

      var $el = $(el);
      var parts = String(value).split("|");

      $el.data("timeframeSilent", true);
      try {
        if (parts.length === 2) {
          $el.find(".timeframe-number").val(parts[0]);
          $el.find(".timeframe-unit").val(parts[1]);
        }
      } finally {
        el._timeframeDirty = false;
        $el.data("timeframeSilent", false);
      }
    }

    function commitTimeframeValue(el) {
      if (!el) return;
      el._timeframeDirty = false;
      $(el).trigger("timeframe-input:commit");
    }

    function setFormulaValue(el, value) {
      if (!el) return;

      if (typeof el._formulaSetValue === "function") {
        el._formulaSetValue(value);
        return;
      }

      if (el._formulaEditor) {
        el._formulaSuppressChange = true;
        try {
          el._formulaEditor.setValue(value != null ? String(value) : "", -1);
        } finally {
          el._formulaDirty = false;
          el._formulaSuppressChange = false;
        }
      }
    }

    function commitFormulaValue(el) {
      if (!el) return;
      var updateOn = el.getAttribute("data-update-on") || "change";
      if (updateOn === "blur") {
        el._formulaDirty = false;
        $(el).trigger("formula-input:commit");
      } else {
        $(el).trigger("formula-input:change");
      }
    }

    // =========================================================================
    // TimeframeInputBinding - combines numeric + unit select into "number|unit"
    // =========================================================================
    var TimeframeInputBinding = new Shiny.InputBinding();

    $.extend(TimeframeInputBinding, {
      find: function(scope) {
        return $(scope).find(".timeframe-input");
      },

      getValue: function(el) {
        var number = $(el).find(".timeframe-number").val() || "1";
        var unit = $(el).find(".timeframe-unit").val() || "year";
        return number + "|" + unit;
      },

      setValue: function(el, value) {
        setTimeframeValue(el, value);
      },

      subscribe: function(el, callback) {
        el._timeframeDirty = false;

        $(el).on("input.timeframeInputBinding change.timeframeInputBinding",
          ".timeframe-number, .timeframe-unit",
          function() {
            if ($(el).data("timeframeSilent")) {
              return;
            }

            el._timeframeDirty = true;
          }
        );

        $(el).on("timeframe-input:commit.timeframeInputBinding", function() {
          el._timeframeDirty = false;
          callback(false);
        });

        $(el).on("keydown.timeframeInputBinding", ".timeframe-number", function(event) {
          if (event.key !== "Enter") return;
          commitTimeframeValue(el);
        });

        $(el).on("focusout.timeframeInputBinding", function() {
          setTimeout(function() {
            if (el.contains(document.activeElement) || !el._timeframeDirty) {
              return;
            }

            commitTimeframeValue(el);
          }, 0);
        });
      },

      unsubscribe: function(el) {
        $(el).off(".timeframeInputBinding");
      },

      receiveMessage: function(el, data) {
        if (data.hasOwnProperty("value")) {
          this.setValue(el, data.value);
        }
      }
    });

    Shiny.inputBindings.register(
      TimeframeInputBinding,
      "openqalyshiny.timeframeInput"
    );

    // =========================================================================
    // OverrideSliderInputBinding - commits only when slider interaction finishes
    // =========================================================================
    var OverrideSliderInputBinding = new Shiny.InputBinding();

    $.extend(OverrideSliderInputBinding, {
      find: function(scope) {
        return $(scope).find("input.override-slider-input");
      },

      getType: function(el) {
        var dataType = $(el).data("data-type");
        if (dataType === "date") return "shiny.date";
        if (dataType === "datetime") return "shiny.datetime";
        return null;
      },

      getValue: function(el) {
        var slider = $(el).data("ionRangeSlider");
        var dataType = $(el).data("data-type");
        var result = slider ? slider.result : null;

        function convert(val) {
          if (dataType === "date") {
            return window.strftime.utc("%F", new Date(Number(val)));
          }
          if (dataType === "datetime") {
            return Number(val) / 1000;
          }
          return Number(val);
        }

        if (!result) {
          return Number($(el).attr("data-from"));
        }

        if (getOverrideSliderNumValues(el) === 2) {
          return [convert(result.from), convert(result.to)];
        }

        return convert(result.from);
      },

      setValue: function(el, value) {
        setOverrideSliderValue(el, value);
      },

      subscribe: function(el, callback) {
        $(el).on("override-slider:commit.overrideSliderInputBinding", function() {
          callback(false);
        });
      },

      unsubscribe: function(el) {
        $(el).off(".overrideSliderInputBinding");
      },

      receiveMessage: function(el, data) {
        var $el = $(el);
        var slider = $el.data("ionRangeSlider");
        var msg = {};
        var features = ["min", "max", "step"];
        var i;

        if (!slider) return;

        if (data.hasOwnProperty("value")) {
          if (getOverrideSliderNumValues(el) === 2 && Array.isArray(data.value)) {
            msg.from = data.value[0];
            msg.to = data.value[1];
          } else {
            msg.from = data.value;
          }
        }

        for (i = 0; i < features.length; i++) {
          if (data.hasOwnProperty(features[i])) {
            msg[features[i]] = data[features[i]];
          }
        }

        $el.data("overrideSliderSilent", true);
        try {
          slider.update(msg);
        } finally {
          $el.data("overrideSliderSilent", false);
        }
      },

      initialize: function(el) {
        var $el = $(el);
        var dataType = $el.data("data-type");
        var timeFormat = $el.data("time-format");
        var timezone = $el.data("timezone");

        if ($el.data("ionRangeSlider")) return;

        $el.ionRangeSlider({
          prettify: getSliderPrettifyer(dataType, timeFormat, timezone),
          onFinish: function() {
            if ($el.data("overrideSliderSilent")) {
              return;
            }

            commitOverrideSliderValue(el);
          }
        });
      }
    });

    Shiny.inputBindings.register(
      OverrideSliderInputBinding,
      "openqalyshiny.overrideSliderInput"
    );

    $(document).on("mousedown", ".override-reset-btn", function(event) {
      event.preventDefault();
    });

    // =========================================================================
    // Reset button handler
    // =========================================================================
    $(document).on("click", ".override-reset-btn", function() {
      var btn = $(this);
      var overrideId = btn.data("override-id");
      var defaultValue = btn.data("default-value");
      var inputType = btn.data("input-type");

      if (!overrideId) return;

      switch (inputType) {
        case "numeric":
          var numInput = $("#" + CSS.escape(overrideId));
          numInput.val(defaultValue);
          if (document.activeElement === numInput[0]) {
            numInput.trigger("blur");
          } else {
            numInput.trigger("change");
          }
          break;

        case "slider":
          var sliderEl = $("#" + CSS.escape(overrideId));
          setOverrideSliderValue(sliderEl[0], parseFloat(defaultValue));
          commitOverrideSliderValue(sliderEl[0]);
          break;

        case "dropdown":
          var selectEl = $("#" + CSS.escape(overrideId));
          selectEl.val(defaultValue);
          selectEl.trigger("change");
          break;

        case "formula":
          var formulaEl = document.getElementById(overrideId);
          setFormulaValue(formulaEl, defaultValue);
          commitFormulaValue(formulaEl);
          break;

        case "timeframe":
          var tfEl = $("#" + CSS.escape(overrideId));
          setTimeframeValue(tfEl[0], defaultValue || "1|year");
          commitTimeframeValue(tfEl[0]);
          break;
      }
    });

    // =========================================================================
    // Update handler for updateOverrideInput from R
    // =========================================================================
    Shiny.addCustomMessageHandler("override-input-update", function(message) {
      var id = message.id;
      var value = message.value;
      var el = document.getElementById(id);

      if (!el) return;

      // Detect input type from element
      if ($(el).hasClass("timeframe-input")) {
        setTimeframeValue(el, value);
      } else if ($(el).hasClass("formula-input")) {
        setFormulaValue(el, value);
      } else if (el.tagName === "SELECT") {
        $(el).val(value);
      } else if ($(el).hasClass("override-slider-input")) {
        setOverrideSliderValue(el, parseFloat(value));
      } else {
        $(el).val(value);
      }
    });

    // =========================================================================
    // Numeric validation feedback
    // =========================================================================
    $(document).on("input change", ".override-card input[type='number']", function() {
      var input = $(this);
      var val = parseFloat(input.val());
      var min = parseFloat(input.attr("min"));
      var max = parseFloat(input.attr("max"));
      var card = input.closest(".override-card");

      // Remove existing validation message
      card.find(".override-validation-error").remove();

      if (isNaN(val)) return;

      var msg = null;
      if (!isNaN(min) && val < min) {
        msg = "Value must be at least " + min;
      } else if (!isNaN(max) && val > max) {
        msg = "Value must be at most " + max;
      }

      if (msg) {
        var errorEl = $('<div class="override-validation-error"></div>').text(msg);
        input.after(errorEl);
      }
    });

    // =========================================================================
    // Gear button click handler - opens override manager
    // =========================================================================
    $(document).on("click", ".override-manage-btn", function() {
      var btn = $(this);
      var inputId = btn.data("input-id");
      if (!inputId) return;
      Shiny.setInputValue(inputId + "-manage_click", Date.now(), {priority: "event"});
    });

    // Rebind to pick up any existing elements
    Shiny.bindAll(document.body);

    console.log("overrideInput binding registered successfully");
  }).catch(function(error) {
    console.error("Failed to initialize overrideInput:", error);
  });
})();
