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
        if (!value) return;
        var parts = String(value).split("|");
        if (parts.length === 2) {
          $(el).find(".timeframe-number").val(parts[0]);
          $(el).find(".timeframe-unit").val(parts[1]);
        }
      },

      subscribe: function(el, callback) {
        $(el).on("change.timeframeInputBinding input.timeframeInputBinding",
          ".timeframe-number, .timeframe-unit",
          function() {
            callback(false);
          }
        );
      },

      unsubscribe: function(el) {
        $(el).off(".timeframeInputBinding");
      },

      receiveMessage: function(el, data) {
        if (data.hasOwnProperty("value")) {
          this.setValue(el, data.value);
        }
        $(el).trigger("change");
      },

      getRatePolicy: function() {
        return { policy: "debounce", delay: 250 };
      }
    });

    Shiny.inputBindings.register(
      TimeframeInputBinding,
      "openqalyshiny.timeframeInput"
    );

    // Initialize any existing timeframe inputs
    $(".timeframe-input").each(function() {
      // Trigger initial binding
      $(this).trigger("change");
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
          numInput.trigger("change");
          break;

        case "slider":
          var sliderEl = $("#" + CSS.escape(overrideId));
          // Shiny slider uses ionRangeSlider
          var slider = sliderEl.data("ionRangeSlider");
          if (slider) {
            slider.update({ from: parseFloat(defaultValue) });
          }
          break;

        case "dropdown":
          var selectEl = $("#" + CSS.escape(overrideId));
          selectEl.val(defaultValue);
          selectEl.trigger("change");
          break;

        case "formula":
          // Formula inputs use Ace editor - find the container and set value
          var formulaEl = document.getElementById(overrideId);
          if (formulaEl && formulaEl._formulaEditor) {
            formulaEl._formulaEditor.setValue(
              defaultValue != null ? String(defaultValue) : "",
              -1
            );
            $(formulaEl).trigger("formula-input:change");
          }
          break;

        case "timeframe":
          var tfEl = $("#" + CSS.escape(overrideId));
          var parts = String(defaultValue || "1|year").split("|");
          if (parts.length === 2) {
            tfEl.find(".timeframe-number").val(parts[0]);
            tfEl.find(".timeframe-unit").val(parts[1]);
          }
          tfEl.find(".timeframe-number").trigger("change");
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
        var parts = String(value).split("|");
        if (parts.length === 2) {
          $(el).find(".timeframe-number").val(parts[0]);
          $(el).find(".timeframe-unit").val(parts[1]);
        }
        $(el).find(".timeframe-number").trigger("change");
      } else if ($(el).hasClass("formula-input")) {
        if (el._formulaEditor) {
          el._formulaEditor.setValue(value != null ? String(value) : "", -1);
          $(el).trigger("formula-input:change");
        }
      } else if (el.tagName === "SELECT") {
        $(el).val(value).trigger("change");
      } else if ($(el).hasClass("js-range-slider")) {
        var slider = $(el).data("ionRangeSlider");
        if (slider) {
          slider.update({ from: parseFloat(value) });
        }
      } else {
        // Numeric or other standard input
        $(el).val(value).trigger("change");
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

    // Rebind to pick up any existing elements
    Shiny.bindAll(document.body);

    console.log("overrideInput binding registered successfully");
  }).catch(function(error) {
    console.error("Failed to initialize overrideInput:", error);
  });
})();
