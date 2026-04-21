/* OQGrid — Shared Delete/Confirm Column */
(function() {
  "use strict";

  var OQGrid = window.OQGrid = window.OQGrid || {};
  OQGrid.columns = OQGrid.columns || {};

  // =========================================================================
  // Generates the delete column definition for a grid
  // Handles both:
  //   - _isNew rows with confirm/cancel buttons (when addRow.requireConfirm)
  //   - Regular rows with delete button
  // =========================================================================
  OQGrid.columns.deleteColumn = function(controller) {
    var spec = controller.spec;
    var addRow = spec.addRow || {};

    return {
      title: "",
      field: "_delete",
      width: 60,
      widthGrow: 0,
      hozAlign: "center",
      headerSort: false,
      editor: false,
      clipboard: false,
      formatter: function(cell) {
        var data = cell.getRow().getData();

        // New row with confirm/cancel buttons
        if (data._isNew && addRow.requireConfirm) {
          var wrapper = document.createElement("span");

          var confirmBtn = document.createElement("button");
          confirmBtn.type = "button";
          confirmBtn.className = "oq-grid-confirm-btn";
          confirmBtn.textContent = "\u2713";
          confirmBtn.addEventListener("click", function(e) {
            e.stopPropagation();
            var rowData = cell.getRow().getData();

            // Validate if spec provides validation
            if (spec.actions.addValidate) {
              var error = spec.actions.addValidate(rowData);
              if (error) {
                if (typeof Shiny !== "undefined" && Shiny.notifications) {
                  Shiny.notifications.show({ html: error, type: "warning", duration: 3000 });
                } else {
                  alert(error);
                }
                return;
              }
            }

            // Build and dispatch add payload
            var payload = spec.actions.add(rowData);
            cell.getRow().delete();
            OQGrid.relayout(controller.table);
            if (payload) {
              OQGrid.shiny.dispatch(controller.inputId, payload);
            }
          });

          var cancelBtn = document.createElement("button");
          cancelBtn.type = "button";
          cancelBtn.className = "oq-grid-cancel-btn";
          cancelBtn.textContent = "\u00d7";
          cancelBtn.addEventListener("click", function(e) {
            e.stopPropagation();
            cell.getRow().delete();
            OQGrid.relayout(controller.table);
          });

          wrapper.appendChild(confirmBtn);
          wrapper.appendChild(cancelBtn);
          return wrapper;
        }

        // Regular delete button
        var btn = document.createElement("button");
        btn.type = "button";
        btn.className = "oq-grid-delete-btn";
        btn.textContent = "\u00d7";
        btn.addEventListener("click", function(e) {
          e.stopPropagation();
          var rowData = cell.getRow().getData();
          var payload = spec.actions.remove(rowData);

          if (spec.dispatchMode === "sync") {
            cell.getRow().delete();
            OQGrid.relayout(controller.table);
            OQGrid.actions.sync.syncData(controller);
          } else {
            cell.getRow().delete();
            OQGrid.relayout(controller.table);
            if (payload) {
              OQGrid.shiny.dispatch(controller.inputId, payload);
            }
          }

          // Post-delete hook
          if (spec.onRowDeleted) {
            spec.onRowDeleted(controller, rowData);
          }
        });
        return btn;
      }
    };
  };
})();
