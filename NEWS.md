# openqalyshiny 1.1.0

## Values and settings (requires openqaly >= 1.0.0)

* Values grid: new **Half-Cycle Method** column. Leave it empty to use the
  model setting; all rows of a value share one setting, so editing it on one
  row updates the others.
* Values grid: **Discounting Override** now uses the formula editor, with
  suggestions for `discount_rate`, `discount_factors`, `discount_time` and
  `trace("state")`.
* Values grid: PSM models now have a **Destination** column, limited to the
  two PSM transitions (state 1 -> state 2 and state 2 -> state 3).
* Settings: new **Discount Method** control (by cycle / by year), also
  available as a DSA setting.
* Half-cycle help text explains that the method also sets when values are
  discounted, and how to use the per-value setting for one-off costs.
* Model run errors with long messages (such as error tables) show a
  "Show details" link that opens the full message.
* Opening a model that fails validation at startup, or after saving, shows an
  error instead of breaking the session.

# openqalyshiny 1.0.2

## New features

* `register_editor_extension()` gains a `type` parameter accepting `"page"`
  (default) or `"overlay"`. Page extensions render as before — a dropdown
  menu item plus an `.app-page` div. Overlay extensions render inside a
  fixed-positioned `.editor-overlay` wrapper at the model editor's top
  level, suitable for floating widgets (e.g. support-chat-style
  launchers + panels). Existing extension registrations are unchanged.
* `register_editor_extension()` now rejects `tab_id` values that collide
  with built-in editor pages (`documentation`, `editor`, `results`,
  `vbp`, `dsa`, `scenario`, `twsa`, `threshold`, `psa`).
* `register_editor_extension()` now errors when a `tab_id` is
  re-registered with a different `type`. Same-type re-registration
  continues to overwrite the prior entry.
* `register_editor_extension()` accepts an optional `icon` argument
  prepended to the dropdown label for page extensions.

## CSS

* New `.editor-overlay` rule: `position: fixed; inset: 0; z-index: 9998;
  pointer-events: none;`. Overlay extensions are responsible for
  enabling `pointer-events: auto` on their own visible affordances.
