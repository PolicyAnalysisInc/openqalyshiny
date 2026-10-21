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
