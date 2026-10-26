import { describe, test, expect, beforeAll, vi } from 'vitest'

// Markup payload: any formatter output containing "<img" renders it as HTML.
const PAYLOAD = '<img src=x onerror=alert(1)>'

// Vendored DOMPurify build (copied from node_modules/dompurify/dist).
const PURIFY_PATH = '../../inst/www/vendor/purify.min.js'

beforeAll(async () => {
  window.OQGrid = {}
  await import('../../inst/www/grid/core/utils.js')
  await import('../../inst/www/grid/core/grid-factory.js')
  await import('../../inst/www/grid/formatters/formatters.js')
  await import('../../inst/www/grid/helpers/distribution-registry.js')
  await import('../../inst/www/grid/helpers/distribution-parser.js')
  await import('../../inst/www/grid/helpers/targeting.js')
  await import('../../inst/www/grid/helpers/combo-tracking.js')
  await import('../../inst/www/grid/helpers/settings-exclusions.js')
  await import('../../inst/www/grid/editors/distribution-editor.js')
  await import('../../inst/www/grid/editors/mv-distribution-editor.js')
  await import('../../inst/www/grid/editors/formula-editor.js')
  await import('../../inst/www/grid/editors/values-popup-editor.js')
  await import('../../inst/www/grid/editors/condition-editor.js')
  await import('../../inst/www/grid/editors/multi-tag-editor.js')
  await import('../../inst/www/grid/editors/typeahead-editor.js')
  await import('../../inst/www/grid/editors/numeric-editor.js')
  await import('../../inst/www/grid/specs/psa-spec.js')
  await import('../../inst/www/grid/specs/dsa-spec.js')
  await import('../../inst/www/grid/specs/scenario-spec.js')
  await import('../../inst/www/grid/specs/twsa-spec.js')
  await import('../../inst/www/grid/specs/threshold-spec.js')
})

// Minimal Tabulator-like cell. getValue returns the payload; the row data is
// supplied by the caller so spec formatters can read their row fields.
function fakeCell(value, rowData) {
  var data = rowData || {}
  return {
    getValue: function() { return value },
    getRow: function() {
      return { getData: function() { return data }, getCells: function() { return [] } }
    },
    getData: function() { return data },
    getTable: function() { return null },
    getElement: function() { return document.createElement('div') },
    getField: function() { return null }
  }
}

// Build a container-like object whose dataset holds JSON strings, which is
// what the spec parseData functions read via OQGrid.utils.parseDataAttr.
function fakeContainer(attrs) {
  return {
    dataset: new Proxy({}, {
      get: function(_t, key) {
        return Object.prototype.hasOwnProperty.call(attrs, key) ? JSON.stringify(attrs[key]) : undefined
      }
    })
  }
}

// Serialised form of a formatter result, for checking that it contains no markup.
function renderedMarkup(out) {
  if (out === null || out === undefined) return ''
  if (typeof out === 'string') return out
  if (typeof out.outerHTML === 'string') return out.outerHTML
  return String(out)
}

// =========================================================================
// sanitiser used by documentation-editor.js
// OQGrid.utils.sanitizeHtml(html) delegates to the vendored DOMPurify.
// =========================================================================
describe('documentation sanitiser', () => {
  test('OQGrid.utils.sanitizeHtml is a function', () => {
    expect(typeof window.OQGrid.utils.sanitizeHtml).toBe('function')
  })

  test('strips <script> and onerror handlers', async () => {
    if (!window.DOMPurify) await import(PURIFY_PATH)
    const out = window.OQGrid.utils.sanitizeHtml(
      '<p>ok</p><script>alert(1)</script><img src=x onerror=alert(1)>'
    )
    expect(out).toContain('<p>ok</p>')
    expect(out).not.toMatch(/<script/i)
    expect(out).not.toMatch(/onerror/i)
  })
})

