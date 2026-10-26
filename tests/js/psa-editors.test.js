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
// MV builder passes a serialised string to parseDistributionString
// Drives the real add-multivariate modal builder (initAddMvModalBuilder) by
// firing the type <select>'s change event, which runs setMvDistributionBuilderType.
// Type "mvnormal" is used because it is the MV_DISTRIBUTIONS key that yields
// a non-null value from collectMvBuilderValue; "normal" is a univariate type.
// =========================================================================
describe('MV builder type change parses a string', () => {
  test('changing the MV type to mvnormal does not throw', () => {
    document.body.innerHTML =
      '<div id="add_mv_distribution_builder" data-terms="null" data-suggestions="null"></div>' +
      '<select id="add_mv_type"><option value="mvnormal" selected>mvnormal</option></select>'

    const errors = []
    const onError = function(e) { errors.push(e.error || e.message); e.preventDefault() }
    window.addEventListener('error', onError)
    try {
      window.OQGrid.editors._initAddMvModalBuilder()
      const typeInput = document.getElementById('add_mv_type')
      typeInput.dispatchEvent(new Event('change'))
    } finally {
      window.removeEventListener('error', onError)
      window.OQGrid.editors._cleanupAddMvModalBuilder()
      document.body.innerHTML = ''
    }
    expect(errors.map(String)).toEqual([])
  })
})

// =========================================================================
// MV strategy/group editor values are {label, value} pairs; the
// formatter shows the label.
// =========================================================================
describe('MV strategy and group editor values', () => {
  const spec = () => window.OQGrid._specs['psa-multivariate']()
  const choices = () => spec().parseData(fakeContainer({
    strategies: { 'Drug A': 'drug_a', 'Drug B': 'drug_b' },
    groups: { Male: 'male', Female: 'female' },
    variables: ['x']
  }))
  const column = (field) => spec().getColumnDefs(choices(), {}).filter(function(d) {
    return d && d.field === field
  })[0]

  test('Strategy editor values are {label, value} pairs including the (all) option', () => {
    const col = column('strategy')
    expect(col.editorParams.values).toEqual([
      { label: '(all)', value: '' },
      { label: 'Drug A', value: 'drug_a' },
      { label: 'Drug B', value: 'drug_b' }
    ])
  })

  test('Group editor values are {label, value} pairs including the (all) option', () => {
    const col = column('group')
    expect(col.editorParams.values).toEqual([
      { label: '(all)', value: '' },
      { label: 'Male', value: 'male' },
      { label: 'Female', value: 'female' }
    ])
  })

  test('Strategy formatter shows the label for a stored value', () => {
    expect(column('strategy').formatter(fakeCell('drug_a', {}))).toBe('Drug A')
  })

  test('Group formatter shows the label for a stored value', () => {
    expect(column('group').formatter(fakeCell('male', {}))).toBe('Male')
  })
})

// =========================================================================
// distribution editor opened on a non-distribution value ("0.5") and
// closed without changes commits the original value.
// =========================================================================
describe('distribution editor keeps an unchanged value', () => {
  test('opened on "0.5" and closed by an outside click commits "0.5"', () => {
    document.body.innerHTML = '<div id="outside">outside</div>'
    const success = vi.fn()
    const cancel = vi.fn()
    let rendered = null

    const editor = window.OQGrid.editors.distribution(null, null)
    editor(fakeCell('0.5', {}), function(fn) { rendered = fn }, success, cancel)
    expect(typeof rendered).toBe('function')
    rendered()

    document.getElementById('outside').dispatchEvent(new MouseEvent('mousedown', { bubbles: true }))
    document.body.innerHTML = ''

    expect(success).toHaveBeenCalledTimes(1)
    expect(success).toHaveBeenCalledWith('0.5')
  })
})
