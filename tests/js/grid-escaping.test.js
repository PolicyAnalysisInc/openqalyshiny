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
// grid formatters return escaped text
// =========================================================================
describe('formatters escape payload markup', () => {
  test('OQGrid.fmt.emdash escapes markup', () => {
    const out = window.OQGrid.fmt.emdash(fakeCell(PAYLOAD))
    expect(renderedMarkup(out)).not.toContain('<img')
  })

  test('OQGrid.fmt.cycleLimit escapes markup', () => {
    const out = window.OQGrid.fmt.cycleLimit(fakeCell(PAYLOAD))
    expect(renderedMarkup(out)).not.toContain('<img')
  })

  test('OQGrid.fmt.displayMap escapes markup for unmapped values', () => {
    const out = window.OQGrid.fmt.displayMap({ drug_a: 'Drug A' })(fakeCell(PAYLOAD))
    expect(renderedMarkup(out)).not.toContain('<img')
  })

  test('OQGrid.fmt.capitalize escapes markup', () => {
    const out = window.OQGrid.fmt.capitalize(fakeCell(PAYLOAD))
    expect(renderedMarkup(out)).not.toContain('<img')
  })

  // Every formatter column of each spec, given the payload as the cell value
  // and as the row's identifying fields.
  const specFixtures = {
    'psa-univariate': {
      strategies: { [PAYLOAD]: 's1' },
      groups: { [PAYLOAD]: 'g1' },
      variables: [PAYLOAD],
      variableTargeting: { [PAYLOAD]: { strategies: ['s1'], groups: ['g1'] } },
      variableInstances: [{ name: PAYLOAD, strategy: 's1', group: 'g1', sampling: PAYLOAD }],
      initial: []
    },
    'psa-multivariate': {
      strategies: { [PAYLOAD]: 's1' },
      groups: { [PAYLOAD]: 'g1' },
      variables: [PAYLOAD],
      initial: []
    },
    'dsa': {
      strategies: { [PAYLOAD]: 's1' },
      groups: { [PAYLOAD]: 'g1' },
      variables: [PAYLOAD],
      variableTargeting: { [PAYLOAD]: { strategies: ['s1'], groups: ['g1'] } },
      initial: []
    },
    'scenario': {
      strategies: { [PAYLOAD]: 's1' },
      groups: { [PAYLOAD]: 'g1' },
      variables: [PAYLOAD],
      variableTargeting: { [PAYLOAD]: { strategies: ['s1'], groups: ['g1'] } },
      initial: []
    },
    'twsa': {
      strategies: { [PAYLOAD]: 's1' },
      groups: { [PAYLOAD]: 'g1' },
      variables: [PAYLOAD],
      variableTargeting: { [PAYLOAD]: { strategies: ['s1'], groups: ['g1'] } },
      initial: []
    },
    'threshold': {
      strategies: { [PAYLOAD]: 's1' },
      groups: { [PAYLOAD]: 'g1' },
      variables: [PAYLOAD],
      variableTargeting: { [PAYLOAD]: { strategies: ['s1'], groups: ['g1'] } },
      initial: []
    }
  }

  Object.keys(specFixtures).forEach(function(specName) {
    test(`${specName} spec formatters escape payload markup`, () => {
      const spec = window.OQGrid._specs[specName]()
      const data = spec.parseData(fakeContainer(specFixtures[specName]))
      const defs = spec.getColumnDefs(data, {}).filter(function(d) { return d && typeof d.formatter === 'function' })
      expect(defs.length).toBeGreaterThan(0)

      const rowData = {
        name: PAYLOAD, variable: PAYLOAD, strategy: 's1', group: 'g1',
        variable_strategy: 's1', variable_group: 'g1',
        type: 'setting', param_type: 'setting', sampling: PAYLOAD
      }
      const offenders = []
      defs.forEach(function(def) {
        const out = def.formatter(fakeCell(PAYLOAD, rowData))
        if (renderedMarkup(out).indexOf('<img') >= 0) offenders.push(def.field || def.title)
      })
      expect(offenders).toEqual([])
    })
  })
})

