import { describe, test, expect, beforeAll, vi } from 'vitest'

beforeAll(async () => {
  await import('../../inst/www/formula-input-autocomplete.js')
})

const suggestions = {
  Variables: [
    { name: 'cost', label: 'cost', description: 'Treatment cost' },
    { name: 'utility', label: 'utility', description: 'Health utility' },
    { name: 'cycle_length', label: 'cycle_length' },
  ],
  Functions: [
    { name: 'cumsum', label: 'cumsum', signature: 'cumsum(x)', description: 'Cumulative sum' },
  ],
}

function getCompletions(completer, prefix) {
  return new Promise((resolve) => {
    completer.getCompletions(null, null, null, prefix, (err, matches) => resolve(matches))
  })
}

describe('FormulaInputAutocomplete.FormulaCompleter', () => {
  test('is accessible on window', () => {
    expect(window.FormulaInputAutocomplete).toBeDefined()
    expect(typeof window.FormulaInputAutocomplete.FormulaCompleter).toBe('function')
  })

  test('can be instantiated with null editor', () => {
    const completer = new window.FormulaInputAutocomplete.FormulaCompleter(null)
    expect(completer.completionList).toEqual([])
  })

  test('setSuggestions builds flat completion list', () => {
    const completer = new window.FormulaInputAutocomplete.FormulaCompleter(null, suggestions)
    expect(completer.completionList).toHaveLength(4)
  })

  test('completion items have expected shape', () => {
    const completer = new window.FormulaInputAutocomplete.FormulaCompleter(null, suggestions)
    const item = completer.completionList.find(i => i.name === 'cost')
    expect(item.value).toBe('cost')
    expect(item.meta).toBe('Variables')
    expect(item._description).toBe('Treatment cost')
  })

  test('getCompletions returns empty array for empty prefix', async () => {
    const completer = new window.FormulaInputAutocomplete.FormulaCompleter(null, suggestions)
    const matches = await getCompletions(completer, '')
    expect(matches).toEqual([])
  })

  test('getCompletions filters by prefix (case-insensitive)', async () => {
    const completer = new window.FormulaInputAutocomplete.FormulaCompleter(null, suggestions)
    const matches = await getCompletions(completer, 'co')
    expect(matches.map(m => m.name)).toContain('cost')
    expect(matches.map(m => m.name)).not.toContain('utility')
  })

  test('getCompletions is case-insensitive', async () => {
    const completer = new window.FormulaInputAutocomplete.FormulaCompleter(null, suggestions)
    const matches = await getCompletions(completer, 'Co')
    expect(matches.map(m => m.name)).toContain('cost')
  })

  test('getCompletions returns sorted results', async () => {
    const completer = new window.FormulaInputAutocomplete.FormulaCompleter(null, suggestions)
    const matches = await getCompletions(completer, 'c')
    const names = matches.map(m => m.name)
    expect(names).toEqual([...names].sort())
  })

  test('getCompletions returns empty array when no prefix matches', async () => {
    const completer = new window.FormulaInputAutocomplete.FormulaCompleter(null, suggestions)
    const matches = await getCompletions(completer, 'xyz')
    expect(matches).toEqual([])
  })

  test('getDocTooltip returns undefined when no description or signature', () => {
    const completer = new window.FormulaInputAutocomplete.FormulaCompleter(null)
    const result = completer.getDocTooltip({ _description: null, _signature: null })
    expect(result).toBeUndefined()
  })

  test('getDocTooltip returns docHTML when description present', () => {
    const completer = new window.FormulaInputAutocomplete.FormulaCompleter(null)
    const result = completer.getDocTooltip({ _description: 'some desc', _signature: null, _package: null })
    expect(result.docHTML).toContain('some desc')
  })
})
