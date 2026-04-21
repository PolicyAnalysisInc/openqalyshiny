import { describe, test, expect, beforeAll } from 'vitest'

beforeAll(async () => {
  window.OQGrid = {}
  await import('../../inst/www/grid/core/utils.js')
})

describe('OQGrid.utils.buildDisplayMap', () => {
  test('inverts display name to value mapping', () => {
    const map = window.OQGrid.utils.buildDisplayMap({ 'Drug A': 'drug_a', 'Drug B': 'drug_b' })
    expect(map).toEqual({ drug_a: 'Drug A', drug_b: 'Drug B' })
  })

  test('returns empty object for empty input', () => {
    expect(window.OQGrid.utils.buildDisplayMap({})).toEqual({})
  })

  test('handles null/undefined input', () => {
    expect(window.OQGrid.utils.buildDisplayMap(null)).toEqual({})
    expect(window.OQGrid.utils.buildDisplayMap(undefined)).toEqual({})
  })

  test('last value wins for duplicate values', () => {
    const map = window.OQGrid.utils.buildDisplayMap({ 'Label A': 'x', 'Label B': 'x' })
    expect(map['x']).toBe('Label B')
  })
})

describe('OQGrid.utils.toListValues', () => {
  test('converts object to label/value array', () => {
    const result = window.OQGrid.utils.toListValues({ 'Drug A': 'drug_a', 'Drug B': 'drug_b' })
    expect(result).toEqual([
      { label: 'Drug A', value: 'drug_a' },
      { label: 'Drug B', value: 'drug_b' },
    ])
  })

  test('prepends empty option when emptyLabel provided', () => {
    const result = window.OQGrid.utils.toListValues({ 'Drug A': 'drug_a' }, { emptyLabel: '— (None)' })
    expect(result[0]).toEqual({ label: '— (None)', value: '' })
    expect(result).toHaveLength(2)
  })

  test('returns only empty option for empty object with emptyLabel', () => {
    const result = window.OQGrid.utils.toListValues({}, { emptyLabel: '— (None)' })
    expect(result).toEqual([{ label: '— (None)', value: '' }])
  })

  test('returns empty array for empty object without emptyLabel', () => {
    expect(window.OQGrid.utils.toListValues({})).toEqual([])
  })

  test('handles null input', () => {
    expect(window.OQGrid.utils.toListValues(null)).toEqual([])
  })
})
