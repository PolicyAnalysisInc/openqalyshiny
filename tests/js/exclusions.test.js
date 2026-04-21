import { describe, test, expect, beforeAll } from 'vitest'

beforeAll(async () => {
  window.OQGrid = {}
  await import('../../inst/www/grid/helpers/settings-exclusions.js')
})

const exclusions = () => window.OQGrid.helpers.exclusions

describe('OQGrid.helpers.exclusions.SETTING_EXCLUSIONS', () => {
  test('discount_rate excludes discount_cost and discount_outcomes', () => {
    expect(exclusions().SETTING_EXCLUSIONS.discount_rate).toContain('discount_cost')
    expect(exclusions().SETTING_EXCLUSIONS.discount_rate).toContain('discount_outcomes')
  })

  test('discount_cost excludes discount_rate', () => {
    expect(exclusions().SETTING_EXCLUSIONS.discount_cost).toContain('discount_rate')
  })
})

describe('OQGrid.helpers.exclusions.expandWithExclusions', () => {
  test('expands discount_rate to include its exclusions', () => {
    const used = new Set(['discount_rate'])
    exclusions().expandWithExclusions(used)
    expect(used.has('discount_cost')).toBe(true)
    expect(used.has('discount_outcomes')).toBe(true)
  })

  test('expands discount_cost to include discount_rate', () => {
    const used = new Set(['discount_cost'])
    exclusions().expandWithExclusions(used)
    expect(used.has('discount_rate')).toBe(true)
  })

  test('does not modify set when no exclusions apply', () => {
    const used = new Set(['cycle_length'])
    exclusions().expandWithExclusions(used)
    expect(used.size).toBe(1)
    expect(used.has('cycle_length')).toBe(true)
  })

  test('mutates the set in-place', () => {
    const used = new Set(['discount_rate'])
    const result = exclusions().expandWithExclusions(used)
    expect(result).toBeUndefined()
    expect(used.size).toBeGreaterThan(1)
  })

  test('handles empty set', () => {
    const used = new Set()
    exclusions().expandWithExclusions(used)
    expect(used.size).toBe(0)
  })
})
