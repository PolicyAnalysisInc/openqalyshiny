import { describe, test, expect, beforeAll } from 'vitest'

beforeAll(async () => {
  window.OQGrid = {}
  await import('../../inst/www/grid/helpers/targeting.js')
})

const t = () => window.OQGrid.helpers.targeting

describe('OQGrid.helpers.targeting.getTargeting', () => {
  test('returns null strategies and groups when variable not in targeting', () => {
    const choices = { variableTargeting: {} }
    expect(t().getTargeting(choices, 'cost')).toEqual({ strategies: null, groups: null })
  })

  test('returns null when variableTargeting is absent', () => {
    expect(t().getTargeting({}, 'cost')).toEqual({ strategies: null, groups: null })
  })

  test('returns arrays when strategies and groups are arrays', () => {
    const choices = {
      variableTargeting: { cost: { strategies: ['s1', 's2'], groups: ['g1'] } }
    }
    const result = t().getTargeting(choices, 'cost')
    expect(result.strategies).toEqual(['s1', 's2'])
    expect(result.groups).toEqual(['g1'])
  })

  test('auto-boxes string strategies into array (R auto_unbox)', () => {
    const choices = {
      variableTargeting: { cost: { strategies: 'strat1', groups: null } }
    }
    const result = t().getTargeting(choices, 'cost')
    expect(result.strategies).toEqual(['strat1'])
  })

  test('normalizes empty object {} (R NULL serialization) to null', () => {
    const choices = {
      variableTargeting: { cost: { strategies: {}, groups: {} } }
    }
    const result = t().getTargeting(choices, 'cost')
    expect(result.strategies).toBeNull()
    expect(result.groups).toBeNull()
  })

  test('normalizes null to null', () => {
    const choices = {
      variableTargeting: { cost: { strategies: null, groups: null } }
    }
    expect(t().getTargeting(choices, 'cost')).toEqual({ strategies: null, groups: null })
  })
})

describe('OQGrid.helpers.targeting.strategyLabel', () => {
  const choices = { strategies: { 'Drug A': 'drug_a', 'Drug B': 'drug_b' } }

  test('returns display name for known value', () => {
    expect(t().strategyLabel(choices, 'drug_a')).toBe('Drug A')
    expect(t().strategyLabel(choices, 'drug_b')).toBe('Drug B')
  })

  test('returns the value itself for unknown', () => {
    expect(t().strategyLabel(choices, 'unknown')).toBe('unknown')
  })

  test('returns empty string for empty value', () => {
    expect(t().strategyLabel(choices, '')).toBe('')
    expect(t().strategyLabel(choices, null)).toBe('')
  })
})

describe('OQGrid.helpers.targeting.groupLabel', () => {
  const choices = { groups: { 'Group 1': 'g1', 'Group 2': 'g2' } }

  test('returns display name for known value', () => {
    expect(t().groupLabel(choices, 'g1')).toBe('Group 1')
  })

  test('returns value itself for unknown', () => {
    expect(t().groupLabel(choices, 'unknown')).toBe('unknown')
  })

  test('returns empty string for empty/null', () => {
    expect(t().groupLabel(choices, '')).toBe('')
    expect(t().groupLabel({groups: {}}, null)).toBe('')
  })
})

describe('OQGrid.helpers.targeting.getBaseCase', () => {
  const choices = {
    settingValues: { discount_rate: 0.035, cycle_length: 1 },
    variableFormulas: {
      'cost||': '1000 + 500',
      'cost|drug_a|': '1500',
      'utility|drug_a|g1': '0.8',
    }
  }

  test('returns setting value as string', () => {
    expect(t().getBaseCase(choices, { type: 'setting', name: 'discount_rate', strategy: '', group: '' }))
      .toBe('0.035')
  })

  test('returns empty string for unknown setting', () => {
    expect(t().getBaseCase(choices, { type: 'setting', name: 'unknown', strategy: '', group: '' }))
      .toBe('')
  })

  test('returns formula for variable with no strategy/group', () => {
    expect(t().getBaseCase(choices, { type: 'variable', name: 'cost', strategy: '', group: '' }))
      .toBe('1000 + 500')
  })

  test('returns formula for variable with strategy', () => {
    expect(t().getBaseCase(choices, { type: 'variable', name: 'cost', strategy: 'drug_a', group: '' }))
      .toBe('1500')
  })

  test('returns formula for variable with strategy and group', () => {
    expect(t().getBaseCase(choices, { type: 'variable', name: 'utility', strategy: 'drug_a', group: 'g1' }))
      .toBe('0.8')
  })

  test('returns empty string for unknown variable formula', () => {
    expect(t().getBaseCase(choices, { type: 'variable', name: 'unknown', strategy: '', group: '' }))
      .toBe('')
  })
})
