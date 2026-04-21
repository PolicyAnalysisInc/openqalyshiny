import { describe, test, expect, beforeAll } from 'vitest'

beforeAll(async () => {
  window.OQGrid = {}
  await import('../../inst/www/grid/helpers/targeting.js')
  await import('../../inst/www/grid/helpers/settings-exclusions.js')
  await import('../../inst/www/grid/helpers/combo-tracking.js')
})

const combo = () => window.OQGrid.helpers.combo

describe('OQGrid.helpers.combo.compositeKey', () => {
  test('builds pipe-separated key', () => {
    expect(combo().compositeKey('cost', 'strat1', 'g1')).toBe('cost|strat1|g1')
  })

  test('normalizes null to empty string', () => {
    expect(combo().compositeKey('cost', null, null)).toBe('cost||')
  })

  test('normalizes undefined to empty string', () => {
    expect(combo().compositeKey('cost', undefined, '')).toBe('cost||')
  })

  test('normalizes object to empty string', () => {
    expect(combo().compositeKey('cost', {}, 'g1')).toBe('cost||g1')
  })

  test('handles all-empty parts', () => {
    expect(combo().compositeKey('', '', '')).toBe('||')
  })
})

describe('OQGrid.helpers.combo.getUsedCombosForVariable', () => {
  function makeTable(combos) {
    return { _rowCombos: combos }
  }

  test('returns empty set when no rows match variable name', () => {
    const table = makeTable({
      1: { type: 'variable', name: 'utility', strategy: 's1', group: '' },
    })
    expect(combo().getUsedCombosForVariable(table, 'cost', null).size).toBe(0)
  })

  test('returns used strategy|group combos for matching variable', () => {
    const table = makeTable({
      1: { type: 'variable', name: 'cost', strategy: 's1', group: '' },
      2: { type: 'variable', name: 'cost', strategy: 's2', group: 'g1' },
      3: { type: 'variable', name: 'utility', strategy: 's1', group: '' },
    })
    const used = combo().getUsedCombosForVariable(table, 'cost', null)
    expect(used.has('s1|')).toBe(true)
    expect(used.has('s2|g1')).toBe(true)
    expect(used.size).toBe(2)
  })

  test('excludes the row with matching excludeId', () => {
    const table = makeTable({
      '1': { type: 'variable', name: 'cost', strategy: 's1', group: '' },
      '2': { type: 'variable', name: 'cost', strategy: 's2', group: '' },
    })
    const used = combo().getUsedCombosForVariable(table, 'cost', '1')
    expect(used.has('s1|')).toBe(false)
    expect(used.has('s2|')).toBe(true)
  })
})

describe('OQGrid.helpers.combo.allVariableCombos', () => {
  test('returns all combos for untargeted variable (no strategies/groups)', () => {
    const choices = {
      variables: ['cost'],
      variableTargeting: { cost: { strategies: null, groups: null } }
    }
    const combos = combo().allVariableCombos(choices)
    expect(combos).toHaveLength(1)
    expect(combos[0]).toEqual({ name: 'cost', strategy: '', group: '' })
  })

  test('returns cartesian product of strategies and groups', () => {
    const choices = {
      variables: ['cost'],
      variableTargeting: { cost: { strategies: ['s1', 's2'], groups: ['g1', 'g2'] } }
    }
    const combos = combo().allVariableCombos(choices)
    expect(combos).toHaveLength(4)
    expect(combos).toContainEqual({ name: 'cost', strategy: 's1', group: 'g1' })
    expect(combos).toContainEqual({ name: 'cost', strategy: 's2', group: 'g2' })
  })

  test('handles strategy-only targeting', () => {
    const choices = {
      variables: ['cost'],
      variableTargeting: { cost: { strategies: ['s1', 's2'], groups: null } }
    }
    const combos = combo().allVariableCombos(choices)
    expect(combos).toHaveLength(2)
    expect(combos[0]).toEqual({ name: 'cost', strategy: 's1', group: '' })
    expect(combos[1]).toEqual({ name: 'cost', strategy: 's2', group: '' })
  })

  test('handles multiple variables', () => {
    const choices = {
      variables: ['cost', 'utility'],
      variableTargeting: {
        cost: { strategies: ['s1'], groups: null },
        utility: { strategies: null, groups: null },
      }
    }
    const combos = combo().allVariableCombos(choices)
    expect(combos).toHaveLength(2)
    expect(combos.map(c => c.name)).toEqual(['cost', 'utility'])
  })
})
