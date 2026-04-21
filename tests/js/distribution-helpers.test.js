import { describe, test, expect, beforeAll } from 'vitest'

beforeAll(async () => {
  window.OQGrid = {}
  await import('../../inst/www/grid/helpers/distribution-registry.js')
  await import('../../inst/www/grid/helpers/distribution-parser.js')
})

describe('OQGrid.helpers.distributions.parseDistributionString', () => {
  test('returns null for empty input', () => {
    expect(window.OQGrid.helpers.distributions.parseDistributionString('')).toBeNull()
    expect(window.OQGrid.helpers.distributions.parseDistributionString(null)).toBeNull()
    expect(window.OQGrid.helpers.distributions.parseDistributionString('  ')).toBeNull()
  })

  test('returns null when no opening paren', () => {
    expect(window.OQGrid.helpers.distributions.parseDistributionString('normal')).toBeNull()
  })

  test('parses normal distribution with named params', () => {
    const result = window.OQGrid.helpers.distributions.parseDistributionString('normal(mean = 0, sd = 1)')
    expect(result.type).toBe('normal')
    expect(result.parameterization).toBeNull()
    expect(result.params).toEqual({ mean: '0', sd: '1' })
  })

  test('parses normal distribution with positional params', () => {
    const result = window.OQGrid.helpers.distributions.parseDistributionString('normal(0, 1)')
    expect(result.type).toBe('normal')
    expect(result.params.mean).toBe('0')
    expect(result.params.sd).toBe('1')
  })

  test('detects lognormal mean_sd parameterization', () => {
    const result = window.OQGrid.helpers.distributions.parseDistributionString('lognormal(mean = 1, sd = 0.2)')
    expect(result.type).toBe('lognormal')
    expect(result.parameterization).toBe('mean_sd')
    expect(result.params).toEqual({ mean: '1', sd: '0.2' })
  })

  test('detects lognormal meanlog_sdlog parameterization', () => {
    const result = window.OQGrid.helpers.distributions.parseDistributionString('lognormal(meanlog = 0, sdlog = 0.5)')
    expect(result.type).toBe('lognormal')
    expect(result.parameterization).toBe('meanlog_sdlog')
  })

  test('detects gamma shape_scale parameterization', () => {
    const result = window.OQGrid.helpers.distributions.parseDistributionString('gamma(shape = 2, scale = 0.5)')
    expect(result.parameterization).toBe('shape_scale')
  })

  test('parses unknown distribution type with raw params', () => {
    const result = window.OQGrid.helpers.distributions.parseDistributionString('custom(a = 1, b = 2)')
    expect(result.type).toBe('custom')
    expect(result.parameterization).toBeNull()
    expect(result.params._raw).toBe('a = 1, b = 2')
  })

  test('handles nested parens in param values', () => {
    const result = window.OQGrid.helpers.distributions.parseDistributionString('bootstrap(x = c(1, 2, 3))')
    expect(result.type).toBe('bootstrap')
    expect(result.params.x).toBe('c(1, 2, 3)')
  })

  test('trims whitespace from param names and values', () => {
    const result = window.OQGrid.helpers.distributions.parseDistributionString('normal( mean = 0.5 , sd = 1.0 )')
    expect(result.params.mean).toBe('0.5')
    expect(result.params.sd).toBe('1.0')
  })
})

describe('OQGrid.helpers.distributions.buildDistributionString', () => {
  test('returns empty string for unknown type', () => {
    const result = window.OQGrid.helpers.distributions.buildDistributionString('unknown', null, {})
    expect(result).toBe('')
  })

  test('builds normal distribution string', () => {
    const result = window.OQGrid.helpers.distributions.buildDistributionString('normal', null, { mean: '0', sd: '1' })
    expect(result).toBe('normal(mean = 0, sd = 1)')
  })

  test('builds uniform distribution string', () => {
    const result = window.OQGrid.helpers.distributions.buildDistributionString('uniform', null, { min: '0', max: '1' })
    expect(result).toBe('uniform(min = 0, max = 1)')
  })

  test('skips params with empty string values', () => {
    const result = window.OQGrid.helpers.distributions.buildDistributionString('normal', null, { mean: '0', sd: '' })
    expect(result).toBe('normal(mean = 0)')
  })

  test('skips null param values', () => {
    const result = window.OQGrid.helpers.distributions.buildDistributionString('normal', null, { mean: null, sd: '1' })
    expect(result).toBe('normal(sd = 1)')
  })

  test('builds lognormal with mean_sd parameterization', () => {
    const result = window.OQGrid.helpers.distributions.buildDistributionString('lognormal', 'mean_sd', { mean: '1', sd: '0.2' })
    expect(result).toBe('lognormal(mean = 1, sd = 0.2)')
  })

  test('round-trips: parse then build', () => {
    const original = 'gamma(mean = 2, sd = 0.5)'
    const parsed = window.OQGrid.helpers.distributions.parseDistributionString(original)
    const rebuilt = window.OQGrid.helpers.distributions.buildDistributionString(
      parsed.type, parsed.parameterization, parsed.params
    )
    expect(rebuilt).toBe(original)
  })
})

describe('OQGrid.helpers.distributions.parseCVector', () => {
  test('returns empty array for empty input', () => {
    expect(window.OQGrid.helpers.distributions.parseCVector('')).toEqual([])
    expect(window.OQGrid.helpers.distributions.parseCVector(null)).toEqual([])
  })

  test('parses simple c() vector', () => {
    expect(window.OQGrid.helpers.distributions.parseCVector('c(0.1, 0.2, 0.3)')).toEqual(['0.1', '0.2', '0.3'])
  })

  test('trims whitespace from elements', () => {
    expect(window.OQGrid.helpers.distributions.parseCVector('c( 1 ,  2 , 3 )')).toEqual(['1', '2', '3'])
  })

  test('returns single-element array for non-c() input', () => {
    expect(window.OQGrid.helpers.distributions.parseCVector('42')).toEqual(['42'])
  })

  test('handles nested parens inside elements', () => {
    const result = window.OQGrid.helpers.distributions.parseCVector('c(a(1,2), b(3,4))')
    expect(result).toEqual(['a(1,2)', 'b(3,4)'])
  })
})

describe('OQGrid.helpers.distributions.buildCVector', () => {
  test('returns empty string for empty array', () => {
    expect(window.OQGrid.helpers.distributions.buildCVector([])).toBe('')
    expect(window.OQGrid.helpers.distributions.buildCVector(null)).toBe('')
  })

  test('builds c() vector from array', () => {
    expect(window.OQGrid.helpers.distributions.buildCVector(['0.1', '0.2', '0.3'])).toBe('c(0.1, 0.2, 0.3)')
  })

  test('round-trips with parseCVector', () => {
    const input = 'c(0.25, 0.35, 0.40)'
    const parsed = window.OQGrid.helpers.distributions.parseCVector(input)
    expect(window.OQGrid.helpers.distributions.buildCVector(parsed)).toBe(input)
  })
})

describe('OQGrid.helpers.distributions._splitTopLevel', () => {
  test('splits at top-level commas', () => {
    expect(window.OQGrid.helpers.distributions._splitTopLevel('a, b, c')).toEqual(['a', ' b', ' c'])
  })

  test('respects nested parentheses', () => {
    expect(window.OQGrid.helpers.distributions._splitTopLevel('a(1,2), b(3,4)')).toEqual(['a(1,2)', ' b(3,4)'])
  })

  test('respects nested brackets', () => {
    expect(window.OQGrid.helpers.distributions._splitTopLevel('a[1,2], b')).toEqual(['a[1,2]', ' b'])
  })

  test('returns single element for input with no commas', () => {
    expect(window.OQGrid.helpers.distributions._splitTopLevel('abc')).toEqual(['abc'])
  })
})
