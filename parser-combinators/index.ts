const failure = (expected, actual) => ({ isFailure: true, expected, actual });
const success = (data, rest) => ({ data, rest });

const integer = (input) => {
  const match = /^\d+/.exec(input);
  if (match !== null) {
    const matchedText = match[0];
    return success(Number(matchedText), input.slice(matchedText.length));
  }
  return failure("an integer", input);
};

const parse = (parser, input) => {
  const result = parser(input);

  if (result.isFailure) {
    throw new Error(`Parse error
            expected: ${result.expected}
            instead found '${result.actual}' 
        `);
  } else {
    return result;
  }
};

const text = (match) => (input) =>
  input.startsWith(match)
    ? success(match, input.slice(match.length))
    : failure(`'${match}'`, input);

const regex = (regex) => {
  const anchoredRegex = new RegExp(`^${regex.source}`);
  return (input) => {
    const match = anchoredRegex.exec(input);
    if (match !== null) {
      const matchedText = match[0];
      return success(matchedText, input.slice(matchedText.length));
    }
    return failure(regex, input);
  };
};
const label = (parser, expected) => (input) => {
  const result = parser(input);
  if (result.isFailure) {
    return failure(expected, result.actual);
  }
  return result;
};
const map = (func, parser) => (input) => {
  const result = parser(input);
  if (result.isFailure) return result;
  return success(func(result.data), result.rest);
};

const apply = (func, parsers) => (input) => {
  const accData = [];
  let currentInput = input;

  for (const parser of parsers) {
    const result = parser(currentInput);
    if (result.isFailure) return result;
    accData.push(result.data);
    currentInput = result.rest;
  }
  return success(func(...accData), currentInput);
};
const lexeme = (junk) => (parser) => apply((data, _) => data, [parser, junk]);
const spaces = regex(/\s*/);
const token = lexeme(spaces);
const eof = (input) => {
  if (input.length === 0) return success(null, input);
  return failure("end of input", input);
};
const plus = text("+");
const decimal = map(
  (x) => Number(x),
  label(regex(/\d+(?:\.\d+)?/), "a decimal")
);
const sequence = (...parsers) =>
  apply((...results) => results[results.length - 1], parsers);
const collect = (...parsers) => apply((...results) => results, parsers);
const oneOf = (...parsers) => (input) => {
  for (const parser of parsers) {
    const result = parser(input);
    if (result.isFailure) continue;
    return result;
  }
  return failure("oneOf", input);
};
const opMap = {
  "+": (left, right) => left + right,
  "-": (left, right) => left - right,
  "*": (left, right) => left * right,
  "/": (left, right) => left / right,
};
const getOp = (op) => opMap[op];
const op = map(getOp, oneOf(text("+"), text("-"), text("*"), text("/")));

const expr = apply((_, num1, opFunc, num2) => opFunc(num1, num2), [
  spaces,
  token(decimal),
  token(label(op, "an arithmetic operator")),
  token(decimal),
  eof,
]);

parse(expr, "   1 + 1"); /*?*/
