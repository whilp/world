/*eslint no-undef: "error"*/
/*eslint-env node*/

module.exports = {
  testEnvironment: "node",
  moduleNameMapper: {
    "^world/(.*)$": "<rootDir>/../$1",
  },
};
