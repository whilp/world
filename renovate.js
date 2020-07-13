/*eslint no-undef: "error"*/
/*eslint-env node*/

module.exports = {
  // $schema: "https://docs.renovatebot.com/renovate-schema.json",
  extends: ["config:base"],
  onboarding: false,
  requireConfig: false,
  gitAuthor: "Renovate Bot <bot@renovateapp.com>",
  logLevel: "debug",
  pinDigests: true,
  enabledManagers: ["github-actions", "bazel", "regex", "npm", "python"],
  platform: "github",
  reviewersFromCodeOwners: true,
  repositories: ["whilp/world"],
  "github-actions": {
    fileMatch: ["^\\.github/workflows/[^/]+\\.ya?ml$"],
    pinDigests: true,
    labels: ["dependencies", "github"],
  },
  bazel: {
    labels: ["dependencies", "bazel"],
  },
  npm: {
    labels: ["dependencies", "javascript"],
  },
  regexManagers: [
    {
      fileMatch: [".devcontainer.json"],
      labels: ["dependencies", "docker"],
      matchStrings: [
        '"image": "(?<depName>.*?):(?<currentValue>.*?)@(?<currentDigest>sha256:[a-f0-9]+)"',
      ],
      datasourceTemplate: "docker",
    },
    {
      fileMatch: ["^\\.github/workflows/[^/]+\\.ya?ml$"],
      labels: ["dependencies", "docker"],
      matchStrings: [
        "docker://(?<depName>.*?):(?<currentValue>.*?)@(?<currentDigest>sha256:[a-f0-9]+)",
      ],
      datasourceTemplate: "docker",
    },
    {
      // https://github.com/renovatebot/renovate/issues/5733
      fileMatch: ["^\\.github/workflows/[^/]+\\.ya?ml$"],
      labels: ["dependencies", "github"],
      matchStrings: ["uses: (?<depName>.*?)@(?<currentValue>.*?)\\s"],
      datasourceTemplate: "github-tags",
    },
    {
      fileMatch: "versions.bzl",
      labels: ["dependencies", "bazel"],
      matchStrings: [
        'datasource = "(?<datasource>.*?)",\n.*name = "(?<depName>.*?)",\n.*version = "(?<currentValue>.*?)"',
      ],
    },
    {
      fileMatch: ".bazelversion",
      labels: ["dependencies", "bazel"],
      matchStrings: ["^(?<currentValue>[0-9.]+)\\s"],
      datasourceTemplate: "github-releases",
      depNameTemplate: "bazelbuild/bazel",
    },
  ],
  packageRules: [
    {
      depTypeList: ["container_pull"],
      packagePatterns: ["ubuntu"],
      allowedVersions: "<=18.04",
      versioning: "regex:^(?<compatibility>[a-z]+?)-(?<minor>\\d+)?$",
    },
    {
      updateTypes: ["minor", "patch", "pin", "digest"],
      automerge: true,
    },
    {
      updateTypes: ["pin", "digest"],
      automerge: true,
      schedule: ["on monday"],
    },
    {
      // Keep in sync with rules_nodejs
      // https://github.com/bazelbuild/rules_nodejs/blob/d660ca109fcf86fe0dbfb9908faaefb0e30c25a0/internal/node/node_repositories.bzl#L108-L112
      packagePatterns: ["nodejs/node"],
      allowedVersions: "<=12.13.0",
    },
    {
      // Keep in sync with rules_nodejs
      // https://github.com/bazelbuild/rules_nodejs/blob/d660ca109fcf86fe0dbfb9908faaefb0e30c25a0/internal/node/node_repositories.bzl#L204
      packagePatterns: ["yarnpkg/yarn"],
      allowedVersions: "<=1.22.4",
    },
  ],
};
