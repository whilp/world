module.exports = {
  // $schema: "https://docs.renovatebot.com/renovate-schema.json",
  extends: ["config:base"],
  onboarding: false,
  requireConfig: false,
  gitAuthor: "Renovate Bot <bot@renovateapp.com>",
  logLevel: "debug",
  pinDigests: true,
  enabledManagers: ["github-actions", "bazel", "regex", "npm"],
  platform: "github",
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
      fileMatch: "versions.bzl",
      matchStrings: [
        'datasource = "(?<datasource>.*?)",\n.*name = "(?<depName>.*?)",\n.*version = "(?<currentValue>.*?)"',
      ],
    },
    {
      fileMatch: ".bazelversion",
      matchStrings: ["^(?<currentValue>.*)$"],
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
      updateTypes: ["digest"],
      schedule: ["before 3am on monday"],
    },
  ],
};
