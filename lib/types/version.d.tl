-- type declarations for version module

local record version
  versions: {string:VersionConfig}

  load_file: function(path: string, kinds?: {string:function(VersionConfig)}): boolean, string
  write: function(config: VersionConfig, callback_name?: string): string
  get: function(name: string): VersionConfig
  get_by_file: function(source: string): {VersionConfig}
  interpolate: function(template: string, context: {string:any}): string
  expand: function(value: any, context: {string:any}): any
  merge: function(...: {string:any}): {string:any}
  load: function(source: string): any
  render: function(data: {string:any}, opts?: RenderOpts): string
  parse_version_dir: function(name: string): string, string
  is_version_dir: function(name: string): boolean
  compare_versions: function(a: string, b: string): boolean
  find_binary: function(version_dir: string, tool_name: string): string
  scan_versions: function(tool_name: string, share_dir?: string): {VersionInfo}
  find_latest: function(tool_name: string, share_dir?: string): VersionInfo
  resolve_bin: function(tool_name: string, share_dir?: string): string
end

local record VersionConfig
  name: string
  version: string
  url: string
  path: string
  repo: string
  strip_components: number
  platforms: {string:PlatformData}
  urls: {string:string}
  executables: {Executable}
  platform: string
  _meta: MetaInfo
end

local record PlatformData
  sha256: string
  arch: string
  ext: string
end

local record Executable
  name: string
  path: string
  symlinks: {string:string}
end

local record MetaInfo
  source: string
  kind: string
end

local record RenderOpts
  format: string
  exclude: {string}
  indent: string
end

local record VersionInfo
  version: string
  sha: string
  path: string
  dir: string
end

return version
