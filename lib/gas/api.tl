-- Google Apps Script REST API client
-- https://developers.google.com/apps-script/api/reference/rest

local cosmo = require("cosmo")
local auth_mod = require("gas.auth")

local API_BASE <const> = "https://script.googleapis.com/v1"

local record File
  name: string
  type: string  -- "SERVER_JS", "HTML", "JSON"
  source: string
  createTime: string
  updateTime: string
  functionSet: {string:any}
end

local record Content
  scriptId: string
  files: {File}
end

local record Project
  scriptId: string
  title: string
  parentId: string
  createTime: string
  updateTime: string
  creator: {string:string}
  lastModifyUser: {string:string}
end

local record DeploymentConfig
  scriptId: string
  versionNumber: number
  manifestFileName: string
  description: string
end

local record Deployment
  deploymentId: string
  deploymentConfig: DeploymentConfig
  updateTime: string
  entryPoints: {{string:any}}
end

local record Version
  scriptId: string
  versionNumber: number
  description: string
  createTime: string
end

local record ApiError
  code: number
  message: string
  status: string
end

local record ApiResponse
  error: ApiError
end

local record Client
  auth: auth_mod.AuthState
end

-- make authenticated API request
local function api_request(client: Client, method: string, endpoint: string, body: any): any, string
  local token, err = auth_mod.get_access_token(client.auth)
  if not token then
    return nil, "auth failed: " .. err
  end

  local url = API_BASE .. endpoint
  local opts: {string:any} = {
    method = method,
    headers = {
      ["Authorization"] = "Bearer " .. token,
      ["Content-Type"] = "application/json",
    },
  }

  if body then
    opts.body = cosmo.EncodeJson(body)
  end

  local status, headers, response = cosmo.Fetch(url, opts)
  if not status then
    return nil, "request failed: " .. tostring(headers)
  end

  if response == "" or response == nil then
    if status >= 200 and status < 300 then
      return {}
    end
    return nil, "empty response with status " .. tostring(status)
  end

  local ok, result = pcall(cosmo.DecodeJson, response)
  if not ok then
    return nil, "failed to parse response: " .. tostring(result)
  end

  local resp = result as ApiResponse
  if resp.error then
    return nil, string.format("API error %d: %s", resp.error.code or 0, resp.error.message or "unknown")
  end

  return result
end

-- create a new client
local function new_client(creds_path: string): Client, string
  local auth, err = auth_mod.create_auth(creds_path)
  if not auth then
    return nil, err
  end
  return {auth = auth}
end

-- get project metadata
local function get_project(client: Client, script_id: string): Project, string
  local result, err = api_request(client, "GET", "/projects/" .. script_id, nil)
  if not result then return nil, err end
  return result as Project
end

-- create a new project
local function create_project(client: Client, title: string, parent_id: string): Project, string
  local body: {string:string} = {title = title}
  if parent_id then
    body.parentId = parent_id
  end
  local result, err = api_request(client, "POST", "/projects", body)
  if not result then return nil, err end
  return result as Project
end

-- get project content (pull)
local function get_content(client: Client, script_id: string): Content, string
  local result, err = api_request(client, "GET", "/projects/" .. script_id .. "/content", nil)
  if not result then return nil, err end
  return result as Content
end

-- update project content (push)
local function update_content(client: Client, script_id: string, files: {File}): Content, string
  local body: Content = {
    scriptId = script_id,
    files = files,
  }
  local result, err = api_request(client, "PUT", "/projects/" .. script_id .. "/content", body)
  if not result then return nil, err end
  return result as Content
end

-- list versions
local function list_versions(client: Client, script_id: string): {Version}, string
  local result, err = api_request(client, "GET", "/projects/" .. script_id .. "/versions", nil)
  if not result then return nil, err end
  local r = result as {string:{Version}}
  return r.versions or {}
end

-- create a new version
local function create_version(client: Client, script_id: string, description: string): Version, string
  local body: {string:string} = {}
  if description then
    body.description = description
  end
  local result, err = api_request(client, "POST", "/projects/" .. script_id .. "/versions", body)
  if not result then return nil, err end
  return result as Version
end

-- list deployments
local function list_deployments(client: Client, script_id: string): {Deployment}, string
  local result, err = api_request(client, "GET", "/projects/" .. script_id .. "/deployments", nil)
  if not result then return nil, err end
  local r = result as {string:{Deployment}}
  return r.deployments or {}
end

-- create a deployment
local function create_deployment(client: Client, script_id: string, version_number: number, description: string): Deployment, string
  local body: {string:any} = {
    versionNumber = version_number,
  }
  if description then
    body.description = description
  end
  local result, err = api_request(client, "POST", "/projects/" .. script_id .. "/deployments", body)
  if not result then return nil, err end
  return result as Deployment
end

-- update a deployment
local function update_deployment(client: Client, script_id: string, deployment_id: string, version_number: number, description: string): Deployment, string
  local body: {string:any} = {
    deploymentConfig = {
      scriptId = script_id,
      versionNumber = version_number,
    },
  }
  if description then
    (body.deploymentConfig as {string:any}).description = description
  end
  local result, err = api_request(client, "PUT", "/projects/" .. script_id .. "/deployments/" .. deployment_id, body)
  if not result then return nil, err end
  return result as Deployment
end

return {
  new_client = new_client,
  get_project = get_project,
  create_project = create_project,
  get_content = get_content,
  update_content = update_content,
  list_versions = list_versions,
  create_version = create_version,
  list_deployments = list_deployments,
  create_deployment = create_deployment,
  update_deployment = update_deployment,
}
