-- skill - cosmic skill modules
-- usage: cosmic /zip/.lua/skill/<name>.lua [args...]

local record SkillModule
  _VERSION: string
  _DESCRIPTION: string
end

local M: SkillModule = {
  _VERSION = "0.1.0",
  _DESCRIPTION = "Skill modules for cosmic-lua",
}

return M
