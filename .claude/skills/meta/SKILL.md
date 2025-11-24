---
name: meta
description: Guide for writing and improving Claude Code skills. Use when creating new skills, debugging why skills aren't activating, or improving skill descriptions and structure.
---

# Writing effective Claude Code skills

## Required structure

```
skill-name/
├── SKILL.md (required)
├── reference.md (optional)
├── scripts/ (optional)
└── templates/ (optional)
```

## SKILL.md frontmatter

```yaml
---
name: lowercase-with-hyphens
description: Explain WHAT it does AND WHEN to use it. Include trigger terms users would say.
allowed-tools: [Read, Write, Bash]  # optional: restrict permissions
---
```

## Critical best practices

1. **Keep skills focused** - one capability per skill
2. **Write specific descriptions** - include actual terms users would mention
3. **Test activation** - verify Claude invokes it when expected
4. **Use allowed-tools** - restrict permissions for security

## Description examples

❌ Bad: "Helps with data"
✅ Good: "Analyze Excel spreadsheets, generate pivot tables, create charts. Use when working with .xlsx files."

❌ Bad: "Manages configuration"
✅ Good: "Read and update YAML/JSON config files. Use when modifying settings, environment variables, or application configuration."

## Debugging checklist

If Claude doesn't use your skill:
- Description lacks trigger terms users would say
- YAML syntax errors (check `---` markers, indentation)
- Description not specific enough about WHEN to use it
- Wrong file path or permissions

## Skills vs slash commands

**Use skills for:**
- Complex workflows with multiple files
- Automatic contextual invocation
- Comprehensive capabilities

**Use slash commands for:**
- Simple single prompts
- Manual explicit control
- Quick frequently-used operations
