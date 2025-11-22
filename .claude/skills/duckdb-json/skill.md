---
description: Process JSON with DuckDB SQL queries
location: user
---

# duckdb-json

Use DuckDB to process JSON data with SQL queries instead of jq. DuckDB provides a more powerful and familiar SQL interface for JSON manipulation.

## Basic usage

```bash
# Read JSON from stdin
cat file.json | duckdb -c "SELECT * FROM read_json('/dev/stdin')"

# Read JSON from file
duckdb -c "SELECT * FROM read_json('file.json')"

# Read JSONL (newline-delimited JSON) - auto-detected
cat file.jsonl | duckdb -c "SELECT * FROM read_json('/dev/stdin')"

# Read JSONL explicitly
duckdb -c "SELECT * FROM read_json('file.jsonl', format='newline_delimited')"

# Output as JSON
duckdb -json -c "SELECT * FROM read_json('/dev/stdin')"
```

## Common patterns

### Filtering and selecting

```bash
# Select specific fields
echo '[{"name":"Alice","age":30}]' | duckdb -c "SELECT name FROM read_json('/dev/stdin')"

# Filter rows
echo '[{"name":"Alice","age":30},{"name":"Bob","age":25}]' | \
  duckdb -c "SELECT * FROM read_json('/dev/stdin') WHERE age > 28"

# Sort results
duckdb -c "SELECT * FROM read_json('/dev/stdin') ORDER BY age DESC"
```

### Aggregations

```bash
# Count, sum, average
duckdb -c "SELECT COUNT(*), AVG(age), MAX(age) FROM read_json('/dev/stdin')"

# Group by
duckdb -c "SELECT city, COUNT(*) as count FROM read_json('/dev/stdin') GROUP BY city"

# Conditional aggregation with FILTER
duckdb -c "
  SELECT
    COUNT(*) FILTER (WHERE type = 'assistant') as assistant_count,
    COUNT(*) FILTER (WHERE type = 'user') as user_count,
    SUM(tokens) FILTER (WHERE status = 'error') as error_tokens
  FROM read_json('/dev/stdin')
"

# Calculate percentages
duckdb -c "
  SELECT
    category,
    count,
    ROUND(100.0 * count / SUM(count) OVER (), 2) as percentage
  FROM (SELECT category, COUNT(*) as count FROM read_json('/dev/stdin') GROUP BY category)
"
```

### Nested data

```bash
# Unnest arrays
echo '[{"name":"Alice","scores":[90,85,92]}]' | \
  duckdb -c "SELECT name, unnest(scores) as score FROM read_json('/dev/stdin')"

# Access nested objects with dot notation
duckdb -c "SELECT data.field FROM read_json('/dev/stdin')"

# Extract nested JSON fields with json_extract_string
duckdb -c "
  SELECT
    json_extract_string(message, '$.content[0]') as first_content,
    json_extract_string(message, '$.id') as msg_id
  FROM read_json('/dev/stdin')
"

# Extract and parse nested JSON objects
duckdb -c "
  SELECT
    json_extract(message, '$.content') as content_array
  FROM read_json('/dev/stdin')
"
```

### Joins and transformations

```bash
# Join multiple JSON sources
duckdb -c "
  SELECT a.*, b.value
  FROM read_json('file1.json') a
  JOIN read_json('file2.json') b ON a.id = b.id
"

# Create computed fields
duckdb -c "SELECT name, age, age + 10 as age_plus_10 FROM read_json('/dev/stdin')"
```

## Output formats

```bash
# Table format (default)
duckdb -c "SELECT * FROM read_json('/dev/stdin')"

# JSON output (array format)
duckdb -json -c "SELECT * FROM read_json('/dev/stdin')"

# JSON lines output (newline-delimited JSON/NDJSON)
duckdb -jsonlines -c "SELECT * FROM read_json('/dev/stdin')"

# CSV output
duckdb -csv -c "SELECT * FROM read_json('/dev/stdin')"

# Markdown table
duckdb -markdown -c "SELECT * FROM read_json('/dev/stdin')"
```

## Advantages over jq

- SQL is more familiar to many developers
- Better for complex transformations and aggregations
- Easier to join multiple data sources
- More powerful filtering with WHERE clauses
- Native support for GROUP BY and window functions
- Can output to multiple formats (JSON, CSV, markdown, etc.)

## Advanced patterns

### String matching and text analysis

```bash
# Pattern matching with LIKE
duckdb -c "
  SELECT *
  FROM read_json('/dev/stdin')
  WHERE json_extract_string(message, '$.content') LIKE '%error%'
"

# Multiple pattern matching with OR
duckdb -c "
  SELECT *
  FROM read_json('/dev/stdin')
  WHERE lower(text) LIKE '%error%'
     OR lower(text) LIKE '%fail%'
     OR lower(text) LIKE '%stuck%'
"

# String operations
duckdb -c "
  SELECT
    substr(content, 1, 100) as preview,
    length(content) as content_length
  FROM read_json('/dev/stdin')
"
```

### Window functions and analytics

```bash
# Row numbering and ranking
duckdb -c "
  SELECT
    sessionId,
    count,
    ROW_NUMBER() OVER (ORDER BY count DESC) as rank
  FROM read_json('/dev/stdin')
"

# LAG to compare with previous row
duckdb -c "
  SELECT
    value,
    LAG(value) OVER (ORDER BY timestamp) as prev_value,
    value - LAG(value) OVER (ORDER BY timestamp) as change
  FROM read_json('/dev/stdin')
"

# Moving averages
duckdb -c "
  SELECT
    timestamp,
    value,
    AVG(value) OVER (ORDER BY timestamp ROWS BETWEEN 2 PRECEDING AND CURRENT ROW) as moving_avg_3
  FROM read_json('/dev/stdin')
"
```

### CTEs for complex queries

```bash
# Common table expressions for readability
duckdb -c "
  WITH error_analysis AS (
    SELECT
      CASE
        WHEN content LIKE '%Exit code%' THEN 'command_failure'
        WHEN content LIKE '%not found%' THEN 'missing_resource'
        ELSE 'other'
      END as error_type
    FROM read_json('/dev/stdin')
    WHERE is_error = true
  )
  SELECT error_type, COUNT(*) as count
  FROM error_analysis
  GROUP BY error_type
  ORDER BY count DESC
"
```

### Working with multiple files

```bash
# Process multiple JSONL files with glob patterns
duckdb -c "
  SELECT
    COUNT(DISTINCT sessionId) as total_sessions,
    COUNT(*) as total_events
  FROM read_json('~/.claude/projects/**/*.jsonl', format='newline_delimited')
"

# Analyze across file patterns
cat ~/.claude/projects/-Users-wcm/*.jsonl | duckdb -c "
  SELECT type, COUNT(*) as count
  FROM read_json('/dev/stdin', format='newline_delimited')
  GROUP BY type
  ORDER BY count DESC
"
```

## Tips

- Use `/dev/stdin` to read from stdin in SQL queries
- Flag order matters: use `-json`, `-jsonlines`, or `-csv` before `-c`
- Use `-jsonlines` for NDJSON output (one JSON object per line)
- DuckDB auto-detects JSON and JSONL formats
- For explicit JSONL input: use `format='newline_delimited'` parameter
- Use `unnest()` to flatten arrays
- Use `json_extract_string()` for extracting text values from nested JSON
- Use `json_extract()` for extracting complex JSON structures
- Use `FILTER` clause for conditional aggregations instead of CASE WHEN
- DuckDB supports complex SQL features like CTEs, window functions, and subqueries
- For percentage calculations, use `ROUND(100.0 * x / y, 2)` to avoid integer division
- Use `nullif()` to handle division by zero: `x / nullif(y, 0)`
