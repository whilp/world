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
```

### Nested data

```bash
# Unnest arrays
echo '[{"name":"Alice","scores":[90,85,92]}]' | \
  duckdb -c "SELECT name, unnest(scores) as score FROM read_json('/dev/stdin')"

# Access nested objects
duckdb -c "SELECT data.field FROM read_json('/dev/stdin')"
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

## Tips

- Use `/dev/stdin` to read from stdin in SQL queries
- Flag order matters: use `-json`, `-jsonlines`, or `-csv` before `-c`
- Use `-jsonlines` for NDJSON output (one JSON object per line)
- DuckDB auto-detects JSON and JSONL formats
- For explicit JSONL input: use `format='newline_delimited'` parameter
- Use `unnest()` to flatten arrays
- DuckDB supports complex SQL features like CTEs, window functions, and subqueries
