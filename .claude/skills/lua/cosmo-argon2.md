# cosmo.argon2

Argon2 password hashing for Lua, provided by the `cosmo` module.

Argon2 is a modern password hashing algorithm that won the Password Hashing Competition in 2015. It provides strong security against GPU cracking attacks and side-channel attacks.

## Usage

```lua
local cosmo = require("cosmo")
local argon2 = cosmo.argon2
```

## Functions

### hash_encoded

```lua
hash = argon2.hash_encoded(password, salt [, options])
```

Hash a password with Argon2 and return encoded hash string.

Parameters:
- `password` - plaintext password to hash
- `salt` - salt string (should be unique per password, minimum 8 bytes recommended)
- `options` - optional table with:
  - `t_cost` - time cost (number of iterations, default 3)
  - `m_cost` - memory cost in KB (default 4096)
  - `parallelism` - number of threads (default 1)

Returns encoded hash string in format: `$argon2id$v=19$m=4096,t=3,p=1$<salt>$<hash>`

```lua
local hash = argon2.hash_encoded('my_password', 'random_salt_16')
print(hash)
-- $argon2id$v=19$m=4096,t=3,p=1$...
```

With custom options:
```lua
local hash = argon2.hash_encoded('my_password', 'random_salt_16', {
  t_cost = 2,
  m_cost = 8192,
  parallelism = 2
})
```

### verify

```lua
ok = argon2.verify(encoded_hash, password)
```

Verify password against encoded hash. Returns `true` if password matches, `false` otherwise.

```lua
local hash = argon2.hash_encoded('secret', 'salt12341234')

if argon2.verify(hash, 'secret') then
  print("Password correct")
else
  print("Password incorrect")
end
```

### hash_len

```lua
hasher = argon2.hash_len(length)
```

Create hasher with specified hash output length.

### t_cost

```lua
hasher = argon2.t_cost(iterations)
```

Create hasher with specified time cost (number of iterations).

### m_cost

```lua
hasher = argon2.m_cost(memory_kb)
```

Create hasher with specified memory cost in kilobytes.

### parallelism

```lua
hasher = argon2.parallelism(threads)
```

Create hasher with specified parallelism (number of threads).

### variant

```lua
hasher = argon2.variant(variant)
```

Create hasher with specified variant. Use constants from `argon2.variants` table.

## Variants

Argon2 has three variants available in `argon2.variants`:

- `argon2i` - optimized against timing attacks (data-independent)
- `argon2d` - optimized for resistance to GPU attacks (data-dependent)
- `argon2id` - hybrid combining both (recommended, default)

## Configuration

### Time cost (t_cost)

Number of iterations. Higher values increase computation time and security.

- Minimum: 1
- Recommended: 2-3 for interactive use, higher for sensitive data
- Default: 3

### Memory cost (m_cost)

Amount of memory in kilobytes. Higher values increase memory requirements and security against GPU attacks.

- Minimum: 8
- Recommended: 65536 (64 MB) or higher for interactive use
- Default: 4096 (4 MB)

### Parallelism

Number of parallel threads. Should match available CPU cores.

- Minimum: 1
- Recommended: number of CPU cores
- Default: 1

## Security recommendations

### Generating salts

Always use unique, random salts for each password:

```lua
local unix = require('cosmo').unix

-- Generate cryptographically secure random salt
local salt = cosmo.GetRandomBytes(16)
local hash = argon2.hash_encoded(password, salt)
```

### Choosing parameters

Balance security and performance:

```lua
-- For interactive login (< 500ms)
local hash = argon2.hash_encoded(password, salt, {
  t_cost = 2,
  m_cost = 65536,  -- 64 MB
  parallelism = 4
})

-- For sensitive data (1-2 seconds acceptable)
local hash = argon2.hash_encoded(password, salt, {
  t_cost = 4,
  m_cost = 262144,  -- 256 MB
  parallelism = 4
})
```

### Storing hashes

Store only the encoded hash, never the plaintext password:

```lua
-- During registration
local salt = cosmo.GetRandomBytes(16)
local hash = argon2.hash_encoded(user_password, salt)
-- Store 'hash' in database

-- During login
local stored_hash = get_from_database(username)
if argon2.verify(stored_hash, submitted_password) then
  -- Login successful
end
```

## Examples

### Basic password hashing

```lua
local argon2 = require('cosmo').argon2

-- Hash password
local password = "user_secret_password"
local salt = "unique_salt_1234"
local hash = argon2.hash_encoded(password, salt)

print("Hash: " .. hash)

-- Verify password
local is_valid = argon2.verify(hash, password)
print("Valid: " .. tostring(is_valid))

local is_invalid = argon2.verify(hash, "wrong_password")
print("Invalid: " .. tostring(is_invalid))
```

### Registration flow

```lua
local cosmo = require('cosmo')
local argon2 = cosmo.argon2

local function register_user(username, password)
  -- Generate random salt
  local salt = cosmo.GetRandomBytes(16)

  -- Hash password with strong parameters
  local hash = argon2.hash_encoded(password, salt, {
    t_cost = 3,
    m_cost = 65536,
    parallelism = 2
  })

  -- Store username and hash in database
  save_to_database(username, hash)

  return true
end
```

### Login flow

```lua
local argon2 = require('cosmo').argon2

local function authenticate_user(username, password)
  -- Retrieve stored hash from database
  local stored_hash = get_from_database(username)

  if not stored_hash then
    return false, "User not found"
  end

  -- Verify password
  if argon2.verify(stored_hash, password) then
    return true, "Login successful"
  else
    return false, "Invalid password"
  end
end
```

### Testing different parameters

```lua
local argon2 = require('cosmo').argon2

local function benchmark_params(params)
  local password = "test_password"
  local salt = "test_salt_123456"

  local start = cosmo.GetTime()
  local hash = argon2.hash_encoded(password, salt, params)
  local elapsed = cosmo.GetTime() - start

  print(string.format("t=%d m=%d p=%d: %.3f seconds",
    params.t_cost, params.m_cost, params.parallelism, elapsed))

  return elapsed
end

-- Test different configurations
benchmark_params({t_cost = 2, m_cost = 4096, parallelism = 1})
benchmark_params({t_cost = 3, m_cost = 65536, parallelism = 2})
benchmark_params({t_cost = 4, m_cost = 262144, parallelism = 4})
```

### Password migration

```lua
-- Migrate from weaker hash to Argon2
local function migrate_password(username, old_hash, password)
  -- Verify old hash first
  if not verify_old_hash(old_hash, password) then
    return false, "Invalid password"
  end

  -- Generate new Argon2 hash
  local salt = cosmo.GetRandomBytes(16)
  local new_hash = argon2.hash_encoded(password, salt, {
    t_cost = 3,
    m_cost = 65536,
    parallelism = 2
  })

  -- Update database
  update_database(username, new_hash)

  return true
end
```

## Notes

- Argon2id is recommended for most use cases (default)
- Always use unique random salts for each password
- Choose parameters based on available memory and acceptable delay
- Increase parameters over time as hardware improves
- Store the complete encoded hash (includes all parameters)
- The encoded hash is self-describing (includes variant, version, parameters)

## References

- [Argon2 specification](https://github.com/P-H-C/phc-winner-argon2)
- [Password Hashing Competition](https://password-hashing.net/)
- [OWASP Password Storage Cheat Sheet](https://cheatsheetseries.owasp.org/cheatsheets/Password_Storage_Cheat_Sheet.html)
