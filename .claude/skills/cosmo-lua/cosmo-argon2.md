# cosmo.argon2

### hash_encoded

```lua
argon2.hash_encoded(pass, salt, config)
```

Hashes password.
This is consistent with the README of the reference implementation:
>: assert(argon2.hash_encoded("password", "somesalt", {
variant = argon2.variants.argon2_i,
hash_len = 24,
t_cost = 2,
}))
`salt` is a nonce value used to hash the string.
`config.m_cost` is the memory hardness in kibibytes, which defaults
to 4096 (4 mibibytes). It's recommended that this be tuned upwards.
`config.t_cost` is the number of iterations, which defaults to 3.
`config.parallelism` is the parallelism factor, which defaults to 1.
`config.hash_len` is the number of desired bytes in hash output,
which defaults to 32.
`config.variant` may be:
- `argon2.variants.argon2_id` blend of other two methods [default]
- `argon2.variants.argon2_i` maximize resistance to side-channel attacks
- `argon2.variants.argon2_d` maximize resistance to gpu cracking attacks

**Parameters:**

- `pass` (string)
- `salt` (string)
- `config` (argon2.Config)

**Returns:**

- `string`: ascii

### verify

```lua
argon2.verify(encoded, pass)
```

Verifies password, e.g.
>: argon2.verify(
"p=4$c29tZXNhbHQ$RdescudvJCsgt3ub+b+dWRWJTmaaJObG",
true

**Parameters:**

- `encoded` (string)
- `pass` (string)

**Returns:**

- `boolean`: ok
