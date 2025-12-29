# cosmo.argon2

### Config

```lua
argon2.Config
```

This module implements a password hashing algorithm based on blake2b that won
the Password Hashing Competition.
It can be used to securely store user passwords in your SQLite database, in a
way that destroys the password, but can be verified by regenerating the hash
again the next time the user logs in. Destroying the password is important,
since if your database is compromised, the bad guys won't be able to use
rainbow tables to recover the plain text of the passwords.
Argon2 achieves this security by being expensive to compute. Care should be
taken in choosing parameters, since an HTTP endpoint that uses Argon2 can just
as easily become a denial of service vector. For example, you may want to
consider throttling your login endpoint.

### hash_encoded

```lua
argon2.hash_encoded(pass, salt, config)
```

The memory hardness in kibibytes, which defaults
to 4096 (4 mibibytes). It's recommended that this be tuned upwards.
The number of iterations, which defaults to `3`.
The parallelism factor, which defaults to `1`.
the number of desired bytes in hash output,
which defaults to 32.
- `argon2.variants.argon2_id` blend of other two methods [default]
- `argon2.variants.argon2_i` maximize resistance to side-channel attacks
- `argon2.variants.argon2_d` maximize resistance to gpu cracking attacks
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
