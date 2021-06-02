# fpTOML Parser

TOML parser for Free Pascal Compiler. Learn more about Tom's Obvious, Minimal Language at https://toml.io/.

```toml
# This is a TOML document.

# TOML aims to be a minimal configuration file format that's easy to read due to obvious semantics. 
# TOML is designed to map unambiguously to a hash table. TOML should be easy to parse into 
# data structures in a wide variety of languages.

title = "TOML Example"

[owner]
name = "Tom Preston-Werner"
dob = 1979-05-27T07:32:00-08:00 # First class dates

[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
connection_max = 5000
enabled = true

[servers]

  # Indentation (tabs and/or spaces) is allowed but not required
  [servers.alpha]
  ip = "10.0.0.1"
  dc = "eqdc10"

  [servers.beta]
  ip = "10.0.0.2"
  dc = "eqdc10"

[clients]
data = [ ["gamma", "delta"], [1, 2] ]

# Line breaks are OK when inside arrays
hosts = [
  "alpha",
  "omega"
]
```

[TOML v1.0.0-rc.1](https://toml.io/en/v1.0.0-rc.1) compliant.

### 👌 Features:

 - Fast. Single stream tokenizer and lexer which doesn't use regex.
 - Convert TOML data to TJSONData (see fpJSON in RTL).

### 🛠 TODO:

 - Line endings/white space rules are not 100% correct
 - Output TOML data structures as TOML text.
 - Better support for building TOML data programmatically. 
