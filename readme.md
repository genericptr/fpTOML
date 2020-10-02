# fpTOML Parser

TOML parser for Free Pascal Compiler. Learn more about Tom's Obvious, Minimal Language at https://toml.io/.

[TOML v1.0.0-rc.1](https://toml.io/en/v1.0.0-rc.1) compliant.

### Features:

 - Fast. Single stream lexer and parser which doesn't use regex.
 - Convert TOML data to TJSONData (see fpJSON in RTL).

### TODO:

 - Line endings are not enforced in all areas.
 - Convert TOML date for TDateTime.
 - Output TOML data structures as TOML text.
 - Better support for building TOML data programmatically. 
