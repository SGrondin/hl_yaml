# HL YAML

High Level YAML ("HL YAML") is a lightweight all-in-one tool to read, parse, preprocess, validate and deserialize YAML files.

For example, it enables configuration files written in YAML to reference environment variables, inline files, and import other configuration files.
Additional features include the `<<:` "operator" similar to OCaml's `include`, and reusing config fragments through anchors (`&`) and references (`*`).
Features can be disabled individually and new ones added to serve specific use cases.

For IO, it can use Lwt, Eio, or the standard library.

### Usage

```
opam install hl_yaml
```

Suppose we have a file (`config.yml`) and we want to deserialize it into a value of type `config`.

```ocaml
module Y = Hl_yaml.Unix

type config = {
  name: string;
  api_key: string;
  credentials: string;
}
[@@deriving of_yojson]

(* contents of config.yml *)
let raw = {|
name: Hello World
api_key: !ENV_OPT API_KEY # use the `API_KEY` environment variable
credentials: !FILE creds.json # use the `creds.json` file
|}

let config = Y.parse ~of_yojson:config_of_yojson raw
```

The library performs the following steps:
1. Parse our `raw` string to YAML
2. Resolve all tags (`!ENV_OPT`, `!FILE`, etc), perhaps recursively
3. Convert YAML to JSON
4. Validate against the JSON Schema `?validate` (optional, not used here)
5. Deserialize using `~of_yojson`

Tags (and more!) are documented below, notably `!CONFIG myfile.yml` lets you break up large files into smaller ones.

Tip: in the above example we didn't even need to load `config.yml` into the `raw` variable:
```ocaml
let config = Y.parse ~of_yojson:config_of_yojson "!CONFIG config.yml"
```

#### Lwt

Simply replace `module Y = Hl_yaml.Unix` with:
```ocaml
module Y = Hl_yaml.Make_Lwt (Lwt) (Lwt_io)
```

#### Eio

Simply replace `module Y = Hl_yaml.Unix` with:
```ocaml
module Y = Hl_yaml.Make_Eio (Eio)
```

## Features

### Tags

Tags are YAML syntactical elements that can be placed immediately before "scalars": simple values like strings and integers.

By default, HL YAML recognizes the following tags:
| Name | Example | Description |
|------|---------|-------------|
| !ENV | `!ENV var_name` | Replaces the `"var_name"` string with the contents of the environment variable of the same name. It is an error if the environment variable does not exist. |
| !FILE | `!FILE dir1/file1.txt` | Replaces the `"dir1/file1.txt"` string with the contents of the file of the same name. It is an error if the file does not exist.<br />See `~file_path_filter_map` ([options](#options)) if you wish to validate and/or edit file paths dynamically. |
| !CONFIG | `!CONFIG myfile.yml` | Replaces the `"myfile.yml"` string with the YAML structure found in the file of the same name. It is an error if the file does not exist.<br /><br />See `~config_path_filter_map` ([options](#options)) if you wish to validate and/or edit config paths dynamically. |
| !ENV_FILE | `!ENV_FILE var_name` | Same as `!FILE` but reads from the file path found in the `var_name` environment variable. |
| !ENV_OPT | `!ENV_OPT var_name` | Same as `!ENV`, but uses an empty scalar (equivalent to `""` or `null`) if the environment variable does not exist. |
| !ENV_STR | `!ENV_STR var_name` | Same as `!ENV`, but quotes the resulting string (single quotes, backslash for escaping). |

The [`~process_scalar_tag` option](#options) can be used to support additional tags and/or to override the behavior of certain tags (such as to disable them for example).

The `!CONFIG` tag can be disabled using the [`~enable_imports:false` option](#options).

### Anchors (`&`) and References (`*`)

An anchor is a name given to a YAML fragment to make it referenceable later, for reuse.

The following fragment:
```yaml
&base_settings:
  host: 127.0.0.1
  port: 5432
  dbname: postgres
  user: postgres
  password: !ENV PW

environments:
  dev: *base_settings
  staging: *base_settings
```
expands to:
```yaml
environments:
  dev:
    host: 127.0.0.1
    port: 5432
    dbname: postgres
    user: postgres
    password: supersecret
  staging:
    host: 127.0.0.1
    port: 5432
    dbname: postgres
    user: postgres
    password: supersecret
```

Remember that `&base_settings:` is equivalent to `&base_settings "":` it's a key-value pair where the key is the empty scalar. HL YAML drops all key-value pairs having an empty key from the final output.

Anchors can be placed anywhere, not just on key-value pairs. Examples: `hello: &whom Bob` and `[m, &x iss, *x, ippi]`

By default, unused anchors are errors. See the [`~allow_unused_anchors` option](#options) to change this behavior.

By default, attempting to re-define anchors is an error. See the [`~allow_redefining_anchors` option](#options) to change this behavior.

### Includes

To continue our previous example, what if we wanted a different **password** and **host** in each environment?

```yaml
&base_settings:
  host: 127.0.0.1
  port: 5432
  dbname: postgres
  user: postgres

environments:
  dev:
    <<: *base_settings
    password: !ENV DEV_PW
  staging:
    <<: *base_settings
    host: 1.2.3.4
    password: !ENV STAGING_PW
```
expands to:
```yaml
environments:
  dev:
    host: 127.0.0.1
    port: 5432
    dbname: postgres
    user: postgres
    password: my-dev-password
  staging:
    port: 5432
    dbname: postgres
    user: postgres
    host: 1.2.3.4
    password: my-staging-password
```

HL YAML recognizes the special key `"<<"` and processes it similarly to OCaml's `include` keyword. In the example, `dev` grew from containing 2 key-value pairs (`<<` and `password`) to 5 key-value pairs when the key-value pairs contained under `<<` were pulled in.

Note that `staging` defined **host** twice, but the second definition (`host: 1.2.3.4`) shadowed the first (`host: 127.0.0.1`).

Arrays can also be included using `<<` such as the `&supervisor` array:
```yaml
&supervisors supervisors:
  - name: Daniel
    role: project manager
  - name: Elizabeth
    role: product owner

employees:
  - name: Alice
    &programmer <<:
      role: programmer
      language: OCaml
  - name: Bob
    <<: *programmer
  - name: Charlie
    <<: *programmer
    language: JavaScript
  - <<: *supervisors
```
expands to:
```yaml
supervisors:
  - name: Daniel
    role: project manager
  - name: Elizabeth
    role: product owner

employees:
  - name: Alice
    role: programmer
    language: OCaml
  - name: Bob
    role: programmer
    language: OCaml
  - name: Charlie
    role: programmer
    language: JavaScript
  - name: Daniel
    role: project manager
  - name: Elizabeth
    role: product owner
```

The `!CONFIG` tag can be disabled using the [`~enable_includes:false` option](#options).

#### `!IF_DEF` and `!IF_NOT_DEF`

Syntactically speaking, `!IF_DEF` and `!IF_NOT_DEF` are tags.

Unlike all tags previously discussed, they cannot be placed on scalars, they must be on the **key** of key-value pairs.

```yaml
settings:
  host: !ENV SERVER_IP
  port: 8080
  debug: false
  !IF_NOT_DEF IS_LIVE:
    debug: true
```
If the `IS_LIVE` environment variable **exists** (and isn't empty), it expands to:
```yaml
settings:
  host: 1.2.3.4
  port: 8080
  debug: false
```
If the `IS_LIVE` environment variable **does not exist** (or it is empty), it expands to:
```yaml
settings:
  host: !ENV SERVER_IP
  port: 8080
  debug: true
```

In other words, `!IF_DEF` (and `!IF_NOT_DEF`) is simply a **conditional** `<<`.

## Options

```ocaml
let options = Y.make_options (* pass options here *) () in
Y.parse ~options "!CONFIG config.yml"
```

| Name | Default | Description |
|------|---------|-------------|
| get_env | `Sys.getenv_opt` | The function used to query environment variables. Useful in tests. |
| get_file | (I/O dependent) | The function used to read files. |
| config_path_filter_map | `fun s -> IO.return (Some s)` | This `filter_map` function is useful to `map` paths of `!CONFIG` tags, to account for the current working for example. If this function returns `None`, then the file path is considered "illegal" and an error is returned to the user. |
| file_path_filter_map | `fun s -> IO.return (Some s)` | Same as `config_path_filter_map`, but for `!FILE` and `!ENV_FILE` tags. |
| enable_includes | `true` | When `false`, `<<` is no different from other keys. |
| enable_conditional_includes | `true` | When `false`, `!IF_DEF foo`  and `!IF_NOT_DEF foo` are no different from other keys. The tag itself is ignored leaving just e.g. `"foo"`. |
| allow_imports | `true` | When `false`, using the `!CONFIG` tag results in an error. |
| allow_unused_anchors | `false` | When `false`, anchors (`&`) without at least one reference (`*`) result in an error. |
| allow_redefining_anchors | `false` | When `false`, anchors (`&`) it is an error to define two anchors of the same name. When `true`, the second definition (once encountered) overrides the value of the anchor. |
| process_scalar_tag | | When set, this function is queried before normal tags processing. Return `Some` to indicate a recognized tag. |
