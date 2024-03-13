# HL YAML

High Level YAML ("HL YAML") is a lightweight all-in-one tool to read, parse, preprocess, validate and deserialize YAML files.

For example, it enables configuration files written in YAML to reference environment variables, inline credentials files, and import other configuration files.
Additionally, the `<<:` "operator" similar to OCaml's `include`, as well as anchors (`&`) and references (`*`) can greatly reduce the amount of repetition in your YAML files.

HL YAML's features can be disabled individually and new ones added to serve specific use cases.

For IO, HL YAML can use Lwt, Eio, or the standard library.

## Usage

```
opam install hl_yaml
```

Suppose we have a file `config.yml`:
```yaml
name: Hello World
api_key: !ENV_OPT API_KEY # use the `API_KEY` environment variable
credentials: !FILE creds.json # inline the `creds.json` file
```
and we want to deserialize it into a value of type `config`
```ocaml
module Y = Hl_yaml.Unix

type config = {
  name: string;
  api_key: string;
  credentials: string;
}
[@@deriving of_yojson]

let config =
  Y.parse ~of_yojson:config_of_yojson "<contents of config.yml>"
  |> Y.ok_or_raise
```

The library performs the following steps:
1. Parse our string to YAML
2. Resolve all tags (`!ENV_OPT`, `!FILE`, etc) and other forms of metaprogramming
3. Convert YAML to JSON
4. Validate against the JSON Spec `?validate` (optional, not used here)
5. Deserialize using `~of_yojson`

Here we used two tags (`!ENV` and `!FILE`) to provide dynamism. Other notable tags include `!CONFIG` which lets you import YAML files, allowing you to break large config files into smaller, more reusable ones.

Tip 1: in the previous example we don't even need to read the `config.yml` file, we can let HL YAML do it for us:
```ocaml
let config = Y.parse ~of_yojson:config_of_yojson "!CONFIG config.yml"
```

Tip 2: `~of_yojson` error messages are notoriously poor. Use the [~validate](#validations) argument to provide a Spec to validate the JSON data and thus generate highly detailed error messages. HL YAML will only invoke `~of_yojson` if the data matches the Spec.

Tip 3: pass `~of_yojson:Result.ok` to skip the deserialization step.

### Lwt

Replace `module Y = Hl_yaml.Unix` with:
```ocaml
module Y = Hl_yaml.Make_Lwt (Lwt) (Lwt_io)
```

### Eio

Replace `module Y = Hl_yaml.Unix` with:
```ocaml
module Y = Hl_yaml.Make_Eio (Eio)
```

# Features

## Tags

Tags are YAML syntactical elements that can be placed immediately before "scalars" (i.e. single values such as strings, booleans, numbers).

By default, HL YAML recognizes the following tags:
| Name | Description |
|------|-------------|
| `!ENV` | `!ENV var_name` replaces the `"var_name"` string with the contents of the environment variable of the same name. It is an error if the environment variable does not exist. |
| `!FILE` | `!FILE dir1/file1.txt` replaces the `"dir1/file1.txt"` string with the contents of the file of the same name. It is an error if the file does not exist.<br />See `~file_path_filter_map` ([options](#options)) if you wish to validate and/or edit file paths dynamically. |
| `!CONFIG` | `!CONFIG myfile.yml` replaces the `"myfile.yml"` string with the YAML structure found in the file of the same name. It is an error if the file does not exist.<br /><br />See `~config_path_filter_map` ([options](#options)) if you wish to validate and/or edit config paths dynamically. |
| `!ENV_FILE` | Same as `!FILE` but reads from the file path found in the environment variable. |
| `!ENV_OPT` | Same as `!ENV`, but uses an empty scalar (equivalent to `""` or `null`) if the environment variable does not exist. |
| `!ENV_STR` | Same as `!ENV`, but quotes the resulting string (single quotes, backslash for escaping). |

The [`~process_scalar_tag`](#options) option allows you to support additional tags and/or to override the behavior of certain tags. The `!CONFIG` tag can be disabled using the [`~enable_imports:false`](#options) option.

## Anchors (`&`) and References (`*`)

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

Anchors can be placed anywhere, not just on key-value pairs, e.g. `[m, &x iss, *x, ippi]` expands to `[m, iss, iss, ippi]`.

By default, unused anchors and attempting to re-define anchors are errors. See the [options](#options) to change these behaviors.

## Includes

To continue with our last example, what if we wanted a different **password** and **host** in each environment?

```yaml
# Anchor: "base_settings", Key: ""
# Reminder: HL YAML removes empty keys from the final output
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
    host: 1.2.3.4 # overrides the previous definition of "host"
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

HL YAML recognizes the special key `"<<"` and processes it similarly to OCaml's `include` keyword. In the example, `dev` grew from containing 2 key-value pairs (`"<<"` and `"password"`) to 5 when the key-value pairs contained under the `"<<"` key were pulled in.

Note that `staging` defined **host** twice, but the second definition (`host: 1.2.3.4`) shadowed the first (`host: 127.0.0.1`).

Arrays can also be included using `<<`, such as the `&supervisor` array in this example:
```yaml
&supervisors supervisors: # Anchor: "supervisors", Key: "supervisors"
  - name: Daniel
    role: project manager
  - name: Elizabeth
    role: product owner

employees:
  - name: Alice
    &programmer <<: # Anchor: "programmer", Key: "<<"
      role: programmer
      language: OCaml
  - name: Bob
    <<: *programmer
  - name: Charlie
    <<: *programmer
    language: JavaScript
  - <<: *supervisors # here
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

The special behavior of the `"<<"` key can be disabled using the [`~enable_includes:false`](#options) option.

## Conditional includes

Conditional includes, as the name suggests, are nothing more than conditional versions of `<<`.

This is done with the `!IF_DEF` and `!IF_NOT_DEF` tags.

Note that unlike all tags previously discussed, these two tags cannot be placed on scalars; they must be on the **key of key-value pairs**.

```yaml
settings:
  host: !ENV SERVER_IP
  port: 8080
  debug: false
  !IF_NOT_DEF IS_LIVE: # Tag: "IF_NOT_DEF", key: "IS_LIVE"
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
  host: 1.2.3.4
  port: 8080
  debug: true
```

## Options

```ocaml
let options = Y.make_options (* pass options here *) () in
Y.parse ~options "!CONFIG config.yml"
```

| Name | Default | Description |
|------|---------|-------------|
| get_env | `Sys.getenv_opt` | The function used to query environment variables. Useful in tests. |
| get_file | (I/O dependent) | The function used to read files. |
| config_path_filter_map | `fun s -> IO.return s` | This `filter_map` function is useful to `map` paths of `!CONFIG` tags, to account for the current working directory for example. If this function returns `None`, then the file path is considered "illegal" and an error is returned to the caller. |
| file_path_filter_map | `fun s -> IO.return s` | Same as `config_path_filter_map`, but for `!FILE` and `!ENV_FILE` tags. |
| enable_includes | `true` | When `false`, `<<` is no different from other keys. |
| enable_conditional_includes | `true` | When `false`, `!IF_DEF`  and `!IF_NOT_DEF` are no different from other keys. The tag itself is ignored, so for example `!IF_DEF foo:` becomes just `foo:`. |
| allow_imports | `true` | When `false`, using the `!CONFIG` tag results in an error. |
| allow_unused_anchors | `false` | When `false`, anchors (`&`) without at least one reference (`*`) result in an error. |
| allow_redefining_anchors | `false` | When `false`, it is an error to define two anchors (`&`) of the same name. When `true`, the second definition (once encountered) overrides the value of the anchor. |
| process_scalar_tag | | When set, this function is queried before normal tags processing. Return `Some` to indicate a recognized tag. |

## Validations

Yojson deserialization errors are notoriously poor in information. For this reason, HL YAML can validate the shape of your YAML data after it has been converted to JSON, but before it is deserialized into your final data type through `~of_yojson`.

See [spec.mli](src/spec.mli).

Complete example:
```yaml
# File: people.yml

- firstName: Bobby
  lastName: Tables
  age: 52
  roles: Developer

- firstName: John
  lastName: Doe
  roles:
    - manager
    - devops
```

```ocaml
module Y = Hl_yaml.Unix

type role =
  | Manager
  | Developer
  | Devops

let role_of_yojson = function
| `String "manager" -> Ok Manager
| `String "developer" -> Ok Developer
| `String "devops" -> Ok Devops
| json -> Error ("Invalid JSON for 'role': " ^ Yojson.Safe.to_string json)

type person = {
  first_name: string option; [@key "firstName"] [@default None]
  last_name: string; [@key "lastName"]
  age: int option; [@default None]

  roles: role list; [@of_yojson Y.Spec.OneOrMany.of_yojson role_of_yojson]
  (* Here we pass a custom @of_yojson because
     according to people.yml, "roles" can be
     either a single role or a YAML list of them *)
}
[@@deriving of_yojson]

type t = person list [@@deriving of_yojson]

let spec =
  let open Y.Spec in
  let role = make_enum [ `String "manager"; `String "developer"; `String "devops" ] in
  let person =
    make_schema ~name:"person" ~reject_extras:true
      [
        { key = "firstName"; required = false; spec = JString };
        { key = "lastName"; required = true; spec = JString };
        { key = "age"; required = false; spec = JInt };
        (* Note the use of `JOneOrArray` instead of `JArray` on "roles" *)
        { key = "roles"; required = true; spec = JOneOrArray role };
      ]
  in
  JArray person

let people : t =
  Y.parse ~validate:spec ~of_yojson "!CONFIG people.yml"
  |> Y.ok_or_raise
```

This code should fail because John has `dateOfBirth` instead of `lastName` and Bobby's role doesn't match any of the enum values.
Without passing `~validate:spec`, we only get Yojson's basic error message:
```
Unexpected format: person
```
but with `~validate:spec`, we get:
```
Incorrect value at $[0].roles, found "Developer", but expected one of: "manager" | "developer" | "devops"
Missing key: $[1].lastName (String)
Extraneous key: $[1].dateOfBirth (String)
```

## Error handling

HL YAML always returns `('a, Y.Spec.error list) result` instead of raising exceptions.

HL YAML will not catch exceptions thrown by the user's lambdas, except if that exception is an instance of `Y.HL_YAML_error`, in which case it is returned as `Error (Y.Spec.Processing { message })`.
