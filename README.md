# dot-env.el

dot-env.el makes it easy to keep track of emacs configuration variables across multiple environments and machines.
It aims to mimic the functionality found in [https://github.com/motdotla/dotenv](https://github.com/motdotla/dotenv),
allowing you to specify values for config items that are likely to change across machines.

Examples of such config items are font sizes, paths to executables such as `ispell`, and authentication tokens.

## Available Functions
dot-env.el implements 3 of the 4 public methods available in the original dotenv.js:

```elisp
(dot-env-config (&optional path))
;; Load the values from file located at PATH and return them or an error.
;; PATH defaults to `user-emacs-directory'/.env.
```

```elisp
(dot-env-parse (dotenv-str))
;; Parse DOTENV-STR into an association list and return the result.
```

```elisp
(dot-env-populate (alist &optional override debug))
;; Load the values from the association list ALIST into `dot-env-environment'.
;; If OVERRIDE is non-nil, override existing values.
;; If debug is non-nil, print log messages.
;; ALIST should be in the form of '((symbol string))
;; Populates dot-env-environment and returns it.
```

An additional convenience function is available to easily get a dotenv value:

```elisp
(dot-env-get (field &optional default))
;; Get the value of FIELD from dot-env-environment.
;; Use DEFAULT if no value is found.
```

## Example Usage: setting font
Value in .env file
```bash
FONT="DejaVu Sans Mono-11"
```

Emacs config
```elisp
;; load the .env file
(dot-env-config)

;; somewhere in your emacs config
(set-frame-font (dot-env-get "FONT" "DejaVu Sans Mono-13") nil t)
```
The above code changes the font to `DejaVu Sans Mono-11`. 
If no value had been specified in the .env file, `DejaVu Sans Mono-13` would have been loaded instead.

## Tips
* It's easiest just to use the default name and location for the .env file - `.env` in your `.emacs.d` directory.
* You should add the name/path of your .env file to your `.gitignore` file.

## Additional Information
* This package implements 3 of the 4 public methods within dotenv.js - `config()`, `parse()`, and `populate()`.
* `decrypt()` and all `.env.vault` functionality has been left out.
* The parsing logic is identical with the original but there are some minor differences in function signatures and functionality due to the differences between node.js and Emacs environments:
    1. The option to specify the file encoding of the .env file has been left out because Emacs handles this automatically.
    1. The option to specify an arbitrary `target` argument to the `populate()` method has been left out. Due to the convention of enabling lexical binding in elisp code, there is no way to pass a variable by reference. The ablity to populate a target which is passed into the function, as it exists in the original library, isn't possible in elisp code.
    1. The `(dot-env-config)` function does not take a debug option as the error that triggers the message that the debug option would've enabled prints to the message buffer automatically.
    1. The `debug` option has been left out of `(dot-env-parse)` because, while the option exists in the original library's documentation, it isn't actually used anywhere in the `parse()` method code.
