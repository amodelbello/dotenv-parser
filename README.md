# dot-env.el

dot-env.el makes it easy to keep track of emacs configuration variables across multiple environments and machines.
It aims to mimic the functionality found in [https://github.com/motdotla/dotenv](https://github.com/motdotla/dotenv),
allowing you to specify values for config items that are likely to change across machines.

Examples of such config items are font sizes, paths to executables such as `ispell`, and authentication tokens.

## Usage
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
(defun dot-env-get (field &optional default))
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

## Disclaimer
...
