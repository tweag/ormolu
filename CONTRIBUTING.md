# Contributing

Issues (bugs, feature requests, or other feedback) may be reported in [the
GitHub issue tracker for this project][issues]. Pull requests are also
welcome.

When contributing to this repository, please first discuss the change you
wish to make via an issue, unless it's entirely trivial (typo fixes, etc.).
If there is already an issue that describes the change you have in mind,
comment on it to indicate that you're going to work on it. This way we can
avoid situations where several people work on the same thing.

Please make sure that all non-trivial changes are described in the commit
messages and PR descriptions.

## Testing

Testing is well taken care of, so it usually amounts to just adding examples
under `data/examples`. Each example is a pair of files: `<example-name>.hs`
for the input and `<example-name>-out.hs` for the corresponding expected
output.

Testing is performed as follows:

* The given snippet of source code is parsed and pretty-printed.
* The result of printing is parsed again, and its AST is compared to the AST
  obtained from the original file. The two should match.
* The output of the printer is checked against the expected output.
* The idempotence property is verified: formatting already formatted code
  results in exactly the same output.

Examples can be organized into sub-directories; see the existing ones for
inspiration.

Please note that we try to keep individual files at most 25 lines long,
because otherwise it's hard to figure out what went wrong when a test fails.

To regenerate outputs that have changed, you can set the
`ORMOLU_REGENERATE_EXAMPLES` environment variable before running tests.

## Formatting

 - Use the `nix run .#format` script to format Ormolu with the current
   version of Ormolu.

 - Additional formatters are configured via a pre-commit hook, which is
   installed automatically when you enter the Nix shell. You can also run it
   via `pre-commit run` or `pre-commit run -a`.

If Ormolu is not formatted this way, CI will fail.

[issues]: https://github.com/mrkkrp/ormolu/issues
