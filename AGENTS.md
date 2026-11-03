# AGENTS.md — AI Assistant Guide for abap2UI5 popups

> This file follows the cross-tool AGENTS.md convention and is the single
> agent instruction file of this repository — `CLAUDE.md` only points here.

## Project Overview

Ready-to-use popup and dialog apps for [abap2UI5](https://github.com/abap2UI5/abap2UI5) (`z2ui5_cl_popup_*`). The classes were moved here from the abap2UI5 core framework.

**Language:** English — all code, comments, commit messages, PRs, issues, documentation, and communication must be in English.

## Package Structure

| Package | Content |
|---|---|
| `src/` | Popup classes (`z2ui5_cl_popup_*`) |
| `src/00/` | Context/utility class `z2ui5_cl_popup_context` and its exception `z2ui5_cx_popup_error` — **vendored copies from abap-util, see below** |
| `src/02/` | Samples (`z2ui5_cl_popup_sample_*`) |
| `src/03/` | Popups with layout-management dependency (Value-Help, Search-Help) |

## The Utility Copy Principle (`src/00/`)

`z2ui5_cl_popup_context` is a **renamed copy** of `zabaputil_cl_util_context` from the [abap-util](https://github.com/abap-util/abap-util) **master catalog** (which contains all utility classes with all methods), **trimmed to the methods the popup apps actually use**. This repo has no install-time dependency on abap-util — abapGit has no dependency management, so utilities are vendored instead of referenced. The same pattern is used by the abap2UI5 core (`z2ui5_cl_ui5_util_context` in its `src/00/03/`).

**How the copy is maintained:**

1. **Nothing from the core's frozen `src/99/01`.** `z2ui5_cl_util*` and `z2ui5_cx_util_error` are kept in abap2UI5 for compatibility only; every utility call goes through `z2ui5_cl_popup_context`, every utility exception is `z2ui5_cx_popup_error` (a renamed copy of `zabaputil_cx_util_error`, copied as-is).
2. **Method-level trimming:** the copy carries only the methods the popup apps use, including the private helpers those methods need. (Method-level trimming applies to the context class only — any other class vendored from abap-util is copied as-is.)
3. **New methods are added locally.** When a popup needs a utility method the copy doesn't have yet, write it directly into `z2ui5_cl_popup_context`. If the method already exists in abap-util, copy it from there (with its private helpers) instead of re-implementing it. Do not add a dependency on another project's copy.
4. **Periodic AI sync-back:** every few weeks an AI compares abap-util with all consumers and merges methods that were added locally back into abap-util, so the master catalog stays the superset of all methods and other projects can reuse them.
5. **Popup-specific logic does not belong in the context class.** Keep it in the popup class; only generic, reusable utilities live in the vendored copy (they will be harvested into abap-util by the sync).

## Dependencies

Installed alongside via abapGit; declared in the abaplint configs:

* [abap2UI5](https://github.com/abap2UI5/abap2UI5) — **pinned to a release
  tag** (the `"branch"` key of the core dependency, abap2UI5 CONVENTIONS §9):
  users install a release next to this addon, so the checks run against that
  release and not against the framework's `main`. Three abaplint configs carry
  the pin (`abaplint.jsonc`, `.github/abaplint/abap_cloud.jsonc` and the
  downported `<tag>-702` form in `.github/abaplint/abap_702.jsonc`; the
  namespace rename clones the core at the pin of `abaplint.jsonc`); read and
  move them only with
  `scripts/core-pin.mjs` (`get` fails when they disagree). `bump-deps.yaml`
  moves the pin monthly to the newest release, in its one pull request whose
  checks lint against it (`bump-core.yaml` does it alone, by hand), and
  the scheduled runs of `abap-standard` / `abap-cloud` lint once against the
  core's `main` as the canary (`core-pin.mjs set main`, never committed). Do
  not drop the key: abaplint then clones `main` silently.
* [layout-management](https://github.com/abap2UI5-addons/layout-management) — used by the Value-Help/Search-Help popups in `src/03/`; resolved from its `main` branch (it has no release tags to pin). A system that pulls this repository but not layout-management
  fails activation of `src/03/` with `"HEADER" is not type-compatible with formal
  parameter "XML"` (the old `render_layout_function` takes `z2ui5_cl_xml_view`);
  CI cannot see that skew — the fix is pulling layout-management

## Security

The value-help and search-help popups read the DDIC check table the user
selects, without an authorization check of their own. The dynamic WHERE clause
doubles single quotes to prevent SQL injection and escapes `%`, `_` and the
escape character `#`, so a typed wildcard only matches itself; but before using
these popups beyond a development system add your own authorization checks and restrict which
tables may be browsed.

## Coding Style

This project follows the conventions of the abap2UI5 core framework (see its [AGENTS.md](https://github.com/abap2UI5/abap2UI5/blob/main/AGENTS.md)): Clean ABAP with Hungarian prefixes, all three section blocks in every class (the popup classes are not `FINAL` - an app may subclass one), backtick string literals, `xsdbool()`, `NEW #()`, no `boolc()`.

## Validation

Run `npm run check` before considering changes complete: it runs the same
abaplint, abap2UI5-linter and rename steps as CI, and all of them must pass.
CI:

* `abap-standard` / `abap-cloud` — lint against Standard ABAP
  (`abaplint.jsonc`) and ABAP Cloud (`.github/abaplint/abap_cloud.jsonc`)
  (`uncaught_exception` is on: a checked exception that is neither caught nor
  declared is only a warning in SE80/ADT, so abaplint is where it is caught);
  their scheduled runs lint once against the core's `main` (the canary above)
* `check-abap2ui5` — the abap2UI5-linter over the app classes and their
  views (`abap2ui5lint.jsonc`)
* `publish-702` / `abap-702` — `publish-702` writes the downported `702`
  branch on every push to `main` (`npm run auto_downport`: `abaplint --fix`
  against `.github/abaplint/abap_702.jsonc`), and `abap-702` lints it
* `abap-unit` — the ABAP Unit tests on abap2UI5's transpiled Node runtime, the way abap2UI5 tests itself: `npm run unit` (`.github/scripts/unit.mjs`) copies the packages into a checkout of abap2UI5's `main` (abap2UI5-ext and layout-management beside it), downports, transpiles and runs the tests of this repository's classes
* `check-rename` — namespace-rename check (`npm run rename`, `.github/scripts/rename.mjs`)
* `build-rename` — manual workflow that pushes a namespace-renamed branch
  `rename_<name>` for a parallel install
* `bump-core` — moves the abap2UI5 release pin, see Dependencies

All `.abap`/`.xml`/config files are LF-only (`.gitattributes` enforces it).

## Namespace rename

`npm run rename` (CI: `check-rename`) and the manual `build-rename` workflow
are abap2UI5's namespace rename for this addon - `.github/scripts/rename.mjs`,
the same file in every addon that has one. Every `z2ui5_` object becomes
`<name>_...`, for a customer name such as `ZMYUI5` or a registered namespace
such as `/CA2UI5/` (at most 10 characters), and so does every reference to
abap2UI5 and to the addons this one builds on: the branch `rename_<name>` runs
on an abap2UI5 renamed with the same name by its own `build-rename`. What
follows from it:

- Every object name has at most 25 characters (a transparent table
  11, an ICF node 10) - 30 minus what the longest new name adds.
- No literal names a `z2ui5_` object: the rename rewrites references, never a
  literal. Derive the name - from a typed reference, or behind the prefix of
  the class's own name.
- A change that spans several repositories: `RENAME_DEPS_FROM=<dir> npm run
  rename` takes the dependencies from the clones in `<dir>`.
