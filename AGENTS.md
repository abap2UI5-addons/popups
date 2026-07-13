# AGENTS.md — AI Assistant Guide for abap2UI5 popups

> This file follows the cross-tool AGENTS.md convention and is the single
> agent instruction file of this repository — there is no separate
> `CLAUDE.md`; Claude Code reads `AGENTS.md` natively.

## Project Overview

Ready-to-use popup and dialog apps for [abap2UI5](https://github.com/abap2UI5/abap2UI5) (`z2ui5_cl_popup_*`). The classes were moved here from the abap2UI5 core framework.

**Language:** English — all code, comments, commit messages, PRs, issues, documentation, and communication must be in English.

## Package Structure

| Package | Content |
|---|---|
| `src/` | Popup classes (`z2ui5_cl_popup_*`) |
| `src/00/` | Context/utility class `z2ui5_cl_popup_context` — **vendored copy from abap-util, see below** |
| `src/02/` | Samples (`z2ui5_cl_popup_sample_*`) |
| `src/03/` | Popups with layout-management dependency (Value-Help, Search-Help) |
| `src/99/` | Obsolete: the original classes of this repository, kept for compatibility — do not extend |

## The Utility Copy Principle (`src/00/`)

`z2ui5_cl_popup_context` is a **renamed copy** of `zabaputil_cl_util_context` from the [abap-util](https://github.com/abap-util/abap-util) **master catalog** (which contains all utility classes with all methods), **trimmed to the methods the popup apps actually use**. This repo has no install-time dependency on abap-util — abapGit has no dependency management, so utilities are vendored instead of referenced. The same pattern is used by the abap2UI5 core (`z2ui5_cl_a2ui5_context` in its `src/00/03/`).

**How the copy is maintained:**

1. **Method-level trimming:** the copy carries only the methods the popup apps use, including the private helpers those methods need. (Method-level trimming applies to the context class only — any other class vendored from abap-util is copied as-is.)
2. **New methods are added locally.** When a popup needs a utility method the copy doesn't have yet, write it directly into `z2ui5_cl_popup_context`. If the method already exists in abap-util, copy it from there (with its private helpers) instead of re-implementing it. Do not add a dependency on another project's copy.
3. **Periodic AI sync-back:** every few weeks an AI compares abap-util with all consumers and merges methods that were added locally back into abap-util, so the master catalog stays the superset of all methods and other projects can reuse them.
4. **Popup-specific logic does not belong in the context class.** Keep it in the popup class; only generic, reusable utilities live in the vendored copy (they will be harvested into abap-util by the sync).

## Dependencies

Installed alongside via abapGit; declared in the abaplint configs:

* [abap2UI5](https://github.com/abap2UI5/abap2UI5)
* [layout-management](https://github.com/abap2UI5-addons/layout-management) — used by the Value-Help/Search-Help popups in `src/03/`

## Security

The value-help and search-help popups read the DDIC check table the user
selects, without an authorization check of their own. The dynamic WHERE clause
doubles single quotes to prevent SQL injection, but before using these popups
beyond a development system add your own authorization checks and restrict which
tables may be browsed.

## Coding Style

This project follows the conventions of the abap2UI5 core framework (see its [AGENTS.md](https://github.com/abap2UI5/abap2UI5/blob/main/AGENTS.md)): Clean ABAP with Hungarian prefixes, `FINAL` classes with all three section blocks, backtick string literals, `xsdbool()`, `NEW #()`, no `boolc()`.

## Validation

Run `npx abaplint` before considering changes complete (0 issues expected). CI:

* `ABAP_STANDARD` / `ABAP_CLOUD` — lint against Standard ABAP and ABAP Cloud
* `ABAP_702` — lint the downported `702` branch; `npm run downport` /
  `auto_downport` produce it (`abaplint --fix` against `.github/abaplint/abap_702.jsonc`)
* `renaming` (`rename_test.yaml`) — namespace-rename check
* `build_rename` — manual workflow that pushes a namespace-renamed branch
  `rename_<name>` for a parallel install

All `.abap`/`.xml`/config files are LF-only (`.gitattributes` enforces it).
