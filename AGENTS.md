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

`z2ui5_cl_popup_context` is a **renamed copy** of `zabaputil_cl_util_context` from the [abap-util](https://github.com/abap-util/abap-util) **master repository**, **trimmed to the methods the popup apps actually use**. This repo has no install-time dependency on abap-util — abapGit has no dependency management, so utilities are vendored instead of referenced. The same pattern is used by the abap2UI5 core (`z2ui5_cl_abap2ui5_context` in its `src/00/03/`).

**Rules — these are hard constraints:**

1. **Never fix utility logic only in `z2ui5_cl_popup_context`.** The fix belongs in abap-util (where it is unit-tested), and is then applied identically to the copy here. A copy-only fix is lost for every other consumer and overwritten by the next sync.
2. **The copy may differ from the abap-util master in exactly two ways:** the class name (`z2ui5_cl_popup_context`) and the set of methods (only what the popups use). Method implementations must stay textually identical to `zabaputil_cl_util_context`.
3. **When a popup needs a utility method that is not in the copy yet,** copy it — together with every private helper it calls — from the current abap-util master state. Do not write a new implementation here and do not add a dependency on another project's copy.
4. **Popup-specific logic does not belong in the context class.** Keep it in the popup class; only generic, reusable utilities live in the vendored copy (and therefore in abap-util).

## Coding Style

This project follows the conventions of the abap2UI5 core framework (see its [AGENTS.md](https://github.com/abap2UI5/abap2UI5/blob/main/AGENTS.md)): Clean ABAP with Hungarian prefixes, `FINAL` classes with all three section blocks, backtick string literals, `xsdbool()`, `NEW #()`, no `boolc()`.

## Validation

Run `npx abaplint` before considering changes complete. CI lints against Standard ABAP and ABAP Cloud (`ABAP_STANDARD.yaml`, `ABAP_CLOUD.yaml`) and runs the namespace-rename check (`rename_test.yaml`).
