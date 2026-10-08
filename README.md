# popups

[![abap2UI5-addons](https://img.shields.io/badge/abap2UI5--addons-library-1873b4)](https://github.com/abap2UI5-addons)
[![ABAP](https://img.shields.io/badge/ABAP-Cloud%20%7C%20Standard%20%E2%89%A5%207.50%20%7C%207.02-blue)](#installation)
[![abap2UI5](https://img.shields.io/badge/requires-abap2UI5-blue)](https://github.com/abap2UI5/abap2UI5)
[![layout-management](https://img.shields.io/badge/requires-layout--management-blue)](https://github.com/abap2UI5-addons/layout-management)
[![License](https://img.shields.io/github/license/abap2UI5-addons/popups)](LICENSE)
<br>
[![ABAP Cloud](https://img.shields.io/github/actions/workflow/status/abap2UI5-addons/popups/abap-cloud.yaml?branch=main&label=ABAP%20Cloud)](https://github.com/abap2UI5-addons/popups/actions/workflows/abap-cloud.yaml)
[![ABAP Standard](https://img.shields.io/github/actions/workflow/status/abap2UI5-addons/popups/abap-standard.yaml?branch=main&label=ABAP%20Standard)](https://github.com/abap2UI5-addons/popups/actions/workflows/abap-standard.yaml)
[![ABAP 7.02](https://img.shields.io/github/actions/workflow/status/abap2UI5-addons/popups/abap-702.yaml?branch=main&label=ABAP%207.02)](https://github.com/abap2UI5-addons/popups/actions/workflows/abap-702.yaml)
[![rename](https://img.shields.io/github/actions/workflow/status/abap2UI5-addons/popups/check-rename.yaml?branch=main&label=rename)](https://github.com/abap2UI5-addons/popups/actions/workflows/check-rename.yaml)
[![check-abap2UI5](https://img.shields.io/endpoint?url=https%3A%2F%2Fraw.githubusercontent.com%2Fabap2UI5-addons%2Fpopups%2Fbadges%2Fcheck-abap2ui5.json)](https://github.com/abap2UI5-addons/popups/actions/workflows/check-abap2ui5.yaml)
[![abap2UI5](https://img.shields.io/endpoint?url=https%3A%2F%2Fraw.githubusercontent.com%2Fabap2UI5-addons%2Fpopups%2Fbadges%2Fabap2ui5.json)](https://github.com/abap2UI5-addons/popups/actions/workflows/check-abap2ui5.yaml)

**Ready-to-use popups and dialogs for your abap2UI5 apps - one `factory( )` call each.**
Confirm, inform, select from a list, edit a text, up- and download a file,
show a table, a PDF, HTML or messages, pick a range, a value help or a search
help: each popup is an abap2UI5 app of its own that you call with
`client->nav_app_call( )` and that hands its result back to your app. The
classes were moved here from the abap2UI5 core framework.

> Part of [abap2UI5-addons](https://github.com/abap2UI5-addons) - addons and apps for [abap2UI5](https://github.com/abap2UI5/abap2UI5), installed with [abapGit](https://abapgit.org).

## Why

Almost every app needs the same handful of dialogs: "Are you sure?", "Enter a
value", "Pick one of these", "Upload a file". Building each of them as an XML
view with its own events, buffers and callbacks is repetitive work.

popups gives you these dialogs as finished classes. You call the popup, it
renders itself, and your app gets the result back - either as an event you
name, or through the popup's `result( )` method.

Good for:

- **abap2UI5 developers** who need standard dialogs without writing views.
- **Moving classic code to abap2UI5** - counterparts for `POPUP_TO_CONFIRM`,
  `POPUP_TO_INFORM`, file up- and download and `CL_DEMO_OUTPUT`.

## Installation

**Requirements**

- ABAP Cloud (S/4 Public Cloud, BTP ABAP Environment), S/4 Private Cloud or
  On-Premise, or SAP NetWeaver AS ABAP 7.50 or higher; NetWeaver 7.02 with the
  downported branch `702`
- [abap2UI5](https://github.com/abap2UI5/abap2UI5)
- [abap2UI5-addons/layout-management](https://github.com/abap2UI5-addons/layout-management) -
  pull it together with this repository: the Value-Help/Search-Help popups
  build their views with `z2ui5_cl_ui5_view_builder`, so an older
  layout-management (whose `z2ui5_cl_layo_pop=>render_layout_function` still
  takes a `z2ui5_cl_xml_view`) fails activation with
  `"HEADER" is not type-compatible with formal parameter "XML"`

No dependency on abap-util at installation time - the needed utilities are
embedded as a vendored copy (see [Utility class](#utility-class---vendored-copy-from-abap-util)).

**Steps** - with [abapGit](https://abapgit.org), in this order:

1. [abap2UI5](https://github.com/abap2UI5/abap2UI5)
2. [abap2UI5-addons/layout-management](https://github.com/abap2UI5-addons/layout-management)
3. this repository, from the branch that fits your system:

| System | Branch to pull |
|---|---|
| ABAP Cloud, S/4HANA, NetWeaver 7.50 or higher | `main` |
| NetWeaver 7.02 - 7.40 | `702` (downported automatically from `main`) |

On NetWeaver 7.02 pull layout-management from its `702` branch as well.

**Start** - run a sample like any abap2UI5 app, e.g.
`?app_start=z2ui5_cl_popup_sample_05` for the confirm popup. All samples are
listed under [Samples](#samples).

## Usage

Call a popup with `nav_app_call( )` and react to the event it raises when it
closes:

```abap
METHOD z2ui5_if_app~main.

  IF client->check_on_event( `DELETE` ).
    client->nav_app_call( z2ui5_cl_popup_to_confirm=>factory(
        i_question_text = `Delete the entry?`
        i_event_confirm = `DELETE_CONFIRMED`
        i_event_cancel  = `DELETE_CANCELED` ) ).

  ELSEIF client->check_on_event( `DELETE_CONFIRMED` ).
    client->message_box_display( `Entry deleted` ).

  ELSEIF client->check_on_event( `DELETE_CANCELED` ).
    client->message_box_display( `Nothing changed` ).

  ENDIF.

ENDMETHOD.
```

Popups that return data hand it over through `result( )`. When control comes
back, read it from the previous app:

```abap
" open
client->nav_app_call( z2ui5_cl_popup_textedit=>factory( `this is a text` ) ).

" back in your app
IF client->check_on_navigated( ).
  TRY.
      DATA(lo_prev) = client->get_app( client->get( )-s_draft-id_prev_app ).
      DATA(lv_text) = CAST z2ui5_cl_popup_textedit( lo_prev )->result( )-text.
    CATCH cx_root.
  ENDTRY.
ENDIF.
```

## Features

* Confirm, Inform & Select popups (`z2ui5_cl_popup_to_confirm`, `z2ui5_cl_popup_to_inform`, `z2ui5_cl_popup_to_select`)
* File Download & Upload (`z2ui5_cl_popup_file_dl`, `z2ui5_cl_popup_file_ul`)
* Table, Data & Demo Output (`z2ui5_cl_popup_table`, `z2ui5_cl_popup_data`, `z2ui5_cl_popup_demo_output`)
* Text Editor, HTML & PDF display (`z2ui5_cl_popup_textedit`, `z2ui5_cl_popup_html`, `z2ui5_cl_popup_pdf`)
* Messages, Error & Input Validation (`z2ui5_cl_popup_messages`, `z2ui5_cl_popup_error`, `z2ui5_cl_popup_input_val`)
* Range Selection (`z2ui5_cl_popup_get_range`, `z2ui5_cl_popup_get_range_m`)
* Image Editor & JS Loader (`z2ui5_cl_popup_image_edit`, `z2ui5_cl_popup_js_loader`)
* Value-Help & Search-Help (`z2ui5_cl_popup_value_help`, `z2ui5_cl_popup_search_help`)
* Transport Requests (`z2ui5_cl_popup_show_tr`)
* Samples for all popups (`src/02/`, `z2ui5_cl_popup_sample_*`)

## Samples

| Sample | Popup |
|---|---|
| `z2ui5_cl_popup_sample_01` | Value-Help (`z2ui5_cl_popup_value_help`) |
| `z2ui5_cl_popup_sample_02` | Search-Help (`z2ui5_cl_popup_search_help`) |
| `z2ui5_cl_popup_sample_03` | Image editor (`z2ui5_cl_popup_image_edit`) |
| `z2ui5_cl_popup_sample_04` | HTML (`z2ui5_cl_popup_html`) |
| `z2ui5_cl_popup_sample_05` | Confirm (`z2ui5_cl_popup_to_confirm`) |
| `z2ui5_cl_popup_sample_06` | Inform (`z2ui5_cl_popup_to_inform`) |
| `z2ui5_cl_popup_sample_07`, `_17` | Select from a list (`z2ui5_cl_popup_to_select`) |
| `z2ui5_cl_popup_sample_08` | Messages and errors (`z2ui5_cl_popup_messages`, `z2ui5_cl_popup_error`) |
| `z2ui5_cl_popup_sample_09` | Text editor (`z2ui5_cl_popup_textedit`) |
| `z2ui5_cl_popup_sample_10` | Input value (`z2ui5_cl_popup_input_val`) |
| `z2ui5_cl_popup_sample_11` | File upload (`z2ui5_cl_popup_file_ul`) |
| `z2ui5_cl_popup_sample_12`, `_13` | PDF (`z2ui5_cl_popup_pdf`) |
| `z2ui5_cl_popup_sample_14` | Select-options over several fields (`z2ui5_cl_popup_get_range_m`) |
| `z2ui5_cl_popup_sample_15` | Table (`z2ui5_cl_popup_table`) |
| `z2ui5_cl_popup_sample_16` | File download (`z2ui5_cl_popup_file_dl`) |
| `z2ui5_cl_popup_sample_18` | `CL_DEMO_OUTPUT` (`z2ui5_cl_popup_demo_output`) |
| `z2ui5_cl_popup_sample_19` | Range of one field (`z2ui5_cl_popup_get_range`) |

## Demo

### Search-Help
<img width="600" alt="image" src="https://github.com/user-attachments/assets/e7662e27-d16d-4949-87dc-bb8f13246719" />

### Value-Help
<img width="600" alt="image" src="https://github.com/user-attachments/assets/68d1c41a-52d0-4d98-b1dd-e3b869c5500d" />

### Transport Requests
<img width="600" alt="image" src="https://github.com/user-attachments/assets/5614466c-0b92-45f3-945d-0cdb5e3acb4c" />

## Package Structure

| Package | Content |
|---|---|
| `src/` | Popup classes (`z2ui5_cl_popup_*`) |
| `src/00/` | Context/utility class `z2ui5_cl_popup_context` — a vendored copy from [abap-util](https://github.com/abap-util/abap-util), see below |
| `src/02/` | Samples (`z2ui5_cl_popup_sample_*`) |
| `src/03/` | Popups with layout-management dependency (Value-Help, Search-Help) |
| `src/99/` | Obsolete: the original classes of this repository, kept for compatibility |

The classes in `src/` were moved here from the abap2UI5 core framework
(formerly the built-in popups in its obsolete package); the previous content
of this repository lives in `src/99/` (obsolete).

### Utility class - vendored copy from abap-util

`z2ui5_cl_popup_context` (`src/00/`) is a **renamed copy** of `zabaputil_cl_util_context` from the [abap-util](https://github.com/abap-util/abap-util) master catalog, **reduced to the methods actually used by the popup apps**. This keeps the installation dependency-free (abapGit has no dependency management) and namespace-isolated, while abap-util remains the catalog that contains all utility classes with all methods.

How the copy is maintained:
* Only the context class is trimmed at method level — unused methods are removed (the private helpers a kept method needs stay in the copy); other classes from abap-util would be vendored as-is.
* If a popup needs a utility method that is not in the copy yet, it is added directly to the local `z2ui5_cl_popup_context` (if it already exists in abap-util, it is copied from there with its private helpers instead of re-implemented).
* Every few weeks an AI compares abap-util with all consumers and merges locally added methods back into abap-util, so the master catalog stays the superset of all methods.

## Security

The value-help and search-help popups read the DDIC check table the user selects, without an authorization check of their own. Before using them beyond a development system, add your own authorization checks and restrict which tables may be browsed.

## Limitations & Todo

* Transports currently only work in On-Premise
* Search-Help not yet running on Cloud Stack

## Development

```bash
npm ci
npm run check           # all gates below, as in CI
npm run lint            # abaplint, Standard ABAP (v750)
npm run check:cloud     # abaplint, ABAP Cloud
npm run check:abap2ui5  # abap2UI5-linter over apps and views
npm run rename          # namespace rename check
npm run downport        # downport the source to 7.02 syntax
```

## Contributing

Issues and pull requests are welcome - whether you're fixing bugs, adding new
functionality, or improving documentation. Read
[CONTRIBUTING.md](CONTRIBUTING.md) first.

## License

MIT - see [LICENSE](LICENSE).
