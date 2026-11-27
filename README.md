# popups

[![abap2UI5-addons](https://img.shields.io/badge/abap2UI5--addons-library-1873b4)](https://github.com/abap2UI5-addons)
[![ABAP](https://img.shields.io/badge/ABAP-Cloud%20%7C%20Standard%20%E2%89%A5%207.50%20%7C%207.02-blue)](#installation)
[![abap2UI5](https://img.shields.io/badge/requires-abap2UI5-blue)](https://github.com/abap2UI5/abap2UI5)
[![layout-management](https://img.shields.io/badge/requires-layout--management-blue)](https://github.com/abap2UI5-addons/layout-management)
[![License](https://img.shields.io/github/license/abap2UI5-addons/popups)](LICENSE)
<br>
[![CI](https://img.shields.io/github/actions/workflow/status/abap2UI5-addons/popups/ci.yaml?branch=main&label=CI)](https://github.com/abap2UI5-addons/popups/actions/workflows/ci.yaml)
[![ABAP 7.02](https://img.shields.io/github/actions/workflow/status/abap2UI5-addons/popups/ci-702.yaml?branch=main&label=ABAP%207.02)](https://github.com/abap2UI5-addons/popups/actions/workflows/ci-702.yaml)
[![check-abap2UI5](https://img.shields.io/endpoint?url=https%3A%2F%2Fraw.githubusercontent.com%2Fabap2UI5-addons%2Fpopups%2Fbadges%2Fcheck-abap2ui5.json)](https://github.com/abap2UI5-addons/popups/actions/workflows/ci.yaml)
[![abap2UI5](https://img.shields.io/endpoint?url=https%3A%2F%2Fraw.githubusercontent.com%2Fabap2UI5-addons%2Fpopups%2Fbadges%2Fabap2ui5.json)](https://github.com/abap2UI5-addons/popups/actions/workflows/ci.yaml)

**Ready-to-use popups and dialogs for your abap2UI5 apps - one `factory( )` call each.**
Confirm, inform, select from a list, edit a text, up- and download a file,
show a table, a PDF, HTML or messages, pick a range, a value help or a search
help: each popup is an abap2UI5 app of its own that you call with
`client->nav_app_call( )` and that hands its result back to your app. The
classes were moved here from the abap2UI5 core framework.

> Part of [abap2UI5-addons](https://github.com/abap2UI5-addons) - addons and apps for [abap2UI5](https://github.com/abap2UI5/abap2UI5), installed with [abapGit](https://abapgit.org). Documentation: [abap2ui5-addons.github.io/popups](https://abap2ui5-addons.github.io/popups/).

## Why

Almost every app needs the same handful of dialogs: "Are you sure?", "Enter a
value", "Pick one of these", "Upload a file". Building each of them as an XML
view with its own events, buffers and callbacks is repetitive work.

popups gives you these dialogs as finished classes. You call the popup, it
renders itself, and your app gets the result back - either as an event you
name, or through `z2ui5_if_popup_result`, the same for every popup.

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

### Parallel installation (namespace rename)

To run a second copy next to the original in the same system, rename it the
way abap2UI5 itself is renamed: run the workflow **build-rename** (Actions →
build-rename → Run workflow) with the name your abap2UI5 installation was
renamed to - e.g. `ZMYUI5` or `/CA2UI5/`. It pushes the branch `rename_<name>`, in
which every object and every reference to abap2UI5 and to the addons this one
needs carries that name; pull it with abapGit. Rename abap2UI5 and every addon
of the installation with the same name.

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

### Reading the result

Every popup implements `z2ui5_if_popup_result`, so the result is read the
same way whichever popup you called. When control comes back to your app:

```abap
" open
client->nav_app_call( z2ui5_cl_popup_input_val=>factory( `Enter a name` ) ).

" back in your app - check_on_nav_back( ) is true only on the roundtrip on
" which the popup has left, so get_app_prev( ) is the popup there
IF client->check_on_nav_back( ).
  DATA(popup) = CAST z2ui5_if_popup_result( client->get_app_prev( ) ).
  IF popup->check_confirmed( ) = abap_true.
    DATA(name) = popup->value_string( ).
    " a table or structure: CAST to the type the popup documents
    " DATA(range) = CAST z2ui5_cl_popup_context=>ty_t_range( popup->value( ) ).
    " LOOP AT range->* INTO DATA(row). ...
  ENDIF.
ENDIF.

IF client->check_on_navigated( ).
  " show the view - on the first start, on a restore and after the popup
ENDIF.
```

An app that opens several popups tells their returns apart by a tag:
``client->nav_app_call( app = <popup> tag = `PICK_UNIT` )``, and back in the
app `CASE client->get_nav_tag( ).` (samples 01, 07 and 20).

- `check_confirmed( )` - `abap_true` when the user confirmed (OK, Save, a
  chosen row, an option, a transport), `abap_false` when the popup was
  cancelled or closed without a choice. A popup that only shows something
  has nothing to decline and always returns `abap_true`.
- `value( )` - a reference to the popup's value (`REF TO data`); unbound for
  a popup without one. CAST it to the type the popup documents into a
  variable and dereference that: `DATA(r) = CAST <type>( popup->value( ) ).`
  then `r->*` (`->*` directly behind a call is refused below 7.56).
- `value_string( )` - the value as text for a popup with a single value;
  empty when the value is a table or a structure.

| Popup | `check_confirmed( )` is `abap_true` when | `value( )` references | `value_string( )` |
|---|---|---|---|
| `z2ui5_cl_popup_to_confirm` | OK | - (unbound) | empty |
| `z2ui5_cl_popup_to_decide` | an option was pressed (not Cancel) | `string`, the number of the option | the number, `1` for the first |
| `z2ui5_cl_popup_to_inform` | always (only informs) | - | empty |
| `z2ui5_cl_popup_to_select` | Confirm | the chosen row (`LIKE LINE OF` your table); with `i_multiselect` the chosen rows (`LIKE` your table) | single select: the line of a table of elementary lines, else the first component of the row; empty with `i_multiselect` |
| `z2ui5_cl_popup_input_val` | OK or Enter | `string`, the value entered | the value |
| `z2ui5_cl_popup_textedit` | OK | `string`, the text | the text |
| `z2ui5_cl_popup_get_values` | OK | `z2ui5_cl_popup_get_values=>ty_t_field`, the fields with their values | empty (`result_value( )` reads one field) |
| `z2ui5_cl_popup_get_range` | OK | `z2ui5_cl_popup_context=>ty_t_range` | empty |
| `z2ui5_cl_popup_get_ranges` | OK | `z2ui5_cl_popup_context=>ty_t_filter_multi`, the ranges per field | empty |
| `z2ui5_cl_popup_value_help` | a row was chosen | `string`, the value of field `i_fname` of the row | the value |
| `z2ui5_cl_popup_shlp` | a row was chosen | `string`, the value of search-help parameter `i_fname` of the row | the value |
| `z2ui5_cl_popup_show_tr` | a transport request was selected, or No Transport | `z2ui5_cl_popup_context=>ty_s_transport` (`locl` = `abap_true` for No Transport) | the transport request, empty for No Transport |
| `z2ui5_cl_popup_calculator` | OK | `decfloat34`, the number | the number |
| `z2ui5_cl_popup_dtel_srch` | a data element was chosen | `z2ui5_cl_popup_dtel_srch=>ty_s_result`, the data element and its texts | the data element |
| `z2ui5_cl_popup_msg_search` | a message was chosen | `z2ui5_cl_popup_msg_search=>ty_s_result`, message class, number and text | empty |
| `z2ui5_cl_popup_file_ul` | OK | `z2ui5_cl_popup_file_ul=>ty_s_result`, the files with name and bytes | the last file as UTF-8 text |
| `z2ui5_cl_popup_file_dl` | the file was downloaded | - | empty |
| `z2ui5_cl_popup_image_edit` | Save | `string`, the image as data URL | the data URL |
| `z2ui5_cl_popup_pdf` | OK | - | empty |
| `z2ui5_cl_popup_table` | OK | the row reference of `result( )` | empty |
| `z2ui5_cl_popup_data` | always (only shows) | the data shown | empty |
| `z2ui5_cl_popup_demo_out`, `_html`, `_error` | always (only shows) | - | empty |
| `z2ui5_cl_popup_messages`, `_msgpopover` | always (only shows) | the messages shown (`ty_t_msg` of the class) | empty |
| `z2ui5_cl_popup_js_loader` | always | `string`, `result( )`; with `factory_check_open_ui5` `abap_bool`, `mv_is_open_ui5` | `result( )`; with `factory_check_open_ui5` the UI5 runtime |

`value( )` references the popup's own attribute, not a copy. Copy the value
(`DATA(r) = CAST <type>( popup->value( ) ).` then `my_field = r->*.`); do not
keep the reference in an attribute of your app - a `REF TO data` attribute
that is not PUBLIC fails to serialize into the draft on a system
(CX_REFERENCE_NOT_SERIALIZABLE), although the transpiled tests pass. The class of
each popup repeats its line of the table above its
`INTERFACES z2ui5_if_popup_result`. The older ways to read a result -
`result( )`, `ms_result`, `mv_return_value`, `ms_transport` - keep working.

### Message popover

`z2ui5_cl_popup_msgpopover` shows messages in a `sap.m.MessagePopover` next to
a control of your view - with a filter and a count per message type. It takes
what `z2ui5_cl_popup_messages` takes: BAPIRET2 tables, `sy`, exceptions, T100,
string tables. Pass the id of the control it opens by; closing it (a click
outside, Esc) returns to your app, with `i_event_close` when given:

```abap
" a Button with id="btnMessages" in your view raises SHOW_MESSAGES
IF client->check_on_event( `SHOW_MESSAGES` ).
  client->nav_app_call( z2ui5_cl_popup_msgpopover=>factory( i_messages    = lt_bapiret
                                                            i_open_by_id  = `btnMessages`
                                                            i_event_close = `MESSAGES_CLOSED` ) ).
ENDIF.
```

To keep the popover in your own view instead - toggled from a message button
in the footer, say - use the parts: `msg_get_t( )` turns the messages into
the item table you bind (`ty_t_msg`), `count_get( )` gives the count per type
and the worst type with its icon for the button, and `popover_xml( )` builds
the popover over your binding:

```abap
mt_msg = z2ui5_cl_popup_msgpopover=>msg_get_t( lt_bapiret ).
DATA(count) = z2ui5_cl_popup_msgpopover=>count_get( mt_msg ).   " count-error, count-icon, ...

client->popover_display( xml   = z2ui5_cl_popup_msgpopover=>popover_xml(
                                     i_items       = client->_bind( mt_msg )
                                     i_event_close = client->_event( `MESSAGES_CLOSED` ) )
                         by_id = `btnMessages` ).
```

### Embedded mode

The text searches `z2ui5_cl_popup_dtel_srch` and `z2ui5_cl_popup_msg_search`
also run as a page inside another app - a tab of a view cluster, say -
instead of as a popup. They implement `z2ui5_if_popup_embed`, the same
contract the table maintenance has with `z2ui5_if_tm_001`:

* the host sets `mo_parent_view` - the node its content goes into, normally
  the `sap.m.Page` of its view - before **every** `main( )` and calls `main( )`
  with its own client in every roundtrip (it builds its page anew each time);
* the app appends its content there - search field, language selects, result
  table - and sets `mv_view_display` when the host has to display its view
  again (the first call); the host clears it after reading it;
* embedded, there is no `popup_display( )`, no OK/Cancel, no
  `nav_app_leave( )`, and the rows cannot be selected - there is nothing to
  take them. The search runs on Enter and on a language change (event
  `SEARCH`; a host should not use that event name itself);
* the app handles its first call itself - `check_on_init( )` belongs to the
  host.

```abap
" once: the tab's app
mo_search = z2ui5_cl_popup_dtel_srch=>factory( ).

" in every roundtrip
DATA(page) = view->ele( `Page` ).     " the host's own content first
DATA(embed) = CAST z2ui5_if_popup_embed( mo_search ).
embed->mo_parent_view = page.
CAST z2ui5_if_app( mo_search )->main( client ).
IF embed->mv_view_display = abap_true.
  CLEAR embed->mv_view_display.
  client->view_display( view->stringify( ) ).
ENDIF.
```

Without `mo_parent_view` both are the popups they always were - the same view,
byte for byte. The instance must be reachable from the host's attributes
(public, e.g. `DATA mo_search TYPE REF TO object`), so that the bindings of
the search reach it.

## Features

* Confirm, Inform & Select popups (`z2ui5_cl_popup_to_confirm`, `z2ui5_cl_popup_to_inform`, `z2ui5_cl_popup_to_select`)
* Decide and Get Values - the classic POPUP_TO_DECIDE (a button per option) and POPUP_GET_VALUES (inputs, dates, checkboxes, listboxes, required fields) (`z2ui5_cl_popup_to_decide`, `z2ui5_cl_popup_get_values`)
* File Download & Upload (`z2ui5_cl_popup_file_dl`, `z2ui5_cl_popup_file_ul` - the upload hands over the file's name and bytes, binary files too, several files with `i_multiple` and limited to types with `i_file_type`)
* Table, Data & Demo Output (`z2ui5_cl_popup_table`, `z2ui5_cl_popup_data`, `z2ui5_cl_popup_demo_out`)
* Text Editor, HTML & PDF display (`z2ui5_cl_popup_textedit`, `z2ui5_cl_popup_html`, `z2ui5_cl_popup_pdf`)
* Messages, Error & Input Validation (`z2ui5_cl_popup_messages`, `z2ui5_cl_popup_error`, `z2ui5_cl_popup_input_val`)
* Message popover - a MessagePopover with a count per type, opened by a control of your view (`z2ui5_cl_popup_msgpopover`)
* Range Selection (`z2ui5_cl_popup_get_range`, `z2ui5_cl_popup_get_ranges`)
* Image Editor & JS Loader (`z2ui5_cl_popup_image_edit`, `z2ui5_cl_popup_js_loader` - only `factory_check_open_ui5`, whether the frontend runs OpenUI5; the script of `factory( i_js )` is not run any more, since abap2UI5 has no path for raw JavaScript - ship frontend code as a custom control)
* Value-Help & Search-Help - a search help opened by field or by name, with prefilled fields (`z2ui5_cl_popup_value_help`, `z2ui5_cl_popup_shlp`)
* Calculator built with CSS grid (`z2ui5_cl_popup_calculator`)
* Developer text search - data elements (DD04T) and messages (T100) in two languages (`z2ui5_cl_popup_dtel_srch`, `z2ui5_cl_popup_msg_search`)
* Transport Requests (`z2ui5_cl_popup_show_tr`)
* Samples for all popups (`src/02/`, `z2ui5_cl_popup_sample_*`)
* Selecting rows with layout variants (search, column layouts, multi select) is `z2ui5_cl_layo_pop_w_sel` in [layout-management](https://github.com/abap2UI5-addons/layout-management)

## Samples

| Sample | Popup |
|---|---|
| `z2ui5_cl_popup_sample_01` | Value-Help (`z2ui5_cl_popup_value_help`) |
| `z2ui5_cl_popup_sample_02` | Search-Help (`z2ui5_cl_popup_shlp`) |
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
| `z2ui5_cl_popup_sample_14` | Select-options over several fields (`z2ui5_cl_popup_get_ranges`) |
| `z2ui5_cl_popup_sample_15` | Table (`z2ui5_cl_popup_table`) |
| `z2ui5_cl_popup_sample_16` | File download (`z2ui5_cl_popup_file_dl`) |
| `z2ui5_cl_popup_sample_18` | `CL_DEMO_OUTPUT` (`z2ui5_cl_popup_demo_out`) |
| `z2ui5_cl_popup_sample_19` | Range of one field (`z2ui5_cl_popup_get_range`) |
| `z2ui5_cl_popup_sample_20` | Search-Help by name, prefilled (`z2ui5_cl_popup_shlp`) |
| `z2ui5_cl_popup_sample_22` | Calculator (`z2ui5_cl_popup_calculator`) |
| `z2ui5_cl_popup_sample_23` | Data element search (`z2ui5_cl_popup_dtel_srch`) |
| `z2ui5_cl_popup_sample_24` | Message search (`z2ui5_cl_popup_msg_search`) |
| `z2ui5_cl_popup_sample_25` | Message popover by a button, and in the app's own view (`z2ui5_cl_popup_msgpopover`) |

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
| `src/` | Popup classes (`z2ui5_cl_popup_*`), their result interface `z2ui5_if_popup_result` and the embedded-mode interface `z2ui5_if_popup_embed` |
| `src/00/` | Context/utility class `z2ui5_cl_popup_context` and its exception `z2ui5_cx_popup_error` — vendored copies from [abap-util](https://github.com/abap-util/abap-util), see below |
| `src/02/` | Samples (`z2ui5_cl_popup_sample_*`) |
| `src/03/` | Popups with layout-management dependency (Value-Help, Search-Help) |

The classes in `src/` were moved here from the abap2UI5 core framework
(formerly the built-in popups in its obsolete package). The previous content
of this repository, `z2ui5_cl_pop_value_help`, `z2ui5_cl_pop_search_help` and
`z2ui5_cl_pop_show_tr` with two samples, is gone: `z2ui5_cl_popup_value_help`,
`z2ui5_cl_popup_shlp` and `z2ui5_cl_popup_show_tr` have the same public
interface. After the pull abapGit lists the old classes as deleted in the
repository - delete them.

Three classes were shortened to 25 characters, so that a namespace rename
(`npm run rename`, workflow `build-rename`) of up to 10 characters still fits
the 30-character limit: `z2ui5_cl_popup_search_help` is now
`z2ui5_cl_popup_shlp`, `z2ui5_cl_popup_get_range_m` is
`z2ui5_cl_popup_get_ranges` and `z2ui5_cl_popup_demo_output` is
`z2ui5_cl_popup_demo_out`, with the same public interface. Delete the old
ones after the pull as well.

### Utility class - vendored copy from abap-util

`z2ui5_cl_popup_context` (`src/00/`) is a **renamed copy** of `zabaputil_cl_util_context` from the [abap-util](https://github.com/abap-util/abap-util) master catalog, **reduced to the methods actually used by the popup apps**. This keeps the installation dependency-free (abapGit has no dependency management) and namespace-isolated, while abap-util remains the catalog that contains all utility classes with all methods.

How the copy is maintained:
* Only the context class is trimmed at method level — unused methods are removed (the private helpers a kept method needs stay in the copy); other classes from abap-util would be vendored as-is.
* If a popup needs a utility method that is not in the copy yet, it is added directly to the local `z2ui5_cl_popup_context` (if it already exists in abap-util, it is copied from there with its private helpers instead of re-implemented).
* Every few weeks an AI compares abap-util with all consumers and merges locally added methods back into abap-util, so the master catalog stays the superset of all methods.

## Commits

The abap2UI5 addons (layout-management, popups, table-maintenance) share one
commit model:

- **The addon commits only in its own apps.** A popup or app of the addon
  that saves on a button of its own runs in a roundtrip of its own and ends
  its own LUW - `COMMIT WORK AND WAIT`, or `ROLLBACK WORK` on failure.
- **A library method commits only with `iv_commit = abap_true`.** Every
  public method that writes on behalf of its caller takes
  `iv_commit TYPE abap_bool DEFAULT abap_true`. The default keeps the old
  behaviour: the method ends with `COMMIT WORK AND WAIT` - and so commits
  whatever the caller has open at that moment as well. With
  `iv_commit = abap_false` it neither commits nor rolls back: its writes join
  the caller's LUW, and the caller commits them once, together with its own
  changes.
- **On failure the method raises its exception.** With
  `iv_commit = abap_false` it never runs `ROLLBACK WORK` - whatever it wrote
  before the failure is still in the caller's LUW, and the caller decides,
  normally with `ROLLBACK WORK`. With `iv_commit = abap_true` it fails as it
  always did (the table below).
- **The caller must commit.** abap2UI5 runs `ROLLBACK WORK` before and after
  `main( )` of a stateless app, so what an app writes - itself, or through a
  method called with `iv_commit = abap_false` - is lost unless the app commits
  it in the same roundtrip.

In this addon:

| Where | Commits | On failure with `iv_commit = abap_true` |
|---|---|---|
| `z2ui5_cl_popup_show_tr=>add_data_to_tranport( )` - library | with `iv_commit = abap_true` (default), and only when it recorded entries | `z2ui5_cx_popup_error`, no `ROLLBACK WORK` and no commit |
| `z2ui5_cl_popup_context=>bus_tr_add( )` - the same, called by it | as above | as above |
| The popup apps | never - a popup only returns what the user chose (`z2ui5_cl_popup_show_tr` the transport request) | - |

`add_data_to_tranport( )` records nothing and commits nothing for a local
change (`is_transport-locl`), on ABAP Cloud, or for an empty `ir_data` - a
caller must not rely on it to commit its own changes. An app that writes rows
and records them on the request commits once:

```abap
TRY.
    MODIFY ztab FROM TABLE @lt_rows.
    z2ui5_cl_popup_show_tr=>add_data_to_tranport( ir_data      = REF #( lt_rows )
                                                  iv_tabname   = `ZTAB`
                                                  is_transport = ms_transport
                                                  iv_commit    = abap_false ).
    COMMIT WORK AND WAIT.
  CATCH z2ui5_cx_popup_error INTO DATA(lx).
    ROLLBACK WORK.
    client->message_box_display( text = lx->get_text( ) type = `error` ).
ENDTRY.
```

## Security

The value-help and search-help popups read the DDIC check table the user selects, without an authorization check of their own. Before using them beyond a development system, add your own authorization checks and restrict which tables may be browsed.

## Limitations

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
npm run unit            # ABAP Unit tests on abap2UI5's transpiled Node runtime
npm run downport        # downport the source to 7.02 syntax
npm run site            # the GitHub Pages site (VitePress) into build/site
npm run site:dev        # the same with the VitePress dev server
```

## Contributing

Issues and pull requests are welcome - whether you're fixing bugs, adding new
functionality, or improving documentation. Read
[CONTRIBUTING.md](CONTRIBUTING.md) first.

## License

MIT - see [LICENSE](LICENSE).
