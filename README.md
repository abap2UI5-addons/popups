[![ABAP_STANDARD](https://github.com/abap2UI5-addons/popups/actions/workflows/ABAP_STANDARD.yaml/badge.svg)](https://github.com/abap2UI5-addons/popups/actions/workflows/ABAP_STANDARD.yaml)
[![ABAP_CLOUD](https://github.com/abap2UI5-addons/popups/actions/workflows/ABAP_CLOUD.yaml/badge.svg)](https://github.com/abap2UI5-addons/popups/actions/workflows/ABAP_CLOUD.yaml)
<br>
[![rename_test](https://github.com/abap2UI5-addons/popups/actions/workflows/rename_test.yaml/badge.svg)](https://github.com/abap2UI5-addons/popups/actions/workflows/rename_test.yaml)
[![ABAP_702](https://github.com/abap2UI5-addons/popups/actions/workflows/ABAP_702.yaml/badge.svg)](https://github.com/abap2UI5-addons/popups/actions/workflows/ABAP_702.yaml)

# Popups

Ready-to-use popup and dialog apps for abap2UI5. The classes in `src/` were moved here from the abap2UI5 core framework (formerly the built-in popups in its obsolete package); the previous content of this repository lives in `src/99/` (obsolete).

#### Key Features
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

#### Package Structure
| Package | Content |
|---|---|
| `src/` | Popup classes (`z2ui5_cl_popup_*`) |
| `src/00/` | Context/utility class `z2ui5_cl_popup_context` — a vendored copy from [abap-util](https://github.com/abap-util/abap-util), see below |
| `src/02/` | Samples (`z2ui5_cl_popup_sample_*`) |
| `src/03/` | Popups with layout-management dependency (Value-Help, Search-Help) |
| `src/99/` | Obsolete: the original classes of this repository, kept for compatibility |

#### Utility Class — Vendored Copy from abap-util

`z2ui5_cl_popup_context` (`src/00/`) is a **renamed copy** of `zabaputil_cl_util_context` from the [abap-util](https://github.com/abap-util/abap-util) master catalog, **reduced to the methods actually used by the popup apps**. This keeps the installation dependency-free (abapGit has no dependency management) and namespace-isolated, while abap-util remains the catalog that contains all utility classes with all methods.

How the copy is maintained:
* Only the context class is trimmed at method level — unused methods are removed (the private helpers a kept method needs stay in the copy); other classes from abap-util would be vendored as-is.
* If a popup needs a utility method that is not in the copy yet, it is added directly to the local `z2ui5_cl_popup_context` (if it already exists in abap-util, it is copied from there with its private helpers instead of re-implemented).
* Every few weeks an AI compares abap-util with all consumers and merges locally added methods back into abap-util, so the master catalog stays the superset of all methods.

#### Compatibility
* S/4 Private Cloud or On-Premise (Standard ABAP)
* SAP NetWeaver AS ABAP 7.50 or higher (Standard ABAP)

#### Security
The value-help and search-help popups read the DDIC check table the user selects, without an authorization check of their own. Before using them beyond a development system, add your own authorization checks and restrict which tables may be browsed.

#### Dependencies
* [abap2UI5](https://github.com/abap2UI5/abap2UI5)
* [layout-management](https://github.com/abap2UI5-addons/layout-management)

No dependency on abap-util at installation time — the needed utilities are embedded as a vendored copy (see above).

#### Limitations & Todo
* Transports currently only work in On-Premise
* Search-Help not yet running on Cloud Stack

#### Demo

###### Search-Help
<img width="600" alt="image" src="https://github.com/user-attachments/assets/e7662e27-d16d-4949-87dc-bb8f13246719" />

###### Value-Help
<img width="600" alt="image" src="https://github.com/user-attachments/assets/68d1c41a-52d0-4d98-b1dd-e3b869c5500d" />


###### Transport Requests
<img width="600" alt="image" src="https://github.com/user-attachments/assets/5614466c-0b92-45f3-945d-0cdb5e3acb4c" />


#### Contribution & Support
Pull requests are welcome! Whether you're fixing bugs, adding new functionality, or improving documentation, your contributions are highly appreciated. If you encounter any issues, feel free to open an issue.
