[![ABAP_STANDARD](https://github.com/abap2UI5-addons/popups/actions/workflows/ABAP_STANDARD.yaml/badge.svg)](https://github.com/abap2UI5-addons/popups/actions/workflows/ABAP_STANDARD.yaml)
[![ABAP_CLOUD](https://github.com/abap2UI5-addons/popups/actions/workflows/ABAP_CLOUD.yaml/badge.svg)](https://github.com/abap2UI5-addons/popups/actions/workflows/ABAP_CLOUD.yaml)
<br>
[![rename_test](https://github.com/abap2UI5-addons/popups/actions/workflows/rename_test.yaml/badge.svg)](https://github.com/abap2UI5-addons/popups/actions/workflows/rename_test.yaml)

# Popups

Ready-to-use popup and dialog apps for abap2UI5. The classes in `src/` were moved here from the abap2UI5 core framework (formerly the built-in popups in its obsolete package); the previous content of this repository lives in `src/99/` (obsolete).

#### Key Features
* Confirm, Inform & Select popups (`z2ui5_cl_popup_to_confirm`, `z2ui5_cl_popup_to_inform`, `z2ui5_cl_popup_to_select`)
* File Download & Upload (`z2ui5_cl_popup_file_dl`, `z2ui5_cl_popup_file_ul`)
* Table, Data & Demo Output (`z2ui5_cl_popup_table`, `z2ui5_cl_popup_data`, `z2ui5_cl_popup_demo_output`)
* Text Editor, HTML & PDF display (`z2ui5_cl_popup_textedit`, `z2ui5_cl_popup_html`, `z2ui5_cl_popup_pdf`)
* Messages, Error & Input Validation (`z2ui5_cl_popup_messages`, `z2ui5_cl_popup_error`, `z2ui5_cl_popup_input_val`)
* Range Selection (`z2ui5_cl_popup_get_range`, `z2ui5_cl_popup_get_range_m`)
* Image Editor & JS Loader (`z2ui5_cl_popup_image_editor`, `z2ui5_cl_popup_js_loader`)
* Value-Help & Search-Help (`z2ui5_cl_popup_value_help`, `z2ui5_cl_popup_search_help`)
* Transport Requests (`z2ui5_cl_popup_show_tr`)
* Samples for all popups (`src/02/`, `z2ui5_cl_popup_sample_*`)

#### Package Structure
| Package | Content |
|---|---|
| `src/` | Popup classes (`z2ui5_cl_popup_*`) |
| `src/00/` | Context/utility class `z2ui5_cl_popup_context` (no external util dependencies) |
| `src/02/` | Samples (`z2ui5_cl_popup_sample_*`) |
| `src/03/` | Popups with layout-management dependency (Value-Help, Search-Help) |
| `src/99/` | Obsolete: the original classes of this repository, kept for compatibility |

#### Compatibility
* S/4 Private Cloud or On-Premise (Standard ABAP)
* SAP NetWeaver AS ABAP 7.50 or higher (Standard ABAP)

#### Dependencies
* [abap2UI5](https://github.com/abap2UI5/abap2UI5)
* [layout-management](https://github.com/abap2UI5-addons/layout-management)

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
