[![ABAP_STANDARD](https://github.com/abap2UI5-addons/popups/actions/workflows/ABAP_STANDARD.yaml/badge.svg)](https://github.com/abap2UI5-addons/popups/actions/workflows/ABAP_STANDARD.yaml)
[![ABAP_CLOUD](https://github.com/abap2UI5-addons/popups/actions/workflows/ABAP_CLOUD.yaml/badge.svg)](https://github.com/abap2UI5-addons/popups/actions/workflows/ABAP_CLOUD.yaml)
<br>
[![rename_test](https://github.com/abap2UI5-addons/popups/actions/workflows/rename_test.yaml/badge.svg)](https://github.com/abap2UI5-addons/popups/actions/workflows/rename_test.yaml)

# Popups

Ready-to-use popup and dialog apps for abap2UI5. The classes in `src/` were moved here from the abap2UI5 core framework (formerly the built-in popups in its obsolete package); the previous content of this repository (Value-Help, Search-Help, Transport Requests and their samples) now lives in `src/99/`.

#### Key Features
* Confirm, Inform & Select popups (`z2ui5_cl_pop_to_confirm`, `z2ui5_cl_pop_to_inform`, `z2ui5_cl_pop_to_select`)
* File Download & Upload (`z2ui5_cl_pop_file_dl`, `z2ui5_cl_pop_file_ul`)
* Table, Data & Demo Output (`z2ui5_cl_pop_table`, `z2ui5_cl_pop_data`, `z2ui5_cl_pop_demo_output`)
* Text Editor, HTML & PDF display (`z2ui5_cl_pop_textedit`, `z2ui5_cl_pop_html`, `z2ui5_cl_pop_pdf`)
* Messages, Error & Input Validation (`z2ui5_cl_pop_messages`, `z2ui5_cl_pop_error`, `z2ui5_cl_pop_input_val`)
* Range Selection (`z2ui5_cl_pop_get_range`, `z2ui5_cl_pop_get_range_m`)
* Image Editor & JS Loader (`z2ui5_cl_pop_image_editor`, `z2ui5_cl_pop_js_loader`)
* Legacy (`src/99/`): Value-Help, Search-Help, Transport Requests

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
