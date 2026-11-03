---
layout: home

hero:
  name: "Popups"
  text: "Ready-to-use popups and dialogs, one factory( ) call each."
  image:
    src: /favicon.png
    alt: abap2UI5
  actions:
    - theme: brand
      text: Get started
      link: /guide
    - theme: alt
      text: GitHub
      link: https://github.com/abap2UI5-addons/popups
    - theme: alt
      text: All addons
      link: https://abap2ui5-addons.github.io/

features:
  - title: "Confirm, Inform, Select, Decide and Get Values popups"
  - title: "File upload and download, including binary and multiple files"
  - title: "Table, data and CL_DEMO_OUTPUT style output popups"
  - title: "Text editor, HTML and PDF display; messages, error and input validation"
  - title: "Range selection, value help and search help popups"
  - title: "Transport request popup (on-premise only)"
  - title: "A sample app for every popup (z2ui5_cl_popup_sample_*)"
---

## What it does

popups is a library of finished dialog classes for abap2UI5 apps: confirm, inform, select from a list, decide, get values, edit a text, upload and download files, show a table, a PDF, HTML or messages, pick a range, a value help or a search help. Each popup is an abap2UI5 app of its own that you call with client->nav_app_call( ) and that hands its result back as an event you name or through its result( ) method. The classes were moved here from the abap2UI5 core framework. Use it when you need standard dialogs without writing views, or as counterparts for POPUP_TO_CONFIRM, POPUP_TO_INFORM and CL_DEMO_OUTPUT when moving classic code.

## Requirements

- abap2UI5
- abap2UI5-addons/layout-management (current version, pulled together with this repository)
- ABAP Cloud (S/4 Public Cloud, BTP ABAP Environment), S/4 Private Cloud or On-Premise, or NetWeaver 7.50 or higher
- NetWeaver 7.02 - 7.40 with the downported branch 702
- Transports only on On-Premise; search help not yet on the Cloud stack

## Installation

With [abapGit](https://abapgit.org), from [the repository](https://github.com/abap2UI5-addons/popups):

- Install with abapGit in this order: 1. abap2UI5, 2. layout-management, 3. this repository from the branch that fits your system
- Branch main for ABAP Cloud, S/4HANA and NetWeaver 7.50 or higher; branch 702 for NetWeaver 7.02 - 7.40 (pull layout-management from 702 as well)
- Start a sample like any abap2UI5 app, e.g. ?app_start=z2ui5_cl_popup_sample_05

The [Guide](/guide) has every step.

## A first app

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

## Links

- [abap2UI5-addons/layout-management](https://github.com/abap2UI5-addons/layout-management)
- [abap-util](https://github.com/abap-util/abap-util)
- [abap2UI5](https://github.com/abap2UI5/abap2UI5)
- [The repository](https://github.com/abap2UI5-addons/popups) - issues and pull requests welcome
