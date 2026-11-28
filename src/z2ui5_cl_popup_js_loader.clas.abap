CLASS z2ui5_cl_popup_js_loader DEFINITION PUBLIC.

  PUBLIC SECTION.
    " abap2ui5lint-disable unbound-public-attribute -- a popup's attributes are its interface to the app that called it, which reads them once the popup returns
    INTERFACES z2ui5_if_app.
    " check_confirmed( ) - always abap_true, the popup only loads a script;
    " value( ) - REF TO string, result( ) - for factory_check_open_ui5 REF TO
    " abap_bool, mv_is_open_ui5; value_string( ) - result( ), for
    " factory_check_open_ui5 the UI5 runtime (ui5_gav)
    INTERFACES z2ui5_if_popup_result.

    "! DEPRECATED - i_js is NOT run. abap2UI5 removed every path for raw
    "! JavaScript (no script element in a view, no JavaScript in
    "! follow_up_action( )); frontend code of an app's own ships as a custom
    "! control in the customer frontend BSP (z2ui5_ccc). The popup leaves at
    "! once and result( ) still returns i_result, so an existing caller goes
    "! on as before - but nothing has been loaded. Kept for compatibility;
    "! factory_check_open_ui5( ) is unaffected.
    CLASS-METHODS factory
      IMPORTING
        i_js            TYPE string
        i_result        TYPE string DEFAULT `LOADED`
      RETURNING
        VALUE(r_result) TYPE REF TO z2ui5_cl_popup_js_loader.

    CLASS-METHODS factory_check_open_ui5
      RETURNING
        VALUE(r_result) TYPE REF TO z2ui5_cl_popup_js_loader.

    METHODS result
      RETURNING
        VALUE(result) TYPE string.

    DATA mv_is_open_ui5 TYPE abap_bool.
    DATA ui5_gav        TYPE string.
    " abap2ui5lint-enable unbound-public-attribute

  PROTECTED SECTION.
    DATA client         TYPE REF TO z2ui5_if_client.
    DATA js             TYPE string.
    DATA user_command   TYPE string.
    DATA check_open_ui5 TYPE abap_bool.

    METHODS view_display.

  PRIVATE SECTION.
ENDCLASS.


CLASS z2ui5_cl_popup_js_loader IMPLEMENTATION.

  METHOD factory.

    CREATE OBJECT r_result.
    r_result->js           = i_js.
    r_result->user_command = i_result.

  ENDMETHOD.

  METHOD factory_check_open_ui5.
    CREATE OBJECT r_result.
    r_result->check_open_ui5 = abap_true.
  ENDMETHOD.

  METHOD result.

    result = user_command.

  ENDMETHOD.

  METHOD view_display.

    " i_js is not run (see factory( )): the script element this popup used
    " to render through the ZZPLAIN pseudo-tag of the old xml_view reached
    " the browser as an empty element, and abap2UI5 has no other path for
    " raw JavaScript - so there is no popup to show, the app leaves at once
    client->nav_app_leave( ).

  ENDMETHOD.

  METHOD z2ui5_if_app~main.
        DATA temp1 TYPE xsdboolean.

    me->client = client.

    IF client->check_on_init( ) IS NOT INITIAL.

      IF check_open_ui5 = abap_true.
        " the frontend reports the UI5 runtime with the request itself, so
        " the answer is known right away - no popup, no extra roundtrip
        ui5_gav = client->get( )-s_ui5-gav.

        temp1 = boolc( ui5_gav CS `OPEN` ).
        mv_is_open_ui5 = temp1.
        client->nav_app_leave( ).
        RETURN.
      ENDIF.

      view_display( ).
      RETURN.
    ELSEIF client->check_on_navigated( ) IS NOT INITIAL.
      " a restored draft (browser Forward, a reload)
      view_display( ).
      RETURN.
    ENDIF.

    " a popup still open from before i_js was given up: its timer fires
    IF client->get( )-event = `TIMER_FINISHED`.
      client->popup_destroy( ).
      client->nav_app_leave( ).
    ENDIF.

  ENDMETHOD.

  METHOD z2ui5_if_popup_result~check_confirmed.

    " shows something only - nothing to decline
    result = abap_true.

  ENDMETHOD.

  METHOD z2ui5_if_popup_result~value.

    IF check_open_ui5 = abap_true.
      GET REFERENCE OF mv_is_open_ui5 INTO result.
    ELSE.
      GET REFERENCE OF user_command INTO result.
    ENDIF.

  ENDMETHOD.

  METHOD z2ui5_if_popup_result~value_string.

    IF check_open_ui5 = abap_true.
      result = ui5_gav.
    ELSE.
      result = user_command.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
