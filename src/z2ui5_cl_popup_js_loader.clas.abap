CLASS z2ui5_cl_popup_js_loader DEFINITION PUBLIC.

  PUBLIC SECTION.
    " abap2ui5lint-disable unbound-public-attribute -- a popup's attributes are its interface to the app that called it, which reads them once the popup returns
    INTERFACES z2ui5_if_app.

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

    DATA popup TYPE REF TO z2ui5_cl_ui5_view_builder.
      DATA temp4 TYPE string_table.
    popup = z2ui5_cl_ui5_view_builder=>factory(
                      )->ele( n = `FragmentDefinition` ns = `core`
                      )->a( n = `xmlns` v = `sap.m`
                      )->a( n = `xmlns:core` v = `sap.ui.core`
                      )->a( n = `xmlns:html` v = `http://www.w3.org/1999/xhtml`
                      )->ele( `Dialog`
                      )->a( n = `title` v = `Setup UI...`
                      )->ele( `content` ).

    IF js IS NOT INITIAL.
      popup->ele( n = `script` ns = `html`
          )->tag( n = `ZZPLAIN` ns = `html`
          " abap2ui5lint-disable-next-line unescaped-text-in-attribute -- raw JavaScript for the script tag; escaping its braces would change the code
          )->a( n = `VALUE` v = js ).
    ENDIF.

    client->popup_display( popup->stringify( ) ).

    IF js IS NOT INITIAL.
      " closes the popup in the roundtrip the client timer fires once the
      " popup with the script has rendered

      CLEAR temp4.
      INSERT `TIMER_FINISHED` INTO TABLE temp4.
      INSERT `0` INTO TABLE temp4.
      client->follow_up_action( val   = z2ui5_if_client=>cs_event-start_timer
                                t_arg = temp4 ).
    ENDIF.

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
    ENDIF.

    CASE client->get( )-event.
      WHEN `TIMER_FINISHED`.
        client->popup_destroy( ).
        client->nav_app_leave( ).
    ENDCASE.

  ENDMETHOD.

ENDCLASS.
