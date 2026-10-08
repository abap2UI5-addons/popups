CLASS z2ui5_cl_demo_app_155 DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.

    METHODS view_display.
    METHODS on_event.
    METHODS on_navigation.

  PROTECTED SECTION.
    DATA client TYPE REF TO z2ui5_if_client.

  PRIVATE SECTION.
ENDCLASS.


CLASS z2ui5_cl_demo_app_155 IMPLEMENTATION.

  METHOD on_event.
        DATA lo_app TYPE REF TO z2ui5_cl_pop_textedit.

    CASE client->get( )-event.

      WHEN `POPUP`.

        lo_app = z2ui5_cl_pop_textedit=>factory( `this is a text` ).
        client->nav_app_call( lo_app ).
    ENDCASE.

  ENDMETHOD.


  METHOD view_display.

    DATA view TYPE REF TO z2ui5_cl_xml_view.
    view = z2ui5_cl_xml_view=>factory( ).
    view->shell(
        )->page(
                title          = `abap2UI5 - Popup To Text Edit`
                navbuttonpress = client->_event_nav_app_leave( )
                shownavbutton  = client->check_app_prev_stack( )
           )->button(
            text  = `Open Popup...`
            press = client->_event( `POPUP` ) ).

    client->view_display( view->stringify( ) ).

  ENDMETHOD.


  METHOD z2ui5_if_app~main.

    me->client = client.

    IF client->get( )-check_on_navigated = abap_true.

      view_display( ).
      on_navigation( ).
      RETURN.
    ENDIF.

    on_event( ).

  ENDMETHOD.


  METHOD on_navigation.
        DATA lo_prev TYPE REF TO z2ui5_if_app.
        DATA temp1 TYPE REF TO z2ui5_cl_pop_textedit.
        DATA lv_text TYPE z2ui5_cl_pop_textedit=>ty_s_result-text.

    TRY.

        lo_prev = client->get_app( client->get( )-s_draft-id_prev_app ).

        temp1 ?= lo_prev.

        lv_text = temp1->result( )-text.
        client->message_box_display( |the result is { lv_text }| ).
      CATCH cx_root.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.
