CLASS z2ui5_cl_popup_sample_11 DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.

    METHODS view_display.
    METHODS on_event.
    METHODS on_navigation.

  PROTECTED SECTION.
    DATA client TYPE REF TO z2ui5_if_client.

  PRIVATE SECTION.
ENDCLASS.


CLASS z2ui5_cl_popup_sample_11 IMPLEMENTATION.

  METHOD on_navigation.
        DATA lo_prev TYPE REF TO z2ui5_if_app.
        DATA temp1 TYPE REF TO z2ui5_cl_popup_file_ul.
        DATA lv_text TYPE z2ui5_cl_popup_file_ul=>ty_s_result-value.

    TRY.

        lo_prev = client->get_app( client->get( )-s_draft-id_prev_app ).

        temp1 ?= lo_prev.

        lv_text = temp1->result( )-value.
        client->message_box_display( |the input is { lv_text }| ).
      CATCH cx_root ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.


  METHOD view_display.

    DATA view TYPE REF TO z2ui5_cl_ui5_view_builder.
    view = z2ui5_cl_ui5_view_builder=>factory(
                     )->ele( n = `View` ns = `mvc`
                     )->a( n = `xmlns` v = `sap.m`
                     )->a( n = `xmlns:mvc` v = `sap.ui.core.mvc`
                     )->a( n = `displayBlock` v = `true`
                     )->a( n = `height` v = `100%` ).
    view->ele( `Shell`
        )->ele( `Page`
        )->a( n = `title` v = `abap2UI5 - Popup File Upload`
        )->a( n = `navButtonPress` v = client->_event_nav_app_leave( )
        )->a( n = `showNavButton` b = client->check_app_prev_stack( )
        )->tag( `Button`
        )->a( n = `text` v = `Open Popup...`
        )->a( n = `press` v = client->_event( `POPUP` ) ).

    client->view_display( view->stringify( ) ).

  ENDMETHOD.


  METHOD on_event.
        DATA lo_app TYPE REF TO z2ui5_cl_popup_file_ul.

    CASE client->get( )-event.

      WHEN `POPUP`.

        lo_app = z2ui5_cl_popup_file_ul=>factory( ).
        client->nav_app_call( lo_app ).
    ENDCASE.

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

ENDCLASS.
