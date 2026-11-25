CLASS z2ui5_cl_popup_sample_23 DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.

    DATA mv_value  TYPE string.
    DATA mv_result TYPE string.

  PROTECTED SECTION.
    DATA client TYPE REF TO z2ui5_if_client.

    METHODS view_display.
    METHODS on_event.
    METHODS on_navigated.

  PRIVATE SECTION.
ENDCLASS.


CLASS z2ui5_cl_popup_sample_23 IMPLEMENTATION.

  METHOD z2ui5_if_app~main.

    me->client = client.

    IF client->check_on_init( ).
      mv_value = `material`.
      view_display( ).
      RETURN.
    ENDIF.

    IF client->check_on_navigated( ).
      on_navigated( ).
      view_display( ).
      RETURN.
    ENDIF.

    on_event( ).

  ENDMETHOD.

  METHOD on_event.

    IF client->get( )-event = `SEARCH`.
      client->nav_app_call( z2ui5_cl_popup_dtel_srch=>factory( mv_value ) ).
    ENDIF.

  ENDMETHOD.

  METHOD on_navigated.

    TRY.
        DATA(lo_popup) = CAST z2ui5_cl_popup_dtel_srch( client->get_app_prev( ) ).
      CATCH cx_sy_move_cast_error.
        RETURN.
    ENDTRY.

    DATA(ls_result) = lo_popup->result( ).
    IF ls_result-check_confirmed = abap_false.
      client->message_toast_display( `Search cancelled` ).
      RETURN.
    ENDIF.

    mv_value  = ls_result-rollname.
    mv_result = |{ ls_result-rollname } - { ls_result-description } ({ ls_result-datatype } { ls_result-leng })|.

  ENDMETHOD.

  METHOD view_display.

    DATA(view) = z2ui5_cl_ui5_view_builder=>factory(
                     )->ele( n = `View` ns = `mvc`
                     )->a( n = `xmlns` v = `sap.m`
                     )->a( n = `xmlns:mvc` v = `sap.ui.core.mvc`
                     )->a( n = `displayBlock` v = `true`
                     )->a( n = `height` v = `100%` ).

    view->ele( `Shell`
        )->ele( `Page`
        )->a( n = `title` v = `abap2UI5 - Popup Data Element Search`
        )->a( n = `navButtonPress` v = client->_event_nav_app_leave( )
        )->a( n = `showNavButton` b = client->check_app_prev_stack( )
        )->ele( `VBox`
        )->a( n = `class` v = `sapUiSmallMargin`
        )->tag( `Label`
        )->a( n = `text` v = `Search Text`
        )->tag( `Input`
        )->a( n = `value` v = client->_bind( mv_value )
        )->a( n = `submit` v = client->_event( `SEARCH` )
        )->a( n = `width` v = `20rem`
        )->tag( `Button`
        )->a( n = `text` v = `Search Data Element...`
        )->a( n = `press` v = client->_event( `SEARCH` )
        )->tag( `Label`
        )->a( n = `text` v = `Selected Data Element`
        )->a( n = `class` v = `sapUiSmallMarginTop`
        )->tag( `Text`
        )->a( n = `text` v = client->_bind( mv_result ) ).

    client->view_display( view->stringify( ) ).

  ENDMETHOD.

ENDCLASS.
