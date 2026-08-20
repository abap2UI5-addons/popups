CLASS z2ui5_cl_popup_sample_03 DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.

    DATA mv_image TYPE string.

    METHODS view_display.
    METHODS on_event.
    METHODS on_navigation.

  PROTECTED SECTION.
    DATA client TYPE REF TO z2ui5_if_client.

  PRIVATE SECTION.
ENDCLASS.


CLASS z2ui5_cl_popup_sample_03 IMPLEMENTATION.

  METHOD on_event.

    CASE client->get( )-event.

      WHEN `POPUP`.
        DATA(lo_app) = z2ui5_cl_popup_image_edit=>factory( mv_image ).
        client->nav_app_call( lo_app ).
    ENDCASE.

  ENDMETHOD.


  METHOD view_display.

    IF mv_image IS INITIAL.
      mv_image = `https://raw.githubusercontent.com/abap2UI5/abap2UI5/main/docs/images/logo.png`.
    ENDIF.

    DATA(view) = z2ui5_cl_ui5_view_builder=>factory( 
                     )->ele( n = `View` ns = `mvc` 
                     )->a( n = `xmlns` v = `sap.m` 
                     )->a( n = `xmlns:mvc` v = `sap.ui.core.mvc` 
                     )->a( n = `xmlns:core` v = `sap.ui.core` 
                     )->a( n = `displayBlock` v = `true` 
                     )->a( n = `height` v = `100%` ).
    view->ele( `Shell` 
        )->ele( `Page` 
        )->a( n = `title` v = `abap2UI5 - Popup Image Editor` 
        )->a( n = `navButtonPress` v = client->_event_nav_app_leave( ) 
        )->a( n = `showNavButton` b = client->check_app_prev_stack( ) 
        )->tag( `Button` 
        )->a( n = `text` v = `Open Popup...` 
        )->a( n = `press` v = client->_event( `POPUP` ) 
        )->tag( `Image` 
        )->a( n = `src` v = client->_bind( mv_image ) 
        )->a( n = `width` v = `20rem` ).

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

    TRY.
        DATA(lo_prev) = client->get_app( client->get( )-s_draft-id_prev_app ).
        DATA(ls_result) = CAST z2ui5_cl_popup_image_edit( lo_prev )->result( ).
        IF ls_result-check_confirmed = abap_true AND ls_result-image IS NOT INITIAL.
          mv_image = ls_result-image.
          client->message_toast_display( `image saved` ).
        ENDIF.
      CATCH cx_root.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.
