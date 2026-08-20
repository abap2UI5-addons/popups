CLASS z2ui5_cl_popup_sample_05 DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.


CLASS z2ui5_cl_popup_sample_05 IMPLEMENTATION.

  METHOD z2ui5_if_app~main.

    IF client->check_on_init( ).

      DATA(view) = z2ui5_cl_ui5_view_builder=>factory( 
                       )->ele( n = `View` ns = `mvc` 
                       )->a( n = `xmlns` v = `sap.m` 
                       )->a( n = `xmlns:mvc` v = `sap.ui.core.mvc` 
                       )->a( n = `xmlns:core` v = `sap.ui.core` 
                       )->a( n = `displayBlock` v = `true` 
                       )->a( n = `height` v = `100%` ).
      view->ele( `Shell` 
          )->ele( `Page` 
          )->a( n = `title` v = `abap2UI5 - Popup To Confirm` 
          )->a( n = `navButtonPress` v = client->_event_nav_app_leave( ) 
          )->a( n = `showNavButton` b = client->check_app_prev_stack( ) 
          )->tag( `Button` 
          )->a( n = `text` v = `Open Popup...` 
          )->a( n = `press` v = client->_event( `POPUP` ) ).
      client->view_display( view->stringify( ) ).

    ELSEIF client->check_on_event( `POPUP` ).

      DATA(lo_app) = z2ui5_cl_popup_to_confirm=>factory( i_question_text = `this is a question`
                                                       i_event_confirm = `POPUP_TRUE`
                                                       i_event_cancel  = `POPUP_FALSE` ).
      client->nav_app_call( lo_app ).

    ELSEIF client->check_on_event( `POPUP_TRUE` ).
      client->message_box_display( `the result is SUCCESS` ).

    ELSEIF client->check_on_event( `POPUP_FALSE` ).
      client->message_box_display( `the result is CANCEL` ).

    ENDIF.

  ENDMETHOD.

ENDCLASS.
