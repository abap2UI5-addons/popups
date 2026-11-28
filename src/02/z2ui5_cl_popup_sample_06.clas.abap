CLASS z2ui5_cl_popup_sample_06 DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.

    METHODS view_display.
    METHODS on_event.
    METHODS on_navigation.

  PROTECTED SECTION.
    DATA client TYPE REF TO z2ui5_if_client.

  PRIVATE SECTION.
ENDCLASS.


CLASS z2ui5_cl_popup_sample_06 IMPLEMENTATION.

  METHOD on_event.
      DATA lo_app TYPE REF TO z2ui5_cl_popup_to_inform.

    IF client->get( )-event = `POPUP`.

      lo_app = z2ui5_cl_popup_to_inform=>factory( `this is a question` ).
      client->nav_app_call( lo_app ).
    ENDIF.

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
        )->a( n = `title` v = `abap2UI5 - Popup To Inform`
        )->a( n = `navButtonPress` v = client->_event_nav_app_leave( )
        )->a( n = `showNavButton` b = client->check_app_prev_stack( )
        )->tag( `Button`
        )->a( n = `text` v = `Open Popup...`
        )->a( n = `press` v = client->_event( `POPUP` ) ).

    client->view_display( view->stringify( ) ).

  ENDMETHOD.


  METHOD z2ui5_if_app~main.

    me->client = client.

    IF client->check_on_navigated( ) IS NOT INITIAL.
      view_display( ).
      " true only on the way back from the popup - not on the first start
      " or on a restore, where get_app_prev( ) is no popup
      IF client->check_on_nav_back( ) IS NOT INITIAL.
        on_navigation( ).
      ENDIF.
      RETURN.
    ENDIF.

    on_event( ).

  ENDMETHOD.


  METHOD on_navigation.

    " the popup only informs - it has no value to read
    client->message_box_display( `callback after popup to inform` ).

  ENDMETHOD.

ENDCLASS.
