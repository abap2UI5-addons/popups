CLASS z2ui5_cl_popup_sample_10 DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.

    METHODS view_display.
    METHODS on_event.
    METHODS on_navigation.

  PROTECTED SECTION.
    DATA client TYPE REF TO z2ui5_if_client.

  PRIVATE SECTION.
ENDCLASS.


CLASS z2ui5_cl_popup_sample_10 IMPLEMENTATION.

  METHOD on_navigation.

    " every popup hands its result over through z2ui5_if_popup_result
    DATA temp1 TYPE REF TO z2ui5_if_popup_result.
    DATA popup LIKE temp1.
    temp1 ?= client->get_app_prev( ).

    popup = temp1.
    IF popup->check_confirmed( ) = abap_true.
      client->message_box_display( |the input is { popup->value_string( ) }| ).
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
        )->a( n = `title` v = `abap2UI5 - Popup Input Value`
        )->a( n = `navButtonPress` v = client->_event_nav_app_leave( )
        )->a( n = `showNavButton` b = client->check_app_prev_stack( )
        )->tag( `Button`
        )->a( n = `text` v = `Open Popup...`
        )->a( n = `press` v = client->_event( `POPUP` ) ).

    client->view_display( view->stringify( ) ).

  ENDMETHOD.


  METHOD on_event.
      DATA lo_app TYPE REF TO z2ui5_cl_popup_input_val.

    IF client->get( )-event = `POPUP`.

      lo_app = z2ui5_cl_popup_input_val=>factory( text = `Amount of products:` ).
      client->nav_app_call( lo_app ).
    ENDIF.

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

ENDCLASS.
