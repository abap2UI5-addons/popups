CLASS z2ui5_cl_popup_sample_22 DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.

    DATA amount TYPE p LENGTH 15 DECIMALS 2.

  PROTECTED SECTION.
    DATA client TYPE REF TO z2ui5_if_client.

    METHODS view_display.
    METHODS on_event.
    METHODS on_navigated.

  PRIVATE SECTION.
ENDCLASS.


CLASS z2ui5_cl_popup_sample_22 IMPLEMENTATION.

  METHOD z2ui5_if_app~main.

    me->client = client.

    IF client->check_on_init( ) IS NOT INITIAL.
      amount = `12.50`.
      view_display( ).
    ELSEIF client->check_on_navigated( ) IS NOT INITIAL.
      on_navigated( ).
      view_display( ).
    ELSEIF client->check_on_event( ) IS NOT INITIAL.
      on_event( ).
    ENDIF.

  ENDMETHOD.


  METHOD view_display.

    DATA view TYPE REF TO z2ui5_cl_ui5_view_builder.
    DATA page TYPE REF TO z2ui5_cl_ui5_view_builder.
    view = z2ui5_cl_ui5_view_builder=>factory(
        )->ele( n = `View` ns = `mvc`
            )->a( n = `xmlns`        v = `sap.m`
            )->a( n = `xmlns:mvc`    v = `sap.ui.core.mvc`
            )->a( n = `displayBlock` v = `true`
            )->a( n = `height`       v = `100%` ).

    page = view->ele( `Shell`
        )->ele( `Page`
            )->a( n = `title`          v = `abap2UI5 - Popup Calculator`
            )->a( n = `navButtonPress` v = client->_event_nav_app_leave( )
            )->a( n = `showNavButton`  b = client->check_app_prev_stack( ) ).

    page->ele( `VBox`
        )->a( n = `class` v = `sapUiSmallMargin`
        )->tag( `Label`
            )->a( n = `text`     v = `Amount`
            )->a( n = `labelFor` v = `amount`
        )->tag( `Input`
            )->a( n = `id`    v = `amount`
            )->a( n = `value` v = client->_bind( amount )
            )->a( n = `width` v = `15rem`
        )->tag( `Button`
            )->a( n = `text`  v = `Calculate...`
            )->a( n = `press` v = client->_event( `CALCULATOR` ) ).

    client->view_display( view->stringify( ) ).

  ENDMETHOD.


  METHOD on_event.
      DATA temp1 TYPE decfloat34.

    IF client->get_event( ) = `CALCULATOR`.
      " the calculator computes in DECFLOAT34 - the amount is packed

      temp1 = amount.
      client->nav_app_call( z2ui5_cl_popup_calculator=>factory( i_value = temp1
                                                                i_title = `Calculate Amount` ) ).
    ENDIF.

  ENDMETHOD.


  METHOD on_navigated.
        DATA temp2 TYPE REF TO z2ui5_cl_popup_calculator.
        DATA calculator LIKE temp2.
    DATA result TYPE z2ui5_cl_popup_calculator=>ty_s_result.

    TRY.

        temp2 ?= client->get_app_prev( ).

        calculator = temp2.
      CATCH cx_sy_move_cast_error.
        RETURN.
    ENDTRY.


    result = calculator->result( ).
    IF result-check_confirmed = abap_false.
      RETURN.
    ENDIF.

    TRY.
        amount = result-value.
        client->message_toast_display( `Amount taken over from the calculator` ).
      CATCH cx_sy_conversion_error cx_sy_arithmetic_error.
        client->message_box_display( text = `The result does not fit into the amount field`
                                     type = `error` ).
    ENDTRY.

  ENDMETHOD.

ENDCLASS.
