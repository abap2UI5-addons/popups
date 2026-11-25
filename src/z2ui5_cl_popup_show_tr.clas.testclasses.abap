CLASS ltcl_popup_result DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    DATA mo_action TYPE REF TO z2ui5_cl_ui5_action.

    METHODS roundtrip
      IMPORTING
        io_app   TYPE REF TO z2ui5_if_app
        iv_event TYPE string
        it_arg   TYPE string_table OPTIONAL.

    METHODS test_initial_not_confirmed FOR TESTING RAISING cx_static_check.
    METHODS test_confirmed             FOR TESTING RAISING cx_static_check.
    METHODS test_cancelled             FOR TESTING RAISING cx_static_check.
    METHODS test_no_transport          FOR TESTING RAISING cx_static_check.
    METHODS test_select_without_row    FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_popup_result IMPLEMENTATION.

  METHOD roundtrip.

    " a roundtrip with an event, after the first one - as the button press
    " of the user
    mo_action = NEW #( NEW z2ui5_cl_ui5_handler( `` ) ).
    mo_action->mo_app->mo_app = io_app.
    mo_action->mo_app->mv_check_initialized = abap_true.
    mo_action->ms_actual-event = iv_event.
    mo_action->ms_actual-t_event_arg = it_arg.
    io_app->main( NEW z2ui5_cl_ui5_client( mo_action ) ).

  ENDMETHOD.

  METHOD test_initial_not_confirmed.

    DATA(lo_pop) = z2ui5_cl_popup_show_tr=>factory( ).
    DATA(li_result) = CAST z2ui5_if_popup_result( lo_pop ).

    cl_abap_unit_assert=>assert_false( li_result->check_confirmed( ) ).

  ENDMETHOD.

  METHOD test_confirmed.

    DATA(lo_pop) = z2ui5_cl_popup_show_tr=>factory( ).
    lo_pop->mt_data = VALUE #( ( transport = `DEVK900000` )
                               ( transport = `DEVK900001` selkz = abap_true ) ).
    roundtrip( io_app   = lo_pop
               iv_event = `SELECT` ).
    DATA(li_result) = CAST z2ui5_if_popup_result( lo_pop ).

    cl_abap_unit_assert=>assert_true( li_result->check_confirmed( ) ).
    cl_abap_unit_assert=>assert_equals( exp = `DEVK900001`
                                        act = li_result->value_string( ) ).
    DATA(lr_value) = CAST z2ui5_cl_popup_context=>ty_s_transport( li_result->value( ) ).
    cl_abap_unit_assert=>assert_equals( exp = `DEVK900001`
                                        act = lr_value->transport ).

  ENDMETHOD.

  METHOD test_cancelled.

    DATA(lo_pop) = z2ui5_cl_popup_show_tr=>factory( ).
    roundtrip( io_app   = lo_pop
               iv_event = `CLOSE` ).
    DATA(li_result) = CAST z2ui5_if_popup_result( lo_pop ).

    cl_abap_unit_assert=>assert_false( li_result->check_confirmed( ) ).

  ENDMETHOD.

  METHOD test_no_transport.

    DATA(lo_pop) = z2ui5_cl_popup_show_tr=>factory( ).
    roundtrip( io_app   = lo_pop
               iv_event = `LOCL` ).
    DATA(li_result) = CAST z2ui5_if_popup_result( lo_pop ).

    cl_abap_unit_assert=>assert_true( li_result->check_confirmed( ) ).
    DATA(lr_value) = CAST z2ui5_cl_popup_context=>ty_s_transport( li_result->value( ) ).
    cl_abap_unit_assert=>assert_true( lr_value->locl ).
    cl_abap_unit_assert=>assert_initial( li_result->value_string( ) ).

  ENDMETHOD.

  METHOD test_select_without_row.

    DATA(lo_pop) = z2ui5_cl_popup_show_tr=>factory( ).
    lo_pop->mt_data = VALUE #( ( transport = `DEVK900000` ) ).
    roundtrip( io_app   = lo_pop
               iv_event = `SELECT` ).
    DATA(li_result) = CAST z2ui5_if_popup_result( lo_pop ).

    cl_abap_unit_assert=>assert_false( li_result->check_confirmed( ) ).

  ENDMETHOD.

ENDCLASS.
