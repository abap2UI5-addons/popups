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
    DATA temp2 TYPE REF TO z2ui5_cl_ui5_handler.
    DATA temp1 TYPE REF TO z2ui5_cl_ui5_client.
    CREATE OBJECT temp2 TYPE z2ui5_cl_ui5_handler EXPORTING VAL = ``.
    CREATE OBJECT mo_action EXPORTING VAL = temp2.
    mo_action->mo_app->mo_app = io_app.
    mo_action->mo_app->mv_check_initialized = abap_true.
    mo_action->ms_actual-event = iv_event.
    mo_action->ms_actual-t_event_arg = it_arg.

    CREATE OBJECT temp1 TYPE z2ui5_cl_ui5_client EXPORTING ACTION = mo_action.
    io_app->main( temp1 ).

  ENDMETHOD.

  METHOD test_initial_not_confirmed.

    DATA lo_pop TYPE REF TO z2ui5_cl_popup_show_tr.
    DATA temp2 TYPE REF TO z2ui5_if_popup_result.
    DATA li_result LIKE temp2.
    lo_pop = z2ui5_cl_popup_show_tr=>factory( ).

    temp2 ?= lo_pop.

    li_result = temp2.

    cl_abap_unit_assert=>assert_false( li_result->check_confirmed( ) ).

  ENDMETHOD.

  METHOD test_confirmed.

    DATA lo_pop TYPE REF TO z2ui5_cl_popup_show_tr.
    DATA temp3 LIKE lo_pop->mt_data.
    DATA temp4 LIKE LINE OF temp3.
    DATA temp5 TYPE REF TO z2ui5_if_popup_result.
    DATA li_result LIKE temp5.
    DATA temp6 TYPE REF TO z2ui5_cl_popup_context=>ty_s_transport.
    DATA lr_value LIKE temp6.
    lo_pop = z2ui5_cl_popup_show_tr=>factory( ).

    CLEAR temp3.

    CLEAR temp4.
    temp4-transport = `DEVK900000`.
    INSERT temp4 INTO TABLE temp3.
    CLEAR temp4.
    temp4-transport = `DEVK900001`.
    temp4-selkz = abap_true.
    INSERT temp4 INTO TABLE temp3.
    lo_pop->mt_data = temp3.
    roundtrip( io_app   = lo_pop
               iv_event = `SELECT` ).

    temp5 ?= lo_pop.

    li_result = temp5.

    cl_abap_unit_assert=>assert_true( li_result->check_confirmed( ) ).
    cl_abap_unit_assert=>assert_equals( exp = `DEVK900001`
                                        act = li_result->value_string( ) ).

    temp6 ?= li_result->value( ).

    lr_value = temp6.
    cl_abap_unit_assert=>assert_equals( exp = `DEVK900001`
                                        act = lr_value->transport ).

  ENDMETHOD.

  METHOD test_cancelled.

    DATA lo_pop TYPE REF TO z2ui5_cl_popup_show_tr.
    DATA temp7 TYPE REF TO z2ui5_if_popup_result.
    DATA li_result LIKE temp7.
    lo_pop = z2ui5_cl_popup_show_tr=>factory( ).
    roundtrip( io_app   = lo_pop
               iv_event = `CLOSE` ).

    temp7 ?= lo_pop.

    li_result = temp7.

    cl_abap_unit_assert=>assert_false( li_result->check_confirmed( ) ).

  ENDMETHOD.

  METHOD test_no_transport.

    DATA lo_pop TYPE REF TO z2ui5_cl_popup_show_tr.
    DATA temp8 TYPE REF TO z2ui5_if_popup_result.
    DATA li_result LIKE temp8.
    DATA temp9 TYPE REF TO z2ui5_cl_popup_context=>ty_s_transport.
    DATA lr_value LIKE temp9.
    lo_pop = z2ui5_cl_popup_show_tr=>factory( ).
    roundtrip( io_app   = lo_pop
               iv_event = `LOCL` ).

    temp8 ?= lo_pop.

    li_result = temp8.

    cl_abap_unit_assert=>assert_true( li_result->check_confirmed( ) ).

    temp9 ?= li_result->value( ).

    lr_value = temp9.
    cl_abap_unit_assert=>assert_true( lr_value->locl ).
    cl_abap_unit_assert=>assert_initial( li_result->value_string( ) ).

  ENDMETHOD.

  METHOD test_select_without_row.

    DATA lo_pop TYPE REF TO z2ui5_cl_popup_show_tr.
    DATA temp10 LIKE lo_pop->mt_data.
    DATA temp11 LIKE LINE OF temp10.
    DATA temp12 TYPE REF TO z2ui5_if_popup_result.
    DATA li_result LIKE temp12.
    lo_pop = z2ui5_cl_popup_show_tr=>factory( ).

    CLEAR temp10.

    temp11-transport = `DEVK900000`.
    INSERT temp11 INTO TABLE temp10.
    lo_pop->mt_data = temp10.
    roundtrip( io_app   = lo_pop
               iv_event = `SELECT` ).

    temp12 ?= lo_pop.

    li_result = temp12.

    cl_abap_unit_assert=>assert_false( li_result->check_confirmed( ) ).

  ENDMETHOD.

ENDCLASS.
