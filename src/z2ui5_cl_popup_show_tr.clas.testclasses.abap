CLASS ltcl_popup_result DEFINITION DEFERRED.
CLASS z2ui5_cl_popup_show_tr DEFINITION LOCAL FRIENDS ltcl_popup_result.
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
    METHODS test_select_ignores_client FOR TESTING RAISING cx_static_check.

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
    DATA temp3 TYPE z2ui5_cl_popup_context=>ty_t_data.
    DATA temp4 LIKE LINE OF temp3.
    FIELD-SYMBOLS <temp5> LIKE LINE OF lo_pop->mt_data.
    DATA temp6 LIKE sy-tabix.
    DATA temp7 TYPE REF TO z2ui5_if_popup_result.
    DATA li_result LIKE temp7.
    DATA temp8 TYPE REF TO z2ui5_cl_popup_context=>ty_s_transport.
    DATA lr_value LIKE temp8.
    lo_pop = z2ui5_cl_popup_show_tr=>factory( ).

    CLEAR temp3.

    temp4-transport = `DEVK900000`.
    INSERT temp4 INTO TABLE temp3.
    temp4-transport = `DEVK900001`.
    INSERT temp4 INTO TABLE temp3.
    lo_pop->mt_data_read = temp3.
    lo_pop->mt_data = lo_pop->mt_data_read.


    temp6 = sy-tabix.
    READ TABLE lo_pop->mt_data INDEX 2 ASSIGNING <temp5>.
    sy-tabix = temp6.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    <temp5>-selkz = abap_true.
    roundtrip( io_app   = lo_pop
               iv_event = `SELECT` ).

    temp7 ?= lo_pop.

    li_result = temp7.

    cl_abap_unit_assert=>assert_true( li_result->check_confirmed( ) ).
    cl_abap_unit_assert=>assert_equals( exp = `DEVK900001`
                                        act = li_result->value_string( ) ).

    temp8 ?= li_result->value( ).

    lr_value = temp8.
    cl_abap_unit_assert=>assert_equals( exp = `DEVK900001`
                                        act = lr_value->transport ).

  ENDMETHOD.

  METHOD test_cancelled.

    DATA lo_pop TYPE REF TO z2ui5_cl_popup_show_tr.
    DATA temp9 TYPE REF TO z2ui5_if_popup_result.
    DATA li_result LIKE temp9.
    lo_pop = z2ui5_cl_popup_show_tr=>factory( ).
    roundtrip( io_app   = lo_pop
               iv_event = `CLOSE` ).

    temp9 ?= lo_pop.

    li_result = temp9.

    cl_abap_unit_assert=>assert_false( li_result->check_confirmed( ) ).

  ENDMETHOD.

  METHOD test_no_transport.

    DATA lo_pop TYPE REF TO z2ui5_cl_popup_show_tr.
    DATA temp10 TYPE REF TO z2ui5_if_popup_result.
    DATA li_result LIKE temp10.
    DATA temp11 TYPE REF TO z2ui5_cl_popup_context=>ty_s_transport.
    DATA lr_value LIKE temp11.
    lo_pop = z2ui5_cl_popup_show_tr=>factory( ).
    roundtrip( io_app   = lo_pop
               iv_event = `LOCL` ).

    temp10 ?= lo_pop.

    li_result = temp10.

    cl_abap_unit_assert=>assert_true( li_result->check_confirmed( ) ).

    temp11 ?= li_result->value( ).

    lr_value = temp11.
    cl_abap_unit_assert=>assert_true( lr_value->locl ).
    cl_abap_unit_assert=>assert_initial( li_result->value_string( ) ).

  ENDMETHOD.

  METHOD test_select_without_row.

    DATA lo_pop TYPE REF TO z2ui5_cl_popup_show_tr.
    DATA temp12 TYPE z2ui5_cl_popup_context=>ty_t_data.
    DATA temp13 LIKE LINE OF temp12.
    DATA temp14 TYPE REF TO z2ui5_if_popup_result.
    DATA li_result LIKE temp14.
    lo_pop = z2ui5_cl_popup_show_tr=>factory( ).

    CLEAR temp12.

    temp13-transport = `DEVK900000`.
    INSERT temp13 INTO TABLE temp12.
    lo_pop->mt_data_read = temp12.
    lo_pop->mt_data = lo_pop->mt_data_read.
    roundtrip( io_app   = lo_pop
               iv_event = `SELECT` ).

    temp14 ?= lo_pop.

    li_result = temp14.

    cl_abap_unit_assert=>assert_false( li_result->check_confirmed( ) ).

  ENDMETHOD.

  METHOD test_select_ignores_client.

    " mt_data is bound: a crafted request can rewrite the selected row's
    " request and task - the popup returns what bus_tr_read( ) offered
    DATA lo_pop TYPE REF TO z2ui5_cl_popup_show_tr.
    DATA temp15 TYPE z2ui5_cl_popup_context=>ty_t_data.
    DATA temp16 LIKE LINE OF temp15.
    DATA temp17 LIKE lo_pop->mt_data.
    DATA temp18 LIKE LINE OF temp17.
    lo_pop = z2ui5_cl_popup_show_tr=>factory( ).

    CLEAR temp15.

    temp16-transport = `DEVK900000`.
    temp16-task = `DEVK900002`.
    INSERT temp16 INTO TABLE temp15.
    lo_pop->mt_data_read = temp15.

    CLEAR temp17.

    temp18-transport = `PRDK900099`.
    temp18-task = `PRDK900098`.
    temp18-selkz = abap_true.
    INSERT temp18 INTO TABLE temp17.
    lo_pop->mt_data = temp17.
    roundtrip( io_app   = lo_pop
               iv_event = `SELECT` ).

    cl_abap_unit_assert=>assert_equals( exp = `DEVK900000`
                                        act = lo_pop->ms_transport-transport ).
    cl_abap_unit_assert=>assert_equals( exp = `DEVK900002`
                                        act = lo_pop->ms_transport-task ).

  ENDMETHOD.

ENDCLASS.
