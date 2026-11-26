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

    DATA lo_pop TYPE REF TO z2ui5_cl_popup_calculator.
    DATA temp2 TYPE REF TO z2ui5_if_popup_result.
    DATA li_result LIKE temp2.
    lo_pop = z2ui5_cl_popup_calculator=>factory( 12 ).

    temp2 ?= lo_pop.

    li_result = temp2.

    cl_abap_unit_assert=>assert_false( li_result->check_confirmed( ) ).

  ENDMETHOD.

  METHOD test_confirmed.

    DATA lo_pop TYPE REF TO z2ui5_cl_popup_calculator.
    DATA temp3 TYPE REF TO z2ui5_if_popup_result.
    DATA li_result LIKE temp3.
    DATA lv_exp TYPE decfloat34 VALUE 12.
    DATA temp4 TYPE REF TO decfloat34.
    DATA lr_value LIKE temp4.
    lo_pop = z2ui5_cl_popup_calculator=>factory( 12 ).
    roundtrip( io_app   = lo_pop
               iv_event = `BUTTON_CONFIRM` ).

    temp3 ?= lo_pop.

    li_result = temp3.

    cl_abap_unit_assert=>assert_true( li_result->check_confirmed( ) ).
    cl_abap_unit_assert=>assert_equals( exp = `12`
                                        act = li_result->value_string( ) ).


    temp4 ?= li_result->value( ).

    lr_value = temp4.
    cl_abap_unit_assert=>assert_equals( exp = lv_exp
                                        act = lr_value->* ).

  ENDMETHOD.

  METHOD test_cancelled.

    DATA lo_pop TYPE REF TO z2ui5_cl_popup_calculator.
    DATA temp5 TYPE REF TO z2ui5_if_popup_result.
    DATA li_result LIKE temp5.
    lo_pop = z2ui5_cl_popup_calculator=>factory( 12 ).
    roundtrip( io_app   = lo_pop
               iv_event = `BUTTON_CANCEL` ).

    temp5 ?= lo_pop.

    li_result = temp5.

    cl_abap_unit_assert=>assert_false( li_result->check_confirmed( ) ).

  ENDMETHOD.

ENDCLASS.
