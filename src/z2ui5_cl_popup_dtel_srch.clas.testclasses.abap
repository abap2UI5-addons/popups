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
    METHODS test_confirm_without_row   FOR TESTING RAISING cx_static_check.

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

    DATA(lo_pop) = z2ui5_cl_popup_dtel_srch=>factory( ).
    DATA(li_result) = CAST z2ui5_if_popup_result( lo_pop ).

    cl_abap_unit_assert=>assert_false( li_result->check_confirmed( ) ).

  ENDMETHOD.

  METHOD test_confirmed.

    DATA(lo_pop) = z2ui5_cl_popup_dtel_srch=>factory( ).
    lo_pop->mt_result = VALUE #( ( rollname = `MARA` )
                                 ( selkz = abap_true rollname = `MATNR` description = `Material` ) ).
    roundtrip( io_app   = lo_pop
               iv_event = `CONFIRM` ).
    DATA(li_result) = CAST z2ui5_if_popup_result( lo_pop ).

    cl_abap_unit_assert=>assert_true( li_result->check_confirmed( ) ).
    cl_abap_unit_assert=>assert_equals( exp = `MATNR`
                                        act = li_result->value_string( ) ).
    DATA(lr_value) = CAST z2ui5_cl_popup_dtel_srch=>ty_s_result( li_result->value( ) ).
    cl_abap_unit_assert=>assert_equals( exp = `Material`
                                        act = lr_value->description ).

  ENDMETHOD.

  METHOD test_cancelled.

    DATA(lo_pop) = z2ui5_cl_popup_dtel_srch=>factory( ).
    roundtrip( io_app   = lo_pop
               iv_event = `CANCEL` ).
    DATA(li_result) = CAST z2ui5_if_popup_result( lo_pop ).

    cl_abap_unit_assert=>assert_false( li_result->check_confirmed( ) ).

  ENDMETHOD.

  METHOD test_confirm_without_row.

    DATA(lo_pop) = z2ui5_cl_popup_dtel_srch=>factory( ).
    lo_pop->mt_result = VALUE #( ( rollname = `MATNR` ) ).
    roundtrip( io_app   = lo_pop
               iv_event = `CONFIRM` ).
    DATA(li_result) = CAST z2ui5_if_popup_result( lo_pop ).

    cl_abap_unit_assert=>assert_false( li_result->check_confirmed( ) ).

  ENDMETHOD.

ENDCLASS.
