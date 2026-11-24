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
    mo_action = NEW #( NEW z2ui5_cl_ui5_handler( `` ) ).
    mo_action->mo_app->mo_app = io_app.
    mo_action->mo_app->mv_check_initialized = abap_true.
    mo_action->ms_actual-event = iv_event.
    mo_action->ms_actual-t_event_arg = it_arg.
    io_app->main( NEW z2ui5_cl_ui5_client( mo_action ) ).

  ENDMETHOD.

  METHOD test_initial_not_confirmed.

    DATA(lo_pop) = z2ui5_cl_popup_msg_search=>factory( ).
    DATA(li_result) = CAST z2ui5_if_popup_result( lo_pop ).

    cl_abap_unit_assert=>assert_false( li_result->check_confirmed( ) ).

  ENDMETHOD.

  METHOD test_confirmed.

    DATA(lo_pop) = z2ui5_cl_popup_msg_search=>factory( ).
    lo_pop->mt_result = VALUE #( ( selkz = abap_true arbgb = `00` msgnr = `001` text = `&1&2&3&4` ) ).
    roundtrip( io_app   = lo_pop
               iv_event = `CONFIRM` ).
    DATA(li_result) = CAST z2ui5_if_popup_result( lo_pop ).

    cl_abap_unit_assert=>assert_true( li_result->check_confirmed( ) ).
    DATA(lr_result) = CAST z2ui5_cl_popup_msg_search=>ty_s_result( li_result->value( ) ).
    cl_abap_unit_assert=>assert_equals( exp = `00`
                                        act = lr_result->msgid ).
    cl_abap_unit_assert=>assert_equals( exp = `001`
                                        act = lr_result->msgno ).
    cl_abap_unit_assert=>assert_initial( li_result->value_string( ) ).

  ENDMETHOD.

  METHOD test_cancelled.

    DATA(lo_pop) = z2ui5_cl_popup_msg_search=>factory( ).
    roundtrip( io_app   = lo_pop
               iv_event = `CANCEL` ).
    DATA(li_result) = CAST z2ui5_if_popup_result( lo_pop ).

    cl_abap_unit_assert=>assert_false( li_result->check_confirmed( ) ).

  ENDMETHOD.

ENDCLASS.
