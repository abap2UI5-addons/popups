CLASS ltcl_test DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    DATA mo_action TYPE REF TO z2ui5_cl_ui5_action.
    DATA mi_client TYPE REF TO z2ui5_if_client.

    " what the roundtrip queued for the frontend, read off the response
    METHODS popup_xml
      RETURNING
        VALUE(result) TYPE string.
    METHODS popup_destroyed
      RETURNING
        VALUE(result) TYPE abap_bool.

    METHODS client_create
      IMPORTING
        io_app TYPE REF TO z2ui5_if_app.

    METHODS roundtrip_event
      IMPORTING
        io_app   TYPE REF TO z2ui5_if_app
        iv_event TYPE string.

    METHODS test_init_shows_options FOR TESTING RAISING cx_static_check.
    METHODS test_option_answers     FOR TESTING RAISING cx_static_check.
    METHODS test_cancel             FOR TESTING RAISING cx_static_check.
    METHODS test_unknown_option     FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD test_init_shows_options.

    DATA temp1 TYPE string_table.
    DATA lo_pop TYPE REF TO z2ui5_cl_popup_to_decide.
    DATA lv_xml TYPE string.
    DATA temp2 TYPE xsdboolean.
    DATA temp3 TYPE xsdboolean.
    DATA temp4 TYPE xsdboolean.
    CLEAR temp1.
    INSERT `Save` INTO TABLE temp1.
    INSERT `Discard` INTO TABLE temp1.

    lo_pop = z2ui5_cl_popup_to_decide=>factory( i_question_text = `Save the changes?`
                                                      i_options       = temp1 ).
    client_create( lo_pop ).
    lo_pop->z2ui5_if_app~main( mi_client ).


    lv_xml = popup_xml( ).

    temp2 = boolc( lv_xml CS `Save the changes?` ).
    cl_abap_unit_assert=>assert_true( temp2 ).

    temp3 = boolc( lv_xml CS `Discard` ).
    cl_abap_unit_assert=>assert_true( temp3 ).

    temp4 = boolc( lv_xml CS `OPTION_2` ).
    cl_abap_unit_assert=>assert_true( temp4 ).
    cl_abap_unit_assert=>assert_equals( exp = z2ui5_cl_popup_to_decide=>cs_answer-cancel
                                        act = lo_pop->result( ) ).

  ENDMETHOD.

  METHOD test_option_answers.

    DATA temp3 TYPE string_table.
    DATA lo_pop TYPE REF TO z2ui5_cl_popup_to_decide.
    CLEAR temp3.
    INSERT `A` INTO TABLE temp3.
    INSERT `B` INTO TABLE temp3.

    lo_pop = z2ui5_cl_popup_to_decide=>factory( i_question_text = `Q`
                                                      i_options       = temp3 ).
    roundtrip_event( io_app   = lo_pop
                     iv_event = `OPTION_2` ).

    cl_abap_unit_assert=>assert_equals( exp = `2`
                                        act = lo_pop->result( ) ).
    cl_abap_unit_assert=>assert_true( popup_destroyed( ) ).
    cl_abap_unit_assert=>assert_bound( mo_action->ms_next-o_app_leave ).

  ENDMETHOD.

  METHOD test_cancel.

    DATA temp5 TYPE string_table.
    DATA lo_pop TYPE REF TO z2ui5_cl_popup_to_decide.
    CLEAR temp5.
    INSERT `A` INTO TABLE temp5.

    lo_pop = z2ui5_cl_popup_to_decide=>factory( i_question_text = `Q`
                                                      i_options       = temp5 ).
    roundtrip_event( io_app   = lo_pop
                     iv_event = `BUTTON_CANCEL` ).

    cl_abap_unit_assert=>assert_equals( exp = z2ui5_cl_popup_to_decide=>cs_answer-cancel
                                        act = lo_pop->result( ) ).
    cl_abap_unit_assert=>assert_true( popup_destroyed( ) ).

  ENDMETHOD.

  METHOD test_unknown_option.

    " an option the popup does not have changes nothing
    DATA temp7 TYPE string_table.
    DATA lo_pop TYPE REF TO z2ui5_cl_popup_to_decide.
    CLEAR temp7.
    INSERT `A` INTO TABLE temp7.

    lo_pop = z2ui5_cl_popup_to_decide=>factory( i_question_text = `Q`
                                                      i_options       = temp7 ).
    roundtrip_event( io_app   = lo_pop
                     iv_event = `OPTION_7` ).

    cl_abap_unit_assert=>assert_equals( exp = z2ui5_cl_popup_to_decide=>cs_answer-cancel
                                        act = lo_pop->result( ) ).
    cl_abap_unit_assert=>assert_false( popup_destroyed( ) ).

  ENDMETHOD.

  METHOD client_create.

    DATA temp1 TYPE REF TO z2ui5_cl_ui5_handler.
    CREATE OBJECT temp1 TYPE z2ui5_cl_ui5_handler EXPORTING VAL = ``.
    CREATE OBJECT mo_action EXPORTING VAL = temp1.
    mo_action->mo_app->mo_app = io_app.
    CREATE OBJECT mi_client TYPE z2ui5_cl_ui5_client EXPORTING ACTION = mo_action.

  ENDMETHOD.

  METHOD roundtrip_event.

    client_create( io_app ).
    mo_action->mo_app->mv_check_initialized = abap_true.
    mo_action->ms_actual-event = iv_event.
    io_app->main( mi_client ).

  ENDMETHOD.

  METHOD popup_xml.

    DATA ls_action LIKE LINE OF mo_action->ms_next-t_action_front.
    LOOP AT mo_action->ms_next-t_action_front INTO ls_action
         WHERE slot = z2ui5_if_client=>cs_view-popup AND method = `display`.
      result = ls_action-xml.
    ENDLOOP.

  ENDMETHOD.

  METHOD popup_destroyed.

    DATA temp9 LIKE sy-subrc.
    DATA temp5 TYPE xsdboolean.
    READ TABLE mo_action->ms_next-t_action_front WITH KEY slot = z2ui5_if_client=>cs_view-popup method = `destroy` TRANSPORTING NO FIELDS.
    temp9 = sy-subrc.

    temp5 = boolc( temp9 = 0 ).
    result = temp5.

  ENDMETHOD.

ENDCLASS.


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
    DATA temp10 TYPE REF TO z2ui5_cl_ui5_client.
    CREATE OBJECT temp2 TYPE z2ui5_cl_ui5_handler EXPORTING VAL = ``.
    CREATE OBJECT mo_action EXPORTING VAL = temp2.
    mo_action->mo_app->mo_app = io_app.
    mo_action->mo_app->mv_check_initialized = abap_true.
    mo_action->ms_actual-event = iv_event.
    mo_action->ms_actual-t_event_arg = it_arg.

    CREATE OBJECT temp10 TYPE z2ui5_cl_ui5_client EXPORTING ACTION = mo_action.
    io_app->main( temp10 ).

  ENDMETHOD.

  METHOD test_initial_not_confirmed.

    DATA temp11 TYPE string_table.
    DATA lo_pop TYPE REF TO z2ui5_cl_popup_to_decide.
    DATA temp13 TYPE REF TO z2ui5_if_popup_result.
    DATA li_result LIKE temp13.
    CLEAR temp11.
    INSERT `Save` INTO TABLE temp11.
    INSERT `Discard` INTO TABLE temp11.

    lo_pop = z2ui5_cl_popup_to_decide=>factory( i_question_text = `Save?`
                                                     i_options       = temp11 ).

    temp13 ?= lo_pop.

    li_result = temp13.

    cl_abap_unit_assert=>assert_false( li_result->check_confirmed( ) ).

  ENDMETHOD.

  METHOD test_confirmed.

    DATA temp14 TYPE string_table.
    DATA lo_pop TYPE REF TO z2ui5_cl_popup_to_decide.
    DATA temp16 TYPE REF TO z2ui5_if_popup_result.
    DATA li_result LIKE temp16.
    DATA temp17 TYPE REF TO string.
    DATA lr_value LIKE temp17.
    CLEAR temp14.
    INSERT `Save` INTO TABLE temp14.
    INSERT `Discard` INTO TABLE temp14.

    lo_pop = z2ui5_cl_popup_to_decide=>factory( i_question_text = `Save?`
                                                     i_options       = temp14 ).
    roundtrip( io_app   = lo_pop
               iv_event = `OPTION_2` ).

    temp16 ?= lo_pop.

    li_result = temp16.

    cl_abap_unit_assert=>assert_true( li_result->check_confirmed( ) ).
    cl_abap_unit_assert=>assert_equals( exp = `2`
                                        act = li_result->value_string( ) ).

    temp17 ?= li_result->value( ).

    lr_value = temp17.
    cl_abap_unit_assert=>assert_equals( exp = `2`
                                        act = lr_value->* ).

  ENDMETHOD.

  METHOD test_cancelled.

    DATA temp18 TYPE string_table.
    DATA lo_pop TYPE REF TO z2ui5_cl_popup_to_decide.
    DATA temp20 TYPE REF TO z2ui5_if_popup_result.
    DATA li_result LIKE temp20.
    CLEAR temp18.
    INSERT `Save` INTO TABLE temp18.
    INSERT `Discard` INTO TABLE temp18.

    lo_pop = z2ui5_cl_popup_to_decide=>factory( i_question_text = `Save?`
                                                     i_options       = temp18 ).
    roundtrip( io_app   = lo_pop
               iv_event = `BUTTON_CANCEL` ).

    temp20 ?= lo_pop.

    li_result = temp20.

    cl_abap_unit_assert=>assert_false( li_result->check_confirmed( ) ).
    cl_abap_unit_assert=>assert_initial( li_result->value_string( ) ).

  ENDMETHOD.

ENDCLASS.
