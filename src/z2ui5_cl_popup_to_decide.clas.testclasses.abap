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

    DATA(lo_pop) = z2ui5_cl_popup_to_decide=>factory( i_question_text = `Save the changes?`
                                                      i_options       = VALUE #( ( `Save` ) ( `Discard` ) ) ).
    client_create( lo_pop ).
    lo_pop->z2ui5_if_app~main( mi_client ).

    DATA(lv_xml) = popup_xml( ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_xml CS `Save the changes?` ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_xml CS `Discard` ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_xml CS `OPTION_2` ) ).
    cl_abap_unit_assert=>assert_equals( exp = z2ui5_cl_popup_to_decide=>cs_answer-cancel
                                        act = lo_pop->result( ) ).

  ENDMETHOD.

  METHOD test_option_answers.

    DATA(lo_pop) = z2ui5_cl_popup_to_decide=>factory( i_question_text = `Q`
                                                      i_options       = VALUE #( ( `A` ) ( `B` ) ) ).
    roundtrip_event( io_app   = lo_pop
                     iv_event = `OPTION_2` ).

    cl_abap_unit_assert=>assert_equals( exp = `2`
                                        act = lo_pop->result( ) ).
    cl_abap_unit_assert=>assert_true( popup_destroyed( ) ).
    cl_abap_unit_assert=>assert_bound( mo_action->ms_next-o_app_leave ).

  ENDMETHOD.

  METHOD test_cancel.

    DATA(lo_pop) = z2ui5_cl_popup_to_decide=>factory( i_question_text = `Q`
                                                      i_options       = VALUE #( ( `A` ) ) ).
    roundtrip_event( io_app   = lo_pop
                     iv_event = `BUTTON_CANCEL` ).

    cl_abap_unit_assert=>assert_equals( exp = z2ui5_cl_popup_to_decide=>cs_answer-cancel
                                        act = lo_pop->result( ) ).
    cl_abap_unit_assert=>assert_true( popup_destroyed( ) ).

  ENDMETHOD.

  METHOD test_unknown_option.

    " an option the popup does not have changes nothing
    DATA(lo_pop) = z2ui5_cl_popup_to_decide=>factory( i_question_text = `Q`
                                                      i_options       = VALUE #( ( `A` ) ) ).
    roundtrip_event( io_app   = lo_pop
                     iv_event = `OPTION_7` ).

    cl_abap_unit_assert=>assert_equals( exp = z2ui5_cl_popup_to_decide=>cs_answer-cancel
                                        act = lo_pop->result( ) ).
    cl_abap_unit_assert=>assert_false( popup_destroyed( ) ).

  ENDMETHOD.

  METHOD client_create.

    mo_action = NEW #( NEW z2ui5_cl_ui5_handler( `` ) ).
    mo_action->mo_app->mo_app = io_app.
    mi_client = NEW z2ui5_cl_ui5_client( mo_action ).

  ENDMETHOD.

  METHOD roundtrip_event.

    client_create( io_app ).
    mo_action->mo_app->mv_check_initialized = abap_true.
    mo_action->ms_actual-event = iv_event.
    io_app->main( mi_client ).

  ENDMETHOD.

  METHOD popup_xml.

    LOOP AT mo_action->ms_next-t_action_front INTO DATA(ls_action)
         WHERE slot = z2ui5_if_client=>cs_view-popup AND method = `display`.
      result = ls_action-xml.
    ENDLOOP.

  ENDMETHOD.

  METHOD popup_destroyed.

    result = xsdbool( line_exists( mo_action->ms_next-t_action_front[ slot   = z2ui5_if_client=>cs_view-popup
                                                                      method = `destroy` ] ) ).

  ENDMETHOD.

ENDCLASS.
