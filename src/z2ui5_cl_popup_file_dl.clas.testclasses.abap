
CLASS ltcl_test DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.

    METHODS test_factory        FOR TESTING RAISING cx_static_check.
    METHODS test_result_initial FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD test_factory.

    DATA(lo_pop) = z2ui5_cl_popup_file_dl=>factory( i_file = `test_content`
                                                  i_name = `test.csv` ).
    cl_abap_unit_assert=>assert_bound( lo_pop ).
    cl_abap_unit_assert=>assert_equals( exp = `test_content`
                                        act = lo_pop->mv_value ).
    cl_abap_unit_assert=>assert_equals( exp = `data:text/csv;base64,`
                                        act = lo_pop->mv_type ).
    cl_abap_unit_assert=>assert_equals( exp = `test.csv`
                                        act = lo_pop->mv_name ).
    " 12 characters -> 0.01 kB, must not truncate to 0
    cl_abap_unit_assert=>assert_equals( exp = `0.01`
                                        act = lo_pop->mv_size ).

  ENDMETHOD.

  METHOD test_result_initial.

    DATA(lo_pop) = z2ui5_cl_popup_file_dl=>factory( `abc` ).
    cl_abap_unit_assert=>assert_false( lo_pop->result( ) ).

  ENDMETHOD.

ENDCLASS.


CLASS ltcl_test_roundtrip DEFINITION FINAL
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
    " the follow-up actions the roundtrip queued, one JSON array each
    METHODS follow_up_actions
      RETURNING
        VALUE(result) TYPE string_table
      RAISING
        z2ui5_cx_ajson_error.

    METHODS client_create
      IMPORTING
        io_app TYPE REF TO z2ui5_if_app.

    METHODS roundtrip_event
      IMPORTING
        io_app   TYPE REF TO z2ui5_if_app
        iv_event TYPE string.

    METHODS test_init_displays_popup  FOR TESTING RAISING cx_static_check.
    METHODS test_confirm_starts_dl    FOR TESTING RAISING cx_static_check.
    METHODS test_callback_closes      FOR TESTING RAISING cx_static_check.
    METHODS test_cancel_closes        FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_test_roundtrip IMPLEMENTATION.

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

  METHOD test_init_displays_popup.

    DATA(lo_pop) = z2ui5_cl_popup_file_dl=>factory( i_file  = `col1;col2`
                                                  i_title = `Download Title` ).
    client_create( lo_pop ).

    lo_pop->z2ui5_if_app~main( mi_client ).

    DATA(lv_xml) = popup_xml( ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_xml CS `Download Title` ) ).
    cl_abap_unit_assert=>assert_false( xsdbool( lv_xml CS `iframe` ) ).
    cl_abap_unit_assert=>assert_initial( follow_up_actions( ) ).

  ENDMETHOD.

  METHOD test_confirm_starts_dl.

    DATA(lo_pop) = z2ui5_cl_popup_file_dl=>factory( i_file = `col1;col2`
                                                  i_name = `data.csv` ).
    roundtrip_event( io_app   = lo_pop
                     iv_event = `BUTTON_CONFIRM` ).

    " confirm hands the file to the browser and arms the timer that closes
    " the popup - the popup itself stays as it is for this response
    DATA(lt_action) = follow_up_actions( ).
    cl_abap_unit_assert=>assert_equals( exp = 2
                                        act = lines( lt_action ) ).
    cl_abap_unit_assert=>assert_equals(
        exp = `["DOWNLOAD_B64_FILE","data:text/csv;base64,Y29sMTtjb2wy","data.csv"]`
        act = lt_action[ 1 ] ).
    cl_abap_unit_assert=>assert_equals( exp = `["START_TIMER","CALLBACK_DOWNLOAD","0"]`
                                        act = lt_action[ 2 ] ).
    cl_abap_unit_assert=>assert_true( lo_pop->mv_check_download ).
    cl_abap_unit_assert=>assert_initial( popup_xml( ) ).
    cl_abap_unit_assert=>assert_false( popup_destroyed( ) ).
    cl_abap_unit_assert=>assert_not_bound( mo_action->ms_next-o_app_leave ).

  ENDMETHOD.

  METHOD test_callback_closes.

    DATA(lo_pop) = z2ui5_cl_popup_file_dl=>factory( `col1;col2` ).
    roundtrip_event( io_app   = lo_pop
                     iv_event = `CALLBACK_DOWNLOAD` ).

    cl_abap_unit_assert=>assert_true( popup_destroyed( ) ).
    cl_abap_unit_assert=>assert_bound( mo_action->ms_next-o_app_leave ).
    cl_abap_unit_assert=>assert_true( lo_pop->result( ) ).

  ENDMETHOD.

  METHOD test_cancel_closes.

    DATA(lo_pop) = z2ui5_cl_popup_file_dl=>factory( `col1;col2` ).
    roundtrip_event( io_app   = lo_pop
                     iv_event = `BUTTON_CANCEL` ).

    cl_abap_unit_assert=>assert_true( popup_destroyed( ) ).
    cl_abap_unit_assert=>assert_bound( mo_action->ms_next-o_app_leave ).
    cl_abap_unit_assert=>assert_false( lo_pop->result( ) ).

  ENDMETHOD.

  METHOD popup_xml.

    LOOP AT mo_action->ms_next-t_action_front INTO DATA(ls_action)
         WHERE slot = z2ui5_if_client=>cs_view-popup AND method = `display`.
      result = ls_action-xml.
    ENDLOOP.

  ENDMETHOD.

  METHOD follow_up_actions.

    LOOP AT mo_action->ms_next-s_action-t_custom INTO DATA(ls_action).
      INSERT ls_action-o_json->stringify( ) INTO TABLE result.
    ENDLOOP.

  ENDMETHOD.

  METHOD popup_destroyed.

    result = xsdbool( line_exists( mo_action->ms_next-t_action_front[ slot   = z2ui5_if_client=>cs_view-popup
                                                                      method = `destroy` ] ) ).

  ENDMETHOD.

ENDCLASS.
