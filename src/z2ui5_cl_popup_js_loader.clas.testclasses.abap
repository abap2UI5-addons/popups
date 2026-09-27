CLASS ltcl_test DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    METHODS test_factory             FOR TESTING RAISING cx_static_check.
    METHODS test_factory_open_ui5    FOR TESTING RAISING cx_static_check.
    METHODS test_result_initial      FOR TESTING RAISING cx_static_check.
    METHODS test_open_ui5_flag_init  FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS ltcl_test IMPLEMENTATION.

  METHOD test_factory.
    DATA(lo_pop) = z2ui5_cl_popup_js_loader=>factory(
      i_js     = `console.log("hello");`
      i_result = `DONE` ).

    cl_abap_unit_assert=>assert_bound( lo_pop ).
    cl_abap_unit_assert=>assert_equals( exp = `DONE`
                                        act = lo_pop->result( ) ).
  ENDMETHOD.

  METHOD test_factory_open_ui5.
    DATA(lo_pop) = z2ui5_cl_popup_js_loader=>factory_check_open_ui5( ).

    cl_abap_unit_assert=>assert_bound( lo_pop ).
  ENDMETHOD.

  METHOD test_result_initial.
    DATA(lo_pop) = z2ui5_cl_popup_js_loader=>factory( `alert(1);` ).

    cl_abap_unit_assert=>assert_equals( exp = `LOADED`
                                        act = lo_pop->result( ) ).
  ENDMETHOD.

  METHOD test_open_ui5_flag_init.
    DATA(lo_pop) = z2ui5_cl_popup_js_loader=>factory_check_open_ui5( ).

    cl_abap_unit_assert=>assert_false( lo_pop->mv_is_open_ui5 ).
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
        VALUE(result) TYPE string_table.

    METHODS client_create
      IMPORTING
        io_app TYPE REF TO z2ui5_if_app.

    METHODS roundtrip_event
      IMPORTING
        io_app   TYPE REF TO z2ui5_if_app
        iv_event TYPE string.

    METHODS test_init_displays_script FOR TESTING RAISING cx_static_check.
    METHODS test_timer_finished       FOR TESTING RAISING cx_static_check.
    METHODS test_info_open_ui5        FOR TESTING RAISING cx_static_check.
    METHODS test_info_sap_ui5         FOR TESTING RAISING cx_static_check.

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

  METHOD test_init_displays_script.

    DATA(lo_pop) = z2ui5_cl_popup_js_loader=>factory( `console.log('x');` ).
    client_create( lo_pop ).

    lo_pop->z2ui5_if_app~main( mi_client ).

    DATA(lv_xml) = popup_xml( ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_xml CS `script` ) ).
    cl_abap_unit_assert=>assert_false( xsdbool( lv_xml CS `z2ui5.cc` ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE string_table( ( `["START_TIMER","TIMER_FINISHED","0"]` ) )
                                        act = follow_up_actions( ) ).

  ENDMETHOD.

  METHOD test_timer_finished.

    DATA(lo_pop) = z2ui5_cl_popup_js_loader=>factory( `console.log('x');` ).
    roundtrip_event( io_app   = lo_pop
                     iv_event = `TIMER_FINISHED` ).

    cl_abap_unit_assert=>assert_equals( exp = `LOADED`
                                        act = lo_pop->result( ) ).
    cl_abap_unit_assert=>assert_true( popup_destroyed( ) ).
    cl_abap_unit_assert=>assert_bound( mo_action->ms_next-o_app_leave ).

  ENDMETHOD.

  METHOD test_info_open_ui5.

    " the UI5 runtime arrives with the request - the check answers on the
    " first roundtrip and leaves without ever showing a popup
    DATA(lo_pop) = z2ui5_cl_popup_js_loader=>factory_check_open_ui5( ).
    client_create( lo_pop ).
    mo_action->mo_handler->ms_request-s_front-s_ui5-gav = `com.sap.ui5.dist:OPENUI5:zip`.

    lo_pop->z2ui5_if_app~main( mi_client ).

    cl_abap_unit_assert=>assert_true( lo_pop->mv_is_open_ui5 ).
    cl_abap_unit_assert=>assert_equals( exp = `com.sap.ui5.dist:OPENUI5:zip`
                                        act = lo_pop->ui5_gav ).
    cl_abap_unit_assert=>assert_initial( popup_xml( ) ).
    cl_abap_unit_assert=>assert_bound( mo_action->ms_next-o_app_leave ).

  ENDMETHOD.

  METHOD test_info_sap_ui5.

    DATA(lo_pop) = z2ui5_cl_popup_js_loader=>factory_check_open_ui5( ).
    client_create( lo_pop ).
    mo_action->mo_handler->ms_request-s_front-s_ui5-gav = `com.sap.ui5.dist:sapui5:zip`.

    lo_pop->z2ui5_if_app~main( mi_client ).

    cl_abap_unit_assert=>assert_false( lo_pop->mv_is_open_ui5 ).
    cl_abap_unit_assert=>assert_bound( mo_action->ms_next-o_app_leave ).

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
