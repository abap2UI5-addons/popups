
CLASS ltcl_test DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.

    METHODS test_factory               FOR TESTING RAISING cx_static_check.
    METHODS test_result_initial        FOR TESTING RAISING cx_static_check.
    METHODS test_factory_with_path     FOR TESTING RAISING cx_static_check.
    METHODS test_confirm_initially_off FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD test_factory.

    DATA lo_pop TYPE REF TO z2ui5_cl_popup_file_ul.
    lo_pop = z2ui5_cl_popup_file_ul=>factory( ).
    cl_abap_unit_assert=>assert_bound( lo_pop ).

  ENDMETHOD.

  METHOD test_result_initial.

    DATA lo_pop TYPE REF TO z2ui5_cl_popup_file_ul.
    DATA ls_result TYPE z2ui5_cl_popup_file_ul=>ty_s_result.
    lo_pop = z2ui5_cl_popup_file_ul=>factory( ).

    ls_result = lo_pop->result( ).
    cl_abap_unit_assert=>assert_false( ls_result-check_confirmed ).
    cl_abap_unit_assert=>assert_initial( ls_result-value ).

  ENDMETHOD.

  METHOD test_factory_with_path.

    DATA lo_pop TYPE REF TO z2ui5_cl_popup_file_ul.
    lo_pop = z2ui5_cl_popup_file_ul=>factory( i_path = `/tmp/myfile.csv` ).
    cl_abap_unit_assert=>assert_equals( exp = `/tmp/myfile.csv`
                                        act = lo_pop->mv_path ).

  ENDMETHOD.

  METHOD test_confirm_initially_off.

    DATA lo_pop TYPE REF TO z2ui5_cl_popup_file_ul.
    lo_pop = z2ui5_cl_popup_file_ul=>factory( ).
    cl_abap_unit_assert=>assert_false( lo_pop->check_confirm_enabled ).

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

    METHODS client_create
      IMPORTING
        io_app TYPE REF TO z2ui5_if_app.

    METHODS roundtrip_event
      IMPORTING
        io_app   TYPE REF TO z2ui5_if_app
        iv_event TYPE string.

    METHODS test_init_displays_popup FOR TESTING RAISING cx_static_check.
    METHODS test_upload_decodes_file FOR TESTING RAISING cx_static_check.
    METHODS test_confirm             FOR TESTING RAISING cx_static_check.
    METHODS test_cancel              FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_test_roundtrip IMPLEMENTATION.

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

  METHOD test_init_displays_popup.

    DATA lo_pop TYPE REF TO z2ui5_cl_popup_file_ul.
    DATA lv_xml TYPE string.
    DATA temp1 TYPE xsdboolean.
    DATA temp2 TYPE xsdboolean.
    lo_pop = z2ui5_cl_popup_file_ul=>factory( i_title = `Upload Title` ).
    client_create( lo_pop ).

    lo_pop->z2ui5_if_app~main( mi_client ).


    lv_xml = popup_xml( ).

    temp1 = boolc( lv_xml CS `Upload Title` ).
    cl_abap_unit_assert=>assert_true( temp1 ).

    temp2 = boolc( lv_xml CS `FileUploader` ).
    cl_abap_unit_assert=>assert_true( temp2 ).

  ENDMETHOD.

  METHOD test_upload_decodes_file.

    DATA lo_pop TYPE REF TO z2ui5_cl_popup_file_ul.
    lo_pop = z2ui5_cl_popup_file_ul=>factory( ).
    lo_pop->mv_value = `data:text/plain;base64,SGVsbG8gV29ybGQ=`.
    roundtrip_event( io_app   = lo_pop
                     iv_event = `UPLOAD` ).

    cl_abap_unit_assert=>assert_equals( exp = `Hello World`
                                        act = lo_pop->result( )-value ).
    cl_abap_unit_assert=>assert_true( lo_pop->check_confirm_enabled ).
    cl_abap_unit_assert=>assert_initial( lo_pop->mv_value ).

  ENDMETHOD.

  METHOD test_confirm.

    DATA lo_pop TYPE REF TO z2ui5_cl_popup_file_ul.
    lo_pop = z2ui5_cl_popup_file_ul=>factory( ).
    roundtrip_event( io_app   = lo_pop
                     iv_event = `BUTTON_CONFIRM` ).

    cl_abap_unit_assert=>assert_true( lo_pop->result( )-check_confirmed ).
    cl_abap_unit_assert=>assert_true( popup_destroyed( ) ).
    cl_abap_unit_assert=>assert_bound( mo_action->ms_next-o_app_leave ).

  ENDMETHOD.

  METHOD test_cancel.

    DATA lo_pop TYPE REF TO z2ui5_cl_popup_file_ul.
    lo_pop = z2ui5_cl_popup_file_ul=>factory( ).
    roundtrip_event( io_app   = lo_pop
                     iv_event = `BUTTON_CANCEL` ).

    cl_abap_unit_assert=>assert_false( lo_pop->result( )-check_confirmed ).
    cl_abap_unit_assert=>assert_true( popup_destroyed( ) ).

  ENDMETHOD.

  METHOD popup_xml.

    DATA ls_action LIKE LINE OF mo_action->ms_next-t_action_front.
    LOOP AT mo_action->ms_next-t_action_front INTO ls_action
         WHERE slot = z2ui5_if_client=>cs_view-popup AND method = `display`.
      result = ls_action-xml.
    ENDLOOP.

  ENDMETHOD.

  METHOD popup_destroyed.

    DATA temp1 LIKE sy-subrc.
    DATA temp3 TYPE xsdboolean.
    READ TABLE mo_action->ms_next-t_action_front WITH KEY slot = z2ui5_if_client=>cs_view-popup method = `destroy` TRANSPORTING NO FIELDS.
    temp1 = sy-subrc.

    temp3 = boolc( temp1 = 0 ).
    result = temp3.

  ENDMETHOD.

ENDCLASS.
