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

    METHODS test_init_shows_fields  FOR TESTING RAISING cx_static_check.
    METHODS test_required_stays     FOR TESTING RAISING cx_static_check.
    METHODS test_confirm_values     FOR TESTING RAISING cx_static_check.
    METHODS test_cancel             FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD test_init_shows_fields.

    DATA temp1 TYPE z2ui5_cl_popup_get_values=>ty_t_field.
    DATA temp2 LIKE LINE OF temp1.
    DATA temp3 TYPE z2ui5_cl_popup_get_values=>ty_t_listbox.
    DATA temp5 LIKE LINE OF temp3.
    DATA lo_pop TYPE REF TO z2ui5_cl_popup_get_values.
    DATA lv_xml TYPE string.
    DATA temp6 TYPE xsdboolean.
    DATA temp7 TYPE xsdboolean.
    DATA temp8 TYPE xsdboolean.
    FIELD-SYMBOLS <temp3> LIKE LINE OF lo_pop->mt_field.
    DATA temp4 LIKE sy-tabix.
    CLEAR temp1.

    temp2-name = `carrid`.
    temp2-text = `Airline`.
    temp2-kind = z2ui5_cl_popup_get_values=>cs_kind-listbox.
    INSERT temp2 INTO TABLE temp1.
    temp2-name = `fldate`.
    temp2-text = `Date`.
    temp2-kind = z2ui5_cl_popup_get_values=>cs_kind-date.
    INSERT temp2 INTO TABLE temp1.
    temp2-name = `full`.
    temp2-text = `Full only`.
    temp2-kind = z2ui5_cl_popup_get_values=>cs_kind-checkbox.
    INSERT temp2 INTO TABLE temp1.

    CLEAR temp3.

    temp5-name = `carrid`.
    temp5-key = `LH`.
    temp5-text = `Lufthansa`.
    INSERT temp5 INTO TABLE temp3.

    lo_pop = z2ui5_cl_popup_get_values=>factory(
        i_fields  = temp1
        i_listbox = temp3 ).
    client_create( lo_pop ).
    lo_pop->z2ui5_if_app~main( mi_client ).


    lv_xml = popup_xml( ).

    temp6 = boolc( lv_xml CS `Lufthansa` ).
    cl_abap_unit_assert=>assert_true( temp6 ).

    temp7 = boolc( lv_xml CS `DatePicker` ).
    cl_abap_unit_assert=>assert_true( temp7 ).

    temp8 = boolc( lv_xml CS `CheckBox` ).
    cl_abap_unit_assert=>assert_true( temp8 ).


    temp4 = sy-tabix.
    READ TABLE lo_pop->mt_field INDEX 1 ASSIGNING <temp3>.
    sy-tabix = temp4.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `CARRID`
                                        act = <temp3>-name ).

  ENDMETHOD.

  METHOD test_required_stays.

    DATA temp5 TYPE z2ui5_cl_popup_get_values=>ty_t_field.
    DATA temp6 LIKE LINE OF temp5.
    DATA lo_pop TYPE REF TO z2ui5_cl_popup_get_values.
    DATA temp9 TYPE xsdboolean.
    CLEAR temp5.

    temp6-name = `CARRID`.
    temp6-text = `Airline`.
    temp6-required = abap_true.
    INSERT temp6 INTO TABLE temp5.

    lo_pop = z2ui5_cl_popup_get_values=>factory(
        temp5 ).
    roundtrip_event( io_app   = lo_pop
                     iv_event = `BUTTON_CONFIRM` ).

    cl_abap_unit_assert=>assert_false( lo_pop->result( )-check_confirmed ).

    temp9 = boolc( popup_xml( ) CS `Fill in the required field Airline` ).
    cl_abap_unit_assert=>assert_true( temp9 ).
    cl_abap_unit_assert=>assert_false( popup_destroyed( ) ).

  ENDMETHOD.

  METHOD test_confirm_values.

    DATA temp7 TYPE z2ui5_cl_popup_get_values=>ty_t_field.
    DATA temp8 LIKE LINE OF temp7.
    DATA lo_pop TYPE REF TO z2ui5_cl_popup_get_values.
    FIELD-SYMBOLS <temp9> LIKE LINE OF lo_pop->mt_field.
    DATA temp10 LIKE sy-tabix.
    FIELD-SYMBOLS <temp11> LIKE LINE OF lo_pop->mt_field.
    DATA temp12 LIKE sy-tabix.
    CLEAR temp7.

    CLEAR temp8.
    temp8-name = `CARRID`.
    temp8-text = `Airline`.
    temp8-required = abap_true.
    INSERT temp8 INTO TABLE temp7.
    CLEAR temp8.
    temp8-name = `FULL`.
    temp8-text = `Full only`.
    temp8-kind = z2ui5_cl_popup_get_values=>cs_kind-checkbox.
    INSERT temp8 INTO TABLE temp7.

    lo_pop = z2ui5_cl_popup_get_values=>factory(
        temp7 ).


    temp10 = sy-tabix.
    READ TABLE lo_pop->mt_field INDEX 1 ASSIGNING <temp9>.
    sy-tabix = temp10.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    <temp9>-value = `LH`.


    temp12 = sy-tabix.
    READ TABLE lo_pop->mt_field INDEX 2 ASSIGNING <temp11>.
    sy-tabix = temp12.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    <temp11>-flag = abap_true.
    roundtrip_event( io_app   = lo_pop
                     iv_event = `BUTTON_CONFIRM` ).

    cl_abap_unit_assert=>assert_true( lo_pop->result( )-check_confirmed ).
    cl_abap_unit_assert=>assert_equals( exp = `LH`
                                        act = lo_pop->result_value( `carrid` ) ).
    cl_abap_unit_assert=>assert_equals( exp = `X`
                                        act = lo_pop->result_value( `FULL` ) ).
    cl_abap_unit_assert=>assert_true( popup_destroyed( ) ).

  ENDMETHOD.

  METHOD test_cancel.

    DATA temp13 TYPE z2ui5_cl_popup_get_values=>ty_t_field.
    DATA temp14 LIKE LINE OF temp13.
    DATA lo_pop TYPE REF TO z2ui5_cl_popup_get_values.
    CLEAR temp13.

    temp14-name = `CARRID`.
    temp14-text = `Airline`.
    temp14-required = abap_true.
    INSERT temp14 INTO TABLE temp13.

    lo_pop = z2ui5_cl_popup_get_values=>factory(
        temp13 ).
    roundtrip_event( io_app   = lo_pop
                     iv_event = `BUTTON_CANCEL` ).

    cl_abap_unit_assert=>assert_false( lo_pop->result( )-check_confirmed ).
    cl_abap_unit_assert=>assert_true( popup_destroyed( ) ).

  ENDMETHOD.

  METHOD client_create.

    DATA temp6 TYPE REF TO z2ui5_cl_ui5_handler.
    CREATE OBJECT temp6 TYPE z2ui5_cl_ui5_handler EXPORTING VAL = ``.
    CREATE OBJECT mo_action EXPORTING VAL = temp6.
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

    DATA temp15 LIKE sy-subrc.
    DATA temp10 TYPE xsdboolean.
    READ TABLE mo_action->ms_next-t_action_front WITH KEY slot = z2ui5_if_client=>cs_view-popup method = `destroy` TRANSPORTING NO FIELDS.
    temp15 = sy-subrc.

    temp10 = boolc( temp15 = 0 ).
    result = temp10.

  ENDMETHOD.

ENDCLASS.
