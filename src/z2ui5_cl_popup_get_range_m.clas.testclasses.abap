
CLASS ltcl_test DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.

    METHODS test_factory          FOR TESTING RAISING cx_static_check.
    METHODS test_result_initial   FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD test_factory.

    DATA temp1 TYPE z2ui5_cl_popup_context=>ty_t_filter_multi.
    DATA temp2 LIKE LINE OF temp1.
    DATA temp3 TYPE z2ui5_cl_popup_context=>ty_t_range.
    DATA temp4 LIKE LINE OF temp3.
    DATA lt_filter LIKE temp1.
    DATA lo_pop TYPE REF TO z2ui5_cl_popup_get_range_m.
    CLEAR temp1.

    CLEAR temp2.
    temp2-name = `CARRID`.

    CLEAR temp3.

    temp4-sign = `I`.
    temp4-option = `EQ`.
    temp4-low = `AA`.
    INSERT temp4 INTO TABLE temp3.
    temp2-t_range = temp3.
    INSERT temp2 INTO TABLE temp1.
    CLEAR temp2.
    temp2-name = `CONNID`.
    INSERT temp2 INTO TABLE temp1.

    lt_filter = temp1.


    lo_pop = z2ui5_cl_popup_get_range_m=>factory( lt_filter ).
    cl_abap_unit_assert=>assert_bound( lo_pop ).

  ENDMETHOD.

  METHOD test_result_initial.

    DATA temp3 TYPE z2ui5_cl_popup_context=>ty_t_filter_multi.
    DATA temp4 LIKE LINE OF temp3.
    DATA lt_filter LIKE temp3.
    DATA lo_pop TYPE REF TO z2ui5_cl_popup_get_range_m.
    DATA ls_result TYPE z2ui5_cl_popup_get_range_m=>ty_s_result.
    CLEAR temp3.

    temp4-name = `FIELD1`.
    INSERT temp4 INTO TABLE temp3.

    lt_filter = temp3.


    lo_pop = z2ui5_cl_popup_get_range_m=>factory( lt_filter ).

    ls_result = lo_pop->result( ).
    cl_abap_unit_assert=>assert_false( ls_result-check_confirmed ).
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = lines( ls_result-t_filter ) ).

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

    METHODS popup_create
      RETURNING
        VALUE(ro_pop) TYPE REF TO z2ui5_cl_popup_get_range_m.

    METHODS test_init_displays_popup FOR TESTING RAISING cx_static_check.
    METHODS test_confirm             FOR TESTING RAISING cx_static_check.
    METHODS test_list_open_calls_pop FOR TESTING RAISING cx_static_check.
    METHODS test_delete_all          FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_test_roundtrip IMPLEMENTATION.

  METHOD popup_create.

    DATA temp5 TYPE z2ui5_cl_popup_context=>ty_t_filter_multi.
    DATA temp6 LIKE LINE OF temp5.
    DATA temp7 TYPE z2ui5_cl_popup_context=>ty_t_range.
    DATA temp8 LIKE LINE OF temp7.
    DATA temp9 TYPE REF TO z2ui5_cl_ui5_handler.
    CLEAR temp5.

    temp6-name = `MATNR`.

    CLEAR temp7.

    temp8-sign = `I`.
    temp8-option = `EQ`.
    temp8-low = `100`.
    INSERT temp8 INTO TABLE temp7.
    temp6-t_range = temp7.
    INSERT temp6 INTO TABLE temp5.
    ro_pop = z2ui5_cl_popup_get_range_m=>factory( temp5 ).

    CREATE OBJECT temp9 TYPE z2ui5_cl_ui5_handler EXPORTING VAL = ``.
    CREATE OBJECT mo_action EXPORTING VAL = temp9.
    mo_action->mo_app->mo_app = ro_pop.
    CREATE OBJECT mi_client TYPE z2ui5_cl_ui5_client EXPORTING ACTION = mo_action.

    ro_pop->z2ui5_if_app~main( mi_client ).
    mo_action->mo_app->mv_check_initialized = abap_true.

  ENDMETHOD.

  METHOD test_init_displays_popup.

    DATA lo_pop TYPE REF TO z2ui5_cl_popup_get_range_m.
    DATA lv_xml TYPE string.
    DATA temp1 TYPE xsdboolean.
    DATA temp2 TYPE xsdboolean.
    lo_pop = popup_create( ).

    cl_abap_unit_assert=>assert_bound( lo_pop ).

    lv_xml = popup_xml( ).

    temp1 = boolc( lv_xml CS `Define Filter Conditions` ).
    cl_abap_unit_assert=>assert_true( temp1 ).
    " the filter names live in the model, the list only carries the binding

    temp2 = boolc( lv_xml CS `T_FILTER` ).
    cl_abap_unit_assert=>assert_true( temp2 ).

  ENDMETHOD.

  METHOD test_confirm.

    DATA lo_pop TYPE REF TO z2ui5_cl_popup_get_range_m.
    lo_pop = popup_create( ).

    mo_action->ms_actual-event = `BUTTON_CONFIRM`.
    lo_pop->z2ui5_if_app~main( mi_client ).

    cl_abap_unit_assert=>assert_true( lo_pop->result( )-check_confirmed ).
    cl_abap_unit_assert=>assert_true( popup_destroyed( ) ).
    cl_abap_unit_assert=>assert_bound( mo_action->ms_next-o_app_leave ).

  ENDMETHOD.

  METHOD test_list_open_calls_pop.

    DATA lo_pop TYPE REF TO z2ui5_cl_popup_get_range_m.
    DATA temp7 TYPE string_table.
    DATA temp9 TYPE REF TO z2ui5_cl_popup_get_range.
    DATA lo_range_pop LIKE temp9.
    lo_pop = popup_create( ).

    mo_action->ms_actual-event = `LIST_OPEN`.

    CLEAR temp7.
    INSERT `MATNR` INTO TABLE temp7.
    mo_action->ms_actual-t_event_arg = temp7.
    lo_pop->z2ui5_if_app~main( mi_client ).


    temp9 ?= mo_action->ms_next-o_app_call.

    lo_range_pop = temp9.
    cl_abap_unit_assert=>assert_bound( lo_range_pop ).

  ENDMETHOD.

  METHOD test_delete_all.

    DATA lo_pop TYPE REF TO z2ui5_cl_popup_get_range_m.
    FIELD-SYMBOLS <temp10> LIKE LINE OF lo_pop->ms_result-t_filter.
    DATA temp11 LIKE sy-tabix.
    lo_pop = popup_create( ).

    mo_action->ms_actual-event = `POPUP_DELETE_ALL`.
    lo_pop->z2ui5_if_app~main( mi_client ).



    temp11 = sy-tabix.
    READ TABLE lo_pop->ms_result-t_filter INDEX 1 ASSIGNING <temp10>.
    sy-tabix = temp11.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_initial( <temp10>-t_range ).

  ENDMETHOD.

  METHOD popup_xml.

    DATA ls_action LIKE LINE OF mo_action->ms_next-t_action_front.
    LOOP AT mo_action->ms_next-t_action_front INTO ls_action
         WHERE slot = z2ui5_if_client=>cs_view-popup AND method = `display`.
      result = ls_action-xml.
    ENDLOOP.

  ENDMETHOD.

  METHOD popup_destroyed.

    DATA temp12 LIKE sy-subrc.
    DATA temp3 TYPE xsdboolean.
    READ TABLE mo_action->ms_next-t_action_front WITH KEY slot = z2ui5_if_client=>cs_view-popup method = `destroy` TRANSPORTING NO FIELDS.
    temp12 = sy-subrc.

    temp3 = boolc( temp12 = 0 ).
    result = temp3.

  ENDMETHOD.

ENDCLASS.
