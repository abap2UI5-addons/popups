
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
    DATA lo_pop TYPE REF TO z2ui5_cl_popup_get_ranges.
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


    lo_pop = z2ui5_cl_popup_get_ranges=>factory( lt_filter ).
    cl_abap_unit_assert=>assert_bound( lo_pop ).

  ENDMETHOD.

  METHOD test_result_initial.

    DATA temp3 TYPE z2ui5_cl_popup_context=>ty_t_filter_multi.
    DATA temp4 LIKE LINE OF temp3.
    DATA lt_filter LIKE temp3.
    DATA lo_pop TYPE REF TO z2ui5_cl_popup_get_ranges.
    DATA ls_result TYPE z2ui5_cl_popup_get_ranges=>ty_s_result.
    CLEAR temp3.

    temp4-name = `FIELD1`.
    INSERT temp4 INTO TABLE temp3.

    lt_filter = temp3.


    lo_pop = z2ui5_cl_popup_get_ranges=>factory( lt_filter ).

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
        VALUE(ro_pop) TYPE REF TO z2ui5_cl_popup_get_ranges.

    METHODS test_init_displays_popup FOR TESTING RAISING cx_static_check.
    METHODS test_confirm             FOR TESTING RAISING cx_static_check.
    METHODS test_list_open_calls_pop FOR TESTING RAISING cx_static_check.
    METHODS test_delete_all          FOR TESTING RAISING cx_static_check.
    METHODS test_list_open_unknown   FOR TESTING RAISING cx_static_check.

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
    ro_pop = z2ui5_cl_popup_get_ranges=>factory( temp5 ).

    CREATE OBJECT temp9 TYPE z2ui5_cl_ui5_handler EXPORTING VAL = ``.
    CREATE OBJECT mo_action EXPORTING VAL = temp9.
    mo_action->mo_app->mo_app = ro_pop.
    CREATE OBJECT mi_client TYPE z2ui5_cl_ui5_client EXPORTING ACTION = mo_action.

    ro_pop->z2ui5_if_app~main( mi_client ).
    mo_action->mo_app->mv_check_initialized = abap_true.

  ENDMETHOD.

  METHOD test_init_displays_popup.

    DATA lo_pop TYPE REF TO z2ui5_cl_popup_get_ranges.
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

    DATA lo_pop TYPE REF TO z2ui5_cl_popup_get_ranges.
    lo_pop = popup_create( ).

    mo_action->ms_actual-event = `BUTTON_CONFIRM`.
    lo_pop->z2ui5_if_app~main( mi_client ).

    cl_abap_unit_assert=>assert_true( lo_pop->result( )-check_confirmed ).
    cl_abap_unit_assert=>assert_true( popup_destroyed( ) ).
    cl_abap_unit_assert=>assert_bound( mo_action->ms_next-o_app_leave ).

  ENDMETHOD.

  METHOD test_list_open_calls_pop.

    DATA lo_pop TYPE REF TO z2ui5_cl_popup_get_ranges.
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

  METHOD test_list_open_unknown.

    " the name comes from the client - one that is no filter calls nothing
    " instead of raising CX_SY_ITAB_LINE_NOT_FOUND
    DATA lo_pop TYPE REF TO z2ui5_cl_popup_get_ranges.
    DATA temp10 TYPE string_table.
    lo_pop = popup_create( ).

    mo_action->ms_actual-event = `LIST_OPEN`.

    CLEAR temp10.
    INSERT `UNKNOWN` INTO TABLE temp10.
    mo_action->ms_actual-t_event_arg = temp10.
    lo_pop->z2ui5_if_app~main( mi_client ).

    cl_abap_unit_assert=>assert_not_bound( mo_action->ms_next-o_app_call ).

  ENDMETHOD.

  METHOD test_delete_all.

    DATA lo_pop TYPE REF TO z2ui5_cl_popup_get_ranges.
    FIELD-SYMBOLS <temp12> LIKE LINE OF lo_pop->ms_result-t_filter.
    DATA temp13 LIKE sy-tabix.
    lo_pop = popup_create( ).

    mo_action->ms_actual-event = `POPUP_DELETE_ALL`.
    lo_pop->z2ui5_if_app~main( mi_client ).



    temp13 = sy-tabix.
    READ TABLE lo_pop->ms_result-t_filter INDEX 1 ASSIGNING <temp12>.
    sy-tabix = temp13.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_initial( <temp12>-t_range ).

  ENDMETHOD.

  METHOD popup_xml.

    DATA ls_action LIKE LINE OF mo_action->ms_next-t_action_front.
    LOOP AT mo_action->ms_next-t_action_front INTO ls_action
         WHERE slot = z2ui5_if_client=>cs_view-popup AND method = `display`.
      result = ls_action-xml.
    ENDLOOP.

  ENDMETHOD.

  METHOD popup_destroyed.

    DATA temp14 LIKE sy-subrc.
    DATA temp3 TYPE xsdboolean.
    READ TABLE mo_action->ms_next-t_action_front WITH KEY slot = z2ui5_if_client=>cs_view-popup method = `destroy` TRANSPORTING NO FIELDS.
    temp14 = sy-subrc.

    temp3 = boolc( temp14 = 0 ).
    result = temp3.

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
    DATA temp10 TYPE REF TO z2ui5_cl_ui5_handler.
    DATA temp15 TYPE REF TO z2ui5_cl_ui5_client.
    CREATE OBJECT temp10 TYPE z2ui5_cl_ui5_handler EXPORTING VAL = ``.
    CREATE OBJECT mo_action EXPORTING VAL = temp10.
    mo_action->mo_app->mo_app = io_app.
    mo_action->mo_app->mv_check_initialized = abap_true.
    mo_action->ms_actual-event = iv_event.
    mo_action->ms_actual-t_event_arg = it_arg.

    CREATE OBJECT temp15 TYPE z2ui5_cl_ui5_client EXPORTING ACTION = mo_action.
    io_app->main( temp15 ).

  ENDMETHOD.

  METHOD test_initial_not_confirmed.

    DATA temp16 TYPE z2ui5_cl_popup_context=>ty_t_filter_multi.
    DATA temp17 LIKE LINE OF temp16.
    DATA temp11 TYPE z2ui5_cl_popup_context=>ty_t_range.
    DATA temp12 LIKE LINE OF temp11.
    DATA lo_pop TYPE REF TO z2ui5_cl_popup_get_ranges.
    DATA temp18 TYPE REF TO z2ui5_if_popup_result.
    DATA li_result LIKE temp18.
    CLEAR temp16.

    temp17-name = `MATNR`.

    CLEAR temp11.

    temp12-sign = `I`.
    temp12-option = `EQ`.
    temp12-low = `1`.
    INSERT temp12 INTO TABLE temp11.
    temp17-t_range = temp11.
    INSERT temp17 INTO TABLE temp16.

    lo_pop = z2ui5_cl_popup_get_ranges=>factory( temp16 ).

    temp18 ?= lo_pop.

    li_result = temp18.

    cl_abap_unit_assert=>assert_false( li_result->check_confirmed( ) ).

  ENDMETHOD.

  METHOD test_confirmed.

    DATA temp19 TYPE z2ui5_cl_popup_context=>ty_t_filter_multi.
    DATA temp20 LIKE LINE OF temp19.
    DATA temp13 TYPE z2ui5_cl_popup_context=>ty_t_range.
    DATA temp14 LIKE LINE OF temp13.
    DATA lo_pop TYPE REF TO z2ui5_cl_popup_get_ranges.
    DATA temp21 TYPE REF TO z2ui5_if_popup_result.
    DATA li_result LIKE temp21.
    DATA temp22 TYPE REF TO z2ui5_cl_popup_context=>ty_t_filter_multi.
    DATA lr_filter LIKE temp22.
    FIELD-SYMBOLS <temp23> LIKE LINE OF lr_filter->*.
    DATA temp24 LIKE sy-tabix.
    CLEAR temp19.

    temp20-name = `MATNR`.

    CLEAR temp13.

    temp14-sign = `I`.
    temp14-option = `EQ`.
    temp14-low = `1`.
    INSERT temp14 INTO TABLE temp13.
    temp20-t_range = temp13.
    INSERT temp20 INTO TABLE temp19.

    lo_pop = z2ui5_cl_popup_get_ranges=>factory( temp19 ).
    roundtrip( io_app   = lo_pop
               iv_event = `BUTTON_CONFIRM` ).

    temp21 ?= lo_pop.

    li_result = temp21.

    cl_abap_unit_assert=>assert_true( li_result->check_confirmed( ) ).

    temp22 ?= li_result->value( ).

    lr_filter = temp22.


    temp24 = sy-tabix.
    READ TABLE lr_filter->* INDEX 1 ASSIGNING <temp23>.
    sy-tabix = temp24.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `MATNR`
                                        act = <temp23>-name ).
    cl_abap_unit_assert=>assert_initial( li_result->value_string( ) ).

  ENDMETHOD.

  METHOD test_cancelled.

    DATA temp25 TYPE z2ui5_cl_popup_context=>ty_t_filter_multi.
    DATA temp26 LIKE LINE OF temp25.
    DATA temp15 TYPE z2ui5_cl_popup_context=>ty_t_range.
    DATA temp16 LIKE LINE OF temp15.
    DATA lo_pop TYPE REF TO z2ui5_cl_popup_get_ranges.
    DATA temp27 TYPE REF TO z2ui5_if_popup_result.
    DATA li_result LIKE temp27.
    CLEAR temp25.

    temp26-name = `MATNR`.

    CLEAR temp15.

    temp16-sign = `I`.
    temp16-option = `EQ`.
    temp16-low = `1`.
    INSERT temp16 INTO TABLE temp15.
    temp26-t_range = temp15.
    INSERT temp26 INTO TABLE temp25.

    lo_pop = z2ui5_cl_popup_get_ranges=>factory( temp25 ).
    roundtrip( io_app   = lo_pop
               iv_event = `BUTTON_CANCEL` ).

    temp27 ?= lo_pop.

    li_result = temp27.

    cl_abap_unit_assert=>assert_false( li_result->check_confirmed( ) ).

  ENDMETHOD.

ENDCLASS.
