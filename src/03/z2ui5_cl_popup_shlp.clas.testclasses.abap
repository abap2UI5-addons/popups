CLASS ltcl_popup_result DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_s_row,
        werks TYPE string,
      END OF ty_s_row.
    TYPES ty_t_row TYPE STANDARD TABLE OF ty_s_row WITH DEFAULT KEY.

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
    DATA temp1 TYPE REF TO z2ui5_cl_ui5_client.
    CREATE OBJECT temp2 TYPE z2ui5_cl_ui5_handler EXPORTING VAL = ``.
    CREATE OBJECT mo_action EXPORTING VAL = temp2.
    mo_action->mo_app->mo_app = io_app.
    mo_action->mo_app->mv_check_initialized = abap_true.
    mo_action->ms_actual-event = iv_event.
    mo_action->ms_actual-t_event_arg = it_arg.

    CREATE OBJECT temp1 TYPE z2ui5_cl_ui5_client EXPORTING ACTION = mo_action.
    io_app->main( temp1 ).

  ENDMETHOD.

  METHOD test_initial_not_confirmed.

    DATA lo_pop TYPE REF TO z2ui5_cl_popup_shlp.
    DATA temp2 TYPE REF TO z2ui5_if_popup_result.
    DATA li_result LIKE temp2.
    lo_pop = z2ui5_cl_popup_shlp=>factory_by_searchhelp( i_searchhelp = `H_T001W`
                                                         i_fname      = `WERKS` ).

    temp2 ?= lo_pop.

    li_result = temp2.

    cl_abap_unit_assert=>assert_false( li_result->check_confirmed( ) ).

  ENDMETHOD.

  METHOD test_confirmed.

    FIELD-SYMBOLS <tab> TYPE ty_t_row.

    DATA lo_pop TYPE REF TO z2ui5_cl_popup_shlp.
    DATA temp3 TYPE ltcl_popup_result=>ty_t_row.
    DATA temp4 LIKE LINE OF temp3.
    DATA temp5 TYPE string_table.
    DATA temp7 TYPE REF TO z2ui5_if_popup_result.
    DATA li_result LIKE temp7.
    DATA temp8 TYPE REF TO string.
    DATA lr_value LIKE temp8.
    lo_pop = z2ui5_cl_popup_shlp=>factory_by_searchhelp( i_searchhelp = `H_T001W`
                                                         i_fname      = `WERKS` ).
    CREATE DATA lo_pop->mt_data TYPE ty_t_row.
    ASSIGN lo_pop->mt_data->* TO <tab>.

    CLEAR temp3.

    temp4-werks = `1000`.
    INSERT temp4 INTO TABLE temp3.
    temp4-werks = `2000`.
    INSERT temp4 INTO TABLE temp3.
    <tab> = temp3.

    CLEAR temp5.
    INSERT `2` INTO TABLE temp5.
    roundtrip( io_app   = lo_pop
               iv_event = `SHLP_ROW_SELECT`
               it_arg   = temp5 ).

    temp7 ?= lo_pop.

    li_result = temp7.

    cl_abap_unit_assert=>assert_true( li_result->check_confirmed( ) ).
    cl_abap_unit_assert=>assert_equals( exp = `2000`
                                        act = li_result->value_string( ) ).

    temp8 ?= li_result->value( ).

    lr_value = temp8.
    cl_abap_unit_assert=>assert_equals( exp = `2000`
                                        act = lr_value->* ).

  ENDMETHOD.

  METHOD test_cancelled.

    DATA lo_pop TYPE REF TO z2ui5_cl_popup_shlp.
    DATA temp9 TYPE REF TO z2ui5_if_popup_result.
    DATA li_result LIKE temp9.
    lo_pop = z2ui5_cl_popup_shlp=>factory_by_searchhelp( i_searchhelp = `H_T001W`
                                                         i_fname      = `WERKS` ).
    roundtrip( io_app   = lo_pop
               iv_event = `SHLP_CLOSE` ).

    temp9 ?= lo_pop.

    li_result = temp9.

    cl_abap_unit_assert=>assert_false( li_result->check_confirmed( ) ).

  ENDMETHOD.

ENDCLASS.
