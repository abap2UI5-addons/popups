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

    DATA(lo_pop) = z2ui5_cl_popup_get_values=>factory(
        i_fields  = VALUE #( ( name = `carrid` text = `Airline` kind = z2ui5_cl_popup_get_values=>cs_kind-listbox )
                             ( name = `fldate` text = `Date` kind = z2ui5_cl_popup_get_values=>cs_kind-date )
                             ( name = `full` text = `Full only` kind = z2ui5_cl_popup_get_values=>cs_kind-checkbox ) )
        i_listbox = VALUE #( ( name = `carrid` key = `LH` text = `Lufthansa` ) ) ).
    client_create( lo_pop ).
    lo_pop->z2ui5_if_app~main( mi_client ).

    DATA(lv_xml) = popup_xml( ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_xml CS `Lufthansa` ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_xml CS `DatePicker` ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_xml CS `CheckBox` ) ).
    cl_abap_unit_assert=>assert_equals( exp = `CARRID`
                                        act = lo_pop->mt_field[ 1 ]-name ).

  ENDMETHOD.

  METHOD test_required_stays.

    DATA(lo_pop) = z2ui5_cl_popup_get_values=>factory(
        VALUE #( ( name = `CARRID` text = `Airline` required = abap_true ) ) ).
    roundtrip_event( io_app   = lo_pop
                     iv_event = `BUTTON_CONFIRM` ).

    cl_abap_unit_assert=>assert_false( lo_pop->result( )-check_confirmed ).
    cl_abap_unit_assert=>assert_true( xsdbool( popup_xml( ) CS `Fill in the required field Airline` ) ).
    cl_abap_unit_assert=>assert_false( popup_destroyed( ) ).

  ENDMETHOD.

  METHOD test_confirm_values.

    DATA(lo_pop) = z2ui5_cl_popup_get_values=>factory(
        VALUE #( ( name = `CARRID` text = `Airline` required = abap_true )
                            ( name = `FULL` text = `Full only` kind = z2ui5_cl_popup_get_values=>cs_kind-checkbox ) ) ).
    lo_pop->mt_field[ 1 ]-value = `LH`.
    lo_pop->mt_field[ 2 ]-flag = abap_true.
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

    DATA(lo_pop) = z2ui5_cl_popup_get_values=>factory(
        VALUE #( ( name = `CARRID` text = `Airline` required = abap_true ) ) ).
    roundtrip_event( io_app   = lo_pop
                     iv_event = `BUTTON_CANCEL` ).

    cl_abap_unit_assert=>assert_false( lo_pop->result( )-check_confirmed ).
    cl_abap_unit_assert=>assert_true( popup_destroyed( ) ).

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
