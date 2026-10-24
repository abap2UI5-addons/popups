CLASS z2ui5_cl_popup_sample_07 DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.

    TYPES:
      BEGIN OF ty_s_row,
        zzselkz TYPE abap_bool,
        title   TYPE string,
        value   TYPE string,
        descr   TYPE string,
      END OF ty_s_row.

    DATA mv_multiselect TYPE abap_bool.
    DATA mv_preselect TYPE abap_bool.

    METHODS view_display.
    METHODS on_event.
    METHODS on_navigation.

  PROTECTED SECTION.
    DATA mt_tab TYPE STANDARD TABLE OF ty_s_row WITH DEFAULT KEY.
    DATA client TYPE REF TO z2ui5_if_client.

  PRIVATE SECTION.
ENDCLASS.


CLASS z2ui5_cl_popup_sample_07 IMPLEMENTATION.

  METHOD on_event.
        DATA temp1 LIKE mt_tab.
        DATA temp2 LIKE LINE OF temp1.
        DATA temp3 TYPE string.
        DATA lo_app TYPE REF TO z2ui5_cl_popup_to_select.
        DATA temp4 TYPE abap_bool.

    CASE client->get( )-event.

      WHEN `POPUP`.


        CLEAR temp1.

        temp2-descr = `this is a description`.
        temp2-zzselkz = mv_preselect.
        temp2-title = `title_01`.
        temp2-value = `value_01`.
        INSERT temp2 INTO TABLE temp1.
        temp2-zzselkz = mv_preselect.
        temp2-title = `title_02`.
        temp2-value = `value_02`.
        INSERT temp2 INTO TABLE temp1.
        temp2-zzselkz = mv_preselect.
        temp2-title = `title_03`.
        temp2-value = `value_03`.
        INSERT temp2 INTO TABLE temp1.
        temp2-zzselkz = mv_preselect.
        temp2-title = `title_04`.
        temp2-value = `value_04`.
        INSERT temp2 INTO TABLE temp1.
        temp2-zzselkz = mv_preselect.
        temp2-title = `title_05`.
        temp2-value = `value_05`.
        INSERT temp2 INTO TABLE temp1.
        mt_tab = temp1.


        IF mv_multiselect = abap_true.
          temp3 = `Multi select`.
        ELSE.
          temp3 = `Single select`.
        ENDIF.

        lo_app = z2ui5_cl_popup_to_select=>factory(
                           i_tab         = mt_tab
                           i_multiselect = mv_multiselect
                           i_title       = temp3 ).
        client->nav_app_call( lo_app ).

      WHEN `MULTISELECT_TOGGLE`.


        IF mv_multiselect = abap_false.
          temp4 = abap_false.
        ELSE.
          temp4 = mv_preselect.
        ENDIF.
        mv_preselect = temp4.

    ENDCASE.

  ENDMETHOD.


  METHOD view_display.

    DATA view TYPE REF TO z2ui5_cl_ui5_view_builder.
    view = z2ui5_cl_ui5_view_builder=>factory(
                     )->ele( n = `View` ns = `mvc`
                     )->a( n = `xmlns` v = `sap.m`
                     )->a( n = `xmlns:mvc` v = `sap.ui.core.mvc`
                     )->a( n = `displayBlock` v = `true`
                     )->a( n = `height` v = `100%` ).
    view->ele( `Shell`
        )->ele( `Page`
        )->a( n = `title` v = `abap2UI5 - Popup To Select`
        )->a( n = `navButtonPress` v = client->_event_nav_app_leave( )
        )->a( n = `showNavButton` b = client->check_app_prev_stack( )
        )->ele( `HBox`
        )->tag( `Text`
        )->a( n = `text` v = `Multiselect: `
        )->a( n = `class` v = `sapUiTinyMargin`
        )->tag( `Switch`
        )->a( n = `state` v = client->_bind( mv_multiselect )
        )->a( n = `change` v = client->_event( `MULTISELECT_TOGGLE` )
        )->end(
        )->ele( `HBox`
        )->tag( `Text`
        )->a( n = `text` v = `Preselect all entries: `
        )->a( n = `class` v = `sapUiTinyMargin`
        )->tag( `Switch`
        )->a( n = `state` v = client->_bind( mv_preselect )
        )->a( n = `enabled` v = client->_bind( mv_multiselect )
        )->end(
        )->tag( `Button`
        )->a( n = `text` v = `Open Popup...`
        )->a( n = `press` v = client->_event( `POPUP` ) ).

    client->view_display( view->stringify( ) ).

  ENDMETHOD.


  METHOD z2ui5_if_app~main.

    me->client = client.

    IF client->get( )-check_on_navigated = abap_true.

      IF client->check_on_init( ) IS NOT INITIAL.
        view_display( ).

      ELSE.
        on_navigation( ).
      ENDIF.
      RETURN.
    ENDIF.

    on_event( ).

  ENDMETHOD.


  METHOD on_navigation.

    FIELD-SYMBOLS <row> TYPE ty_s_row.
        DATA lo_prev TYPE REF TO z2ui5_if_app.
        DATA temp5 TYPE REF TO z2ui5_cl_popup_to_select.
        DATA ls_result TYPE z2ui5_cl_popup_to_select=>ty_s_result.
          FIELD-SYMBOLS <table> TYPE data.

    TRY.

        lo_prev = client->get_app( client->get( )-s_draft-id_prev_app ).

        temp5 ?= lo_prev.

        ls_result = temp5->result( ).

        IF ls_result-check_confirmed = abap_false.

          client->message_box_display( `Popup was cancelled` ).
          RETURN.
        ENDIF.

        IF mv_multiselect = abap_false.

          ASSIGN ls_result-row->* TO <row>.
          client->message_box_display( |callback after popup to select: { <row>-title }| ).

        ELSE.


          ASSIGN ls_result-table->* TO <table>.
          client->nav_app_call( z2ui5_cl_popup_table=>factory(
                                    i_tab   = <table>
                                    i_title = `Selected rows` ) ).

        ENDIF.

      CATCH cx_root ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.
