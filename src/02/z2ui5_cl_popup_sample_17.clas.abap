CLASS z2ui5_cl_popup_sample_17 DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.

    TYPES:
      BEGIN OF ty_s_row,
        zzselkz TYPE abap_bool,
        title   TYPE string,
        value   TYPE string,
        descr   TYPE string,
      END OF ty_s_row.
    TYPES ty_tab TYPE STANDARD TABLE OF ty_s_row WITH DEFAULT KEY.

    DATA mv_multiselect TYPE abap_bool.
    DATA mv_preselect TYPE abap_bool.

  PROTECTED SECTION.
    DATA mt_tab TYPE ty_tab.
  PRIVATE SECTION.
ENDCLASS.


CLASS z2ui5_cl_popup_sample_17 IMPLEMENTATION.

  METHOD z2ui5_if_app~main.
        DATA temp1 TYPE z2ui5_cl_popup_sample_17=>ty_tab.
        DATA temp2 LIKE LINE OF temp1.
        DATA lr TYPE REF TO data.
        FIELD-SYMBOLS <t> TYPE data.
        DATA temp3 TYPE ty_tab.
        DATA lt3 LIKE temp3.
          FIELD-SYMBOLS <temp4> LIKE LINE OF lt3.
          DATA temp5 LIKE sy-tabix.
        DATA temp6 TYPE abap_bool.

    IF client->check_on_init( ) IS NOT INITIAL.

      client->view_display(
        z2ui5_cl_ui5_view_builder=>factory(
            )->ele( n = `View` ns = `mvc`
            )->a( n = `xmlns` v = `sap.m`
            )->a( n = `xmlns:mvc` v = `sap.ui.core.mvc`
            )->a( n = `displayBlock` v = `true`
            )->a( n = `height` v = `100%`
            )->ele( `Shell`
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
            )->a( n = `press` v = client->_event( `POPUP` )
            )->stringify( ) ).

      RETURN.
    ENDIF.

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

        client->nav_app_call( z2ui5_cl_popup_to_select=>factory(
                           i_tab             = mt_tab
                           i_multiselect     = mv_multiselect
                           i_event_confirmed = `POPUP_CONFIRMED`
                           i_event_canceled  = `POPUP_CANCELED`
          ) ).

      " abap2ui5lint-disable-next-line handler-without-event -- raised by z2ui5_cl_popup_to_select, named in i_event_canceled above
      WHEN `POPUP_CANCELED`.
        client->message_box_display( `Popup was cancelled` ).

      " abap2ui5lint-disable-next-line handler-without-event -- raised by z2ui5_cl_popup_to_select, named in i_event_confirmed above
      WHEN `POPUP_CONFIRMED`.

        lr = client->get( )-r_event_data.

        ASSIGN lr->* TO <t>.

        temp3 = <t>.

        lt3 = temp3.

        IF mv_multiselect = abap_false.


          temp5 = sy-tabix.
          READ TABLE lt3 INDEX 1 ASSIGNING <temp4>.
          sy-tabix = temp5.
          IF sy-subrc <> 0.
            ASSERT 1 = 0.
          ENDIF.
          client->message_box_display( |callback after popup to select: { <temp4>-title }| ).

        ELSE.
          client->nav_app_call( z2ui5_cl_popup_table=>factory( i_tab   = lt3
                                                             i_title = `Selected rows` ) ).
        ENDIF.

      WHEN `MULTISELECT_TOGGLE`.

        IF mv_multiselect = abap_false.
          temp6 = abap_false.
        ELSE.
          temp6 = mv_preselect.
        ENDIF.
        mv_preselect = temp6.
    ENDCASE.

  ENDMETHOD.

ENDCLASS.
