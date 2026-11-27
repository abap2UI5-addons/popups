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
    DATA mt_tab TYPE STANDARD TABLE OF ty_s_row WITH EMPTY KEY.
    DATA client TYPE REF TO z2ui5_if_client.

  PRIVATE SECTION.
ENDCLASS.


CLASS z2ui5_cl_popup_sample_07 IMPLEMENTATION.

  METHOD on_event.

    CASE client->get( )-event.

      WHEN `POPUP`.

        mt_tab = VALUE #( descr = `this is a description`
             ( zzselkz = mv_preselect title = `title_01`  value = `value_01` )
             ( zzselkz = mv_preselect title = `title_02`  value = `value_02` )
             ( zzselkz = mv_preselect title = `title_03`  value = `value_03` )
             ( zzselkz = mv_preselect title = `title_04`  value = `value_04` )
             ( zzselkz = mv_preselect title = `title_05`  value = `value_05` ) ).

        DATA(lo_app) = z2ui5_cl_popup_to_select=>factory(
                           i_tab         = mt_tab
                           i_multiselect = mv_multiselect
                           i_title       = COND #(
                                             WHEN mv_multiselect = abap_true
                                             THEN `Multi Select`
                                             ELSE `Single Select` ) ).
        " the tag tells the return of this popup from the return of the
        " table popup below, which has none
        client->nav_app_call( app = lo_app
                              tag = `SELECT` ).

      WHEN `MULTISELECT_TOGGLE`.

        mv_preselect = COND #( WHEN mv_multiselect = abap_false
                               THEN abap_false
                               ELSE mv_preselect ).

    ENDCASE.

  ENDMETHOD.


  METHOD view_display.

    DATA(view) = z2ui5_cl_ui5_view_builder=>factory(
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

    IF client->check_on_nav_back( ).
      on_navigation( ).
    ELSEIF client->check_on_navigated( ).
      view_display( ).
    ELSE.
      on_event( ).
    ENDIF.

  ENDMETHOD.


  METHOD on_navigation.

    FIELD-SYMBOLS <row> TYPE ty_s_row.
    FIELD-SYMBOLS <table> TYPE STANDARD TABLE.

    " back from the table of the selected rows - nothing to read
    IF client->get_nav_tag( ) <> `SELECT`.
      RETURN.
    ENDIF.

    " every popup hands its result over through z2ui5_if_popup_result
    DATA(popup) = CAST z2ui5_if_popup_result( client->get_app_prev( ) ).
    IF popup->check_confirmed( ) = abap_false.
      client->message_box_display( `Popup was cancelled` ).
      RETURN.
    ENDIF.

    " value( ) is the chosen row, with multiselect the chosen rows
    DATA(lr_value) = popup->value( ).
    IF mv_multiselect = abap_false.

      ASSIGN lr_value->* TO <row>.
      client->message_box_display( |callback after popup to select: { <row>-title }| ).

    ELSE.

      ASSIGN lr_value->* TO <table>.
      client->nav_app_call( z2ui5_cl_popup_table=>factory(
                                i_tab   = <table>
                                i_title = `Selected rows` ) ).

    ENDIF.

  ENDMETHOD.

ENDCLASS.
