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

    DATA mt_tab TYPE STANDARD TABLE OF ty_s_row WITH EMPTY KEY.
    DATA mv_multiselect TYPE abap_bool.
    DATA mv_preselect TYPE abap_bool.

    METHODS view_display.
    METHODS on_event.
    METHODS on_navigation.

  PROTECTED SECTION.
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
                                             THEN `Multi select`
                                             ELSE `Single select` ) ).
        client->nav_app_call( lo_app ).

      WHEN `MULTISELECT_TOGGLE`.

        mv_preselect = COND #( WHEN mv_multiselect = abap_false
                               THEN abap_false
                               ELSE mv_preselect ).

        client->view_model_update( ).
    ENDCASE.

  ENDMETHOD.


  METHOD view_display.

    DATA(view) = z2ui5_cl_ui5_view_builder=>factory( 
                     )->ele( n = `View` ns = `mvc` 
                     )->a( n = `xmlns` v = `sap.m` 
                     )->a( n = `xmlns:mvc` v = `sap.ui.core.mvc` 
                     )->a( n = `xmlns:core` v = `sap.ui.core` 
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
        )->a( n = `state` v = client->_bind_edit( mv_multiselect ) 
        )->a( n = `change` v = client->_event( `MULTISELECT_TOGGLE` ) 
        )->end( 
        )->ele( `HBox` 
        )->tag( `Text` 
        )->a( n = `text` v = `Preselect all entries: ` 
        )->a( n = `class` v = `sapUiTinyMargin` 
        )->tag( `Switch` 
        )->a( n = `state` v = client->_bind_edit( mv_preselect ) 
        )->a( n = `enabled` v = client->_bind_edit( mv_multiselect ) 
        )->end( 
        )->tag( `Button` 
        )->a( n = `text` v = `Open Popup...` 
        )->a( n = `press` v = client->_event( `POPUP` ) ).

    client->view_display( view->stringify( ) ).

  ENDMETHOD.


  METHOD z2ui5_if_app~main.

    me->client = client.

    IF client->get( )-check_on_navigated = abap_true.

      IF client->check_on_init( ).
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

    TRY.
        DATA(lo_prev) = client->get_app( client->get( )-s_draft-id_prev_app ).
        DATA(ls_result) = CAST z2ui5_cl_popup_to_select( lo_prev )->result( ).

        IF ls_result-check_confirmed = abap_false.

          client->message_box_display( `Popup was cancelled` ).
          RETURN.
        ENDIF.

        IF mv_multiselect = abap_false.

          ASSIGN ls_result-row->* TO <row>.
          client->message_box_display( |callback after popup to select: { <row>-title }| ).

        ELSE.

          ASSIGN ls_result-table->* TO FIELD-SYMBOL(<table>).
          client->nav_app_call( z2ui5_cl_popup_table=>factory(
                                    i_tab   = <table>
                                    i_title = `Selected rows` ) ).

        ENDIF.

      CATCH cx_root.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.
