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
    TYPES ty_tab TYPE STANDARD TABLE OF ty_s_row WITH EMPTY KEY.

    DATA mt_tab TYPE ty_tab.
    DATA mv_multiselect TYPE abap_bool.
    DATA mv_preselect TYPE abap_bool.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.


CLASS z2ui5_cl_popup_sample_17 IMPLEMENTATION.

  METHOD z2ui5_if_app~main.

    IF client->check_on_init( ).

      client->view_display(
        z2ui5_cl_ui5_view_builder=>factory( 
            )->ele( n = `View` ns = `mvc` 
            )->a( n = `xmlns` v = `sap.m` 
            )->a( n = `xmlns:mvc` v = `sap.ui.core.mvc` 
            )->a( n = `xmlns:core` v = `sap.ui.core` 
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
            )->a( n = `press` v = client->_event( `POPUP` ) 
            )->stringify( ) ).

      RETURN.
    ENDIF.

    CASE client->get( )-event.

      WHEN `POPUP`.
        mt_tab = VALUE #( descr = `this is a description`
             ( zzselkz = mv_preselect title = `title_01`  value = `value_01` )
             ( zzselkz = mv_preselect title = `title_02`  value = `value_02` )
             ( zzselkz = mv_preselect title = `title_03`  value = `value_03` )
             ( zzselkz = mv_preselect title = `title_04`  value = `value_04` )
             ( zzselkz = mv_preselect title = `title_05`  value = `value_05` ) ).

        client->nav_app_call( z2ui5_cl_popup_to_select=>factory(
                           i_tab             = mt_tab
                           i_multiselect     = mv_multiselect
                           i_event_confirmed = `POPUP_CONFIRMED`
                           i_event_canceled  = `POPUP_CANCEL`
          ) ).

      WHEN `POPUP_CANCELED`.
        client->message_box_display( `Popup was cancelled` ).

      WHEN `POPUP_CONFIRMED`.
        DATA(lr) = client->get( )-r_event_data.
        ASSIGN lr->* TO FIELD-SYMBOL(<t>).
        DATA(lt3) = CONV ty_tab( <t> ).

        IF mv_multiselect = abap_false.
          client->message_box_display( |callback after popup to select: { lt3[ 1 ]-title }| ).

        ELSE.
          client->nav_app_call( z2ui5_cl_popup_table=>factory( i_tab   = lt3
                                                             i_title = `Selected rows` ) ).
        ENDIF.

      WHEN `MULTISELECT_TOGGLE`.
        mv_preselect = COND #( WHEN mv_multiselect = abap_false
                               THEN abap_false
                               ELSE mv_preselect ).
        client->view_model_update( ).
    ENDCASE.

  ENDMETHOD.

ENDCLASS.
