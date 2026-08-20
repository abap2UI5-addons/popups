CLASS z2ui5_cl_popup_sample_19 DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.

    TYPES:
      BEGIN OF ty_s_tab,
        selkz            TYPE abap_bool,
        product          TYPE string,
        create_date      TYPE string,
        create_by        TYPE string,
        storage_location TYPE string,
        quantity         TYPE i,
      END OF ty_s_tab.
    TYPES ty_t_table TYPE STANDARD TABLE OF ty_s_tab WITH EMPTY KEY.

    DATA mt_table TYPE ty_t_table.
    DATA mt_token TYPE z2ui5_cl_sample_context=>ty_t_token.

    DATA mt_tokens_added TYPE z2ui5_cl_sample_context=>ty_t_token.
    DATA mt_tokens_removed TYPE z2ui5_cl_sample_context=>ty_t_token.

  PROTECTED SECTION.
    DATA client TYPE REF TO z2ui5_if_client.

    DATA mt_range TYPE z2ui5_cl_pop_get_range=>ty_s_result-t_range.

    METHODS on_event.
    METHODS view_display.
    METHODS set_data.

  PRIVATE SECTION.
ENDCLASS.



CLASS z2ui5_cl_popup_sample_19 IMPLEMENTATION.


  METHOD on_event.

    CASE client->get( )-event.

      WHEN `BUTTON_START`.
        set_data( ).
        client->view_model_update( ).

      WHEN `UPDATE_TOKENS`.
        LOOP AT mt_tokens_removed INTO DATA(ls_token).
          DELETE mt_token WHERE key = ls_token-key.
        ENDLOOP.

        LOOP AT mt_tokens_added INTO ls_token.
          INSERT VALUE #( key = ls_token-key text = ls_token-text visible = abap_true editable = abap_true ) INTO TABLE mt_token.
        ENDLOOP.

        mt_tokens_removed = VALUE #( ).
        mt_tokens_added   = VALUE #( ).

        mt_range = z2ui5_cl_sample_context=>filter_get_range_t_by_token_t( mt_token ).
        set_data( ).
        client->view_model_update( ).

      WHEN `FILTER_VALUE_HELP`.
        client->nav_app_call( z2ui5_cl_pop_get_range=>factory( mt_range ) ).
    ENDCASE.

  ENDMETHOD.


  METHOD set_data.

    mt_table = VALUE #(
        ( product = `table`    create_date = `01.01.2023` create_by = `Peter` storage_location = `AREA_001` quantity = 400 )
        ( product = `chair`    create_date = `01.01.2023` create_by = `Peter` storage_location = `AREA_001` quantity = 400 )
        ( product = `sofa`     create_date = `01.01.2023` create_by = `Peter` storage_location = `AREA_001` quantity = 400 )
        ( product = `computer` create_date = `01.01.2023` create_by = `Peter` storage_location = `AREA_001` quantity = 400 )
        ( product = `oven`     create_date = `01.01.2023` create_by = `Peter` storage_location = `AREA_001` quantity = 400 )
        ( product = `table2`   create_date = `01.01.2023` create_by = `Peter` storage_location = `AREA_001` quantity = 400 ) ).

    DELETE mt_table WHERE product NOT IN mt_range.

  ENDMETHOD.


  METHOD view_display.

    DATA(view) = z2ui5_cl_ui5_view_builder=>factory( 
                     )->ele( n = `View` ns = `mvc` 
                     )->a( n = `xmlns` v = `sap.m` 
                     )->a( n = `xmlns:mvc` v = `sap.ui.core.mvc` 
                     )->a( n = `xmlns:core` v = `sap.ui.core` 
                     )->a( n = `xmlns:z2ui5` v = `z2ui5.cc` 
                     )->a( n = `displayBlock` v = `true` 
                     )->a( n = `height` v = `100%` ).

    view           = view->ele( `Shell` 
                         )->ele( `Page` 
                         )->a( n = `id` v = `page_main` 
                         )->a( n = `title` v = `abap2UI5 - Select-Options` 
                         )->a( n = `navButtonPress` v = client->_event_nav_app_leave( ) 
                         )->a( n = `showNavButton` b = client->check_app_prev_stack( ) 
                         )->end( ).

    DATA(vbox) = view->ele( `VBox` ).
    vbox->tag( n = `MultiInputExt` ns = `z2ui5` 
        )->a( n = `addedTokens` v = client->_bind_edit( mt_tokens_added ) 
        )->a( n = `removedTokens` v = client->_bind_edit( mt_tokens_removed ) 
        )->a( n = `change` v = client->_event( `UPDATE_TOKENS` ) 
        )->a( n = `MultiInputId` v = `MultiInput` ).

    DATA(tab) = vbox->ele( `Table` 
                    )->a( n = `items` v = client->_bind( val = mt_table ) 
                    )->ele( `headerToolbar` 
                    )->ele( `OverflowToolbar` 
                    )->tag( `Text` 
                    )->a( n = `text` v = `Product:` 
                    )->ele( `MultiInput` 
                    )->a( n = `width` v = `30%` 
                    )->a( n = `id` v = `MultiInput` 
                    )->a( n = `tokens` v = client->_bind( mt_token ) 
                    )->a( n = `showClearIcon` b = abap_true 
                    )->a( n = `valueHelpRequest` v = client->_event( `FILTER_VALUE_HELP` ) 
                    )->tag( n = `Item` ns = `core` 
                    )->a( n = `key` v = `{KEY}` 
                    )->a( n = `text` v = `{TEXT}` 
                    )->ele( `tokens` 
                    )->tag( `Token` 
                    )->a( n = `key` v = `{KEY}` 
                    )->a( n = `text` v = `{TEXT}` 
                    )->a( n = `visible` v = `{VISIBLE}` 
                    )->a( n = `selected` v = `{SELKZ}` 
                    )->a( n = `editable` v = `{EDITABLE}` 
                    )->end( 
                    )->end( 
                    )->tag( `ToolbarSpacer` 
                    )->tag( `Button` 
                    )->a( n = `text` v = `Go` 
                    )->a( n = `press` v = client->_event( `BUTTON_START` ) 
                    )->a( n = `type` v = `Emphasized` 
                    )->end( 
                    )->end( ).

    DATA(lo_columns) = tab->ele( `columns` ).
    lo_columns->ele( `Column` 
        )->tag( `Text` 
        )->a( n = `text` v = `Product` ).
    lo_columns->ele( `Column` 
        )->tag( `Text` 
        )->a( n = `text` v = `Date` ).
    lo_columns->ele( `Column` 
        )->tag( `Text` 
        )->a( n = `text` v = `Name` ).
    lo_columns->ele( `Column` 
        )->tag( `Text` 
        )->a( n = `text` v = `Location` ).
    lo_columns->ele( `Column` 
        )->tag( `Text` 
        )->a( n = `text` v = `Quantity` ).

    DATA(lo_cells) = tab->ele( `items` 
                         )->ele( `ColumnListItem` ).
    lo_cells->tag( `Text` 
        )->a( n = `text` v = `{PRODUCT}` ).
    lo_cells->tag( `Text` 
        )->a( n = `text` v = `{CREATE_DATE}` ).
    lo_cells->tag( `Text` 
        )->a( n = `text` v = `{CREATE_BY}` ).
    lo_cells->tag( `Text` 
        )->a( n = `text` v = `{STORAGE_LOCATION}` ).
    lo_cells->tag( `Text` 
        )->a( n = `text` v = `{QUANTITY}` ).

    client->view_display( view->stringify( ) ).

  ENDMETHOD.


  METHOD z2ui5_if_app~main.

    me->client = client.

    IF client->check_on_init( ).

      view_display( ).
      RETURN.
    ENDIF.

    IF client->get( )-check_on_navigated = abap_true.
      TRY.
          DATA(lo_value_help) = CAST z2ui5_cl_pop_get_range( client->get_app( client->get( )-s_draft-id_prev_app ) ).

          IF lo_value_help->result( )-check_confirmed = abap_false.
            RETURN.
          ENDIF.

          mt_range = lo_value_help->result( )-t_range.
          mt_token = z2ui5_cl_sample_context=>filter_get_token_t_by_range_t( mt_range ).
          set_data( ).
          client->view_model_update( ).

        CATCH cx_root.
      ENDTRY.
      RETURN.
    ENDIF.

    IF client->get( )-event IS NOT INITIAL.
      on_event( ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
