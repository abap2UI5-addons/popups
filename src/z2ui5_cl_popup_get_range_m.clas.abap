CLASS z2ui5_cl_popup_get_range_m DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.

    CLASS-METHODS factory
      IMPORTING
        val             TYPE z2ui5_cl_popup_context=>ty_t_filter_multi
      RETURNING
        VALUE(r_result) TYPE REF TO z2ui5_cl_popup_get_range_m.

    TYPES:
      BEGIN OF ty_s_result,
        t_filter        TYPE z2ui5_cl_popup_context=>ty_t_filter_multi,
        check_confirmed TYPE abap_bool,
      END OF ty_s_result.

    DATA ms_result TYPE ty_s_result.

    METHODS result
      RETURNING
        VALUE(result) TYPE ty_s_result.

  PROTECTED SECTION.
    DATA client        TYPE REF TO z2ui5_if_client.
    DATA mv_popup_name TYPE string.

    METHODS popup_display.

    METHODS init.

  PRIVATE SECTION.
ENDCLASS.


CLASS z2ui5_cl_popup_get_range_m IMPLEMENTATION.

  METHOD factory.

    CREATE OBJECT r_result.
    r_result->ms_result-t_filter = val.

  ENDMETHOD.

  METHOD init.

    popup_display( ).

  ENDMETHOD.

  METHOD popup_display.

    DATA lo_popup TYPE REF TO z2ui5_cl_ui5_view_builder.
    DATA vbox TYPE REF TO z2ui5_cl_ui5_view_builder.
    DATA item TYPE REF TO z2ui5_cl_ui5_view_builder.
    DATA grid TYPE REF TO z2ui5_cl_ui5_view_builder.
    lo_popup = z2ui5_cl_ui5_view_builder=>factory(
                         )->ele( n = `FragmentDefinition` ns = `core`
                         )->a( n = `xmlns` v = `sap.m`
                         )->a( n = `xmlns:core` v = `sap.ui.core`
                         )->a( n = `xmlns:layout` v = `sap.ui.layout` ).
    lo_popup = lo_popup->ele( `Dialog`
                   )->a( n = `afterClose` v = client->_event( `BUTTON_CANCEL` )
                   )->a( n = `contentHeight` v = `50%`
                   )->a( n = `contentWidth` v = `50%`
                   )->a( n = `title` v = `Define Filter Conditions` ).


    vbox = lo_popup->ele( `VBox`
                     )->a( n = `height` v = `100%`
                     )->a( n = `justifyContent` v = `SpaceBetween` ).


    item = vbox->ele( `List`
                     )->a( n = `noDataText` v = `No conditions defined`
                     )->a( n = `items` v = client->_bind( ms_result-t_filter )
                     )->ele( `CustomListItem` ).


    grid = item->ele( n = `Grid` ns = `layout`
                     )->a( n = `class` v = `sapUiSmallMarginTop sapUiSmallMarginBottom sapUiSmallMarginBegin` ).
    grid->tag( `Text`
        )->a( n = `text` v = `{NAME}` ).

    grid->ele( `MultiInput`
        )->a( n = `tokens` v = `{T_TOKEN}`
        )->a( n = `enabled` b = abap_false
        )->ele( `tokens`
        )->tag( `Token`
        )->a( n = `key` v = `{KEY}`
        )->a( n = `text` v = `{TEXT}`
        )->a( n = `visible` v = `{VISIBLE}`
        )->a( n = `selected` v = `{SELKZ}`
        )->a( n = `editable` v = `{EDITABLE}` ).

    grid->tag( `Button`
        )->a( n = `text` v = `Select`
        )->a( n = `press` v = client->_event( val   = `LIST_OPEN`
                                          arg   = `${NAME}` ) ).
    grid->tag( `Button`
        )->a( n = `icon` v = `sap-icon://delete`
        )->a( n = `type` v = `Transparent`
        )->a( n = `text` v = `Clear`
        )->a( n = `press` v = client->_event( val   = `LIST_DELETE`
                                          arg   = `${NAME}` ) ).

    lo_popup->ele( `buttons`
        )->tag( `Button`
        )->a( n = `text` v = `Clear All`
        )->a( n = `icon` v = `sap-icon://delete`
        )->a( n = `type` v = `Transparent`
        )->a( n = `press` v = client->_event( `POPUP_DELETE_ALL` )
        )->tag( `Button`
        )->a( n = `text` v = `Cancel`
        )->a( n = `press` v = client->_event( `BUTTON_CANCEL` )
        )->tag( `Button`
        )->a( n = `text` v = `OK`
        )->a( n = `press` v = client->_event( `BUTTON_CONFIRM` )
        )->a( n = `type` v = `Emphasized` ).

    client->popup_display( lo_popup->stringify( ) ).
  ENDMETHOD.

  METHOD result.
    result = ms_result.
  ENDMETHOD.

  METHOD z2ui5_if_app~main.
    DATA ls_get TYPE z2ui5_if_client=>ty_s_get.
      DATA temp13 TYPE REF TO z2ui5_cl_popup_get_range.
      DATA lo_popup LIKE temp13.
      DATA ls_popup_result TYPE z2ui5_cl_popup_get_range=>ty_s_result.
        FIELD-SYMBOLS <tab> TYPE z2ui5_cl_popup_context=>ty_s_filter_multi.
        FIELD-SYMBOLS <temp14> LIKE LINE OF ms_result-t_filter.
        DATA temp15 LIKE sy-tabix.
        DATA temp16 LIKE LINE OF ms_result-t_filter.
        DATA lr_filter LIKE REF TO temp16.
    me->client = client.

    IF client->check_on_init( ) IS NOT INITIAL.
      init( ).
      RETURN.
    ENDIF.


    ls_get = client->get( ).

    IF ls_get-check_on_navigated = abap_true.


      temp13 ?= client->get_app_prev( ).

      lo_popup = temp13.

      ls_popup_result = lo_popup->result( ).
      IF ls_popup_result-check_confirmed = abap_true.

        READ TABLE ms_result-t_filter WITH KEY name = mv_popup_name ASSIGNING <tab>.
        <tab>-t_range = ls_popup_result-t_range.
        <tab>-t_token = z2ui5_cl_popup_context=>filter_get_token_t_by_range_t( <tab>-t_range ).
      ENDIF.
      popup_display( ).

    ENDIF.

    CASE ls_get-event.

      WHEN `LIST_DELETE`.
        READ TABLE ms_result-t_filter WITH KEY name = client->get_event_arg( ) ASSIGNING <tab>.
        CLEAR <tab>-t_token.
        CLEAR <tab>-t_range.

      WHEN `LIST_OPEN`.
        mv_popup_name = client->get_event_arg( ).


        temp15 = sy-tabix.
        READ TABLE ms_result-t_filter WITH KEY name = mv_popup_name ASSIGNING <temp14>.
        sy-tabix = temp15.
        IF sy-subrc <> 0.
          ASSERT 1 = 0.
        ENDIF.
        client->nav_app_call( z2ui5_cl_popup_get_range=>factory(
            <temp14>-t_range ) ).

      WHEN `BUTTON_CONFIRM`.
        ms_result-check_confirmed = abap_true.
        client->popup_destroy( ).
        client->nav_app_leave( ).

      WHEN `BUTTON_CANCEL`.
        client->popup_destroy( ).
        client->nav_app_leave( ).

      WHEN `POPUP_DELETE_ALL`.


        LOOP AT ms_result-t_filter REFERENCE INTO lr_filter.
          CLEAR lr_filter->t_range.
          CLEAR lr_filter->t_token.
        ENDLOOP.

    ENDCASE.
  ENDMETHOD.

ENDCLASS.
