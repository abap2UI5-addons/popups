CLASS z2ui5_cl_popup_sample_25 DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.

    "! the messages of the button in the footer - the popover in the app's
    "! own view, over its own table
    DATA mt_msg TYPE z2ui5_cl_popup_msgpopover=>ty_t_msg.

  PROTECTED SECTION.
    DATA client TYPE REF TO z2ui5_if_client.

    METHODS view_display.
    METHODS on_event.

  PRIVATE SECTION.
ENDCLASS.


CLASS z2ui5_cl_popup_sample_25 IMPLEMENTATION.

  METHOD z2ui5_if_app~main.

    me->client = client.

    " the popover hands control back with its event AND check_on_navigated( )
    " at once, so the event arm comes before the navigated arm
    IF client->check_on_init( ).
      mt_msg = z2ui5_cl_popup_msgpopover=>msg_get_t( VALUE bapirettab(
                   ( type = `E` id = `MSG1` number = `001` message = `Storage bin is blocked` )
                   ( type = `W` id = `MSG1` number = `002` message = `Quantity exceeds the planned quantity` )
                   ( type = `S` id = `MSG1` number = `003` message = `Handling unit created` ) ) ).
      view_display( ).

    " abap2ui5lint-disable-next-line handler-without-event -- raised by z2ui5_cl_popup_msgpopover, named in i_event_close
    ELSEIF client->check_on_event( `POPOVER_CLOSED` ).
      client->message_toast_display( `Message popover closed` ).

    ELSEIF client->check_on_navigated( ).
      view_display( ).

    ELSE.
      on_event( ).
    ENDIF.

  ENDMETHOD.

  METHOD on_event.

    CASE client->get( )-event.

      WHEN `POPOVER_BAPIRET`.
        client->nav_app_call( z2ui5_cl_popup_msgpopover=>factory(
            i_messages    = VALUE bapirettab(
                ( type = `E` id = `MSG1` number = `001` message = `An empty Report field causes an empty XML Message to be sent` )
                ( type = `W` id = `MSG2` number = `002` message = `Product already in use` )
                ( type = `S` id = `MSG3` number = `003` message = `Order saved` )
                ( type = `I` id = `MSG4` number = `004` message = `Delivery date moved` ) )
            i_open_by_id  = `btnBapiret`
            i_event_close = `POPOVER_CLOSED` ) ).

      WHEN `POPOVER_TEXTS`.
        client->nav_app_call( z2ui5_cl_popup_msgpopover=>factory(
            i_messages   = VALUE string_table( ( `First note` )
                                               ( `Second note` ) )
            i_open_by_id = `btnTexts`
            i_placement  = z2ui5_cl_popup_msgpopover=>cs_placement-bottom ) ).

      WHEN `POPOVER_EXCEPTION`.
        TRY.
            DATA(lv_dummy) = 1 / 0 ##NEEDED.
          CATCH cx_root INTO DATA(lx) ##NO_HANDLER.
        ENDTRY.
        client->nav_app_call( z2ui5_cl_popup_msgpopover=>factory( i_messages   = lx
                                                                  i_open_by_id = `btnException` ) ).

      WHEN `MESSAGES_OPEN`.
        client->popover_display( xml   = z2ui5_cl_popup_msgpopover=>popover_xml(
                                             i_items     = client->_bind( mt_msg )
                                             i_placement = z2ui5_cl_popup_msgpopover=>cs_placement-top )
                                 by_id = `btnMessages` ).

    ENDCASE.

  ENDMETHOD.

  METHOD view_display.

    DATA(ls_count) = z2ui5_cl_popup_msgpopover=>count_get( mt_msg ).

    DATA(view) = z2ui5_cl_ui5_view_builder=>factory(
                     )->ele( n = `View` ns = `mvc`
                     )->a( n = `xmlns` v = `sap.m`
                     )->a( n = `xmlns:mvc` v = `sap.ui.core.mvc`
                     )->a( n = `displayBlock` v = `true`
                     )->a( n = `height` v = `100%` ).

    DATA(page) = view->ele( `Shell`
                     )->ele( `Page`
                     )->a( n = `title` v = `abap2UI5 - Message Popover`
                     )->a( n = `navButtonPress` v = client->_event_nav_app_leave( )
                     )->a( n = `showNavButton` b = client->check_app_prev_stack( ) ).

    page->ele( `content`
        )->ele( `VBox`
        )->a( n = `class` v = `sapUiSmallMargin`
            )->tag( `Button`
            )->a( n = `id` v = `btnBapiret`
            )->a( n = `text` v = `BAPIRET2 messages`
            )->a( n = `press` v = client->_event( `POPOVER_BAPIRET` )
            )->tag( `Button`
            )->a( n = `id` v = `btnTexts`
            )->a( n = `text` v = `Texts`
            )->a( n = `press` v = client->_event( `POPOVER_TEXTS` )
            )->tag( `Button`
            )->a( n = `id` v = `btnException`
            )->a( n = `text` v = `Exception`
            )->a( n = `press` v = client->_event( `POPOVER_EXCEPTION` ) ).

    page->ele( `footer`
        )->ele( `OverflowToolbar`
            )->tag( `Button`
            )->a( n = `id` v = `btnMessages`
            )->a( n = `icon` v = ls_count-icon
            )->a( n = `type` v = COND #( WHEN ls_count-error > 0 THEN `Reject` ELSE `Default` )
            )->a( n = `text` t = |{ ls_count-total }|
            )->a( n = `press` v = client->_event( `MESSAGES_OPEN` ) ).

    client->view_display( view->stringify( ) ).

  ENDMETHOD.

ENDCLASS.
