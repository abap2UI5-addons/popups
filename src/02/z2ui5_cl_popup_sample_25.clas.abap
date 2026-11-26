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
      DATA temp1 TYPE bapirettab.
      DATA temp2 LIKE LINE OF temp1.

    me->client = client.

    " the popover hands control back with its event AND check_on_navigated( )
    " at once, so the event arm comes before the navigated arm
    IF client->check_on_init( ) IS NOT INITIAL.

      CLEAR temp1.

      temp2-type = `E`.
      temp2-id = `MSG1`.
      temp2-number = `001`.
      temp2-message = `Storage bin is blocked`.
      INSERT temp2 INTO TABLE temp1.
      temp2-type = `W`.
      temp2-id = `MSG1`.
      temp2-number = `002`.
      temp2-message = `Quantity exceeds the planned quantity`.
      INSERT temp2 INTO TABLE temp1.
      temp2-type = `S`.
      temp2-id = `MSG1`.
      temp2-number = `003`.
      temp2-message = `Handling unit created`.
      INSERT temp2 INTO TABLE temp1.
      mt_msg = z2ui5_cl_popup_msgpopover=>msg_get_t( temp1 ).
      view_display( ).

    " abap2ui5lint-disable-next-line handler-without-event -- raised by z2ui5_cl_popup_msgpopover, named in i_event_close
    ELSEIF client->check_on_event( `POPOVER_CLOSED` ) IS NOT INITIAL.
      client->message_toast_display( `Message popover closed` ).

    ELSEIF client->check_on_navigated( ) IS NOT INITIAL.
      view_display( ).

    ELSE.
      on_event( ).
    ENDIF.

  ENDMETHOD.

  METHOD on_event.
        DATA temp3 TYPE bapirettab.
        DATA temp4 LIKE LINE OF temp3.
        DATA temp5 TYPE string_table.
            DATA lv_dummy TYPE i.
            DATA lx TYPE REF TO cx_root.

    CASE client->get( )-event.

      WHEN `POPOVER_BAPIRET`.

        CLEAR temp3.

        temp4-type = `E`.
        temp4-id = `MSG1`.
        temp4-number = `001`.
        temp4-message = `An empty Report field causes an empty XML Message to be sent`.
        INSERT temp4 INTO TABLE temp3.
        temp4-type = `W`.
        temp4-id = `MSG2`.
        temp4-number = `002`.
        temp4-message = `Product already in use`.
        INSERT temp4 INTO TABLE temp3.
        temp4-type = `S`.
        temp4-id = `MSG3`.
        temp4-number = `003`.
        temp4-message = `Order saved`.
        INSERT temp4 INTO TABLE temp3.
        temp4-type = `I`.
        temp4-id = `MSG4`.
        temp4-number = `004`.
        temp4-message = `Delivery date moved`.
        INSERT temp4 INTO TABLE temp3.
        client->nav_app_call( z2ui5_cl_popup_msgpopover=>factory(
            i_messages    = temp3
            i_open_by_id  = `btnBapiret`
            i_event_close = `POPOVER_CLOSED` ) ).

      WHEN `POPOVER_TEXTS`.

        CLEAR temp5.
        INSERT `First note` INTO TABLE temp5.
        INSERT `Second note` INTO TABLE temp5.
        client->nav_app_call( z2ui5_cl_popup_msgpopover=>factory(
            i_messages   = temp5
            i_open_by_id = `btnTexts`
            i_placement  = z2ui5_cl_popup_msgpopover=>cs_placement-bottom ) ).

      WHEN `POPOVER_EXCEPTION`.
        TRY.

            lv_dummy = 1 / 0 ##NEEDED.

          CATCH cx_root INTO lx.
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

    DATA ls_count TYPE z2ui5_cl_popup_msgpopover=>ty_s_count.
    DATA view TYPE REF TO z2ui5_cl_ui5_view_builder.
    DATA page TYPE REF TO z2ui5_cl_ui5_view_builder.
    DATA temp7 TYPE string.
    ls_count = z2ui5_cl_popup_msgpopover=>count_get( mt_msg ).


    view = z2ui5_cl_ui5_view_builder=>factory(
                     )->ele( n = `View` ns = `mvc`
                     )->a( n = `xmlns` v = `sap.m`
                     )->a( n = `xmlns:mvc` v = `sap.ui.core.mvc`
                     )->a( n = `displayBlock` v = `true`
                     )->a( n = `height` v = `100%` ).


    page = view->ele( `Shell`
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


    IF ls_count-error > 0.
      temp7 = `Reject`.
    ELSE.
      temp7 = `Default`.
    ENDIF.
    page->ele( `footer`
        )->ele( `OverflowToolbar`
            )->tag( `Button`
            )->a( n = `id` v = `btnMessages`
            )->a( n = `icon` v = ls_count-icon
            )->a( n = `type` v = temp7
            )->a( n = `text` t = |{ ls_count-total }|
            )->a( n = `press` v = client->_event( `MESSAGES_OPEN` ) ).

    client->view_display( view->stringify( ) ).

  ENDMETHOD.

ENDCLASS.
