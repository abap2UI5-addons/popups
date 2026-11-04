"! A question with a button per option - the classic POPUP_TO_DECIDE (two
"! options) and POPUP_TO_DECIDE_LIST, as an abap2UI5 popup app. The answer
"! is the number of the button pressed, `1`, `2`, ..., or cs_answer-cancel:
"!   client->nav_app_call( z2ui5_cl_popup_to_decide=>factory(
"!       i_question_text = `Save the changes?`
"!       i_options       = VALUE #( ( `Save` ) ( `Discard` ) ) ) ).
"!   ...
"!   DATA(lv_answer) = CAST z2ui5_cl_popup_to_decide( client->get_app_prev( ) )->result( ).
CLASS z2ui5_cl_popup_to_decide DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.

    CONSTANTS:
      BEGIN OF cs_event,
        decided  TYPE string VALUE `z2ui5_cl_popup_to_decide_decided`,
        canceled TYPE string VALUE `z2ui5_cl_popup_to_decide_canceled`,
      END OF cs_event.

    CONSTANTS:
      BEGIN OF cs_answer,
        cancel TYPE string VALUE `A`,
      END OF cs_answer.

    CLASS-METHODS factory
      IMPORTING
        i_question_text      TYPE string
        i_options            TYPE string_table
        i_title              TYPE string    DEFAULT `Popup To Decide`
        i_icon               TYPE string    DEFAULT `sap-icon://question-mark`
        i_cancel             TYPE abap_bool DEFAULT abap_true
        i_button_text_cancel TYPE string    DEFAULT `Cancel`
        i_event_decided      TYPE string    DEFAULT cs_event-decided
        i_event_cancel       TYPE string    DEFAULT cs_event-canceled
      RETURNING
        VALUE(r_result)      TYPE REF TO z2ui5_cl_popup_to_decide.

    "! the number of the option chosen (`1` for the first), or
    "! cs_answer-cancel
    METHODS result
      RETURNING
        VALUE(result) TYPE string.

  PROTECTED SECTION.
    DATA client             TYPE REF TO z2ui5_if_client.

    DATA title              TYPE string.
    DATA icon               TYPE string.
    DATA question_text      TYPE string.
    DATA options            TYPE string_table.
    DATA check_cancel       TYPE abap_bool.
    DATA button_text_cancel TYPE string.
    DATA event_decided      TYPE string.
    DATA event_canceled     TYPE string.
    DATA answer             TYPE string.

    METHODS view_display.

  PRIVATE SECTION.
    CONSTANTS cv_event_option TYPE string VALUE `OPTION_`.

ENDCLASS.


CLASS z2ui5_cl_popup_to_decide IMPLEMENTATION.

  METHOD factory.

    CREATE OBJECT r_result.

    r_result->title              = i_title.
    r_result->icon               = i_icon.
    r_result->question_text      = i_question_text.
    r_result->options            = i_options.
    r_result->check_cancel       = i_cancel.
    r_result->button_text_cancel = i_button_text_cancel.
    r_result->event_decided      = i_event_decided.
    r_result->event_canceled     = i_event_cancel.
    r_result->answer             = cs_answer-cancel.

  ENDMETHOD.

  METHOD result.

    result = answer.

  ENDMETHOD.

  METHOD view_display.

    DATA popup TYPE REF TO z2ui5_cl_ui5_view_builder.
    DATA buttons TYPE REF TO z2ui5_cl_ui5_view_builder.
    DATA lv_option LIKE LINE OF options.
      DATA lv_number LIKE sy-tabix.
      DATA temp10 TYPE string.
    popup = z2ui5_cl_ui5_view_builder=>factory(
                      )->ele( n = `FragmentDefinition` ns = `core`
                      )->a( n = `xmlns` v = `sap.m`
                      )->a( n = `xmlns:core` v = `sap.ui.core`
                      )->ele( `Dialog`
                      )->a( n = `title` t = title
                      )->a( n = `icon` v = icon
                      )->a( n = `afterClose` v = client->_event( `BUTTON_CANCEL` ) ).

    popup->ele( `content`
        )->ele( `VBox`
            )->a( n = `class` v = `sapUiMediumMargin`
            )->tag( `Text`
                )->a( n = `text` t = question_text ).


    buttons = popup->ele( `buttons` ).

    LOOP AT options INTO lv_option.
      " sy-tabix is read once: every a( ) below appends and moves it

      lv_number = sy-tabix.

      IF lv_number = 1.
        temp10 = `Emphasized`.
      ELSE.
        temp10 = `Default`.
      ENDIF.
      buttons->tag( `Button`
          )->a( n = `text` t = lv_option
          )->a( n = `type` v = temp10
          )->a( n = `press` v = client->_event( |{ cv_event_option }{ lv_number }| ) ).
    ENDLOOP.
    IF check_cancel = abap_true.
      buttons->tag( `Button`
          )->a( n = `text` t = button_text_cancel
          )->a( n = `press` v = client->_event( `BUTTON_CANCEL` ) ).
    ENDIF.

    client->popup_display( popup->stringify( ) ).

  ENDMETHOD.

  METHOD z2ui5_if_app~main.
    DATA lv_event TYPE z2ui5_if_client=>ty_s_get-event.
        DATA lv_number TYPE string.
        DATA temp11 TYPE i.
        DATA temp2 TYPE i.

    me->client = client.

    IF client->check_on_init( ) IS NOT INITIAL.
      view_display( ).
      RETURN.
    ENDIF.


    lv_event = client->get( )-event.
    CASE lv_event.
      WHEN `BUTTON_CANCEL`.
        answer = cs_answer-cancel.
        client->popup_destroy( ).
        client->nav_app_leave( app   = client->get_app_prev( )
                               event = event_canceled ).

      WHEN OTHERS.
        IF strlen( lv_event ) <= strlen( cv_event_option ) OR lv_event NP |{ cv_event_option }*|.
          RETURN.
        ENDIF.

        lv_number = substring( val = lv_event
                                     off = strlen( cv_event_option ) ).
        " only a button of the popup - the event comes from the browser

        temp11 = lv_number.

        temp2 = lv_number.
        IF lv_number CN `0123456789` OR temp11 < 1 OR temp2 > lines( options ).
          RETURN.
        ENDIF.
        answer = lv_number.
        client->popup_destroy( ).
        client->nav_app_leave( app   = client->get_app_prev( )
                               event = event_decided ).
    ENDCASE.

  ENDMETHOD.

ENDCLASS.
