"! Several values in one popup - the classic POPUP_GET_VALUES, as an
"! abap2UI5 popup app. A field is an input, a date, a checkbox, a listbox or
"! a multi listbox (cs_kind), required or read-only; OK checks the required
"! ones and leaves with the values the user entered:
"!   client->nav_app_call( z2ui5_cl_popup_get_values=>factory(
"!       i_fields = VALUE #( ( name = `CARRID` text = `Airline` required = abap_true )
"!                           ( name = `FLDATE` text = `Date` kind = z2ui5_cl_popup_get_values=>cs_kind-date ) ) ) ).
"!   ...
"!   DATA(lo_popup) = CAST z2ui5_cl_popup_get_values( client->get_app_prev( ) ).
"!   IF lo_popup->result( )-check_confirmed = abap_true.
"!     lv_carrid = lo_popup->result_value( `CARRID` ).
CLASS z2ui5_cl_popup_get_values DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
    " check_confirmed( ) - OK pressed; value( ) - REF TO ty_t_field, the
    " fields with the values entered (ms_result-fields, or result_value( ) for
    " one of them); value_string( ) - empty
    INTERFACES z2ui5_if_popup_result.

    CONSTANTS:
      BEGIN OF cs_event,
        confirmed TYPE string VALUE `z2ui5_cl_popup_get_values_confirmed`,
        canceled  TYPE string VALUE `z2ui5_cl_popup_get_values_canceled`,
      END OF cs_event.

    CONSTANTS:
      BEGIN OF cs_kind,
        input    TYPE string VALUE `INPUT`,
        date     TYPE string VALUE `DATE`,
        checkbox TYPE string VALUE `CHECKBOX`,
        listbox  TYPE string VALUE `LISTBOX`,
        multi    TYPE string VALUE `MULTI`,
      END OF cs_kind.

    TYPES:
      "! a field: value is the text of an input, a date as yyyyMMdd and the
      "! key of a listbox; flag the value of a checkbox; keys the keys picked
      "! in a multi listbox
      BEGIN OF ty_s_field,
        name      TYPE string,
        text      TYPE string,
        kind      TYPE string,
        value     TYPE string,
        flag      TYPE abap_bool,
        keys      TYPE string_table,
        required  TYPE abap_bool,
        read_only TYPE abap_bool,
      END OF ty_s_field.
    TYPES ty_t_field TYPE STANDARD TABLE OF ty_s_field WITH EMPTY KEY.

    TYPES:
      "! a value of the listbox (or multi listbox) of field name
      BEGIN OF ty_s_listbox,
        name TYPE string,
        key  TYPE string,
        text TYPE string,
      END OF ty_s_listbox.
    TYPES ty_t_listbox TYPE STANDARD TABLE OF ty_s_listbox WITH EMPTY KEY.

    TYPES:
      BEGIN OF ty_s_result,
        check_confirmed TYPE abap_bool,
        fields          TYPE ty_t_field,
      END OF ty_s_result.

    "! the fields - PUBLIC, the inputs are bound to them
    DATA mt_field TYPE ty_t_field.

    CLASS-METHODS factory
      IMPORTING
        i_fields              TYPE ty_t_field
        i_title               TYPE string DEFAULT `Popup Get Values`
        i_text                TYPE string OPTIONAL
        i_listbox             TYPE ty_t_listbox OPTIONAL
        i_button_text_confirm TYPE string DEFAULT `OK`
        i_button_text_cancel  TYPE string DEFAULT `Cancel`
        i_text_required       TYPE string DEFAULT `Fill in the required field &1`
        i_event_confirm       TYPE string DEFAULT cs_event-confirmed
        i_event_cancel        TYPE string DEFAULT cs_event-canceled
      RETURNING
        VALUE(r_result)       TYPE REF TO z2ui5_cl_popup_get_values.

    METHODS result
      RETURNING
        VALUE(result) TYPE ty_s_result.

    "! the value of field name - the flag of a checkbox as `X` or empty
    METHODS result_value
      IMPORTING
        name          TYPE clike
      RETURNING
        VALUE(result) TYPE string.

  PROTECTED SECTION.
    DATA client              TYPE REF TO z2ui5_if_client.

    DATA title               TYPE string.
    DATA question_text       TYPE string.
    DATA listbox             TYPE ty_t_listbox.
    DATA button_text_confirm TYPE string.
    DATA button_text_cancel  TYPE string.
    DATA text_required       TYPE string.
    DATA event_confirm       TYPE string.
    DATA event_canceled      TYPE string.
    DATA ms_result           TYPE ty_s_result.
    "! the message of a required field left empty
    DATA mv_error            TYPE string.
    "! the fields as the caller defined them - the server's own copy of
    "! mt_field, taken at the start: mt_field is bound to the browser, which
    "! can rewrite any of its cells, so a field's definition (name, kind,
    "! required, read-only) and the value of a read-only field come from here
    DATA mt_field_def        TYPE ty_t_field.

    METHODS view_display.

    METHODS render_fields
      IMPORTING
        node TYPE REF TO z2ui5_cl_ui5_view_builder.

    "! whether every required field has a value - else mv_error says which
    METHODS check_required
      RETURNING
        VALUE(result) TYPE abap_bool.

    "! mt_field rebuilt from mt_field_def with only what the user may change
    "! taken from the browser: the value of an editable field, of the kind
    "! its control binds, and for a listbox only one of its keys
    METHODS restore_field_definitions.

  PRIVATE SECTION.
ENDCLASS.


CLASS z2ui5_cl_popup_get_values IMPLEMENTATION.

  METHOD factory.

    r_result = NEW #( ).

    r_result->mt_field            = i_fields.
    r_result->title               = i_title.
    r_result->question_text       = i_text.
    r_result->listbox             = i_listbox.
    r_result->button_text_confirm = i_button_text_confirm.
    r_result->button_text_cancel  = i_button_text_cancel.
    r_result->text_required       = i_text_required.
    r_result->event_confirm       = i_event_confirm.
    r_result->event_canceled      = i_event_cancel.
    LOOP AT r_result->mt_field REFERENCE INTO DATA(lr_field).
      lr_field->name = to_upper( lr_field->name ).
      IF lr_field->kind IS INITIAL.
        lr_field->kind = cs_kind-input.
      ENDIF.
    ENDLOOP.
    LOOP AT r_result->listbox REFERENCE INTO DATA(lr_listbox).
      lr_listbox->name = to_upper( lr_listbox->name ).
    ENDLOOP.
    r_result->mt_field_def = r_result->mt_field.

  ENDMETHOD.

  METHOD result.

    result = ms_result.

  ENDMETHOD.

  METHOD result_value.

    DATA(lv_name) = to_upper( name ).
    ASSIGN ms_result-fields[ name = lv_name ] TO FIELD-SYMBOL(<ls_field>).
    IF <ls_field> IS NOT ASSIGNED.
      RETURN.
    ENDIF.
    result = COND #( WHEN <ls_field>-kind = cs_kind-checkbox THEN <ls_field>-flag ELSE <ls_field>-value ).

  ENDMETHOD.

  METHOD check_required.

    result = abap_true.
    LOOP AT mt_field REFERENCE INTO DATA(lr_field)
         WHERE required = abap_true AND kind <> cs_kind-checkbox.
      IF ( lr_field->kind = cs_kind-multi AND lr_field->keys IS INITIAL )
          OR ( lr_field->kind <> cs_kind-multi AND lr_field->value IS INITIAL ).
        mv_error = replace( val  = text_required
                            sub  = `&1`
                            with = lr_field->text ).
        result = abap_false.
        RETURN.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD restore_field_definitions.

    FIELD-SYMBOLS <ls_input> TYPE ty_s_field.

    DATA(lt_input) = mt_field.
    mt_field = mt_field_def.

    LOOP AT mt_field REFERENCE INTO DATA(lr_field) WHERE read_only = abap_false.
      DATA(lv_index) = sy-tabix.
      UNASSIGN <ls_input>.
      ASSIGN lt_input[ lv_index ] TO <ls_input>.
      IF <ls_input> IS NOT ASSIGNED.
        CONTINUE.
      ENDIF.

      CASE lr_field->kind.
        WHEN cs_kind-checkbox.
          lr_field->flag = xsdbool( <ls_input>-flag IS NOT INITIAL ).

        WHEN cs_kind-multi.
          CLEAR lr_field->keys.
          LOOP AT <ls_input>-keys INTO DATA(lv_key).
            IF line_exists( listbox[ name = lr_field->name
                                     key  = lv_key ] ).
              INSERT lv_key INTO TABLE lr_field->keys.
            ENDIF.
          ENDLOOP.

        WHEN cs_kind-listbox.
          IF <ls_input>-value IS INITIAL
             OR line_exists( listbox[ name = lr_field->name
                                      key  = <ls_input>-value ] ).
            lr_field->value = <ls_input>-value.
          ENDIF.

        WHEN OTHERS.
          lr_field->value = <ls_input>-value.
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.

  METHOD view_display.

    DATA(popup) = z2ui5_cl_ui5_view_builder=>factory(
                      )->ele( n = `FragmentDefinition` ns = `core`
                      )->a( n = `xmlns` v = `sap.m`
                      )->a( n = `xmlns:core` v = `sap.ui.core`
                      )->ele( `Dialog`
                      )->a( n = `title` t = title
                      )->a( n = `afterClose` v = client->_event( `BUTTON_CANCEL` ) ).

    DATA(content) = popup->ele( `content`
        )->ele( `VBox`
            )->a( n = `class` v = `sapUiSmallMargin` ).
    IF mv_error IS NOT INITIAL.
      content->tag( `MessageStrip`
          )->a( n = `text` t = mv_error
          )->a( n = `type` v = `Error`
          )->a( n = `class` v = `sapUiSmallMarginBottom` ).
    ENDIF.
    IF question_text IS NOT INITIAL.
      content->tag( `Text`
          )->a( n = `text` t = question_text
          )->a( n = `class` v = `sapUiSmallMarginBottom` ).
    ENDIF.
    render_fields( content ).

    popup->ele( `buttons`
        )->tag( `Button`
            )->a( n = `text` t = button_text_cancel
            )->a( n = `press` v = client->_event( `BUTTON_CANCEL` )
        )->tag( `Button`
            )->a( n = `text` t = button_text_confirm
            )->a( n = `press` v = client->_event( `BUTTON_CONFIRM` )
            )->a( n = `type` v = `Emphasized` ).

    client->popup_display( popup->stringify( ) ).

  ENDMETHOD.

  METHOD render_fields.

    DATA(form) = node->ele( n = `SimpleForm` ns = `form`
        )->a( n = `xmlns:form` v = `sap.ui.layout.form`
        )->a( n = `editable` b = abap_true
        )->a( n = `layout` v = `ResponsiveGridLayout`
        )->a( n = `labelSpanM` v = `4`
        )->a( n = `labelSpanL` v = `4` ).

    LOOP AT mt_field REFERENCE INTO DATA(lr_field).
      DATA(lv_index) = sy-tabix.

      form->tag( `Label`
          )->a( n = `text` t = lr_field->text
          )->a( n = `required` b = lr_field->required ).

      CASE lr_field->kind.
        WHEN cs_kind-checkbox.
          form->tag( `CheckBox`
              )->a( n = `selected` v = client->_bind( val       = lr_field->flag
                                                     tab       = mt_field
                                                     tab_index = lv_index )
              )->a( n = `editable` b = xsdbool( lr_field->read_only = abap_false ) ).

        WHEN cs_kind-date.
          form->tag( `DatePicker`
              )->a( n = `value` v = client->_bind( val       = lr_field->value
                                                  tab       = mt_field
                                                  tab_index = lv_index )
              )->a( n = `valueFormat` v = `yyyyMMdd`
              )->a( n = `editable` b = xsdbool( lr_field->read_only = abap_false ) ).

        WHEN cs_kind-multi.
          DATA(multi) = form->ele( `MultiComboBox`
              )->a( n = `selectedKeys` v = client->_bind( val       = lr_field->keys
                                                         tab       = mt_field
                                                         tab_index = lv_index )
              )->a( n = `editable` b = xsdbool( lr_field->read_only = abap_false )
              )->ele( `items` ).
          LOOP AT listbox REFERENCE INTO DATA(lr_multi) WHERE name = lr_field->name.
            multi->tag( n = `Item` ns = `core`
                )->a( n = `key` t = lr_multi->key
                )->a( n = `text` t = lr_multi->text ).
          ENDLOOP.

        WHEN cs_kind-listbox.
          DATA(items) = form->ele( `Select`
              )->a( n = `selectedKey` v = client->_bind( val       = lr_field->value
                                                        tab       = mt_field
                                                        tab_index = lv_index )
              )->a( n = `forceSelection` b = abap_false
              )->a( n = `editable` b = xsdbool( lr_field->read_only = abap_false )
              )->ele( `items` ).
          items->tag( n = `Item` ns = `core`
              )->a( n = `key` v = ``
              )->a( n = `text` v = `` ).
          LOOP AT listbox REFERENCE INTO DATA(lr_value) WHERE name = lr_field->name.
            items->tag( n = `Item` ns = `core`
                )->a( n = `key` t = lr_value->key
                )->a( n = `text` t = lr_value->text ).
          ENDLOOP.

        WHEN OTHERS.
          form->tag( `Input`
              )->a( n = `value` v = client->_bind( val       = lr_field->value
                                                  tab       = mt_field
                                                  tab_index = lv_index )
              )->a( n = `editable` b = xsdbool( lr_field->read_only = abap_false ) ).
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.

  METHOD z2ui5_if_app~main.

    me->client = client.

    IF client->check_on_init( ).
      " the caller may have changed the public mt_field after factory( )
      mt_field_def = mt_field.
      view_display( ).
      RETURN.
    ELSEIF client->check_on_navigated( ).
      " a restored draft (browser Forward, a reload): the popup went with
      " the page - show it again
      view_display( ).
      RETURN.
    ENDIF.

    CLEAR mv_error.
    restore_field_definitions( ).
    CASE client->get( )-event.
      WHEN `BUTTON_CONFIRM`.
        IF check_required( ) = abap_false.
          view_display( ).
          RETURN.
        ENDIF.
        ms_result-check_confirmed = abap_true.
        ms_result-fields = mt_field.
        client->popup_destroy( ).
        client->nav_app_leave( event = event_confirm ).

      WHEN `BUTTON_CANCEL`.
        ms_result-check_confirmed = abap_false.
        ms_result-fields = mt_field.
        client->popup_destroy( ).
        client->nav_app_leave( event = event_canceled ).
    ENDCASE.

  ENDMETHOD.

  METHOD z2ui5_if_popup_result~check_confirmed.

    result = ms_result-check_confirmed.

  ENDMETHOD.

  METHOD z2ui5_if_popup_result~value.

    result = REF #( ms_result-fields ).

  ENDMETHOD.

  METHOD z2ui5_if_popup_result~value_string.

    " no single value - empty
    CLEAR result.

  ENDMETHOD.

ENDCLASS.
